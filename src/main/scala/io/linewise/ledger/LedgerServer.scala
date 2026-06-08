package io.linewise.ledger

import sttp.tapir.server.netty.sync.NettySyncServer

/* =============================================================================
 * Runnable ledger microservice: direct-style (Ox) Netty, PostgreSQL-backed,
 * business logic supplied by the Stainless-transpiled LedgerService.
 *
 * Configuration:
 *   LEDGER_JDBC_URL      default jdbc:postgresql://localhost:5432/ledger
 *   LEDGER_DB_USER       default postgres
 *   LEDGER_DB_PASSWORD   default postgres
 *   LEDGER_DB_SCHEMA     optional schema, created on startup when set
 *   STRIPE_WEBHOOK_SECRET           whsec_… signing secret for the Stripe webhook (no default;
 *                                   absent it falls back to a per-process random secret).
 *                                   LEDGER_WEBHOOK_SECRET is accepted as a back-compat alias.
 *   STRIPE_API_KEY                  sk_… secret key; when set, the payout dispatcher runs and
 *                                   creates Connect transfers. Unset: dispatch rows queue.
 *   STRIPE_API_BASE_URL             default https://api.stripe.com (point at stripe-mock in dev/tests)
 *   STRIPE_API_VERSION              default 2024-06-20
 *   LEDGER_PRINT_BOOTSTRAP_TOKEN=1  mint and print a one-off admin bearer token (dev only)
 *
 * Run: ./mill runMain io.linewise.ledger.LedgerServer
 * ========================================================================== */
object LedgerServer:
  def main(args: Array[String]): Unit =
    val ds = Jdbc.dataSource("ledger")
    val c0 = ds.getConnection()
    try Jdbc.initSchema(c0) finally c0.close()
    Db.init(ds) // bind the JDBC store the generated @extern repository delegates to
    // Load the webhook signing secret (whsec_…) into config; never ship a known one.
    // STRIPE_WEBHOOK_SECRET is the real name; LEDGER_WEBHOOK_SECRET stays as a back-compat alias.
    val webhookSecret = sys.env.get("STRIPE_WEBHOOK_SECRET").orElse(sys.env.get("LEDGER_WEBHOOK_SECRET"))
    webhookSecret.foreach(s => Db.setSystemConfig("stripe_webhook_secret", s, "bootstrap from env", "system"))
    if webhookSecret.isEmpty then
      System.err.println("WARNING: STRIPE_WEBHOOK_SECRET unset — payout webhook verification uses an ephemeral per-process secret")
    val api = LedgerApi()
    // Start the outbox dispatcher only when a Stripe API key is present. Without it, submit still
    // records dispatch rows (drained once a key is configured) but no outbound call is made.
    sys.env.get("STRIPE_API_KEY") match
      case Some(key) =>
        val base = sys.env.getOrElse("STRIPE_API_BASE_URL", "https://api.stripe.com")
        val ver  = sys.env.getOrElse("STRIPE_API_VERSION", "2024-06-20")
        Db.reclaimStaleInflight(0) // startup reaper: rows left 'inflight' by a prior crashed process are orphans
        PayoutDispatcher(StripeGateway(key, base, ver), onPermanentFailure = api.failWithdrawalForFailedDispatch).run()
        println(s"Payout dispatcher started (Stripe at $base)")
      case None =>
        System.err.println("WARNING: STRIPE_API_KEY unset — payout dispatcher not started; dispatch rows will queue")
    println("Ledger microservice (direct-style Ox tapir, Stainless-verified core) on http://localhost:8081  — Ctrl-C to stop")
    // A bootstrap admin token is a permanent credential; only mint+print it on request.
    if sys.env.get("LEDGER_PRINT_BOOTSTRAP_TOKEN").contains("1") then
      println(s"Bootstrap admin bearer token: ${api.adminToken("bootstrap")}")
    NettySyncServer().host("localhost").port(8081).addEndpoints(api.serverEndpoints).startAndWait()
