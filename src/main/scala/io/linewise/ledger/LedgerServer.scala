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
 *   LEDGER_WEBHOOK_SECRET           shared HMAC secret for the payout webhook (no default;
 *                                   absent it falls back to a per-process random secret)
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
    // Load the webhook secret from the environment into config; never ship a known one.
    sys.env.get("LEDGER_WEBHOOK_SECRET").foreach(s =>
      Db.setSystemConfig("stripe_webhook_secret", s, "bootstrap from env", "system"))
    if sys.env.get("LEDGER_WEBHOOK_SECRET").isEmpty then
      System.err.println("WARNING: LEDGER_WEBHOOK_SECRET unset — payout webhook verification uses an ephemeral per-process secret")
    val api = LedgerApi()
    println("Ledger microservice (direct-style Ox tapir, Stainless-verified core) on http://localhost:8081  — Ctrl-C to stop")
    // A bootstrap admin token is a permanent credential; only mint+print it on request.
    if sys.env.get("LEDGER_PRINT_BOOTSTRAP_TOKEN").contains("1") then
      println(s"Bootstrap admin bearer token: ${api.adminToken("bootstrap")}")
    NettySyncServer().host("localhost").port(8081).addEndpoints(api.serverEndpoints).startAndWait()
