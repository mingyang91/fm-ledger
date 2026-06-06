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
 *
 * Run: ./mill runMain io.linewise.ledger.LedgerServer
 * ========================================================================== */
object LedgerServer:
  def main(args: Array[String]): Unit =
    val ds = Jdbc.dataSource("ledger")
    val c0 = ds.getConnection()
    try Jdbc.initSchema(c0) finally c0.close()
    Db.init(ds) // bind the JDBC store the generated @extern repository delegates to
    val api   = LedgerApi()
    val admin = api.adminToken("bootstrap")
    println("Ledger microservice (direct-style Ox tapir, Stainless-verified core) on http://localhost:8081  — Ctrl-C to stop")
    println(s"Bootstrap admin bearer token: $admin")
    NettySyncServer().host("localhost").port(8081).addEndpoints(api.serverEndpoints).startAndWait()
