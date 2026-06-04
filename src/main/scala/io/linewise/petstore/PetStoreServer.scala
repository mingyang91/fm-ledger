package io.linewise.petstore

import sttp.tapir.server.netty.sync.NettySyncServer

/* =============================================================================
 * Runnable pet-store web server: direct-style (Ox) Netty, JDBC-backed.
 * Run: ./mill runMain io.linewise.petstore.PetStoreServer
 * ========================================================================== */
object PetStoreServer:
  def main(args: Array[String]): Unit =
    val ds = Jdbc.dataSource("petstore")
    val c0 = ds.getConnection()
    try Jdbc.initSchema(c0) finally c0.close()
    Db.init(ds) // bind the Quill DAO over the pooled DataSource
    val api = PetStoreApi(JdbcBackend())
    println("Pet-store tapir server (direct-style Ox) on http://localhost:8080  — Ctrl-C to stop")
    NettySyncServer().host("localhost").port(8080).addEndpoints(api.serverEndpoints).startAndWait()
