package io.linewise.petstore

import io.linewise.petstore.generated.JdbcSupport.Conn

/* =============================================================================
 * PRODUCTION ambient-connection provider — the real counterpart of the verify-only
 * stub. The field-less, transpiler-generated JDBC repositories (JdbcPetRepository
 * etc.) fetch their connection here, per operation, instead of storing it (a stored
 * Conn would have made the abstract repo — and the World — mutable, which the Has
 * lens forbids in the verifier).
 *
 * The connection is bound per thread (a request / transaction scope). `withConn`
 * binds it for the duration of a body and restores the previous binding; the demo
 * and the differential test wrap their JDBC-world calls in it.
 * ========================================================================== */
object ConnProvider:
  private val tl = new ThreadLocal[Conn]()

  def withConn[A](c: Conn)(body: => A): A =
    val prev = tl.get()
    tl.set(c)
    try body
    finally if prev == null then tl.remove() else tl.set(prev)

  def conn(): Conn =
    val c = tl.get()
    if c == null then throw new IllegalStateException(
      "no ambient connection bound — wrap JDBC-repository calls in ConnProvider.withConn(conn) { ... }")
    else c
