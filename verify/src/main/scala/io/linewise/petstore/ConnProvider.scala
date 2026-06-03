package io.linewise.petstore

import stainless.annotation._
import io.linewise.verify.fm.petstore.JdbcSupport.Conn

/* =============================================================================
 * AMBIENT CONNECTION PROVIDER — verify-only stub (NOT a transpiler input).
 *
 * The JDBC repository realizations are FIELD-LESS so they stay immutable in
 * Stainless (a stored Conn would taint the whole abstract hierarchy, and thus the
 * World, mutable — which the Has lens forbids). Instead each @extern op fetches the
 * ambient connection here, INSIDE its havoc'd body, so the connection never appears
 * in a Stainless-visible field or signature.
 *
 * This object lives in the production-shaped package `io.linewise.petstore` (NOT
 * `io.linewise.verify.*`), so the generated code references it verbatim and resolves
 * to the HAND-WRITTEN production `ConnProvider` (a real pooled/contextual connection)
 * — no transpiler import rule needed. `conn()` is @extern: its `???` body is havoc'd
 * in verification and never executed (the repo bodies that call it are themselves
 * @extern), and this file is never transpiled, so the `???` cannot reach production.
 *
 * ACCEPTED RISK (the user's call): the @extern repo ops are TRUSTED pure and linear
 * in the connection; the in-memory-vs-JDBC differential test is the machine-checked
 * guard that the ambient-connection realization agrees with the verified oracle.
 * ========================================================================== */
object ConnProvider {
  @extern def conn(): Conn = ???
}
