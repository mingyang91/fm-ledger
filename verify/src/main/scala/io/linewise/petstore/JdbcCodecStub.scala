package io.linewise.petstore

import stainless.annotation._
import stainless.collection.List

/* =============================================================================
 * VERIFY-ONLY STUB for the list<->VARCHAR codec the JDBC pet repository uses inside
 * its @extern bodies (tags / photoUrls are single VARCHAR columns). Mirrors the
 * signatures of the REAL production `io.linewise.petstore.Jdbc` (Jdbc.scala in
 * src/main): the generated code references `io.linewise.petstore.Jdbc.encodeList /
 * decodeList` verbatim and resolves to the real ones. Both are @extern: the `???`
 * bodies are havoc'd in verification and never executed (the repo bodies that call
 * them are themselves @extern), and this stub is never transpiled.
 * ========================================================================== */
object Jdbc {
  @extern def encodeList(xs: List[String]): String = ???
  @extern def decodeList(s: String): List[String] = ???
}
