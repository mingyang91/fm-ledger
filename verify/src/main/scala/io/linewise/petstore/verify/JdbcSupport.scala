package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import io.linewise.verify.effect.FMLong

/* =============================================================================
 * SHARED JDBC SUPPORT for the @extern real-per-op-JDBC repository realizations.
 *   - Conn wraps the JDK java.sql.Connection behind an @extern field (Stainless
 *     does not model java.sql; an @extern field hides it).
 *   - fmFromLong / longOfFM are the modeled-Long shim: Stainless's BigInt has no
 *     apply(Long) and no native Long, so JDBC's getLong/setLong are bridged through
 *     String/BigInt. In production these erase to Long identities.
 * Transpiler input; @extern bodies are kept (real), the FM types erase to native.
 * ========================================================================== */
object JdbcSupport {
  class Conn(@extern val underlying: java.sql.Connection)

  @extern @pure def fmFromLong(l: Long): FMLong = FMLong(BigInt(l.toString))
  @extern @pure def longOfFM(f: FMLong): Long = f.value.toLong
}
