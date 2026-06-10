package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import ObligationTables._
import LedgerInvariants._

/* =============================================================================
 * OBLIGATION PROOFS — VERIFY-ONLY. The obligation slice on the relational DB: the composite-key
 * upsert (drop-same-key then prepend) preserves key distinctness, and the written row is visible by
 * its source key. The explicit filter-preserves-distinctness reasoning the aggregate gave for free.
 * ========================================================================== */
object ObligationProofs {

  private val store = InMemObligationStore()

  @induct def dropKeyRemoves(os: List[ObligationRow], sk: String, si: String): Unit = { () }
    .ensuring(_ => noKey(dropKey(os, sk, si), sk, si))

  @induct def dropKeyPreservesNoKey(os: List[ObligationRow], sk: String, si: String, sk2: String, si2: String): Unit = {
    require(noKey(os, sk2, si2)); ()
  }.ensuring(_ => noKey(dropKey(os, sk, si), sk2, si2))

  def dropKeyPreservesDistinct(os: List[ObligationRow], sk: String, si: String): Unit = {
    require(distinctObligations(os))
    os match
      case Nil() => ()
      case Cons(o, t) =>
        dropKeyPreservesDistinct(t, sk, si)
        dropKeyPreservesNoKey(t, sk, si, o.sourceKind, o.sourceId)
  }.ensuring(_ => distinctObligations(dropKey(os, sk, si)))

  // ---- upsert preserves composite-key distinctness ----
  def putPreservesDistinct(db: DB, o: Obligation): Unit = {
    require(distinctObligations(db.obligations))
    dropKeyPreservesDistinct(db.obligations, o.sourceKind, o.sourceId)
    dropKeyRemoves(db.obligations, o.sourceKind, o.sourceId)
  }.ensuring(_ => validObligations(store.put(db, o)))

  // ---- the written row is visible by its source key ----
  def putVisibleBySource(db: DB, o: Obligation): Boolean = {
    store.bySource(store.put(db, o), o.sourceKind, o.sourceId) ==
      Some[Obligation](assembleO(toORow(o)))
  }.holds
}
