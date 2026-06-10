package io.linewise.verify.fm.ledger.relational

import stainless.lang.*
import stainless.collection.*
import stainless.annotation.*
import io.linewise.verify.effect.FMLong
import io.linewise.verify.fm.ledger.LedgerModel.ObligationStatus

/* =============================================================================
 * RELATIONAL FM — Phase 2c: the OBLIGATION slice as a single table with a COMPOSITE PK
 *   (sourceKind, sourceId). Status is a column; open/cancel/realize is an UPSERT (drop same-key,
 *   prepend). Proven: upsert reflects in bySource, and upsert preserves composite-key distinctness
 *   (the explicit "filter preserves distinctness" reasoning the aggregate gave for free).
 * ========================================================================== */
object RelObligation {

  case class ObligationRow(sourceKind: String, sourceId: String, userUid: String, estimatedPoints: FMLong, status: ObligationStatus, realizedTxId: Option[BigInt]) {
    require(estimatedPoints.value >= BigInt(0))
  }
  case class ObligationTables(obligations: List[ObligationRow])
  // (Has-per-table lens deferred to Phase 4, as in RelLedger.)

  def dropKey(obs: List[ObligationRow], sk: String, si: String): List[ObligationRow] =
    obs.filter(x => !(x.sourceKind == sk && x.sourceId == si))
  def bySource(obs: List[ObligationRow], sk: String, si: String): Option[ObligationRow] =
    obs.find(x => x.sourceKind == sk && x.sourceId == si)

  def noKey(obs: List[ObligationRow], sk: String, si: String): Boolean = obs match
    case Nil()      => true
    case Cons(o, t) => !(o.sourceKind == sk && o.sourceId == si) && noKey(t, sk, si)
  def distinctKeys(obs: List[ObligationRow]): Boolean = obs match
    case Nil()      => true
    case Cons(o, t) => noKey(t, o.sourceKind, o.sourceId) && distinctKeys(t)

  def put(obs: List[ObligationRow], o: ObligationRow): List[ObligationRow] =
    o :: dropKey(obs, o.sourceKind, o.sourceId)
  def upsert(w: ObligationTables, o: ObligationRow): ObligationTables =
    w.copy(obligations = put(w.obligations, o))
  def distinct(w: ObligationTables): Boolean = distinctKeys(w.obligations)

  // dropKey removes every row with that key
  @induct def dropKeyRemoves(obs: List[ObligationRow], sk: String, si: String): Unit = { () }
    .ensuring(_ => noKey(dropKey(obs, sk, si), sk, si))
  // dropping one key cannot introduce another key (subset is monotone for noKey)
  @induct def dropKeyPreservesNoKey(obs: List[ObligationRow], sk: String, si: String, sk2: String, si2: String): Unit = {
    require(noKey(obs, sk2, si2)); ()
  }.ensuring(_ => noKey(dropKey(obs, sk, si), sk2, si2))
  // filtering a distinct table stays distinct
  def dropKeyPreservesDistinct(obs: List[ObligationRow], sk: String, si: String): Unit = {
    require(distinctKeys(obs))
    obs match
      case Nil() => ()
      case Cons(o, t) =>
        dropKeyPreservesDistinct(t, sk, si)
        dropKeyPreservesNoKey(t, sk, si, o.sourceKind, o.sourceId)
  }.ensuring(_ => distinctKeys(dropKey(obs, sk, si)))

  // ---- PROOF: upsert reflects in bySource (the view shows the written row) ----
  def upsertVisibleBySource(w: ObligationTables, o: ObligationRow): Boolean = {
    bySource(upsert(w, o).obligations, o.sourceKind, o.sourceId) == Some[ObligationRow](o)
  }.holds

  // ---- PROOF: upsert preserves composite-key distinctness ----
  def upsertPreservesDistinct(w: ObligationTables, o: ObligationRow): Boolean = {
    require(distinct(w))
    dropKeyPreservesDistinct(w.obligations, o.sourceKind, o.sourceId)
    dropKeyRemoves(w.obligations, o.sourceKind, o.sourceId)
    distinct(upsert(w, o))
  }.holds
}
