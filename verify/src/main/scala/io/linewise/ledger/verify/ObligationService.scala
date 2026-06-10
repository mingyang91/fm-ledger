package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import ObligationTables._

/* =============================================================================
 * OBLIGATION SERVICE — open/cancel/realize over the DB's composite-key OBLIGATION table, through the
 * single Has[W, DB] lens and the obligation store. Terminal states are sticky; open is idempotent.
 * ========================================================================== */
case class ObligationService[W](has: Has[W, DB], store: ObligationStore) {

  def open(
      w: W, sourceKind: String, sourceId: String, userUid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): (W, Either[LedgerError, Obligation]) = {
    val db = has.get(w)
    if !(estimatedUnit > FMLong(BigInt(0))) then (w, Left[LedgerError, Obligation](NonPositiveAmount))
    else store.bySource(db, sourceKind, sourceId) match
      case Some(o) =>
        if o.status != ObligationStatus.Open then (w, Left[LedgerError, Obligation](SourceTerminal))
        else (w, Right[LedgerError, Obligation](o))
      case _ =>
        val o = Obligation(sourceKind, sourceId, userUid, role, projectRef, taskKind, estimatedUnit, ObligationStatus.Open, None[FMLong]())
        (has(w).write((d: DB) => store.put(d, o)), Right[LedgerError, Obligation](o))
  }

  def cancel(w: W, sourceKind: String, sourceId: String): (W, Either[LedgerError, Obligation]) = {
    val db = has.get(w)
    store.bySource(db, sourceKind, sourceId) match
      case Some(o) =>
        if o.status == ObligationStatus.Realized then (w, Left[LedgerError, Obligation](SourceTerminal))
        else
          val o2 = o.copy(status = ObligationStatus.Cancelled, realizedTxId = None[FMLong]())
          (has(w).write((d: DB) => store.put(d, o2)), Right[LedgerError, Obligation](o2))
      case _ =>
        val o = Obligation(sourceKind, sourceId, "", "", "", "", FMLong(BigInt(0)), ObligationStatus.Cancelled, None[FMLong]())
        (has(w).write((d: DB) => store.put(d, o)), Right[LedgerError, Obligation](o))
  }

  def realize(
      w: W, sourceKind: String, sourceId: String, userUid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong, txId: FMLong): (W, Obligation) = {
    val db = has.get(w)
    val o2 = store.bySource(db, sourceKind, sourceId) match
      case Some(o) if o.status == ObligationStatus.Cancelled => o
      case Some(o) => o.copy(status = ObligationStatus.Realized, realizedTxId = Some[FMLong](txId))
      case _       => Obligation(sourceKind, sourceId, userUid, role, projectRef, taskKind, estimatedUnit, ObligationStatus.Realized, Some[FMLong](txId))
    (has(w).write((d: DB) => store.put(d, o2)), o2)
  }

  def bySource(w: W, kind: String, id: String): Option[Obligation] = store.bySource(has.get(w), kind, id)
  def openObligations(w: W): List[Obligation] = store.allOpen(has.get(w))
}
