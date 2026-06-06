package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * OBLIGATION SERVICE — verified open/cancel/realize over the obligation slice.
 *   open    -> insert an `open` row; idempotent replay; REFUSE a terminal source
 *   cancel  -> open -> cancelled (or a tombstone for an unknown source, so a late
 *              open is refused); REFUSE cancelling a realized source
 *   realize -> upsert a `realized` row (flips an open row, or lands directly when the
 *              credit beat the publish) — called on the credit path so the source
 *              leaves "upcoming" once it mints. The cross-entity credit+realize
 *              atomicity is a production concern; here the lifecycle is verified.
 * ========================================================================== */
case class ObligationService[W](oLens: Has[W, ObligationRepository]) {

  def open(
      w: W, sourceKind: String, sourceId: String, userUid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): (W, Either[LedgerError, Obligation]) =
    oLens.get(w).bySource(sourceKind, sourceId) match
      case Some(o) =>
        if o.status != ObligationStatus.Open then (w, Left[LedgerError, Obligation](SourceTerminal))
        else (w, Right[LedgerError, Obligation](o))   // idempotent replay
      case _ =>
        val o = Obligation(sourceKind, sourceId, userUid, role, projectRef, taskKind, estimatedUnit, ObligationStatus.Open, None[FMLong]())
        (oLens(w).write((r: ObligationRepository) => r.put(o)), Right[LedgerError, Obligation](o))

  def cancel(w: W, sourceKind: String, sourceId: String): (W, Either[LedgerError, Obligation]) =
    oLens.get(w).bySource(sourceKind, sourceId) match
      case Some(o) =>
        if o.status == ObligationStatus.Realized then (w, Left[LedgerError, Obligation](SourceTerminal))
        else
          val o2 = o.copy(status = ObligationStatus.Cancelled)
          (oLens(w).write((r: ObligationRepository) => r.put(o2)), Right[LedgerError, Obligation](o2))
      case _ =>
        val o = Obligation(sourceKind, sourceId, "", "", "", "", FMLong(BigInt(0)), ObligationStatus.Cancelled, None[FMLong]())
        (oLens(w).write((r: ObligationRepository) => r.put(o)), Right[LedgerError, Obligation](o))

  /** Mark a source realized (the credit minted). Flips an open row or lands a terminal
    * realized row directly when the credit arrived before the obligation was opened. */
  def realize(
      w: W, sourceKind: String, sourceId: String, userUid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong, txId: FMLong): (W, Obligation) =
    val o2 = oLens.get(w).bySource(sourceKind, sourceId) match
      case Some(o) => o.copy(status = ObligationStatus.Realized, realizedTxId = Some[FMLong](txId))
      case _       => Obligation(sourceKind, sourceId, userUid, role, projectRef, taskKind, estimatedUnit, ObligationStatus.Realized, Some[FMLong](txId))
    (oLens(w).write((r: ObligationRepository) => r.put(o2)), o2)

  def bySource(w: W, kind: String, id: String): Option[Obligation] = oLens.get(w).bySource(kind, id)
  def openObligations(w: W): List[Obligation] = oLens.get(w).allOpen
}
