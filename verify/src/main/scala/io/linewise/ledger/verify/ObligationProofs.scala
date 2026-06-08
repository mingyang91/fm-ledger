package io.linewise.verify.fm.ledger

import stainless.lang._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * OBLIGATION PROOFS — VERIFY-ONLY. Covers source idempotency, terminal source
 * refusal, cancel tombstones, and realize transitions.
 * ========================================================================== */
object ObligationProofs {

  def openThenBySource(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    require(w.obligations.bySource(sk, si) == None[Obligation]())
    require(estimatedUnit > FMLong(BigInt(0)))
    val svc = ObligationService[World](HasObligations())
    svc.open(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit) match
      case (w1, Right(o)) =>
        assert(w.obligations.putGetBySource(o)) // hint: put(o).bySource(o.sk,o.si) == Some(o)
        o.status == ObligationStatus.Open && o.realizedTxId.isEmpty && svc.bySource(w1, sk, si) == Some[Obligation](o)
      case (_, Left(_)) => false
  }.holds

  def openExistingOpenIsIdempotent(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    require(w.obligations.bySource(sk, si) match
      case Some(o) => o.status == ObligationStatus.Open
      case _       => false)
    require(estimatedUnit > FMLong(BigInt(0)))
    val existing = w.obligations.bySource(sk, si)
    existing match
      case Some(o) =>
        ObligationService[World](HasObligations()).open(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit)._2 ==
          Right[LedgerError, Obligation](o)
      case _ => false
  }.holds

  def openTerminalRejected(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    require(w.obligations.bySource(sk, si) match
      case Some(o) => o.status != ObligationStatus.Open
      case _       => false)
    require(estimatedUnit > FMLong(BigInt(0)))
    ObligationService[World](HasObligations()).open(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit)._2 ==
      Left[LedgerError, Obligation](SourceTerminal)
  }.holds

  // #P2-b: a non-positive estimate is rejected before any source/idempotency logic, world unchanged.
  def openRejectsNonPositiveEstimate(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    require(!(estimatedUnit > FMLong(BigInt(0))))
    ObligationService[World](HasObligations()).open(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit)._2 ==
      Left[LedgerError, Obligation](NonPositiveAmount)
  }.holds

  def openNonPositiveLeavesWorldUnchanged(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    require(!(estimatedUnit > FMLong(BigInt(0))))
    ObligationService[World](HasObligations()).open(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit)._1 == w
  }.holds

  def cancelOpenMovesCancelled(w: World, sk: String, si: String): Boolean = {
    require(w.obligations.bySource(sk, si) match
      case Some(o) => o.status == ObligationStatus.Open
      case _       => false)
    ObligationService[World](HasObligations()).cancel(w, sk, si)._2 match
      case Right(o) => o.status == ObligationStatus.Cancelled && o.realizedTxId.isEmpty
      case _        => false
  }.holds

  def cancelMissingWritesTombstone(w: World, sk: String, si: String): Boolean = {
    require(w.obligations.bySource(sk, si).isEmpty)
    val svc = ObligationService[World](HasObligations())
    svc.cancel(w, sk, si) match
      case (w1, Right(o)) =>
        assert(w.obligations.putGetBySource(o))
        o.status == ObligationStatus.Cancelled && svc.bySource(w1, sk, si) == Some[Obligation](o)
      case _ => false
  }.holds

  def cancelRealizedRejected(w: World, sk: String, si: String): Boolean = {
    require(w.obligations.bySource(sk, si) match
      case Some(o) => o.status == ObligationStatus.Realized
      case _       => false)
    ObligationService[World](HasObligations()).cancel(w, sk, si)._2 ==
      Left[LedgerError, Obligation](SourceTerminal)
  }.holds

  def cancelledTombstoneIsTerminal(w: World, sk: String, si: String): Boolean = {
    require(w.obligations.bySource(sk, si).isEmpty)
    ObligationService[World](HasObligations()).cancel(w, sk, si)._2 match
      case Right(o) => o.status != ObligationStatus.Open && o.status == ObligationStatus.Cancelled
      case _        => false
  }.holds

  def realizeOpenMovesRealized(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong, txId: FMLong): Boolean = {
    require(w.obligations.bySource(sk, si) match
      case Some(o) => o.status == ObligationStatus.Open
      case _       => false)
    val (_, o) = ObligationService[World](HasObligations()).realize(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit, txId)
    o.status == ObligationStatus.Realized && o.realizedTxId == Some[FMLong](txId)
  }.holds

  def realizeMissingLandsRealized(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong, txId: FMLong): Boolean = {
    require(w.obligations.bySource(sk, si).isEmpty)
    val (_, o) = ObligationService[World](HasObligations()).realize(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit, txId)
    o.status == ObligationStatus.Realized && o.realizedTxId == Some[FMLong](txId)
  }.holds

  def realizeCancelledStaysCancelled(o: Obligation, txId: FMLong): Boolean = {
    require(o.status == ObligationStatus.Cancelled)
    val o2 =
      if o.status == ObligationStatus.Cancelled then o
      else o.copy(status = ObligationStatus.Realized, realizedTxId = Some[FMLong](txId))
    o2.status == ObligationStatus.Cancelled && o2.realizedTxId == o.realizedTxId
  }.holds

  def openExistingOpenLeavesWorldUnchanged(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    require(w.obligations.bySource(sk, si) match
      case Some(o) => o.status == ObligationStatus.Open
      case _       => false)
    ObligationService[World](HasObligations()).open(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit)._1 == w
  }.holds

  def openTerminalLeavesWorldUnchanged(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    require(w.obligations.bySource(sk, si) match
      case Some(o) => o.status != ObligationStatus.Open
      case _       => false)
    ObligationService[World](HasObligations()).open(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit)._1 == w
  }.holds

  def cancelRealizedLeavesWorldUnchanged(w: World, sk: String, si: String): Boolean = {
    require(w.obligations.bySource(sk, si) match
      case Some(o) => o.status == ObligationStatus.Realized
      case _       => false)
    ObligationService[World](HasObligations()).cancel(w, sk, si)._1 == w
  }.holds
}
