package io.linewise.verify.fm.ledger

import stainless.lang._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * OBLIGATION PROOFS — VERIFY-ONLY. Opening a fresh source makes it retrievable, and
 * opening a source that is already terminal (realized/cancelled) is refused.
 * ========================================================================== */
object ObligationProofs {

  def openThenBySource(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    require(w.obligations.bySource(sk, si) == None[Obligation]())
    val svc = ObligationService[World](HasObligations())
    svc.open(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit) match
      case (w1, Right(o)) =>
        assert(w.obligations.putGetBySource(o)) // hint: put(o).bySource(o.sk,o.si) == Some(o)
        svc.bySource(w1, sk, si) == Some[Obligation](o)
      case (_, Left(_)) => false
  }.holds

  def openTerminalRejected(
      w: World, sk: String, si: String, uid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    require(w.obligations.bySource(sk, si) match
      case Some(o) => o.status != ObligationStatus.Open
      case _       => false)
    ObligationService[World](HasObligations()).open(w, sk, si, uid, role, projectRef, taskKind, estimatedUnit)._2 ==
      Left[LedgerError, Obligation](SourceTerminal)
  }.holds
}
