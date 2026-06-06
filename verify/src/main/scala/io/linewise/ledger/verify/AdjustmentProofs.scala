package io.linewise.verify.fm.ledger

import stainless.lang._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * ADJUSTMENT PROOFS — VERIFY-ONLY. The two-person control properties: a fresh proposal
 * is pending_review, an approval by the SAME person who proposed it is refused with
 * TwoPersonViolation, and an approval by a DIFFERENT admin moves it to approved.
 * ========================================================================== */
object AdjustmentProofs {

  def proposeReturnsPending(
      w: World, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String,
      amount: FMLong, reason: String, proposedBy: String, freshPid: FMLong): Boolean = {
    require(amount > FMLong(BigInt(0)))
    val svc = AdjustmentService[World](HasLedger(), HasProposals())
    svc.propose(w, kind, userUid, debitAccount, creditAccount, amount, reason, proposedBy, freshPid, None[FMLong]())._2 match
      case Right(p) => p.status == ProposalStatus.PendingReview
      case Left(_)  => false
  }.holds

  def approveByProposerRejected(w: World, id: FMLong, approver: String, freshTxId: FMLong): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status == ProposalStatus.PendingReview && p.proposedBy == approver
      case _       => false)
    AdjustmentService[World](HasLedger(), HasProposals()).approve(w, id, ProposalStatus.PendingReview, approver, freshTxId)._2 ==
      Left[LedgerError, Proposal](TwoPersonViolation)
  }.holds

  def approveByOtherMovesToApproved(w: World, id: FMLong, approver: String, freshTxId: FMLong): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status == ProposalStatus.PendingReview && p.proposedBy != approver
      case _       => false)
    AdjustmentService[World](HasLedger(), HasProposals()).approve(w, id, ProposalStatus.PendingReview, approver, freshTxId)._2 match
      case Right(p) => p.status == ProposalStatus.Approved
      case Left(_)  => false
  }.holds
}
