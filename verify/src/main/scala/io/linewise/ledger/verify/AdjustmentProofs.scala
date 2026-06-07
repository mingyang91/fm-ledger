package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * ADJUSTMENT PROOFS — VERIFY-ONLY. Covers proposal creation, two-person approval,
 * duplicate rollback reversal protection, status conflicts, missing rows, and the
 * admissible ledger tx posted by a successful approval.
 * ========================================================================== */
object AdjustmentProofs {

  private def zero: FMLong = FMLong(BigInt(0))

  def proposeReturnsPending(
      w: World, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String,
      amount: FMLong, reason: String, proposedBy: String, freshPid: FMLong): Boolean = {
    require(amount > zero)
    val svc = AdjustmentService[World](HasLedger(), HasProposals())
    svc.propose(w, kind, userUid, debitAccount, creditAccount, amount, reason, proposedBy, freshPid, None[FMLong]())._2 match
      case Right(p) => p.status == ProposalStatus.PendingReview && p.amount == amount && p.resultTxId.isEmpty
      case Left(_)  => false
  }.holds

  def proposeRejectsNonPositive(
      w: World, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String,
      amount: FMLong, reason: String, proposedBy: String, freshPid: FMLong): Boolean = {
    require(!(amount > zero))
    AdjustmentService[World](HasLedger(), HasProposals()).propose(w, kind, userUid, debitAccount, creditAccount, amount, reason, proposedBy, freshPid, None[FMLong]())._2 ==
      Left[LedgerError, Proposal](NonPositiveAmount)
  }.holds

  // A rollback reversal whose target already has a non-rejected reversal proposal is refused
  // with AlreadyReversed. Phrased against the same `alreadyReversed` predicate propose uses,
  // so the guard is machine-checked, not just correct by inspection.
  def reversalOfAlreadyReversedRefused(
      w: World, userUid: String, debitAccount: String, creditAccount: String,
      amount: FMLong, reason: String, proposedBy: String, freshPid: FMLong, targetTxId: FMLong): Boolean = {
    require(amount > zero)
    val svc = AdjustmentService[World](HasLedger(), HasProposals())
    require(svc.alreadyReversed(w.proposals, TxKind.RollbackReversal, Some[FMLong](targetTxId)))
    svc.propose(w, TxKind.RollbackReversal, userUid, debitAccount, creditAccount, amount, reason, proposedBy, freshPid, Some[FMLong](targetTxId))._2 ==
      Left[LedgerError, Proposal](AlreadyReversed)
  }.holds

  def rejectedReversalDoesNotBlock(targetTxId: FMLong, proposalId: FMLong, amount: FMLong): Boolean = {
    require(amount > zero)
    val rejected = Proposal(proposalId, TxKind.RollbackReversal, "u", "dr", "cr", amount, "reason", "a",
      ProposalStatus.Rejected, None[FMLong](), Some[FMLong](targetTxId))
    val repo = InMemProposalRepository(Cons(rejected, Nil[Proposal]()))
    !AdjustmentService[World](HasLedger(), HasProposals()).alreadyReversed(repo, TxKind.RollbackReversal, Some[FMLong](targetTxId))
  }.holds

  def approveRejectsMissing(w: World, id: FMLong, approver: String, freshTxId: FMLong): Boolean = {
    require(w.proposals.get(id).isEmpty)
    AdjustmentService[World](HasLedger(), HasProposals()).approve(w, id, ProposalStatus.PendingReview, approver, freshTxId)._2 ==
      Left[LedgerError, Proposal](ProposalNotFound)
  }.holds

  def approveRejectsWrongExpectedStatus(w: World, id: FMLong, expectedStatus: ProposalStatus, approver: String, freshTxId: FMLong): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status != expectedStatus || p.status != ProposalStatus.PendingReview
      case _       => false)
    AdjustmentService[World](HasLedger(), HasProposals()).approve(w, id, expectedStatus, approver, freshTxId)._2 ==
      Left[LedgerError, Proposal](StatusConflict)
  }.holds

  def approveByProposerRejected(w: World, id: FMLong, approver: String, freshTxId: FMLong): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status == ProposalStatus.PendingReview && p.proposedBy == approver
      case _       => false)
    AdjustmentService[World](HasLedger(), HasProposals()).approve(w, id, ProposalStatus.PendingReview, approver, freshTxId)._2 ==
      Left[LedgerError, Proposal](TwoPersonViolation)
  }.holds

  def approveRejectsNonPositiveStoredAmount(w: World, id: FMLong, approver: String, freshTxId: FMLong): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status == ProposalStatus.PendingReview && p.proposedBy != approver && !(p.amount > zero)
      case _       => false)
    AdjustmentService[World](HasLedger(), HasProposals()).approve(w, id, ProposalStatus.PendingReview, approver, freshTxId)._2 ==
      Left[LedgerError, Proposal](NonPositiveAmount)
  }.holds

  def approveRejectsDuplicateTxId(w: World, id: FMLong, approver: String, freshTxId: FMLong): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status == ProposalStatus.PendingReview && p.proposedBy != approver && p.amount > zero
      case _       => false)
    require(!w.ledger.get(freshTxId).isEmpty)
    AdjustmentService[World](HasLedger(), HasProposals()).approve(w, id, ProposalStatus.PendingReview, approver, freshTxId)._2 ==
      Left[LedgerError, Proposal](DuplicateTxId)
  }.holds

  def approveByOtherMovesToApproved(w: World, id: FMLong, approver: String, freshTxId: FMLong): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status == ProposalStatus.PendingReview && p.proposedBy != approver && p.amount > zero
      case _       => false)
    require(w.ledger.get(freshTxId).isEmpty)
    AdjustmentService[World](HasLedger(), HasProposals()).approve(w, id, ProposalStatus.PendingReview, approver, freshTxId)._2 match
      case Right(p) => p.status == ProposalStatus.Approved && p.resultTxId == Some[FMLong](freshTxId)
      case Left(_)  => false
  }.holds

  def approvePostsAdmissibleTx(w: World, id: FMLong, approver: String, freshTxId: FMLong): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status == ProposalStatus.PendingReview && p.proposedBy != approver && p.amount > zero
      case _       => false)
    require(w.ledger.get(freshTxId).isEmpty)
    w.proposals.get(id) match
      case Some(p) =>
        val tx = twoLegTx(freshTxId, p.kind, p.debitAccount, p.creditAccount, p.amount, None[String](), None[String](), p.userUid)
        AdjustmentService[World](HasLedger(), HasProposals()).approve(w, id, ProposalStatus.PendingReview, approver, freshTxId) match
          case (w1, Right(_)) =>
            assert(w.ledger.postGet(tx))
            LedgerValidation.admissible(tx) && LedgerService[World](HasLedger()).get(w1, freshTxId) == Right[LedgerError, LedgerTx](tx)
          case _ => false
      case _ => false
  }.holds

  def rejectMovesPendingToRejected(w: World, id: FMLong): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status == ProposalStatus.PendingReview
      case _       => false)
    AdjustmentService[World](HasLedger(), HasProposals()).reject(w, id, ProposalStatus.PendingReview)._2 match
      case Right(p) => p.status == ProposalStatus.Rejected
      case _        => false
  }.holds

  def rejectRejectsMissing(w: World, id: FMLong): Boolean = {
    require(w.proposals.get(id).isEmpty)
    AdjustmentService[World](HasLedger(), HasProposals()).reject(w, id, ProposalStatus.PendingReview)._2 ==
      Left[LedgerError, Proposal](ProposalNotFound)
  }.holds

  def rejectRejectsWrongStatus(w: World, id: FMLong, expectedStatus: ProposalStatus): Boolean = {
    require(w.proposals.get(id) match
      case Some(p) => p.status != expectedStatus || p.status != ProposalStatus.PendingReview
      case _       => false)
    AdjustmentService[World](HasLedger(), HasProposals()).reject(w, id, expectedStatus)._2 ==
      Left[LedgerError, Proposal](StatusConflict)
  }.holds
}
