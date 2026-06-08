package io.linewise.verify.fm.ledger

import stainless.lang._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import PayoutLifecycleProofs._

/* =============================================================================
 * AGGREGATE PRESERVATION PROOFS — VERIFY-ONLY. The core services are polymorphic in
 * the world shape and only receive lenses for the slices they may touch. By wrapping
 * the verified core world together with the payout world, we can prove that ordinary
 * ledger / withdrawal / adjustment / obligation actions cannot mutate payout state.
 * ========================================================================== */
object AggregatePreservationProofs {

  case class AggregateWorld(core: World, payout: PayoutWorld)

  case class HasLedgerAgg() extends Has[AggregateWorld, LedgerRepository] {
    def get(w: AggregateWorld): LedgerRepository = w.core.ledger
    def set(w: AggregateWorld, r: LedgerRepository): AggregateWorld = w.copy(core = w.core.copy(ledger = r))
  }

  case class HasWithdrawalsAgg() extends Has[AggregateWorld, WithdrawalRepository] {
    def get(w: AggregateWorld): WithdrawalRepository = w.core.withdrawals
    def set(w: AggregateWorld, r: WithdrawalRepository): AggregateWorld = w.copy(core = w.core.copy(withdrawals = r))
  }

  case class HasProposalsAgg() extends Has[AggregateWorld, ProposalRepository] {
    def get(w: AggregateWorld): ProposalRepository = w.core.proposals
    def set(w: AggregateWorld, r: ProposalRepository): AggregateWorld = w.copy(core = w.core.copy(proposals = r))
  }

  case class HasObligationsAgg() extends Has[AggregateWorld, ObligationRepository] {
    def get(w: AggregateWorld): ObligationRepository = w.core.obligations
    def set(w: AggregateWorld, r: ObligationRepository): AggregateWorld = w.copy(core = w.core.copy(obligations = r))
  }

  def validAggregateWorld(w: AggregateWorld): Boolean =
    LedgerInvariants.validWorld(w.core) && validPayoutWorld(w.payout)

  def withdrawalRequestPreservesPayout(
      w: AggregateWorld, userUid: String, amount: FMLong, clientReq: String,
      freshWid: FMLong, freshTxId: FMLong, userAccount: String, clearingAccount: String): Boolean = {
    WithdrawalService[AggregateWorld](HasLedgerAgg(), HasWithdrawalsAgg()).request(w, userUid, amount, clientReq, freshWid, freshTxId, userAccount, clearingAccount)._1.payout == w.payout
  }.holds

  def withdrawalApprovePreservesPayout(w: AggregateWorld, id: FMLong, expectedStatus: WithdrawalStatus): Boolean = {
    WithdrawalService[AggregateWorld](HasLedgerAgg(), HasWithdrawalsAgg()).approve(w, id, expectedStatus)._1.payout == w.payout
  }.holds

  def withdrawalSettlePreservesPayout(w: AggregateWorld, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, cashAccount: String): Boolean = {
    WithdrawalService[AggregateWorld](HasLedgerAgg(), HasWithdrawalsAgg()).settle(w, id, expectedStatus, freshTxId, clearingAccount, cashAccount)._1.payout == w.payout
  }.holds

  def withdrawalFailPreservesPayout(w: AggregateWorld, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    WithdrawalService[AggregateWorld](HasLedgerAgg(), HasWithdrawalsAgg()).fail(w, id, expectedStatus, freshTxId, clearingAccount, userAccount)._1.payout == w.payout
  }.holds

  def withdrawalRejectPreservesPayout(w: AggregateWorld, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    WithdrawalService[AggregateWorld](HasLedgerAgg(), HasWithdrawalsAgg()).reject(w, id, expectedStatus, freshTxId, clearingAccount, userAccount)._1.payout == w.payout
  }.holds

  def withdrawalCancelPreservesPayout(w: AggregateWorld, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    WithdrawalService[AggregateWorld](HasLedgerAgg(), HasWithdrawalsAgg()).cancel(w, id, expectedStatus, freshTxId, clearingAccount, userAccount)._1.payout == w.payout
  }.holds

  def adjustmentProposePreservesPayout(
      w: AggregateWorld, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String,
      amount: FMLong, reason: String, proposedBy: String, freshPid: FMLong, targetTxId: Option[FMLong]): Boolean = {
    AdjustmentService[AggregateWorld](HasLedgerAgg(), HasProposalsAgg()).propose(w, kind, userUid, debitAccount, creditAccount, amount, reason, proposedBy, freshPid, targetTxId)._1.payout == w.payout
  }.holds

  def adjustmentApprovePreservesPayout(w: AggregateWorld, id: FMLong, expectedStatus: ProposalStatus, approver: String, freshTxId: FMLong): Boolean = {
    AdjustmentService[AggregateWorld](HasLedgerAgg(), HasProposalsAgg()).approve(w, id, expectedStatus, approver, freshTxId)._1.payout == w.payout
  }.holds

  def adjustmentRejectPreservesPayout(w: AggregateWorld, id: FMLong, expectedStatus: ProposalStatus): Boolean = {
    AdjustmentService[AggregateWorld](HasLedgerAgg(), HasProposalsAgg()).reject(w, id, expectedStatus)._1.payout == w.payout
  }.holds

  def obligationOpenPreservesPayout(
      w: AggregateWorld, sourceKind: String, sourceId: String, userUid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong): Boolean = {
    ObligationService[AggregateWorld](HasObligationsAgg()).open(w, sourceKind, sourceId, userUid, role, projectRef, taskKind, estimatedUnit)._1.payout == w.payout
  }.holds

  def obligationCancelPreservesPayout(w: AggregateWorld, sourceKind: String, sourceId: String): Boolean = {
    ObligationService[AggregateWorld](HasObligationsAgg()).cancel(w, sourceKind, sourceId)._1.payout == w.payout
  }.holds

  def obligationRealizePreservesPayout(
      w: AggregateWorld, sourceKind: String, sourceId: String, userUid: String, role: String,
      projectRef: String, taskKind: String, estimatedUnit: FMLong, txId: FMLong): Boolean = {
    ObligationService[AggregateWorld](HasObligationsAgg()).realize(w, sourceKind, sourceId, userUid, role, projectRef, taskKind, estimatedUnit, txId)._1.payout == w.payout
  }.holds
}
