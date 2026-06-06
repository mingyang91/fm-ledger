package io.linewise.verify.fm.ledger

import stainless.lang._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * WITHDRAWAL PROOFS — VERIFY-ONLY. The WithdrawalService capabilities over the World +
 * the two lenses (HasLedger, HasWithdrawals): a fresh request returns a pending_review
 * withdrawal, approve advances pending_review -> submitted, and the optimistic guard
 * rejects a wrong expectedStatus. The money legs are balanced by construction (the
 * LedgerTx shape); these proofs cover the status machine and the conditions.
 * ========================================================================== */
object WithdrawalProofs {

  def requestReturnsPending(
      w: World, userUid: String, amount: FMLong, clientReq: String,
      freshWid: FMLong, freshTxId: FMLong, userAccount: String, clearingAccount: String): Boolean = {
    require(amount > FMLong(BigInt(0)))
    require(w.withdrawals.findByClientReq(userUid, clientReq).isEmpty)
    val svc = WithdrawalService[World](HasLedger(), HasWithdrawals())
    svc.request(w, userUid, amount, clientReq, freshWid, freshTxId, userAccount, clearingAccount)._2 match
      case Right(wd) => wd.status == WithdrawalStatus.PendingReview
      case Left(_)   => false
  }.holds

  def approveMovesToSubmitted(w: World, id: FMLong): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status == WithdrawalStatus.PendingReview
      case None()   => false)
    val svc = WithdrawalService[World](HasLedger(), HasWithdrawals())
    svc.approve(w, id, WithdrawalStatus.PendingReview)._2 match
      case Right(wd) => wd.status == WithdrawalStatus.Submitted
      case Left(_)   => false
  }.holds

  def approveRejectsWrongExpectedStatus(w: World, id: FMLong, expectedStatus: WithdrawalStatus): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status != expectedStatus
      case None()   => false)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).approve(w, id, expectedStatus)._2 ==
      Left[LedgerError, Withdrawal](StatusConflict)
  }.holds
}
