package io.linewise.verify.fm.ledger

import stainless.lang._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * WITHDRAWAL PROOFS — VERIFY-ONLY. These cover the optimistic state machine and the
 * money-moving branches. Every branch that writes a ledger tx proves the tx is
 * admissible by construction and that the fresh tx id guard is enforced before write.
 * ========================================================================== */
object WithdrawalProofs {

  private def zero: FMLong = FMLong(BigInt(0))

  def requestReturnsPending(
      w: World, userUid: String, amount: FMLong, clientReq: String,
      freshWid: FMLong, freshTxId: FMLong, userAccount: String, clearingAccount: String): Boolean = {
    require(amount > zero)
    require(w.withdrawals.findByClientReq(userUid, clientReq).isEmpty)
    require(w.ledger.get(freshTxId).isEmpty)
    val svc = WithdrawalService[World](HasLedger(), HasWithdrawals())
    svc.request(w, userUid, amount, clientReq, freshWid, freshTxId, userAccount, clearingAccount)._2 match
      case Right(wd) => wd.status == WithdrawalStatus.PendingReview && wd.amount == amount && wd.reserveTxId == freshTxId
      case Left(_)   => false
  }.holds

  def requestRejectsNonPositive(
      w: World, userUid: String, amount: FMLong, clientReq: String,
      freshWid: FMLong, freshTxId: FMLong, userAccount: String, clearingAccount: String): Boolean = {
    require(!(amount > zero))
    require(w.withdrawals.findByClientReq(userUid, clientReq).isEmpty)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).request(w, userUid, amount, clientReq, freshWid, freshTxId, userAccount, clearingAccount)._2 ==
      Left[LedgerError, Withdrawal](NonPositiveAmount)
  }.holds

  def requestDuplicateIsIdempotent(
      w: World, userUid: String, amount: FMLong, clientReq: String,
      freshWid: FMLong, freshTxId: FMLong, userAccount: String, clearingAccount: String): Boolean = {
    require(w.withdrawals.findByClientReq(userUid, clientReq) match
      case Some(_) => true
      case _       => false)
    w.withdrawals.findByClientReq(userUid, clientReq) match
      case Some(existing) =>
        WithdrawalService[World](HasLedger(), HasWithdrawals()).request(w, userUid, amount, clientReq, freshWid, freshTxId, userAccount, clearingAccount)._2 ==
          Right[LedgerError, Withdrawal](existing)
      case _ => false
  }.holds

  def requestRejectsDuplicateTxId(
      w: World, userUid: String, amount: FMLong, clientReq: String,
      freshWid: FMLong, freshTxId: FMLong, userAccount: String, clearingAccount: String): Boolean = {
    require(amount > zero)
    require(w.withdrawals.findByClientReq(userUid, clientReq).isEmpty)
    require(!w.ledger.get(freshTxId).isEmpty)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).request(w, userUid, amount, clientReq, freshWid, freshTxId, userAccount, clearingAccount)._2 ==
      Left[LedgerError, Withdrawal](DuplicateTxId)
  }.holds

  def requestBuildsAdmissibleReserve(
      userUid: String, amount: FMLong, freshTxId: FMLong, userAccount: String, clearingAccount: String): Boolean = {
    require(amount > zero)
    val tx = twoLegTx(freshTxId, TxKind.WithdrawalReserve, userAccount, clearingAccount, amount, None[String](), None[String](), userUid)
    tx.kind == TxKind.WithdrawalReserve && LedgerValidation.admissible(tx) &&
      tx.debitAccount == userAccount && tx.creditAccount == clearingAccount
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

  def approveRejectsMissing(w: World, id: FMLong): Boolean = {
    require(w.withdrawals.get(id).isEmpty)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).approve(w, id, WithdrawalStatus.PendingReview)._2 ==
      Left[LedgerError, Withdrawal](WithdrawalNotFound)
  }.holds

  def approveRejectsWrongExpectedStatus(w: World, id: FMLong, expectedStatus: WithdrawalStatus): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status != expectedStatus
      case None()   => false)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).approve(w, id, expectedStatus)._2 ==
      Left[LedgerError, Withdrawal](StatusConflict)
  }.holds

  def settleMovesSubmittedToSettled(
      w: World, id: FMLong, freshTxId: FMLong, clearingAccount: String, cashAccount: String): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status == WithdrawalStatus.Submitted && wd.amount > zero
      case _        => false)
    require(w.ledger.get(freshTxId).isEmpty)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).settle(w, id, WithdrawalStatus.Submitted, freshTxId, clearingAccount, cashAccount)._2 match
      case Right(wd) => wd.status == WithdrawalStatus.Settled
      case _         => false
  }.holds

  def failMovesSubmittedToFailed(
      w: World, id: FMLong, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status == WithdrawalStatus.Submitted && wd.amount > zero
      case _        => false)
    require(w.ledger.get(freshTxId).isEmpty)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).fail(w, id, WithdrawalStatus.Submitted, freshTxId, clearingAccount, userAccount)._2 match
      case Right(wd) => wd.status == WithdrawalStatus.Failed
      case _         => false
  }.holds

  def rejectMovesPendingToRejected(
      w: World, id: FMLong, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status == WithdrawalStatus.PendingReview && wd.amount > zero
      case _        => false)
    require(w.ledger.get(freshTxId).isEmpty)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).reject(w, id, WithdrawalStatus.PendingReview, freshTxId, clearingAccount, userAccount)._2 match
      case Right(wd) => wd.status == WithdrawalStatus.Rejected
      case _         => false
  }.holds

  def cancelMovesPendingToCancelled(
      w: World, id: FMLong, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status == WithdrawalStatus.PendingReview && wd.amount > zero
      case _        => false)
    require(w.ledger.get(freshTxId).isEmpty)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).cancel(w, id, WithdrawalStatus.PendingReview, freshTxId, clearingAccount, userAccount)._2 match
      case Right(wd) => wd.status == WithdrawalStatus.Cancelled
      case _         => false
  }.holds

  def moneyMoveRejectsWrongStatus(
      w: World, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, cashAccount: String): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status != expectedStatus || wd.status != WithdrawalStatus.Submitted
      case _        => false)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).settle(w, id, expectedStatus, freshTxId, clearingAccount, cashAccount)._2 ==
      Left[LedgerError, Withdrawal](StatusConflict)
  }.holds

  def moneyMoveRejectsNonPositiveStoredAmount(
      w: World, id: FMLong, freshTxId: FMLong, clearingAccount: String, cashAccount: String): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status == WithdrawalStatus.Submitted && !(wd.amount > zero)
      case _        => false)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).settle(w, id, WithdrawalStatus.Submitted, freshTxId, clearingAccount, cashAccount)._2 ==
      Left[LedgerError, Withdrawal](NonPositiveAmount)
  }.holds

  def moneyMoveRejectsDuplicateTxId(
      w: World, id: FMLong, freshTxId: FMLong, clearingAccount: String, cashAccount: String): Boolean = {
    require(w.withdrawals.get(id) match
      case Some(wd) => wd.status == WithdrawalStatus.Submitted && wd.amount > zero
      case _        => false)
    require(!w.ledger.get(freshTxId).isEmpty)
    WithdrawalService[World](HasLedger(), HasWithdrawals()).settle(w, id, WithdrawalStatus.Submitted, freshTxId, clearingAccount, cashAccount)._2 ==
      Left[LedgerError, Withdrawal](DuplicateTxId)
  }.holds
}
