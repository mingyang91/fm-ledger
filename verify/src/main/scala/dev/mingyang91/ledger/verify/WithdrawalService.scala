package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import LedgerModel._
import LedgerTables._
import WithdrawalTables._

/* =============================================================================
 * Withdrawal lifecycle over the DB's ledger + withdrawal tables, through the single Has[W, DB] lens
 * and the ledger/withdrawal stores. A request posts a reserve tx and adds a withdrawal row + initial
 * status; transitions prepend a status row (and, for settle/fail/reject/cancel, post the matching
 * balanced tx). "Current status" is read by the latest-row join in the store.
 * ========================================================================== */
case class WithdrawalService[W](has: Has[W, DB], lstore: LedgerStore, wstore: WithdrawalStore) {

  def request(
      w: W, userUid: String, amount: Long, clientRequestId: String,
      freshWid: Long, freshTxId: Long, userAccount: String, clearingAccount: String)
      : (W, Either[LedgerError, Withdrawal]) = {
    val db = has.get(w)
    wstore.findByClientReq(db, userUid, clientRequestId) match
      case Some(existing) => (w, Right[LedgerError, Withdrawal](existing))
      case _ =>
        if !(amount > 0L) then (w, Left[LedgerError, Withdrawal](NonPositiveAmount))
        else if !lstore.get(db, freshTxId).isEmpty then (w, Left[LedgerError, Withdrawal](DuplicateTxId))
        else
          val reserveTx = twoLegTx(freshTxId, TxKind.WithdrawalReserve, userAccount, clearingAccount, amount,
            None[String](), None[String](), userUid)
          val wd = Withdrawal(freshWid, userUid, amount, WithdrawalStatus.PendingReview, clientRequestId, freshTxId)
          val w1 = has(w).write((d: DB) => wstore.addWithdrawal(lstore.post(d, reserveTx), wd))
          (w1, Right[LedgerError, Withdrawal](wd))
  }

  def approve(w: W, id: Long, expectedStatus: WithdrawalStatus): (W, Either[LedgerError, Withdrawal]) =
    transitionOnly(w, id, expectedStatus, WithdrawalStatus.PendingReview, WithdrawalStatus.Submitted)

  def settle(w: W, id: Long, expectedStatus: WithdrawalStatus, freshTxId: Long, clearingAccount: String, cashAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.Submitted, TxKind.WithdrawalSettle, WithdrawalStatus.Settled, freshTxId, clearingAccount, cashAccount)

  def fail(w: W, id: Long, expectedStatus: WithdrawalStatus, freshTxId: Long, clearingAccount: String, userAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.Submitted, TxKind.WithdrawalReturn, WithdrawalStatus.Failed, freshTxId, clearingAccount, userAccount)

  def reject(w: W, id: Long, expectedStatus: WithdrawalStatus, freshTxId: Long, clearingAccount: String, userAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.PendingReview, TxKind.WithdrawalReturn, WithdrawalStatus.Rejected, freshTxId, clearingAccount, userAccount)

  def cancel(w: W, id: Long, expectedStatus: WithdrawalStatus, freshTxId: Long, clearingAccount: String, userAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.PendingReview, TxKind.WithdrawalReturn, WithdrawalStatus.Cancelled, freshTxId, clearingAccount, userAccount)

  def transitionOnly(
      w: W, id: Long, expectedStatus: WithdrawalStatus, requiredFrom: WithdrawalStatus, toStatus: WithdrawalStatus)
      : (W, Either[LedgerError, Withdrawal]) = {
    val db = has.get(w)
    wstore.get(db, id) match
      case Some(wd) =>
        if wd.status != expectedStatus || wd.status != requiredFrom then
          (w, Left[LedgerError, Withdrawal](StatusConflict))
        else
          (has(w).write((d: DB) => wstore.transition(d, id, toStatus)),
            Right[LedgerError, Withdrawal](wd.copy(status = toStatus)))
      case _ => (w, Left[LedgerError, Withdrawal](WithdrawalNotFound))
  }

  private def moveAndSet(
      w: W, id: Long, expectedStatus: WithdrawalStatus, requiredFrom: WithdrawalStatus, txKind: TxKind, toStatus: WithdrawalStatus,
      freshTxId: Long, debitAccount: String, creditAccount: String): (W, Either[LedgerError, Withdrawal]) = {
    val db = has.get(w)
    wstore.get(db, id) match
      case Some(wd) =>
        if wd.status != expectedStatus || wd.status != requiredFrom then
          (w, Left[LedgerError, Withdrawal](StatusConflict))
        else if !(wd.amount > 0L) then
          (w, Left[LedgerError, Withdrawal](NonPositiveAmount))
        else if !lstore.get(db, freshTxId).isEmpty then
          (w, Left[LedgerError, Withdrawal](DuplicateTxId))
        else
          val tx = twoLegTx(freshTxId, txKind, debitAccount, creditAccount, wd.amount, None[String](), None[String](), wd.userUid)
          val w1 = has(w).write((d: DB) => wstore.transition(lstore.post(d, tx), id, toStatus))
          (w1, Right[LedgerError, Withdrawal](wd.copy(status = toStatus)))
      case _ => (w, Left[LedgerError, Withdrawal](WithdrawalNotFound))
  }

  def get(w: W, id: Long): Either[LedgerError, Withdrawal] =
    wstore.get(has.get(w), id) match
      case Some(wd) => Right[LedgerError, Withdrawal](wd)
      case _        => Left[LedgerError, Withdrawal](WithdrawalNotFound)

  def all(w: W): List[Withdrawal] = wstore.all(has.get(w))
}
