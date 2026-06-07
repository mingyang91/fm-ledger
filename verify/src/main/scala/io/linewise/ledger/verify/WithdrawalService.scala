package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * Withdrawal lifecycle over the ledger + withdrawal status slices. Standard reserve/
 * return/settle paths still post canonical two-leg txs. `transitionOnly` is exposed so
 * the production shell can settle a payout with a richer posting group (for example a
 * multi-entry fee-aware settlement) and then advance the status in the same DB tx.
 * ========================================================================== */
case class WithdrawalService[W](ledgerLens: Has[W, LedgerRepository], wLens: Has[W, WithdrawalRepository]) {

  def request(
      w: W, userUid: String, amount: FMLong, clientRequestId: String,
      freshWid: FMLong, freshTxId: FMLong, userAccount: String, clearingAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    wLens.get(w).findByClientReq(userUid, clientRequestId) match
      case Some(existing) => (w, Right[LedgerError, Withdrawal](existing))
      case _ =>
        if !(amount > FMLong(BigInt(0))) then (w, Left[LedgerError, Withdrawal](NonPositiveAmount))
        else
          val reserveTx = twoLegTx(freshTxId, TxKind.WithdrawalReserve, userAccount, clearingAccount, amount,
            None[String](), None[String](), userUid)
          val w1 = ledgerLens(w).write((r: LedgerRepository) => r.post(reserveTx))
          val wd = Withdrawal(freshWid, userUid, amount, WithdrawalStatus.PendingReview, clientRequestId, freshTxId)
          val w2 = wLens(w1).write((r: WithdrawalRepository) => r.put(wd))
          (w2, Right[LedgerError, Withdrawal](wd))

  def approve(w: W, id: FMLong, expectedStatus: WithdrawalStatus): (W, Either[LedgerError, Withdrawal]) =
    transitionOnly(w, id, expectedStatus, WithdrawalStatus.PendingReview, WithdrawalStatus.Submitted)

  def settle(w: W, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, cashAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.Submitted, TxKind.WithdrawalSettle, WithdrawalStatus.Settled, freshTxId, clearingAccount, cashAccount)

  def fail(w: W, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, userAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.Submitted, TxKind.WithdrawalReturn, WithdrawalStatus.Failed, freshTxId, clearingAccount, userAccount)

  def reject(w: W, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, userAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.PendingReview, TxKind.WithdrawalReturn, WithdrawalStatus.Rejected, freshTxId, clearingAccount, userAccount)

  def cancel(w: W, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, userAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.PendingReview, TxKind.WithdrawalReturn, WithdrawalStatus.Cancelled, freshTxId, clearingAccount, userAccount)

  def transitionOnly(
      w: W, id: FMLong, expectedStatus: WithdrawalStatus, requiredFrom: WithdrawalStatus, toStatus: WithdrawalStatus)
      : (W, Either[LedgerError, Withdrawal]) =
    wLens.get(w).get(id) match
      case Some(wd) =>
        if wd.status != expectedStatus || wd.status != requiredFrom then
          (w, Left[LedgerError, Withdrawal](StatusConflict))
        else
          val wd2 = wd.copy(status = toStatus)
          (wLens(w).write((r: WithdrawalRepository) => r.put(wd2)), Right[LedgerError, Withdrawal](wd2))
      case _ => (w, Left[LedgerError, Withdrawal](WithdrawalNotFound))

  private def moveAndSet(
      w: W, id: FMLong, expectedStatus: WithdrawalStatus, requiredFrom: WithdrawalStatus, txKind: TxKind, toStatus: WithdrawalStatus,
      freshTxId: FMLong, debitAccount: String, creditAccount: String): (W, Either[LedgerError, Withdrawal]) =
    wLens.get(w).get(id) match
      case Some(wd) =>
        if wd.status != expectedStatus || wd.status != requiredFrom then
          (w, Left[LedgerError, Withdrawal](StatusConflict))
        else
          val tx = twoLegTx(freshTxId, txKind, debitAccount, creditAccount, wd.amount, None[String](), None[String](), wd.userUid)
          val w1 = ledgerLens(w).write((r: LedgerRepository) => r.post(tx))
          val wd2 = wd.copy(status = toStatus)
          val w2 = wLens(w1).write((r: WithdrawalRepository) => r.put(wd2))
          (w2, Right[LedgerError, Withdrawal](wd2))
      case _ => (w, Left[LedgerError, Withdrawal](WithdrawalNotFound))

  def get(w: W, id: FMLong): Either[LedgerError, Withdrawal] =
    wLens.get(w).get(id) match
      case Some(wd) => Right[LedgerError, Withdrawal](wd)
      case _        => Left[LedgerError, Withdrawal](WithdrawalNotFound)

  def all(w: W): List[Withdrawal] = wLens.get(w).all
}
