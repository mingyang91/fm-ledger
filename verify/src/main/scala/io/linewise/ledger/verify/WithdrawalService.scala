package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * WITHDRAWAL SERVICE — verified, and the FIRST service that coordinates TWO repository
 * slices through TWO Has lenses: the ledger (append-only money movements) and the
 * withdrawal status. A money transition posts a balanced LedgerTx through the ledger
 * lens AND advances the withdrawal status through the withdrawal lens, returning a new
 * World threaded through both. The lifecycle:
 *   request  -> reserve (DR user / CR clearing), status pending_review
 *   approve  -> status submitted               (no money moves; reserve already held)
 *   settle   -> DR clearing / CR cash,  status settled
 *   reject   -> DR clearing / CR user,  status rejected   (return the reserve)
 *   cancel   -> DR clearing / CR user,  status cancelled  (return the reserve)
 * Every edge carries an `expectedStatus` optimistic-concurrency guard. Idempotency on
 * (userUid, clientRequestId) makes a replay return the existing withdrawal. The balance
 * sufficiency for the reserve is a PRODUCTION guard (it needs a fold over the ledger),
 * so it is enforced in the shell before calling, not here.
 * ========================================================================== */
case class WithdrawalService[W](ledgerLens: Has[W, LedgerRepository], wLens: Has[W, WithdrawalRepository]) {

  def request(
      w: W, userUid: String, amount: FMLong, clientRequestId: String,
      freshWid: FMLong, freshTxId: FMLong, userAccount: String, clearingAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    wLens.get(w).findByClientReq(userUid, clientRequestId) match
      case Some(existing) => (w, Right[LedgerError, Withdrawal](existing))   // idempotent replay
      case _ =>
        if !(amount > FMLong(BigInt(0))) then (w, Left[LedgerError, Withdrawal](NonPositiveAmount))
        else
          val reserveTx = LedgerTx(freshTxId, TxKind.WithdrawalReserve, userAccount, clearingAccount, amount,
            None[String](), None[String](), userUid)
          val w1 = ledgerLens(w).write((r: LedgerRepository) => r.post(reserveTx))
          val wd = Withdrawal(freshWid, userUid, amount, WithdrawalStatus.PendingReview, clientRequestId, freshTxId)
          val w2 = wLens(w1).write((r: WithdrawalRepository) => r.put(wd))
          (w2, Right[LedgerError, Withdrawal](wd))

  /** pending_review -> submitted; no money moves (the reserve already holds the points). */
  def approve(w: W, id: FMLong, expectedStatus: WithdrawalStatus): (W, Either[LedgerError, Withdrawal]) =
    wLens.get(w).get(id) match
      case Some(wd) =>
        if wd.status != expectedStatus || wd.status != WithdrawalStatus.PendingReview then
          (w, Left[LedgerError, Withdrawal](StatusConflict))
        else
          val wd2 = wd.copy(status = WithdrawalStatus.Submitted)
          (wLens(w).write((r: WithdrawalRepository) => r.put(wd2)), Right[LedgerError, Withdrawal](wd2))
      case _ => (w, Left[LedgerError, Withdrawal](WithdrawalNotFound))

  def settle(w: W, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, cashAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.Submitted, TxKind.WithdrawalSettle, WithdrawalStatus.Settled, freshTxId, clearingAccount, cashAccount)

  /** A submitted payout that the provider reports failed: return the reserve to the user. */
  def fail(w: W, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, userAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.Submitted, TxKind.WithdrawalReturn, WithdrawalStatus.Failed, freshTxId, clearingAccount, userAccount)

  def reject(w: W, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, userAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.PendingReview, TxKind.WithdrawalReturn, WithdrawalStatus.Rejected, freshTxId, clearingAccount, userAccount)

  def cancel(w: W, id: FMLong, expectedStatus: WithdrawalStatus, freshTxId: FMLong, clearingAccount: String, userAccount: String)
      : (W, Either[LedgerError, Withdrawal]) =
    moveAndSet(w, id, expectedStatus, WithdrawalStatus.PendingReview, TxKind.WithdrawalReturn, WithdrawalStatus.Cancelled, freshTxId, clearingAccount, userAccount)

  /** Post a balanced movement (debitAccount -> creditAccount of the withdrawal's amount)
    * and advance the status, gated by the optimistic guard and the required from-status. */
  private def moveAndSet(
      w: W, id: FMLong, expectedStatus: WithdrawalStatus, requiredFrom: WithdrawalStatus, txKind: TxKind, toStatus: WithdrawalStatus,
      freshTxId: FMLong, debitAccount: String, creditAccount: String): (W, Either[LedgerError, Withdrawal]) =
    wLens.get(w).get(id) match
      case Some(wd) =>
        if wd.status != expectedStatus || wd.status != requiredFrom then
          (w, Left[LedgerError, Withdrawal](StatusConflict))
        else
          val tx = LedgerTx(freshTxId, txKind, debitAccount, creditAccount, wd.amount, None[String](), None[String](), wd.userUid)
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
