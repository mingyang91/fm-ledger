package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * The one write is still `post`, but it now accepts a true multi-entry journal tx.
 * Canonical callers (`credit`, `adjust`) still build simple two-leg movements through
 * `twoLegTx`, while the shell can construct richer postings and route them through the
 * same verified validation.
 * ========================================================================== */
case class LedgerService[W](has: Has[W, LedgerRepository]) {

  def post(w: W, tx: LedgerTx): (W, Either[LedgerError, LedgerTx]) = {
    val repo = has.get(w)
    if !LedgerValidation.positiveAmount(tx) then
      (w, Left[LedgerError, LedgerTx](NonPositiveAmount))
    else if !LedgerValidation.balanced(tx) then
      (w, Left[LedgerError, LedgerTx](UnbalancedTx))
    else if !LedgerValidation.txIdFresh(repo, tx) then
      (w, Left[LedgerError, LedgerTx](DuplicateTxId))
    else if !LedgerValidation.isFresh(repo, tx) then
      (w, Left[LedgerError, LedgerTx](DuplicateSource))
    else
      val w1 = has(w).write((r: LedgerRepository) => r.post(tx))
      (w1, Right[LedgerError, LedgerTx](tx))
  }

  def credit(
      w: W, expenseAccount: String, userAccount: String, userUid: String,
      amount: FMLong, freshId: FMLong, sourceKind: String, sourceId: String): (W, Either[LedgerError, LedgerTx]) =
    post(w, twoLegTx(freshId, TxKind.IncentiveCredit, expenseAccount, userAccount, amount,
      Some[String](sourceKind), Some[String](sourceId), userUid))

  def adjust(
      w: W, debitAccount: String, creditAccount: String, userUid: String,
      amount: FMLong, freshId: FMLong): (W, Either[LedgerError, LedgerTx]) =
    post(w, twoLegTx(freshId, TxKind.ManualAdjustment, debitAccount, creditAccount, amount,
      None[String](), None[String](), userUid))

  def get(w: W, id: FMLong): Either[LedgerError, LedgerTx] =
    has.get(w).get(id) match
      case Some(t) => Right[LedgerError, LedgerTx](t)
      case _       => Left[LedgerError, LedgerTx](TxNotFound(id))

  def all(w: W): List[LedgerTx] = has.get(w).all
}
