package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * LEDGER — THE SERVICE LAYER, polymorphic in the world W and wired by a Has lens.
 * It composes the validation with the repository it reaches THROUGH the lens:
 *   - reads:  has.get(w)
 *   - writes: has(w).write(repo => repo.post(tx))  -> a new world
 *
 * `post` is the one write: it refuses a non-positive amount and a duplicate source,
 * otherwise appends a balanced tx and returns it. `credit` and `adjust` build the
 * canonical two-account movement and delegate to `post`, so they inherit the same
 * conditions and the append-only guarantee. No cats-effect; id source is the trusted
 * `freshId`. Transpile-clean; transpiler input.
 * ========================================================================== */
case class LedgerService[W](has: Has[W, LedgerRepository]) {

  def post(w: W, tx: LedgerTx): (W, Either[LedgerError, LedgerTx]) = {
    val repo = has.get(w)
    if !LedgerValidation.positiveAmount(tx) then
      (w, Left[LedgerError, LedgerTx](NonPositiveAmount))
    else if !LedgerValidation.isFresh(repo, tx) then
      (w, Left[LedgerError, LedgerTx](DuplicateSource))
    else
      val w1 = has(w).write((r: LedgerRepository) => r.post(tx))
      (w1, Right[LedgerError, LedgerTx](tx))
  }

  /** Mint an incentive credit: DR the expense account, CR the user account. */
  def credit(
      w: W, expenseAccount: String, userAccount: String, userUid: String,
      amount: FMLong, freshId: FMLong, sourceKind: String, sourceId: String): (W, Either[LedgerError, LedgerTx]) =
    post(w, LedgerTx(freshId, TxKind.IncentiveCredit, expenseAccount, userAccount, amount,
      Some[String](sourceKind), Some[String](sourceId), userUid))

  /** A manual two-person adjustment, already directed by the caller (CREDIT vs DEBIT
    * decide which account is debited). No source key (adjustments don't dedupe). */
  def adjust(
      w: W, debitAccount: String, creditAccount: String, userUid: String,
      amount: FMLong, freshId: FMLong): (W, Either[LedgerError, LedgerTx]) =
    post(w, LedgerTx(freshId, TxKind.ManualAdjustment, debitAccount, creditAccount, amount,
      None[String](), None[String](), userUid))

  def get(w: W, id: FMLong): Either[LedgerError, LedgerTx] =
    has.get(w).get(id) match
      case Some(t) => Right[LedgerError, LedgerTx](t)
      case _       => Left[LedgerError, LedgerTx](TxNotFound(id))

  def all(w: W): List[LedgerTx] = has.get(w).all
}
