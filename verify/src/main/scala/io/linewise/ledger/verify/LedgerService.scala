package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import LedgerTables._

/* =============================================================================
 * The ledger write is still `post`, accepting a multi-entry journal tx, but the state is now the
 * DB's TX_HEADER + LEG tables, reached through the single `Has[W, DB]` lens and the `LedgerStore`
 * seam (InMem for the oracle/proofs, Jdbc @extern for production). Freshness checks read through
 * the store instead of a repository.
 * ========================================================================== */
case class LedgerService[W](has: Has[W, DB], store: LedgerStore) {

  private def txIdFresh(db: DB, id: FMLong): Boolean = store.get(db, id).isEmpty
  private def sourceFresh(db: DB, tx: LedgerTx): Boolean =
    (tx.sourceKind, tx.sourceId) match
      case (Some(k), Some(i)) => store.findBySource(db, k, i).isEmpty
      case _                  => true

  def post(w: W, tx: LedgerTx): (W, Either[LedgerError, LedgerTx]) = {
    val db = has.get(w)
    if !LedgerValidation.positiveAmount(tx) then
      (w, Left[LedgerError, LedgerTx](NonPositiveAmount))
    else if !LedgerValidation.balanced(tx) then
      (w, Left[LedgerError, LedgerTx](UnbalancedTx))
    else if !txIdFresh(db, tx.id) then
      (w, Left[LedgerError, LedgerTx](DuplicateTxId))
    else if !sourceFresh(db, tx) then
      (w, Left[LedgerError, LedgerTx](DuplicateSource))
    else
      val w1 = has(w).write((d: DB) => store.post(d, tx))
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
    store.get(has.get(w), id) match
      case Some(t) => Right[LedgerError, LedgerTx](t)
      case _       => Left[LedgerError, LedgerTx](TxNotFound(id))

  def all(w: W): List[LedgerTx] = store.all(has.get(w))
}
