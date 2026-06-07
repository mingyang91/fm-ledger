package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * VERIFY-ONLY proofs over the abstract repository and the verified service branching.
 * Conservation is no longer structural in the header shape, so admissible txs now
 * require both positivity and explicit `balanced`.
 * ========================================================================== */
object LedgerProofs {

  def postThenGet(w: World, tx: LedgerTx): Boolean = {
    require(LedgerValidation.admissible(tx))
    require(LedgerValidation.txIdFresh(w.ledger, tx))
    require(LedgerValidation.isFresh(w.ledger, tx))
    val svc = LedgerService[World](HasLedger())
    svc.post(w, tx) match
      case (w1, Right(posted)) =>
        assert(w.ledger.postGet(tx))
        svc.get(w1, tx.id) == Right[LedgerError, LedgerTx](posted)
      case (_, Left(_)) => false
  }.holds

  def postRejectsNonPositive(w: World, tx: LedgerTx): Boolean = {
    require(!LedgerValidation.positiveAmount(tx))
    LedgerService[World](HasLedger()).post(w, tx)._2 == Left[LedgerError, LedgerTx](NonPositiveAmount)
  }.holds

  def postRejectsUnbalanced(w: World, tx: LedgerTx): Boolean = {
    require(LedgerValidation.positiveAmount(tx))
    require(!LedgerValidation.balanced(tx))
    LedgerService[World](HasLedger()).post(w, tx)._2 == Left[LedgerError, LedgerTx](UnbalancedTx)
  }.holds

  def postRejectsDuplicateTxId(w: World, tx: LedgerTx): Boolean = {
    require(LedgerValidation.admissible(tx))
    require(!LedgerValidation.txIdFresh(w.ledger, tx))
    LedgerService[World](HasLedger()).post(w, tx)._2 == Left[LedgerError, LedgerTx](DuplicateTxId)
  }.holds

  def postRejectsDuplicateSource(w: World, tx: LedgerTx): Boolean = {
    require(LedgerValidation.admissible(tx))
    require(LedgerValidation.txIdFresh(w.ledger, tx))
    require(!LedgerValidation.isFresh(w.ledger, tx))
    LedgerService[World](HasLedger()).post(w, tx)._2 == Left[LedgerError, LedgerTx](DuplicateSource)
  }.holds

  def twoLegTxIsAdmissible(
      id: FMLong, kind: TxKind, debitAccount: String, creditAccount: String,
      amount: FMLong, sourceKind: Option[String], sourceId: Option[String], userUid: String): Boolean = {
    require(amount > FMLong(BigInt(0)))
    val tx = twoLegTx(id, kind, debitAccount, creditAccount, amount, sourceKind, sourceId, userUid)
    tx.debitAccount == debitAccount && tx.creditAccount == creditAccount && tx.amount == amount &&
      LedgerValidation.admissible(tx)
  }.holds

  def creditPostsWhenAdmissible(
      w: World, eAcct: String, uAcct: String, uid: String,
      amount: FMLong, freshId: FMLong, sk: String, si: String): Boolean = {
    require(amount > FMLong(BigInt(0)))
    val tx = twoLegTx(freshId, TxKind.IncentiveCredit, eAcct, uAcct, amount, Some[String](sk), Some[String](si), uid)
    require(LedgerValidation.txIdFresh(w.ledger, tx))
    require(LedgerValidation.isFresh(w.ledger, tx))
    LedgerService[World](HasLedger()).credit(w, eAcct, uAcct, uid, amount, freshId, sk, si)._2 match
      case Right(posted) =>
        posted == tx && LedgerValidation.admissible(posted) && posted.debitAccount == eAcct && posted.creditAccount == uAcct
      case _ => false
  }.holds

  def adjustBuildsAdmissibleTx(
      debitAccount: String, creditAccount: String, uid: String,
      amount: FMLong, freshId: FMLong): Boolean = {
    require(amount > FMLong(BigInt(0)))
    val tx = twoLegTx(freshId, TxKind.ManualAdjustment, debitAccount, creditAccount, amount, None[String](), None[String](), uid)
    tx.kind == TxKind.ManualAdjustment && LedgerValidation.admissible(tx) &&
      tx.debitAccount == debitAccount && tx.creditAccount == creditAccount
  }.holds

  def validLedgerRowsPreservedByFreshPost(rows: List[LedgerTx], tx: LedgerTx): Boolean = {
    require(LedgerInvariants.validLedgerRows(rows))
    require(LedgerValidation.admissible(tx))
    require(!LedgerInvariants.containsTxId(rows, tx.id))
    require(LedgerInvariants.sourceAbsent(rows, tx))
    LedgerInvariants.validLedgerRows(tx :: rows)
  }.holds

}
