package io.linewise.verify.fm.ledger

import stainless.lang._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * VERIFY-ONLY proofs over the abstract repository and the verified service branching.
 * Conservation is no longer structural in the header shape, so admissible txs now
 * require both positivity and explicit `balanced`.
 * ========================================================================== */
object LedgerProofs {

  def postThenGet(w: World, tx: LedgerTx): Boolean = {
    require(LedgerValidation.positiveAmount(tx))
    require(LedgerValidation.balanced(tx))
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

  def postRejectsDuplicate(w: World, tx: LedgerTx): Boolean = {
    require(LedgerValidation.positiveAmount(tx))
    require(LedgerValidation.balanced(tx))
    require(!LedgerValidation.isFresh(w.ledger, tx))
    LedgerService[World](HasLedger()).post(w, tx)._2 == Left[LedgerError, LedgerTx](DuplicateSource)
  }.holds

  def creditPostsWhenAdmissible(
      w: World, eAcct: String, uAcct: String, uid: String,
      amount: FMLong, freshId: FMLong, sk: String, si: String): Boolean = {
    require(amount > FMLong(BigInt(0)))
    require(w.ledger.findBySource(sk, si).isEmpty)
    val tx = twoLegTx(freshId, TxKind.IncentiveCredit, eAcct, uAcct, amount, Some[String](sk), Some[String](si), uid)
    tx.debitAccount == eAcct && tx.creditAccount == uAcct && LedgerValidation.balanced(tx)
  }.holds
}
