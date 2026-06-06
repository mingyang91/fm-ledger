package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.annotation._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * LEDGER PROOFS — VERIFY-ONLY (never transpiled). LedgerService capabilities proven
 * over the World + HasLedger lens and the ABSTRACT LedgerRepository, via the repo's
 * postGet axiom and the service's own branching:
 *   - a posted balanced, fresh tx is retrievable by id (postThenGet)
 *   - a non-positive amount is rejected (postRejectsNonPositive)
 *   - a duplicate source is rejected (postRejectsDuplicate)
 *   - credit builds the canonical DR-expense / CR-user movement and posts it
 * Conservation (ΣDR == ΣCR) is structural in the LedgerTx shape, so it needs no proof.
 * ========================================================================== */
object LedgerProofs {

  def postThenGet(w: World, tx: LedgerTx): Boolean = {
    require(LedgerValidation.positiveAmount(tx))
    require(LedgerValidation.isFresh(w.ledger, tx))
    val svc = LedgerService[World](HasLedger())
    svc.post(w, tx) match
      case (w1, Right(posted)) =>
        assert(w.ledger.postGet(tx)) // hint: post(tx).get(tx.id) == Some(tx)
        svc.get(w1, tx.id) == Right[LedgerError, LedgerTx](tx)
      case (_, Left(_)) => false
  }.holds

  def postRejectsNonPositive(w: World, tx: LedgerTx): Boolean = {
    require(!LedgerValidation.positiveAmount(tx))
    LedgerService[World](HasLedger()).post(w, tx)._2 == Left[LedgerError, LedgerTx](NonPositiveAmount)
  }.holds

  def postRejectsDuplicate(w: World, tx: LedgerTx): Boolean = {
    require(LedgerValidation.positiveAmount(tx))
    require(!LedgerValidation.isFresh(w.ledger, tx))
    LedgerService[World](HasLedger()).post(w, tx)._2 == Left[LedgerError, LedgerTx](DuplicateSource)
  }.holds

  def creditPostsWhenAdmissible(
      w: World, eAcct: String, uAcct: String, uid: String,
      amount: FMLong, freshId: FMLong, sk: String, si: String): Boolean = {
    require(amount > FMLong(BigInt(0)))
    require(w.ledger.findBySource(sk, si).isEmpty)
    val svc = LedgerService[World](HasLedger())
    svc.credit(w, eAcct, uAcct, uid, amount, freshId, sk, si)._2 match
      case Right(t) => t.debitAccount == eAcct && t.creditAccount == uAcct
      case Left(_)  => false
  }.holds
}
