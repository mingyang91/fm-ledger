package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import AccountAssertions._

/* =============================================================================
 * VERIFY-ONLY recipient-pays-fee settlement model. The HTTP shell stores quotes,
 * verifies webhooks, and opens database transactions. These proofs cover the pure
 * accounting decision it makes after a settled provider event arrives.
 * ========================================================================== */
object PayoutProofs {

  case class SettlementIds(netTxId: FMLong, feeRecoveryTxId: FMLong, providerFeeTxId: FMLong)
  case class SettlementAccounts(clearing: String, providerBalance: String, feeRecovery: String, providerFeeExpense: String)
  case class RecipientPaysFeeQuote(gross: FMLong, expectedRecipientNet: FMLong, quotedProviderFee: FMLong)
  case class PayoutSettlement(netTx: LedgerTx, feeRecoveryTx: Option[LedgerTx], providerFeeTx: Option[LedgerTx])
  case class PayoutState(eventIds: List[String], postedTxs: List[LedgerTx])

  def zero: FMLong = FMLong(BigInt(0))

  def quoteMatches(q: RecipientPaysFeeQuote): Boolean =
    q.gross.value == q.expectedRecipientNet.value + q.quotedProviderFee.value

  def nonNegative(amount: FMLong): Boolean = amount >= zero

  def feeRecoveryTx(id: FMLong, userUid: String, amount: FMLong, accounts: SettlementAccounts): Option[LedgerTx] =
    if amount > zero then
      Some[LedgerTx](twoLegTx(id, TxKind.WithdrawalFeeRecovery, accounts.clearing, accounts.feeRecovery, amount, None[String](), None[String](), userUid))
    else None[LedgerTx]()

  def providerFeeTx(id: FMLong, userUid: String, amount: FMLong, accounts: SettlementAccounts): Option[LedgerTx] =
    if amount > zero then
      Some[LedgerTx](twoLegTx(id, TxKind.ProviderPayoutFee, accounts.providerFeeExpense, accounts.providerBalance, amount, None[String](), None[String](), userUid))
    else None[LedgerTx]()

  def recipientPaysFeeSettlement(
      ids: SettlementIds,
      userUid: String,
      q: RecipientPaysFeeQuote,
      observedProviderFee: FMLong,
      accounts: SettlementAccounts): PayoutSettlement =
    PayoutSettlement(
      twoLegTx(ids.netTxId, TxKind.WithdrawalSettle, accounts.clearing, accounts.providerBalance, q.expectedRecipientNet, None[String](), None[String](), userUid),
      feeRecoveryTx(ids.feeRecoveryTxId, userUid, q.quotedProviderFee, accounts),
      providerFeeTx(ids.providerFeeTxId, userUid, observedProviderFee, accounts),
    )

  def optionalAdmissible(tx: Option[LedgerTx]): Boolean =
    tx match
      case Some(t) => LedgerValidation.admissible(t)
      case _       => true

  def optionalAmount(tx: Option[LedgerTx]): BigInt =
    tx match
      case Some(t) => t.amount.value
      case _       => BigInt(0)

  def clearingReleased(s: PayoutSettlement): BigInt =
    s.netTx.amount.value + optionalAmount(s.feeRecoveryTx)

  def feeRecovered(s: PayoutSettlement): BigInt =
    optionalAmount(s.feeRecoveryTx)

  def providerFeeExpensed(s: PayoutSettlement): BigInt =
    optionalAmount(s.providerFeeTx)

  def addOptional(tx: Option[LedgerTx], rows: List[LedgerTx]): List[LedgerTx] =
    tx match
      case Some(t) => t :: rows
      case _       => rows

  def settlementRows(s: PayoutSettlement): List[LedgerTx] =
    addOptional(s.feeRecoveryTx, addOptional(s.providerFeeTx, s.netTx :: Nil[LedgerTx]()))

  def hasEvent(ids: List[String], eventId: String): Boolean =
    ids.exists((id: String) => id == eventId)

  def recordSettlement(state: PayoutState, eventId: String, settlement: PayoutSettlement): PayoutState =
    if hasEvent(state.eventIds, eventId) then state
    else PayoutState(eventId :: state.eventIds, settlementRows(settlement) ++ state.postedTxs)

  def reconciliationResult(expectedFee: FMLong, observedFee: FMLong): String =
    if observedFee == expectedFee then "matched" else "fee_variance"

  def recipientPaysFeeSettlementAdmissible(
      ids: SettlementIds, userUid: String, q: RecipientPaysFeeQuote, observedProviderFee: FMLong, accounts: SettlementAccounts): Boolean = {
    require(q.expectedRecipientNet > zero)
    require(nonNegative(q.quotedProviderFee))
    require(nonNegative(observedProviderFee))
    require(quoteMatches(q))
    val s = recipientPaysFeeSettlement(ids, userUid, q, observedProviderFee, accounts)
    LedgerValidation.admissible(s.netTx) && optionalAdmissible(s.feeRecoveryTx) && optionalAdmissible(s.providerFeeTx) &&
      clearingReleased(s) == q.gross.value
  }.holds

  def zeroQuotedFeeSkipsRecovery(ids: SettlementIds, userUid: String, gross: FMLong, observedProviderFee: FMLong, accounts: SettlementAccounts): Boolean = {
    val q = RecipientPaysFeeQuote(gross, gross, zero)
    recipientPaysFeeSettlement(ids, userUid, q, observedProviderFee, accounts).feeRecoveryTx.isEmpty
  }.holds

  def zeroObservedFeeSkipsProviderExpense(ids: SettlementIds, userUid: String, q: RecipientPaysFeeQuote, accounts: SettlementAccounts): Boolean = {
    recipientPaysFeeSettlement(ids, userUid, q, zero, accounts).providerFeeTx.isEmpty
  }.holds

  def matchedFeeIsNeutral(
      ids: SettlementIds, userUid: String, q: RecipientPaysFeeQuote, accounts: SettlementAccounts): Boolean = {
    require(nonNegative(q.quotedProviderFee))
    val s = recipientPaysFeeSettlement(ids, userUid, q, q.quotedProviderFee, accounts)
    feeRecovered(s) == providerFeeExpensed(s) && reconciliationResult(q.quotedProviderFee, q.quotedProviderFee) == "matched"
  }.holds

  def feeVarianceIsAuditable(expectedFee: FMLong, observedFee: FMLong): Boolean = {
    require(expectedFee != observedFee)
    reconciliationResult(expectedFee, observedFee) == "fee_variance"
  }.holds

  def duplicateProviderEventDoesNotPostAgain(state: PayoutState, eventId: String, settlement: PayoutSettlement): Boolean = {
    require(hasEvent(state.eventIds, eventId))
    recordSettlement(state, eventId, settlement) == state
  }.holds

  def recipientPaysFeeAccountsArePostable(
      ids: SettlementIds, userUid: String, q: RecipientPaysFeeQuote, observedProviderFee: FMLong, accounts: SettlementAccounts): Boolean = {
    require(recipientPaysFeeAccountsKnown(accounts.clearing, accounts.providerBalance, accounts.feeRecovery, accounts.providerFeeExpense))
    val s = recipientPaysFeeSettlement(ids, userUid, q, observedProviderFee, accounts)
    entriesPostableForRecipientPaysFee(s.netTx.entries, accounts.providerBalance) &&
      (s.feeRecoveryTx match
        case Some(t) => entriesPostableForRecipientPaysFee(t.entries, accounts.providerBalance)
        case _       => true) &&
      (s.providerFeeTx match
        case Some(t) => entriesPostableForRecipientPaysFee(t.entries, accounts.providerBalance)
        case _       => true)
  }.holds
}
