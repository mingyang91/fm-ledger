package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import LedgerModel._

/* =============================================================================
 * VERIFY-ONLY account catalog model. Production keeps the catalog in PostgreSQL;
 * these predicates pin down the account roles used by the verified posting builders.
 * ========================================================================== */
object AccountAssertions {

  enum AccountCategory { case Expense, Revenue, Settlement, Clearing, UserLiability, Adjustment }
  enum NormalSide { case Debit, Credit }

  case class AccountSpec(code: String, category: AccountCategory, normalSide: NormalSide, postable: Boolean)

  val IncentiveExpense = "system:incentive_expense"
  val WithdrawalClearing = "system:withdrawal_clearing"
  val Cash = "system:cash"
  val Adjustment = "system:adjustment"
  val FeeRecovery = "system:fee_recovery"
  val ProviderPayoutFee = "system:provider_payout_fee"

  def userAccountSpec(code: String): AccountSpec =
    AccountSpec(code, AccountCategory.UserLiability, NormalSide.Credit, true)

  def providerBalanceSpec(code: String): AccountSpec =
    AccountSpec(code, AccountCategory.Settlement, NormalSide.Credit, true)

  def systemSpec(code: String): Option[AccountSpec] =
    if code == IncentiveExpense then
      Some[AccountSpec](AccountSpec(code, AccountCategory.Expense, NormalSide.Debit, true))
    else if code == WithdrawalClearing then
      Some[AccountSpec](AccountSpec(code, AccountCategory.Clearing, NormalSide.Credit, true))
    else if code == Cash then
      Some[AccountSpec](AccountSpec(code, AccountCategory.Settlement, NormalSide.Credit, true))
    else if code == Adjustment then
      Some[AccountSpec](AccountSpec(code, AccountCategory.Adjustment, NormalSide.Debit, true))
    else if code == FeeRecovery then
      Some[AccountSpec](AccountSpec(code, AccountCategory.Revenue, NormalSide.Credit, true))
    else if code == ProviderPayoutFee then
      Some[AccountSpec](AccountSpec(code, AccountCategory.Expense, NormalSide.Debit, true))
    else None[AccountSpec]()

  def postableSystemAccount(code: String): Boolean =
    systemSpec(code) match
      case Some(spec) => spec.postable
      case _          => false

  def recipientPaysFeeAccountsKnown(clearing: String, providerBalance: String, feeRecovery: String, providerFeeExpense: String): Boolean =
    clearing == WithdrawalClearing && feeRecovery == FeeRecovery && providerFeeExpense == ProviderPayoutFee &&
      providerBalanceSpec(providerBalance).postable

  def entryPostableForRecipientPaysFee(entry: LedgerEntry, providerBalance: String): Boolean =
    entry.account == WithdrawalClearing || entry.account == FeeRecovery ||
      entry.account == ProviderPayoutFee || entry.account == providerBalance

  def entriesPostableForRecipientPaysFee(entries: List[LedgerEntry], providerBalance: String): Boolean =
    entries.forall((e: LedgerEntry) => entryPostableForRecipientPaysFee(e, providerBalance))

  def legalAccountsFor(kind: TxKind, debitAccount: String, creditAccount: String, userAccount: String, providerBalance: String): Boolean =
    kind match
      case TxKind.IncentiveCredit =>
        debitAccount == IncentiveExpense && creditAccount == userAccount && userAccountSpec(userAccount).postable
      case TxKind.WithdrawalReserve =>
        debitAccount == userAccount && creditAccount == WithdrawalClearing && userAccountSpec(userAccount).postable
      case TxKind.WithdrawalSettle =>
        debitAccount == WithdrawalClearing && (creditAccount == Cash || creditAccount == providerBalance) && providerBalanceSpec(providerBalance).postable
      case TxKind.WithdrawalReturn =>
        debitAccount == WithdrawalClearing && creditAccount == userAccount && userAccountSpec(userAccount).postable
      case TxKind.WithdrawalFeeRecovery =>
        debitAccount == WithdrawalClearing && creditAccount == FeeRecovery
      case TxKind.ProviderPayoutFee =>
        debitAccount == ProviderPayoutFee && creditAccount == providerBalance && providerBalanceSpec(providerBalance).postable
      case TxKind.ManualAdjustment =>
        debitAccount == Adjustment || creditAccount == Adjustment
      case TxKind.RollbackReversal =>
        true

  def incentiveCreditUsesLegalAccounts(userAccount: String): Boolean = {
    legalAccountsFor(TxKind.IncentiveCredit, IncentiveExpense, userAccount, userAccount, "") &&
      postableSystemAccount(IncentiveExpense)
  }.holds

  def withdrawalReserveUsesLegalAccounts(userAccount: String): Boolean = {
    legalAccountsFor(TxKind.WithdrawalReserve, userAccount, WithdrawalClearing, userAccount, "") &&
      postableSystemAccount(WithdrawalClearing)
  }.holds

  def recipientPaysFeeUsesLegalAccounts(providerBalance: String): Boolean = {
    legalAccountsFor(TxKind.WithdrawalSettle, WithdrawalClearing, providerBalance, "", providerBalance) &&
      legalAccountsFor(TxKind.WithdrawalFeeRecovery, WithdrawalClearing, FeeRecovery, "", providerBalance) &&
      legalAccountsFor(TxKind.ProviderPayoutFee, ProviderPayoutFee, providerBalance, "", providerBalance)
  }.holds
}
