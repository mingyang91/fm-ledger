package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong

/* =============================================================================
 * LEDGER — the DATA LAYER, translated to Stainless.
 *
 * The core is now a JOURNAL HEADER plus MANY entry legs. This keeps the model small
 * enough for Stainless while making multi-entry postings possible for payout fee and
 * future tax/settlement flows. Compatibility accessors (`amount`, `debitAccount`,
 * `creditAccount`) remain for callers that still deal in simple two-leg transactions;
 * for true multi-entry postings they mean "first DR", "first CR", and total debits.
 * ========================================================================== */
object LedgerModel {

  enum ObligationStatus { case Open, Realized, Cancelled }
  enum TxKind {
    case IncentiveCredit, ManualAdjustment, RollbackReversal,
      WithdrawalReserve, WithdrawalSettle, WithdrawalReturn,
      WithdrawalFeeRecovery, ProviderPayoutFee
  }
  enum WithdrawalStatus { case PendingReview, Submitted, Settled, Rejected, Cancelled, Failed }
  enum ProposalStatus { case PendingReview, Approved, Rejected }
  enum EntryDirection { case DR, CR }

  case class LedgerEntry(account: String, direction: EntryDirection, amount: FMLong)

  // recursion via isEmpty/head/tail (not `case Nil()/Cons()`) so it transpiles clean — the transpiler
  // does not rewrite List patterns, and direct recursion (not foldLeft) keeps the conservation proof simple.
  def sumDirection(entries: List[LedgerEntry], direction: EntryDirection): BigInt =
    if entries.isEmpty then BigInt(0)
    else {
      val rest = sumDirection(entries.tail, direction)
      if entries.head.direction == direction then rest + entries.head.amount.value else rest
    }

  def hasDirection(entries: List[LedgerEntry], direction: EntryDirection): Boolean =
    entries.exists((e: LedgerEntry) => e.direction == direction)

  def allPositive(entries: List[LedgerEntry]): Boolean =
    entries.forall((e: LedgerEntry) => e.amount > FMLong(BigInt(0)))

  def firstAccount(entries: List[LedgerEntry], direction: EntryDirection): String =
    entries.find((e: LedgerEntry) => e.direction == direction) match
      case Some(e) => e.account
      case _       => ""

  case class LedgerTx(
      id:         FMLong,
      kind:       TxKind,
      entries:    List[LedgerEntry],
      sourceKind: Option[String],
      sourceId:   Option[String],
      userUid:    String,
  ) {
    def amount: FMLong = entries.find((e: LedgerEntry) => e.direction == EntryDirection.DR) match
      case Some(e) => e.amount
      case _       => FMLong(BigInt(0))
    def debitAccount: String = firstAccount(entries, EntryDirection.DR)
    def creditAccount: String = firstAccount(entries, EntryDirection.CR)
  }

  def twoLegTx(
      id: FMLong, kind: TxKind, debitAccount: String, creditAccount: String,
      amount: FMLong, sourceKind: Option[String], sourceId: Option[String], userUid: String): LedgerTx =
    LedgerTx(id, kind,
      List(
        LedgerEntry(debitAccount, EntryDirection.DR, amount),
        LedgerEntry(creditAccount, EntryDirection.CR, amount),
      ),
      sourceKind, sourceId, userUid)

  sealed trait LedgerError
  case object NonPositiveAmount extends LedgerError
  case object UnbalancedTx extends LedgerError
  case object DuplicateTxId extends LedgerError
  case object DuplicateSource extends LedgerError
  case class TxNotFound(id: FMLong) extends LedgerError
  case object WithdrawalNotFound extends LedgerError
  case object StatusConflict extends LedgerError
  case object ProposalNotFound extends LedgerError
  case object TwoPersonViolation extends LedgerError
  case object SourceTerminal extends LedgerError
  case object AlreadyReversed extends LedgerError
}
