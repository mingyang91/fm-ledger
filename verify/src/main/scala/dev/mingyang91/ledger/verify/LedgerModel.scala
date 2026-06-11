package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.math.wrapping

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

  case class LedgerEntry(account: String, direction: EntryDirection, amount: Long)

  // recursion via isEmpty/head/tail (not `case Nil()/Cons()`) so it transpiles clean — the transpiler
  // does not rewrite List patterns, and direct recursion (not foldLeft) keeps the conservation proof simple.
  def sumDirection(entries: List[LedgerEntry], direction: EntryDirection): Long =
    if entries.isEmpty then 0L
    else {
      val rest = sumDirection(entries.tail, direction)
      if entries.head.direction == direction then wrapping { rest + entries.head.amount } else rest
    }

  def hasDirection(entries: List[LedgerEntry], direction: EntryDirection): Boolean =
    entries.exists((e: LedgerEntry) => e.direction == direction)

  def allPositive(entries: List[LedgerEntry]): Boolean =
    entries.forall((e: LedgerEntry) => e.amount > 0L)

  def firstAccount(entries: List[LedgerEntry], direction: EntryDirection): String =
    entries.find((e: LedgerEntry) => e.direction == direction) match
      case Some(e) => e.account
      case _       => ""

  case class LedgerTx(
      id:         Long,
      kind:       TxKind,
      entries:    List[LedgerEntry],
      sourceKind: Option[String],
      sourceId:   Option[String],
      userUid:    String,
  ) {
    def amount: Long = entries.find((e: LedgerEntry) => e.direction == EntryDirection.DR) match
      case Some(e) => e.amount
      case _       => 0L
    def debitAccount: String = firstAccount(entries, EntryDirection.DR)
    def creditAccount: String = firstAccount(entries, EntryDirection.CR)
  }

  def twoLegTx(
      id: Long, kind: TxKind, debitAccount: String, creditAccount: String,
      amount: Long, sourceKind: Option[String], sourceId: Option[String], userUid: String): LedgerTx =
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
  case class TxNotFound(id: Long) extends LedgerError
  case object WithdrawalNotFound extends LedgerError
  case object StatusConflict extends LedgerError
  case object ProposalNotFound extends LedgerError
  case object TwoPersonViolation extends LedgerError
  case object SourceTerminal extends LedgerError
  case object AlreadyReversed extends LedgerError
}
