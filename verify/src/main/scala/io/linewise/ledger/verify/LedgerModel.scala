package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong

/* =============================================================================
 * LEDGER — the DATA LAYER, translated to Stainless (transpiler input #1).
 *
 * A posted transaction is a SINGLE balanced movement: `amount` is debited from
 * `debitAccount` and credited to `creditAccount`. Modeling a tx this way makes the
 * double-entry conservation invariant (ΣDR == ΣCR) STRUCTURAL — every tx is exactly
 * one debit and one credit of the same amount — so it needs no aggregate-fold proof.
 * The only thing to validate is a positive amount and idempotency. Accounts are
 * strings ("system:incentive_expense", "user:<uid>", …); the optional flat source
 * key is the incentive-credit idempotency key.
 *
 * `amount: Option[FMLong]`-free on purpose; ids are FMLong (BIGSERIAL, erases to Long).
 * Generated core lands in io.linewise.ledger.generated with the FM types erased.
 * ========================================================================== */
object LedgerModel {

  // Constrained values are sum types, not bare strings. Scala 3 enums auto-namespace
  // their cases, so the collisions across entities (PendingReview/Rejected/Cancelled)
  // The status enums are top-level to keep generated case-object names stable.
  enum ObligationStatus { case Open, Realized, Cancelled }
  enum TxKind { case IncentiveCredit, ManualAdjustment, RollbackReversal, WithdrawalReserve, WithdrawalSettle, WithdrawalReturn }
  enum WithdrawalStatus { case PendingReview, Submitted, Settled, Rejected, Cancelled, Failed }
  enum ProposalStatus { case PendingReview, Approved, Rejected }

  case class LedgerTx(
      id:            FMLong,
      kind:          TxKind,
      debitAccount:  String,
      creditAccount: String,
      amount:        FMLong,
      sourceKind:    Option[String],
      sourceId:      Option[String],
      userUid:       String,
  )

  sealed trait LedgerError
  case object NonPositiveAmount extends LedgerError
  case object DuplicateSource extends LedgerError
  case class TxNotFound(id: FMLong) extends LedgerError
  case object WithdrawalNotFound extends LedgerError
  case object StatusConflict extends LedgerError
  case object ProposalNotFound extends LedgerError
  case object TwoPersonViolation extends LedgerError
  case object SourceTerminal extends LedgerError
  case object AlreadyReversed extends LedgerError
}
