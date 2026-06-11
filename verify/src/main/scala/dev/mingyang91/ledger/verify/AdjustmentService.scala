package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import LedgerModel._
import LedgerTables._
import ProposalTables._

/* =============================================================================
 * Two-person manual adjustments and rollback reversals over the DB's ledger + proposal tables.
 * Propose adds a proposal row + initial status; approve posts the materialized tx and records an
 * Approved status (with the result tx id); reject records a Rejected status. Through the single
 * Has[W, DB] lens and the ledger/proposal stores.
 * ========================================================================== */
case class AdjustmentService[W](has: Has[W, DB], lstore: LedgerStore, pstore: ProposalStore) {

  def propose(
      w: W, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String,
      amount: Long, reason: String, proposedBy: String, freshPid: Long,
      targetTxId: Option[Long]): (W, Either[LedgerError, Proposal]) = {
    val db = has.get(w)
    if !(amount > 0L) then (w, Left[LedgerError, Proposal](NonPositiveAmount))
    else if alreadyReversed(db, kind, targetTxId) then
      (w, Left[LedgerError, Proposal](AlreadyReversed))
    else
      val p = Proposal(freshPid, kind, userUid, debitAccount, creditAccount, amount, reason, proposedBy,
        ProposalStatus.PendingReview, None[Long](), targetTxId)
      (has(w).write((d: DB) => pstore.propose(d, p)), Right[LedgerError, Proposal](p))
  }

  def alreadyReversed(db: DB, kind: TxKind, targetTxId: Option[Long]): Boolean =
    kind == TxKind.RollbackReversal && (targetTxId match
      case Some(t) => pstore.all(db).exists((p: Proposal) => p.targetTxId == Some[Long](t) && p.status != ProposalStatus.Rejected)
      case _       => false)

  def approve(w: W, id: Long, expectedStatus: ProposalStatus, approver: String, freshTxId: Long): (W, Either[LedgerError, Proposal]) = {
    val db = has.get(w)
    pstore.get(db, id) match
      case Some(p) =>
        if p.status != expectedStatus || p.status != ProposalStatus.PendingReview then
          (w, Left[LedgerError, Proposal](StatusConflict))
        else if p.proposedBy == approver then
          (w, Left[LedgerError, Proposal](TwoPersonViolation))
        else if !(p.amount > 0L) then
          (w, Left[LedgerError, Proposal](NonPositiveAmount))
        else if !lstore.get(db, freshTxId).isEmpty then
          (w, Left[LedgerError, Proposal](DuplicateTxId))
        else
          val tx = twoLegTx(freshTxId, p.kind, p.debitAccount, p.creditAccount, p.amount, None[String](), None[String](), p.userUid)
          val w1 = has(w).write((d: DB) => pstore.decide(lstore.post(d, tx), id, ProposalStatus.Approved, Some[Long](freshTxId)))
          (w1, Right[LedgerError, Proposal](p.copy(status = ProposalStatus.Approved, resultTxId = Some[Long](freshTxId))))
      case _ => (w, Left[LedgerError, Proposal](ProposalNotFound))
  }

  def reject(w: W, id: Long, expectedStatus: ProposalStatus): (W, Either[LedgerError, Proposal]) = {
    val db = has.get(w)
    pstore.get(db, id) match
      case Some(p) =>
        if p.status != expectedStatus || p.status != ProposalStatus.PendingReview then
          (w, Left[LedgerError, Proposal](StatusConflict))
        else
          (has(w).write((d: DB) => pstore.decide(d, id, ProposalStatus.Rejected, None[Long]())),
            Right[LedgerError, Proposal](p.copy(status = ProposalStatus.Rejected)))
      case _ => (w, Left[LedgerError, Proposal](ProposalNotFound))
  }

  def get(w: W, id: Long): Either[LedgerError, Proposal] =
    pstore.get(has.get(w), id) match
      case Some(p) => Right[LedgerError, Proposal](p)
      case _       => Left[LedgerError, Proposal](ProposalNotFound)

  def all(w: W): List[Proposal] = pstore.all(has.get(w))
}
