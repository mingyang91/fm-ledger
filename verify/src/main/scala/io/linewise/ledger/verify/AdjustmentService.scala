package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * Two-person manual adjustments and rollback reversals. Proposals stay scalar/two-leg
 * for now; approval materializes them as a LedgerTx whose core can now support many
 * entries even though this service still emits canonical two-leg postings.
 * ========================================================================== */
case class AdjustmentService[W](ledgerLens: Has[W, LedgerRepository], pLens: Has[W, ProposalRepository]) {

  def propose(
      w: W, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String,
      amount: FMLong, reason: String, proposedBy: String, freshPid: FMLong,
      targetTxId: Option[FMLong]): (W, Either[LedgerError, Proposal]) =
    if !(amount > FMLong(BigInt(0))) then (w, Left[LedgerError, Proposal](NonPositiveAmount))
    else if alreadyReversed(pLens.get(w), kind, targetTxId) then
      (w, Left[LedgerError, Proposal](AlreadyReversed))
    else
      val p = Proposal(freshPid, kind, userUid, debitAccount, creditAccount, amount, reason, proposedBy,
        ProposalStatus.PendingReview, None[FMLong](), targetTxId)
      (pLens(w).write((r: ProposalRepository) => r.put(p)), Right[LedgerError, Proposal](p))

  def alreadyReversed(repo: ProposalRepository, kind: TxKind, targetTxId: Option[FMLong]): Boolean =
    kind == TxKind.RollbackReversal && (targetTxId match
      case Some(t) => repo.all.exists((p: Proposal) => p.targetTxId == Some[FMLong](t) && p.status != ProposalStatus.Rejected)
      case _       => false)

  def approve(w: W, id: FMLong, expectedStatus: ProposalStatus, approver: String, freshTxId: FMLong): (W, Either[LedgerError, Proposal]) =
    pLens.get(w).get(id) match
      case Some(p) =>
        if p.status != expectedStatus || p.status != ProposalStatus.PendingReview then
          (w, Left[LedgerError, Proposal](StatusConflict))
        else if p.proposedBy == approver then
          (w, Left[LedgerError, Proposal](TwoPersonViolation))
        else
          val tx = twoLegTx(freshTxId, p.kind, p.debitAccount, p.creditAccount, p.amount, None[String](), None[String](), p.userUid)
          val w1 = ledgerLens(w).write((r: LedgerRepository) => r.post(tx))
          val p2 = p.copy(status = ProposalStatus.Approved, resultTxId = Some[FMLong](freshTxId))
          (pLens(w1).write((r: ProposalRepository) => r.put(p2)), Right[LedgerError, Proposal](p2))
      case _ => (w, Left[LedgerError, Proposal](ProposalNotFound))

  def reject(w: W, id: FMLong, expectedStatus: ProposalStatus): (W, Either[LedgerError, Proposal]) =
    pLens.get(w).get(id) match
      case Some(p) =>
        if p.status != expectedStatus || p.status != ProposalStatus.PendingReview then
          (w, Left[LedgerError, Proposal](StatusConflict))
        else
          val p2 = p.copy(status = ProposalStatus.Rejected)
          (pLens(w).write((r: ProposalRepository) => r.put(p2)), Right[LedgerError, Proposal](p2))
      case _ => (w, Left[LedgerError, Proposal](ProposalNotFound))

  def get(w: W, id: FMLong): Either[LedgerError, Proposal] =
    pLens.get(w).get(id) match
      case Some(p) => Right[LedgerError, Proposal](p)
      case _       => Left[LedgerError, Proposal](ProposalNotFound)

  def all(w: W): List[Proposal] = pLens.get(w).all
}
