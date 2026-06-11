package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import LedgerModel._
import ProposalTables._
import LedgerInvariants._

/* =============================================================================
 * ADJUSTMENT / PROPOSAL PROOFS — VERIFY-ONLY. The proposal slice on the relational DB: proposing a
 * fresh proposal and deciding an existing one preserve the proposal invariant (PK distinctness +
 * status-stream referential integrity), and a decision sets the current status. Proven over InMem.
 * ========================================================================== */
object AdjustmentProofs {

  private val ps = InMemProposalStore()

  @induct def pStatusRefOkConsP(ss: List[PStatusRow], prs: List[ProposalRow], p: ProposalRow): Unit = {
    require(pStatusRefOk(ss, prs)); ()
  }.ensuring(_ => pStatusRefOk(ss, p :: prs))

  def proposePreservesValidP(db: DB, p: Proposal): Unit = {
    require(distinctProposals(db.proposals))
    require(pStatusRefOk(db.pstatuses, db.proposals))
    require(noProposalWith(db.proposals, p.id))
    pStatusRefOkConsP(db.pstatuses, db.proposals, toPRow(p))
  }.ensuring(_ => validProposals(ps.propose(db, p)))

  def decidePreservesValidP(db: DB, id: Long, st: ProposalStatus, resultTx: Option[Long]): Unit = {
    require(validProposals(db))
    require(hasProposal(db.proposals, id))
  }.ensuring(_ => validProposals(ps.decide(db, id, st, resultTx)))

  def decideSetsCurrentStatus(db: DB, id: Long, st: ProposalStatus, resultTx: Option[Long]): Boolean = {
    latest(ps.decide(db, id, st, resultTx).pstatuses, id) == Some[PStatusRow](PStatusRow(id, st, resultTx))
  }.holds
}
