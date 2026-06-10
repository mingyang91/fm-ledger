package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import io.linewise.verify.effect.FMLong
import io.linewise.ledger.Db
import LedgerModel._

/* =============================================================================
 * PROPOSAL TABLE STORE — read/modify seam over PROPOSAL + PROPOSAL_STATUS_CHANGE. Same event-stream
 * shape as withdrawals; the latest status row also carries the result tx id. A decision prepends a
 * status row; the row is immutable. InMem here; Jdbc deferred to integration.
 * ========================================================================== */
object ProposalTables {

  def toPRow(p: Proposal): ProposalRow =
    ProposalRow(p.id, p.kind, p.userUid, p.debitAccount, p.creditAccount, p.amount, p.reason, p.proposedBy, p.targetTxId)
  def assembleP(r: ProposalRow, st: ProposalStatus, resultTx: Option[FMLong]): Proposal =
    Proposal(r.id, r.kind, r.userUid, r.debitAccount, r.creditAccount, r.amount, r.reason, r.proposedBy, st, resultTx, r.targetTxId)

  def latest(ss: List[PStatusRow], id: FMLong): Option[PStatusRow] =
    ss.find((s: PStatusRow) => s.proposalId == id)

  sealed abstract class ProposalStore {
    def propose(db: DB, p: Proposal): DB
    def decide(db: DB, id: FMLong, toStatus: ProposalStatus, resultTx: Option[FMLong]): DB
    def get(db: DB, id: FMLong): Option[Proposal]
    def all(db: DB): List[Proposal]
  }

  case class InMemProposalStore() extends ProposalStore {
    def propose(db: DB, p: Proposal): DB =
      db.copy(proposals = toPRow(p) :: db.proposals, pstatuses = PStatusRow(p.id, ProposalStatus.PendingReview, None[FMLong]()) :: db.pstatuses)
    def decide(db: DB, id: FMLong, toStatus: ProposalStatus, resultTx: Option[FMLong]): DB =
      db.copy(pstatuses = PStatusRow(id, toStatus, resultTx) :: db.pstatuses)
    def get(db: DB, id: FMLong): Option[Proposal] =
      db.proposals.find((r: ProposalRow) => r.id == id) match
        case Some(r) =>
          latest(db.pstatuses, id) match
            case Some(s) => Some[Proposal](assembleP(r, s.toStatus, s.resultTxId))
            case _       => Some[Proposal](assembleP(r, ProposalStatus.PendingReview, None[FMLong]()))
        case _ => None[Proposal]()
    def all(db: DB): List[Proposal] =
      db.proposals.map((r: ProposalRow) =>
        latest(db.pstatuses, r.id) match
          case Some(s) => assembleP(r, s.toStatus, s.resultTxId)
          case _       => assembleP(r, ProposalStatus.PendingReview, None[FMLong]()))
  }

  // Jdbc realization: @extern SQL via the production Db facade; db ignored.
  case class JdbcProposalStore() extends ProposalStore {
    @extern @pure
    def propose(db: DB, p: Proposal): DB = { Db.proposalPut(p); db }
    @extern @pure
    def decide(db: DB, id: FMLong, toStatus: ProposalStatus, resultTx: Option[FMLong]): DB = {
      Db.proposalById(id) match
        case Some(p) => Db.proposalPut(p.copy(status = toStatus, resultTxId = resultTx))
        case _       => ()
      db
    }
    @extern @pure
    def get(db: DB, id: FMLong): Option[Proposal] = Db.proposalById(id)
    @extern @pure
    def all(db: DB): List[Proposal] = Db.allProposals
  }
}
