package io.linewise.verify.fm.ledger.relational

import stainless.lang.*
import stainless.collection.*
import stainless.annotation.*
import io.linewise.verify.effect.FMLong
import io.linewise.verify.fm.ledger.LedgerModel.{TxKind, ProposalStatus}

/* =============================================================================
 * RELATIONAL FM — Phase 2b: the PROPOSAL slice as tables (two-person adjustments/reversals).
 *   PROPOSAL row + PROPOSAL_STATUS_CHANGE rows (event stream, newest-first); current status +
 *   resultTxId are a latest-row JOIN. Same template as the withdrawal slice.
 * ========================================================================== */
object RelProposal {

  case class ProposalRow(
      id: BigInt, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String,
      amount: FMLong, reason: String, proposedBy: String, targetTxId: Option[BigInt]) {
    require(amount.value > BigInt(0))
  }
  case class PStatusRow(proposalId: BigInt, toStatus: ProposalStatus, resultTxId: Option[BigInt])
  case class ProposalTables(proposals: List[ProposalRow], statuses: List[PStatusRow])
  // (Has-per-table lens deferred to Phase 4, as in RelLedger.)

  def proposalIds(ps: List[ProposalRow]): List[BigInt] = ps.map(p => p.id)
  def currentStatus(statuses: List[PStatusRow], pid: BigInt): Option[ProposalStatus] =
    statuses.find(s => s.proposalId == pid).map(s => s.toStatus)
  def currentResultTx(statuses: List[PStatusRow], pid: BigInt): Option[BigInt] =
    statuses.find(s => s.proposalId == pid) match
      case Some(s) => s.resultTxId
      case _       => None[BigInt]()

  def statusRefOk(statuses: List[PStatusRow], pids: List[BigInt]): Boolean = statuses match
    case Nil()      => true
    case Cons(s, t) => pids.contains(s.proposalId) && statusRefOk(t, pids)
  def noProposalWith(ps: List[ProposalRow], id: BigInt): Boolean = ps match
    case Nil()      => true
    case Cons(p, t) => p.id != id && noProposalWith(t, id)
  def distinctPids(ps: List[ProposalRow]): Boolean = ps match
    case Nil()      => true
    case Cons(p, t) => noProposalWith(t, p.id) && distinctPids(t)

  def refIntegrity(w: ProposalTables): Boolean = statusRefOk(w.statuses, proposalIds(w.proposals))
  def distinctProposals(w: ProposalTables): Boolean = distinctPids(w.proposals)

  def decide(w: ProposalTables, pid: BigInt, s: ProposalStatus, resultTx: Option[BigInt]): ProposalTables =
    w.copy(statuses = PStatusRow(pid, s, resultTx) :: w.statuses)
  def propose(w: ProposalTables, pr: ProposalRow): ProposalTables =
    ProposalTables(pr :: w.proposals, PStatusRow(pr.id, ProposalStatus.PendingReview, None[BigInt]()) :: w.statuses)

  @induct def statusRefOkConsId(statuses: List[PStatusRow], pids: List[BigInt], y: BigInt): Unit = {
    require(statusRefOk(statuses, pids)); ()
  }.ensuring(_ => statusRefOk(statuses, y :: pids))

  def decideSetsCurrentStatus(w: ProposalTables, pid: BigInt, s: ProposalStatus, resultTx: Option[BigInt]): Boolean = {
    currentStatus(decide(w, pid, s, resultTx).statuses, pid) == Some[ProposalStatus](s)
  }.holds

  def decidePreservesRefIntegrity(w: ProposalTables, pid: BigInt, s: ProposalStatus, resultTx: Option[BigInt]): Boolean = {
    require(refIntegrity(w))
    require(proposalIds(w.proposals).contains(pid))
    refIntegrity(decide(w, pid, s, resultTx))
  }.holds

  def proposePreservesRefIntegrity(w: ProposalTables, pr: ProposalRow): Boolean = {
    require(refIntegrity(w))
    statusRefOkConsId(w.statuses, proposalIds(w.proposals), pr.id)
    refIntegrity(propose(w, pr))
  }.holds

  def proposePreservesDistinct(w: ProposalTables, pr: ProposalRow): Boolean = {
    require(distinctProposals(w))
    require(noProposalWith(w.proposals, pr.id))
    distinctProposals(propose(w, pr))
  }.holds
}
