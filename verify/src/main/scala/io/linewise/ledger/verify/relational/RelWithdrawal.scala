package io.linewise.verify.fm.ledger.relational

import stainless.lang.*
import stainless.collection.*
import stainless.annotation.*
import io.linewise.verify.effect.FMLong
import io.linewise.verify.fm.ledger.LedgerModel.WithdrawalStatus

/* =============================================================================
 * RELATIONAL FM — Phase 2a: the WITHDRAWAL slice as tables.
 *   WITHDRAWAL row + WITHDRAWAL_STATUS_CHANGE rows (event stream). "Current status" is a JOIN:
 *   the latest status row for the withdrawal. We model the status table newest-first (a transition
 *   PREPENDS) so the latest = find-first, mirroring production's `ORDER BY ID DESC LIMIT 1`.
 *   Proven: a transition sets the current status; appends preserve referential integrity; adding a
 *   withdrawal preserves PK distinctness. (Concrete per-slice predicates — Stainless's first-order
 *   sweet spot; the kit reuses as a TEMPLATE, since generic higher-order extractors don't unify.)
 * ========================================================================== */
object RelWithdrawal {

  case class WithdrawalRow(id: BigInt, userUid: String, amount: FMLong, clientRequestId: String, reserveTxId: BigInt) {
    require(amount.value > BigInt(0))
  }
  case class WStatusRow(withdrawalId: BigInt, toStatus: WithdrawalStatus)
  case class WithdrawalTables(withdrawals: List[WithdrawalRow], statuses: List[WStatusRow])
  // (Has-per-table lens deferred to Phase 4, as in RelLedger.)

  def withdrawalIds(ws: List[WithdrawalRow]): List[BigInt] = ws.map(w => w.id)
  // the latest-status JOIN: statuses are newest-first, so the current status is the first match.
  def currentStatus(statuses: List[WStatusRow], wid: BigInt): Option[WithdrawalStatus] =
    statuses.find(s => s.withdrawalId == wid).map(s => s.toStatus)

  def statusRefOk(statuses: List[WStatusRow], wids: List[BigInt]): Boolean = statuses match
    case Nil()      => true
    case Cons(s, t) => wids.contains(s.withdrawalId) && statusRefOk(t, wids)
  def noWithdrawalWith(ws: List[WithdrawalRow], id: BigInt): Boolean = ws match
    case Nil()      => true
    case Cons(w, t) => w.id != id && noWithdrawalWith(t, id)
  def distinctWids(ws: List[WithdrawalRow]): Boolean = ws match
    case Nil()      => true
    case Cons(w, t) => noWithdrawalWith(t, w.id) && distinctWids(t)

  def refIntegrity(w: WithdrawalTables): Boolean = statusRefOk(w.statuses, withdrawalIds(w.withdrawals))
  def distinctWithdrawals(w: WithdrawalTables): Boolean = distinctWids(w.withdrawals)

  // a status append is a prepend (newest-first)
  def transition(w: WithdrawalTables, wid: BigInt, s: WithdrawalStatus): WithdrawalTables =
    w.copy(statuses = WStatusRow(wid, s) :: w.statuses)
  // requesting a withdrawal adds the row + an initial status atomically
  def addWithdrawal(w: WithdrawalTables, wr: WithdrawalRow, init: WithdrawalStatus): WithdrawalTables =
    WithdrawalTables(wr :: w.withdrawals, WStatusRow(wr.id, init) :: w.statuses)

  // weakening: more withdrawal ids can only help referential integrity
  @induct def statusRefOkConsId(statuses: List[WStatusRow], wids: List[BigInt], y: BigInt): Unit = {
    require(statusRefOk(statuses, wids)); ()
  }.ensuring(_ => statusRefOk(statuses, y :: wids))

  // ---- PROOF: a transition sets the current status (the view reflects the write) ----
  def transitionSetsCurrentStatus(w: WithdrawalTables, wid: BigInt, s: WithdrawalStatus): Boolean = {
    currentStatus(transition(w, wid, s).statuses, wid) == Some[WithdrawalStatus](s)
  }.holds

  // ---- PROOF: a transition of an EXISTING withdrawal preserves referential integrity ----
  def transitionPreservesRefIntegrity(w: WithdrawalTables, wid: BigInt, s: WithdrawalStatus): Boolean = {
    require(refIntegrity(w))
    require(withdrawalIds(w.withdrawals).contains(wid))
    refIntegrity(transition(w, wid, s))
  }.holds

  // ---- PROOF: adding a withdrawal (+ its initial status) preserves referential integrity ----
  def addWithdrawalPreservesRefIntegrity(w: WithdrawalTables, wr: WithdrawalRow, init: WithdrawalStatus): Boolean = {
    require(refIntegrity(w))
    statusRefOkConsId(w.statuses, withdrawalIds(w.withdrawals), wr.id)
    refIntegrity(addWithdrawal(w, wr, init))
  }.holds

  // ---- PROOF: adding a fresh withdrawal preserves PK distinctness ----
  def addWithdrawalPreservesDistinct(w: WithdrawalTables, wr: WithdrawalRow, init: WithdrawalStatus): Boolean = {
    require(distinctWithdrawals(w))
    require(noWithdrawalWith(w.withdrawals, wr.id))
    distinctWithdrawals(addWithdrawal(w, wr, init))
  }.holds
}
