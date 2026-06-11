package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import LedgerModel._
import WithdrawalTables._
import LedgerInvariants._

/* =============================================================================
 * WITHDRAWAL PROOFS — VERIFY-ONLY. The withdrawal slice on the relational DB: adding a fresh
 * withdrawal and transitioning an existing one both preserve the withdrawal invariant (PK
 * distinctness + status-stream referential integrity), and a transition sets the current status
 * (the latest-row join reflects the write). Proven over the InMem store.
 * ========================================================================== */
object WithdrawalProofs {

  private val ws = InMemWithdrawalStore()

  // adding a withdrawal can only help status referential integrity (membership weakens)
  @induct def wStatusRefOkConsW(ss: List[WStatusRow], wds: List[WithdrawalRow], w: WithdrawalRow): Unit = {
    require(wStatusRefOk(ss, wds)); ()
  }.ensuring(_ => wStatusRefOk(ss, w :: wds))

  // ---- adding a fresh withdrawal (row + initial status) preserves the invariant ----
  def addWithdrawalPreservesValidW(db: DB, wd: Withdrawal): Unit = {
    require(distinctWithdrawals(db.withdrawals))
    require(wStatusRefOk(db.wstatuses, db.withdrawals))
    require(noWithdrawalWith(db.withdrawals, wd.id))
    wStatusRefOkConsW(db.wstatuses, db.withdrawals, toWRow(wd))
  }.ensuring(_ => validWithdrawals(ws.addWithdrawal(db, wd)))

  // ---- transitioning an existing withdrawal preserves the invariant ----
  def transitionPreservesValidW(db: DB, id: Long, st: WithdrawalStatus): Unit = {
    require(validWithdrawals(db))
    require(hasWithdrawal(db.withdrawals, id))
  }.ensuring(_ => validWithdrawals(ws.transition(db, id, st)))

  // ---- a transition sets the current status (the view reflects the write) ----
  def transitionSetsCurrentStatus(db: DB, id: Long, st: WithdrawalStatus): Boolean = {
    currentStatus(ws.transition(db, id, st).wstatuses, id) == Some[WithdrawalStatus](st)
  }.holds
}
