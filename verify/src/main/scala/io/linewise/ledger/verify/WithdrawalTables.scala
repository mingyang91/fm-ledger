package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import io.linewise.verify.effect.FMLong
import io.linewise.ledger.Db
import LedgerModel._

/* =============================================================================
 * WITHDRAWAL TABLE STORE — read/modify seam over the WITHDRAWAL + WITHDRAWAL_STATUS_CHANGE tables.
 * Status is an event stream kept newest-first; "current status" is the latest matching row, defaulting
 * to PendingReview when none (matching production's COALESCE). A fresh request adds a row plus its
 * initial status; a transition prepends a status row (the row is immutable). InMem here; the Jdbc
 * realization (deferred to integration) delegates to the production Db facade.
 * ========================================================================== */
object WithdrawalTables {

  def toWRow(wd: Withdrawal): WithdrawalRow = WithdrawalRow(wd.id, wd.userUid, wd.amount, wd.clientRequestId, wd.reserveTxId)
  def assembleW(r: WithdrawalRow, st: WithdrawalStatus): Withdrawal = Withdrawal(r.id, r.userUid, r.amount, st, r.clientRequestId, r.reserveTxId)

  def currentStatus(ss: List[WStatusRow], id: FMLong): Option[WithdrawalStatus] =
    ss.find((s: WStatusRow) => s.withdrawalId == id).map((s: WStatusRow) => s.toStatus)
  def currentStatusOr(ss: List[WStatusRow], id: FMLong): WithdrawalStatus =
    currentStatus(ss, id) match
      case Some(st) => st
      case _        => WithdrawalStatus.PendingReview

  sealed abstract class WithdrawalStore {
    def addWithdrawal(db: DB, wd: Withdrawal): DB
    def transition(db: DB, id: FMLong, toStatus: WithdrawalStatus): DB
    def get(db: DB, id: FMLong): Option[Withdrawal]
    def findByClientReq(db: DB, userUid: String, clientRequestId: String): Option[Withdrawal]
    def all(db: DB): List[Withdrawal]
  }

  case class InMemWithdrawalStore() extends WithdrawalStore {
    def addWithdrawal(db: DB, wd: Withdrawal): DB =
      db.copy(withdrawals = toWRow(wd) :: db.withdrawals, wstatuses = WStatusRow(wd.id, wd.status) :: db.wstatuses)
    def transition(db: DB, id: FMLong, toStatus: WithdrawalStatus): DB =
      db.copy(wstatuses = WStatusRow(id, toStatus) :: db.wstatuses)
    def get(db: DB, id: FMLong): Option[Withdrawal] =
      db.withdrawals.find((r: WithdrawalRow) => r.id == id) match
        case Some(r) => Some[Withdrawal](assembleW(r, currentStatusOr(db.wstatuses, id)))
        case _       => None[Withdrawal]()
    def findByClientReq(db: DB, userUid: String, clientRequestId: String): Option[Withdrawal] =
      db.withdrawals.find((r: WithdrawalRow) => r.userUid == userUid && r.clientRequestId == clientRequestId) match
        case Some(r) => Some[Withdrawal](assembleW(r, currentStatusOr(db.wstatuses, r.id)))
        case _       => None[Withdrawal]()
    def all(db: DB): List[Withdrawal] =
      db.withdrawals.map((r: WithdrawalRow) => assembleW(r, currentStatusOr(db.wstatuses, r.id)))
  }

  // Jdbc realization: @extern SQL via the production Db facade (row + status-change tables); db ignored.
  // A transition reads the current row from production, flips the status, and upserts (the facade
  // append the status change).
  case class JdbcWithdrawalStore() extends WithdrawalStore {
    @extern @pure
    def addWithdrawal(db: DB, wd: Withdrawal): DB = { Db.withdrawalPut(wd); db }
    @extern @pure
    def transition(db: DB, id: FMLong, toStatus: WithdrawalStatus): DB = {
      Db.withdrawalById(id) match
        case Some(wd) => Db.withdrawalPut(wd.copy(status = toStatus))
        case _        => ()
      db
    }
    @extern @pure
    def get(db: DB, id: FMLong): Option[Withdrawal] = Db.withdrawalById(id)
    @extern @pure
    def findByClientReq(db: DB, userUid: String, clientRequestId: String): Option[Withdrawal] =
      Db.withdrawalByClientReq(userUid, clientRequestId)
    @extern @pure
    def all(db: DB): List[Withdrawal] = Db.allWithdrawals
  }
}
