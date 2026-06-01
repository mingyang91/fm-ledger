package io.linewise.jobfm

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doobie.*
import doobie.implicits.*
import io.linewise.jobfm.generated.JobModel.*
import io.linewise.jobfm.generated.StoreCore
import io.linewise.jobfm.generated.StoreCore.{JobRow, JobStore}
import io.linewise.jobfm.generated.StoreLaw.AbstractStore

/* =============================================================================
 * THE TWO STORE IMPLEMENTATIONS of the ONE interface (generated.StoreLaw.
 * AbstractStore — the same interface the worker, generated.WorkerCore.runOne, is
 * VERIFIED against via @law). There is no separate World: the worker, the driver,
 * and the differential test all use this one abstraction.
 *
 *   ListStore  — IN-MEMORY, delegates to the transpiler-GENERATED StoreCore
 *                (single-sourced from the verified ops). The differential ORACLE.
 *   JdbcStore  — TRUSTED doobie over `Db` at a single `eval` boundary; the worker
 *                RUNTIME and the differential SUBJECT.
 *
 * DifferentialSpec runs the worker (runOne) + handoff/sweep over BOTH and asserts
 * row-for-row agreement, machine-checking that the trusted doobie store realizes
 * the verified contract. (File named Stores.scala to avoid a case-insensitive-FS
 * clash with store.scala.)
 * ========================================================================== */

/* IN-MEMORY: every op delegates to the GENERATED StoreCore — drift-proof logic. */
final case class ListStore(js: JobStore) extends AbstractStore:
  def view: List[JobRow] = js.rows
  def claimOne(id: Long, worker: Long, stale: Boolean): AbstractStore =
    ListStore(StoreCore.claimOne(js, id, worker, stale))
  def applyOutcome(id: Long, outcome: Event): AbstractStore =
    ListStore(StoreCore.applyOutcome(js, id, outcome))
  def handoff(ev: Event2): AbstractStore = ListStore(StoreCore.handoff(js, ev))
  def sweep(dag: Dag): AbstractStore = ListStore(StoreCore.sweep(js, dag))
  def findById(id: Long): Option[JobState] =
    js.rows.find(_.id == id).map(_.st)

object ListStore:
  def seed(rows: List[JobRow]): ListStore = ListStore(JobStore(rows))

/* TRUSTED doobie: wraps `Db` at a single eval boundary; implements the SAME
 * AbstractStore the worker is verified against. applyOutcome derives the owner
 * from the row, exercising the production owner-revalidated saveState. */
final case class JdbcStore(xa: Transactor[IO]) extends AbstractStore:

  // THE eval boundary — the cats-effect IO -> bare value bridge, confined here.
  private def eval[A](c: ConnectionIO[A]): A = c.transact(xa).unsafeRunSync()

  // --- the AbstractStore interface (worker + differential test) ---

  def view: List[JobRow] =
    eval(Db.dumpAll).map { case (id, kind, status, owner, attempts, blockedOn) =>
      JobRow(id, JobState(status, owner, attempts, kind, blockedOn))
    }

  def claimOne(id: Long, worker: Long, stale: Boolean): AbstractStore =
    // lease policy realizing `stale`: lease=0 => any past heartbeat expired
    // (stale-reclaim); huge lease => a RUNNING row never looks stale.
    val nowN  = 1_000_000L
    val lease = if stale then 0L else 1_000_000_000L
    eval(Db.claim(id, worker, lease, nowN)); this

  def applyOutcome(id: Long, outcome: Event): AbstractStore =
    eval(Db.loadById(id).flatMap {
      case Some(st) if st.status == RUNNING && st.owner.isDefined =>
        Db.saveState(id, st.owner.get, step(st, outcome)).map(_ => ())
      case _ =>
        doobie.free.connection.pure(())
    }); this

  def handoff(ev: Event2): AbstractStore = { eval(Db.handoff(ev)); this }

  def sweep(dag: Dag): AbstractStore = { eval(Db.sweepOrphanedBlocked(dag)); this }

  def findById(id: Long): Option[JobState] = eval(Db.loadById(id))

  // --- driver helpers (main): schema/seed, the claim race, logging dumps ---

  def initSchema(): Unit = { eval(Db.ddlJob); eval(Db.ddlJobQueue); () }
  def seed(id: Long, st: JobState, nowNanos: Long): Unit = { eval(Db.seed(id, st, nowNanos)); () }
  def claim(id: Long, worker: Long, leaseNanos: Long, nowNanos: Long): Option[JobState] =
    eval(Db.claim(id, worker, leaseNanos, nowNanos))
  def handoffEvent(ev: Event2): List[(Long, JobState)] = eval(Db.handoff(ev))
  def sweepOrphans(dag: Dag): List[(Long, JobState)] = eval(Db.sweepOrphanedBlocked(dag))
  def dumpAll: List[(Long, Kind, Status, Option[Long], Int, Option[Event2])] = eval(Db.dumpAll)
  def dumpQueue: List[(Long, Kind, Status, Option[Long])] = eval(Db.dumpQueue)
