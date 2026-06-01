package io.linewise.jobfm

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doobie.*
import doobie.implicits.*
import io.linewise.jobfm.generated.JobModel.*
import io.linewise.jobfm.generated.StoreCore
import io.linewise.jobfm.generated.StoreCore.{JobRow, JobStore}

/* =============================================================================
 * THE SHARED STORE INTERFACE — "same signature, two bodies" (the user's choice).
 * (File named Stores.scala, not Store.scala, to avoid a case-insensitive-FS
 * collision with the existing store.scala on macOS.)
 *
 * The four worker ops + a `rows` observation. ListStore's body is the verified,
 * transpiler-GENERATED StoreCore ops (in-memory, single-sourced — no hand-written
 * logic). JdbcStore's body is the TRUSTED production doobie `Db` (SQL). They
 * share this signature; the differential test (DifferentialSpec) drives both over
 * the same scenarios and asserts they agree row-for-row, machine-checking the
 * trust. The @law contract on the abstract store (verify/StoreLaw.scala) is what
 * the in-memory body is PROVEN to satisfy and the doobie body is TRUSTED to.
 * ========================================================================== */
trait Store:
  def claimOne(id: Long, worker: Long, stale: Boolean): Store
  def applyOutcome(id: Long, outcome: Event): Store
  def handoff(ev: Event2): Store
  def sweep(dag: Dag): Store
  def rows: List[JobRow]

/* IN-MEMORY body: every op delegates to the GENERATED StoreCore, which is
 * transpiled from the Stainless source of truth. The logic is single-sourced, so
 * it cannot drift from what was verified. */
final case class ListStore(js: JobStore) extends Store:
  def rows: List[JobRow] = js.rows
  def claimOne(id: Long, worker: Long, stale: Boolean): Store =
    ListStore(StoreCore.claimOne(js, id, worker, stale))
  def applyOutcome(id: Long, outcome: Event): Store =
    ListStore(StoreCore.applyOutcome(js, id, outcome))
  def handoff(ev: Event2): Store =
    ListStore(StoreCore.handoff(js, ev))
  def sweep(dag: Dag): Store =
    ListStore(StoreCore.sweep(js, dag))

object ListStore:
  def seed(rows: List[JobRow]): ListStore = ListStore(JobStore(rows))

/* TRUSTED doobie body: the SINGLE eval boundary. `eval` is the ONLY place a
 * ConnectionIO is run to a value (.transact(xa).unsafeRunSync(), on an Ox virtual
 * thread). Every public method composes the package-private Db ConnectionIO ops
 * and returns a BARE value — no ConnectionIO/IO escapes into the shell. Two faces:
 *   - the verified-aligned `Store` trait (claimOne/applyOutcome/handoff/sweep/rows),
 *     used by the differential test;
 *   - the operational bare-value API (claim/heartbeatRenew/ownsRunning/loadById/
 *     saveState/seed/initSchema/handoffEvent/sweepOrphans/dumps), used by the
 *     worker loop and the driver in place of raw Db + transact.
 * applyOutcome derives the owner from the row (the worker holding the RUNNING
 * lease), so the production owner-revalidated saveState is exercised. */
final case class JdbcStore(xa: Transactor[IO]) extends Store:

  // THE eval boundary — the cats-effect IO -> bare value bridge, confined here.
  private def eval[A](c: ConnectionIO[A]): A = c.transact(xa).unsafeRunSync()

  // --- the verified-aligned Store trait (used by the differential test) ---

  def rows: List[JobRow] =
    eval(Db.dumpAll).map { case (id, kind, status, owner, attempts, blockedOn) =>
      JobRow(id, JobState(status, owner, attempts, kind, blockedOn))
    }

  def claimOne(id: Long, worker: Long, stale: Boolean): Store =
    // lease policy realizing `stale`: lease=0 => any past heartbeat is expired
    // (stale-reclaim path); huge lease => a RUNNING row never looks stale.
    val nowN  = 1_000_000L
    val lease = if stale then 0L else 1_000_000_000L
    eval(Db.claim(id, worker, lease, nowN)); this

  def applyOutcome(id: Long, outcome: Event): Store =
    eval(Db.loadById(id).flatMap {
      case Some(st) if st.status == RUNNING && st.owner.isDefined =>
        Db.saveState(id, st.owner.get, step(st, outcome)).map(_ => ())
      case _ =>
        doobie.free.connection.pure(())
    }); this

  def handoff(ev: Event2): Store = { eval(Db.handoff(ev)); this }

  def sweep(dag: Dag): Store = { eval(Db.sweepOrphanedBlocked(dag)); this }

  // --- operational bare-value API (worker + driver); no ConnectionIO leaked ---

  def initSchema(): Unit = { eval(Db.ddlJob); eval(Db.ddlJobQueue); () }

  def seed(id: Long, st: JobState, nowNanos: Long): Unit = { eval(Db.seed(id, st, nowNanos)); () }

  def claim(id: Long, worker: Long, leaseNanos: Long, nowNanos: Long): Option[JobState] =
    eval(Db.claim(id, worker, leaseNanos, nowNanos))

  def heartbeatRenew(id: Long, worker: Long, nowNanos: Long): Boolean =
    eval(Db.heartbeatRenew(id, worker, nowNanos))

  def ownsRunning(id: Long, worker: Long): Boolean =
    eval(Db.ownsRunning(id, worker))

  def loadById(id: Long): Option[JobState] =
    eval(Db.loadById(id))

  def saveState(id: Long, worker: Long, post: JobState): Boolean =
    eval(Db.saveState(id, worker, post))

  def handoffEvent(ev: Event2): List[(Long, JobState)] =
    eval(Db.handoff(ev))

  def sweepOrphans(dag: Dag): List[(Long, JobState)] =
    eval(Db.sweepOrphanedBlocked(dag))

  def dumpAll: List[(Long, Kind, Status, Option[Long], Int, Option[Event2])] =
    eval(Db.dumpAll)

  def dumpQueue: List[(Long, Kind, Status, Option[Long])] =
    eval(Db.dumpQueue)
