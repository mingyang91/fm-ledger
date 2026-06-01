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

/* TRUSTED doobie body: wraps the PRODUCTION Db ops (store.scala) — the same SQL
 * the real worker runs. Each op runs its ConnectionIO against the transactor and
 * returns this handle (the DB itself is the mutated state; `rows` reads it back).
 * applyOutcome derives the owner from the row (the worker holding the RUNNING
 * lease), so the production owner-revalidated saveState is exercised. */
final case class JdbcStore(xa: Transactor[IO]) extends Store:
  private def run[A](c: ConnectionIO[A]): A = c.transact(xa).unsafeRunSync()

  def rows: List[JobRow] =
    run(Db.dumpAll).map { case (id, kind, status, owner, attempts, blockedOn) =>
      JobRow(id, JobState(status, owner, attempts, kind, blockedOn))
    }

  def claimOne(id: Long, worker: Long, stale: Boolean): Store =
    // lease policy realizing `stale`: lease=0 => any past heartbeat is expired
    // (stale-reclaim path); huge lease => a RUNNING row never looks stale.
    // PENDING/PARTIAL rows are claimable regardless, so deterministic scenarios
    // (claims on PENDING) succeed either way.
    val now   = 1_000_000L
    val lease = if stale then 0L else 1_000_000_000L
    run(Db.claim(id, worker, lease, now))
    this

  def applyOutcome(id: Long, outcome: Event): Store =
    run(Db.loadById(id).flatMap {
      case Some(st) if st.status == RUNNING && st.owner.isDefined =>
        Db.saveState(id, st.owner.get, step(st, outcome)).map(_ => ())
      case _ =>
        doobie.free.connection.pure(())
    })
    this

  def handoff(ev: Event2): Store =
    run(Db.handoff(ev))
    this

  def sweep(dag: Dag): Store =
    run(Db.sweepOrphanedBlocked(dag))
    this
