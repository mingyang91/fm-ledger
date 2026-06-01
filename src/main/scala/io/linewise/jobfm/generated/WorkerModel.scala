package io.linewise.jobfm.generated

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doobie.*
import doobie.implicits.*
import ox.*
import io.linewise.jobfm.Db
import io.linewise.jobfm.generated.JobModel.*

/* =============================================================================
 * PRODUCTION realization of the worker's `World` capability (the verify model is
 * verify/WorkerModel.scala). HAND-WRITTEN, in the `generated` package so the
 * transpiled generated.WorkerCore's `import WorkerModel.World` resolves to it —
 * same unqualified name, different package, no transpiler rename.
 *
 * The store ops delegate to the package-private doobie `Db` (the trusted SQL,
 * bound to the verified StoreCore by DifferentialSpec). `eval` is the single
 * IO->value boundary. THE HEARTBEAT lives here and only here: `claim` forks a
 * SELF-TERMINATING daemon that renews the lease while this worker owns the
 * RUNNING row and exits on its own once the terminal `saveState` clears
 * ownership (heartbeatRenew then returns false). The Ox scope is captured at
 * construction, so the worker logic (generated.WorkerCore.runOne) needs no
 * `using Ox` — it just calls `w.claim` and a heartbeat appears underneath.
 * ========================================================================== */
object WorkerModel {

  private val LeaseNanos = 1_000_000_000L

  class World(xa: Transactor[IO])(using ox: Ox) {

    private def eval[A](c: ConnectionIO[A]): A = c.transact(xa).unsafeRunSync()

    def claim(id: Long, worker: Long): Option[JobState] =
      eval(Db.claim(id, worker, LeaseNanos, System.nanoTime())) match
        case some @ Some(_) =>
          // self-terminating heartbeat daemon for the claimed row.
          forkCancellable {
            var alive = true
            while alive do
              if heartbeatRenew(id, worker) then sleep(scala.concurrent.duration.DurationInt(5).millis)
              else alive = false
          }
          some
        case none => none

    def heartbeatRenew(id: Long, worker: Long): Boolean =
      eval(Db.heartbeatRenew(id, worker, System.nanoTime()))

    def ownsRunning(id: Long, worker: Long): Boolean =
      eval(Db.ownsRunning(id, worker))

    def loadById(id: Long): Option[JobState] =
      eval(Db.loadById(id))

    def saveState(id: Long, worker: Long, post: JobState): Boolean =
      eval(Db.saveState(id, worker, post))
  }
}
