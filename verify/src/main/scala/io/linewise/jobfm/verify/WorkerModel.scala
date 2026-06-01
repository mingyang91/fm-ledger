package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import StoreCore.{JobRow, JobStore}

/* =============================================================================
 * WORKER CAPABILITY — the VERIFY-ONLY model of the STORE the worker loop runs
 * against (`World`).
 *
 * NEVER TRANSPILED. WorkerCore.scala (the transpiled worker logic) references
 * `WorkerModel.World` via `import`. During verification that resolves to THIS
 * object (the in-memory store). In production the transpiled WorkerCore lands in
 * package io.linewise.jobfm.generated, where `import WorkerModel.World` resolves
 * to the HAND-WRITTEN generated.WorkerModel (a doobie-backed World) — same
 * unqualified name, different package, so no transpiler rename is needed.
 *
 * `World` is a MUTABLE store (var store): claim/saveState reassign it in place,
 * so the verified worker is written in the SAME direct-style mutate-in-place
 * shape as production (Stainless's AntiAliasing + ImperativeCodeElimination
 * verify it).
 *
 * THE HEARTBEAT lives ONLY in the production World (generated.WorkerModel): there
 * `claim` forks a self-terminating daemon for the claimed row. Here `claim` does
 * not heartbeat — the wall-clock lease is absent from the verified state, so the
 * heartbeat is a no-op on what we verify. This keeps the heartbeat (the trusted,
 * Ox-bound, non-terminating leaf) out of the verified worker entirely.
 * ========================================================================== */
object WorkerModel {

  def findState(s: JobStore, id: FMLong): Option[JobState] =
    s.rows.find((r: JobRow) => r.id == id).map((r: JobRow) => r.st)

  def setState(s: JobStore, id: FMLong, post: JobState): JobStore =
    JobStore(s.rows.map((r: JobRow) => if r.id == id then JobRow(r.id, post) else r))

  // The in-memory store: the verification model of the doobie store.
  case class World(var store: JobStore) {

    // claim: advance the row via the verified StoreCore.claimOne, read it back.
    def claim(id: FMLong, worker: FMLong): Option[JobState] = {
      store = StoreCore.claimOne(store, id, worker, false)
      findState(store, id) match
        case Some(st) if st.status == RUNNING && isOwner(st, worker) => Some[JobState](st)
        case _                                                       => None[JobState]()
    }

    // heartbeat renew: true iff this worker still holds the RUNNING lease. No
    // state change (the lease clock is not in the verified state).
    def heartbeatRenew(id: FMLong, worker: FMLong): Boolean =
      findState(store, id) match
        case Some(st) => st.status == RUNNING && isOwner(st, worker)
        case _        => false

    def ownsRunning(id: FMLong, worker: FMLong): Boolean =
      findState(store, id) match
        case Some(st) => st.status == RUNNING && isOwner(st, worker)
        case _        => false

    def loadById(id: FMLong): Option[JobState] = findState(store, id)

    // owner-revalidated write: only commit if this worker still owns the RUNNING
    // row (mirrors the doobie `UPDATE ... WHERE status='RUNNING' AND owner=?`).
    def saveState(id: FMLong, worker: FMLong, post: JobState): Boolean = {
      findState(store, id) match
        case Some(st) if st.status == RUNNING && isOwner(st, worker) =>
          store = setState(store, id, post)
          true
        case _ => false
    }
  }
}
