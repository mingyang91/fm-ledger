package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import WorkerModel.World

/* =============================================================================
 * THE WORKER LOGIC — TRANSPILED. The run-one-row loop, written ONCE in
 * direct-style imperative form over the World capability + an abstract sidecar
 * (Kind => Event). The SAME code is verified here (against WorkerModel's in-memory
 * World) and GENERATED into production (against the hand-written
 * generated.WorkerModel, a doobie-backed World). The production worker is no
 * longer hand-written — it is this.
 *
 * Flow: claim (one store op) -> sidecar(kind) -> owner-recheck -> load ->
 * step(VERIFIED) -> owner-revalidated save. The sidecar is a capability
 * (Kind => Event) passed in: external IO stays outside the verified logic; runOne
 * is verified for ANY sidecar.
 *
 * THE HEARTBEAT IS NOT HERE — by design. It is the canonical trusted, Ox-bound,
 * wall-clock leaf (and Stainless forbids effects in lambdas, so it cannot be a
 * wrapping combinator over the mutating body anyway). In production the doobie
 * `World.claim` forks a SELF-TERMINATING heartbeat daemon for the claimed row;
 * the daemon renews while this worker owns the RUNNING row and exits on its own
 * once the terminal `saveState` clears ownership. The verify `World` does not
 * heartbeat (the wall-clock lease is absent from the verified state). So the
 * worker LOGIC is heartbeat-free and identical across both worlds.
 * ========================================================================== */
object WorkerCore {

  def runOne(w: World, id: FMLong, worker: FMLong, sidecar: Kind => Event): Option[JobState] = {
    // Some-first + `case _` (not a bare `case None()`, which the transpiler's R4
    // would leave un-rewritten); the None[T]()/Some[T](x) CONSTRUCTORS below are
    // bracketed, so R4 erases them to None/Some(x).
    w.claim(id, worker) match
      case Some(claimed) =>
        val event: Event = sidecar(claimed.kind)
        if !w.ownsRunning(id, worker) then None[JobState]()
        else
          w.loadById(id) match
            case Some(cur) =>
              val post = step(cur, event)
              if w.saveState(id, worker, post) then Some[JobState](post) else None[JobState]()
            case _ => None[JobState]()
      case _ => None[JobState]()
  }
}
