package io.linewise.jobfm

import ox.*
import io.linewise.jobfm.generated.JobModel.*

/* =============================================================================
 * THE WORKER — run-one-row. SHELL-ONLY, now fully UNBOXED.
 *
 * The worker no longer touches doobie/ConnectionIO/cats-effect. It talks to the
 * store through its BARE-VALUE API (claim/heartbeatRenew/ownsRunning/loadById/
 * saveState) — the `.transact(xa).unsafeRunSync()` boundary is confined inside
 * JdbcStore.eval (Stores.scala). So in this Ox direct-style loop every store
 * call returns a plain value, with no IO/Monad boxing. This is the linewise-api
 * idiom (private ConnectionIO Queries, composed + eval'd at one boundary, public
 * signatures unboxed) adapted to Ox: the boundary returns a value, not an F[A].
 *
 * TRANSACTION BOUNDARIES (each store call is its own tx, inside the store):
 *   - claim         : ONE tx (SELECT FOR UPDATE SKIP LOCKED + revalidating UPDATE)
 *   - the SIDECAR   : OUTSIDE any tx (no DB connection held while it runs)
 *   - heartbeat     : its own small tx per beat, on the heartbeat fiber
 *   - load + save   : the save is its own owner-revalidated tx
 * ========================================================================== */

// honest sidecar outcome tokens (reused verbatim from the in-memory shell)
enum Reason:
  case BadCodec, Timeout, MissingPrereq, Unplayable
final case class Output(note: String)

object Worker:

  // honest sidecar outcome -> runner Event (pure; no JobState touched).
  def outcomeToEvent(worker: Long, kind: Kind, r: Either[Reason, Output]): Event =
    r match
      case Right(_)                   => OutDone(worker)
      case Left(Reason.BadCodec)      => OutInputRejected(worker)
      case Left(Reason.Unplayable)    => OutNotApplicable(worker)
      case Left(Reason.Timeout)       => OutTransient(worker)
      case Left(Reason.MissingPrereq) => OutDone(worker)

  /** Run one row to a write-back against the store.
    *   1. claim (one tx, inside the store)
    *   2. heartbeat fiber renews the lease (its own small tx each beat)
    *   3. sidecar OUTSIDE the tx
    *   4. outcome -> Event -> step() [VERIFIED] -> saveState (its own tx)
    */
  def runOne(
      store: JdbcStore,
      id: Long,
      worker: Long,
      leaseNanos: Long,
      now: () => Long,
      sidecar: Kind => Either[Reason, Output],
      emit: Option[Kind => Event] = None,
      log: String => Unit
  )(using Ox): Option[JobState] =
    // (1) CLAIM — one transaction, inside the store; returns a bare Option.
    store.claim(id, worker, leaseNanos, now()) match
      case None =>
        None // lost the SKIP LOCKED race / not claimable
      case Some(claimed) =>
        val kind = claimed.kind
        log(f"  [w$worker%d] CLAIM   row=$id%-2d kind=$kind%-9s -> RUNNING")

        supervised:
          // HEARTBEAT FIBER — renew the lease in its own small tx while running.
          val beat = forkCancellable:
            var alive = true
            while alive do
              if !store.heartbeatRenew(id, worker, now()) then alive = false
              else sleep(scala.concurrent.duration.DurationInt(5).millis)
            ()

          // (3) SIDECAR — OUTSIDE any DB transaction. Then map outcome -> Event.
          val event: Event =
            emit match
              case Some(f) => f(kind)
              case None    => outcomeToEvent(worker, kind, sidecar(kind))

          // load the CURRENT state to fold the event through the verified core.
          val result =
            if !store.ownsRunning(id, worker) then
              log(f"  [w$worker%d] CLAIMLOST row=$id%-2d -> aborting write (no terminal state)")
              None
            else
              store.loadById(id) match
                case None => None
                case Some(cur) =>
                  val post = step(cur, event) // <-- VERIFIED step, unchanged
                  // (4) SAVE — its own owner-revalidated tx, inside the store.
                  val ok = store.saveState(id, worker, post)
                  if ok then
                    log(f"  [w$worker%d] WRITE   row=$id%-2d $event%-26s -> ${post.status}")
                    Some(post)
                  else
                    log(f"  [w$worker%d] WRITELOST row=$id%-2d (owner changed) -> no write")
                    None

          beat.cancel()
          result
