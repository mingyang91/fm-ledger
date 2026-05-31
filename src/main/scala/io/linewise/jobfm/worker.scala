package io.linewise.jobfm

import ox.*
import doobie.*
import doobie.implicits.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.linewise.jobfm.generated.JobModel.*

/* =============================================================================
 * THE WORKER — run-one-row, now against the REAL DB. SHELL-ONLY.
 *
 * Structure unchanged from the in-memory worker; only the store is now doobie.
 * The CATS-EFFECT <-> DIRECT-STYLE BRIDGE is the bare-value pattern:
 *   prog.transact(xa).unsafeRunSync()
 * runs a doobie ConnectionIO to a plain value, synchronously, from inside the
 * Ox direct-style code. On JVM 21 virtual threads this blocking call is cheap.
 *
 * TRANSACTION BOUNDARIES:
 *   - claim         : ONE tx (SELECT FOR UPDATE SKIP LOCKED + revalidating UPDATE)
 *   - the SIDECAR   : OUTSIDE any tx (no DB connection held while the external
 *                     service runs — the lease/heartbeat covers that window)
 *   - load + save   : the load is the claim's tx; the save is its OWN tx
 *                     (load-row -> step() -> save-row maps onto .transact(xa))
 * ========================================================================== */

// honest sidecar outcome tokens (reused verbatim from the in-memory shell)
enum Reason:
  case BadCodec, Timeout, MissingPrereq, Unplayable
final case class Output(note: String)

object Worker:

  /** Run a ConnectionIO to a bare value from Ox direct-style. THIS is the
    * cats-effect <-> direct-style boundary. */
  def run[A](prog: ConnectionIO[A], xa: Transactor[IO]): A =
    prog.transact(xa).unsafeRunSync()

  // honest sidecar outcome -> runner Event (pure; no JobState touched).
  def outcomeToEvent(worker: Long, kind: Kind, r: Either[Reason, Output]): Event =
    r match
      case Right(_)                   => OutDone(worker)
      case Left(Reason.BadCodec)      => OutInputRejected(worker)
      case Left(Reason.Unplayable)    => OutNotApplicable(worker)
      case Left(Reason.Timeout)       => OutTransient(worker)
      case Left(Reason.MissingPrereq) => OutDone(worker)

  /** Run one row to a write-back against the DB.
    *   1. claim (one tx)
    *   2. heartbeat fiber renews the lease (its own small tx each beat)
    *   3. sidecar OUTSIDE the tx
    *   4. outcome -> Event -> step() [VERIFIED] -> saveState (its own tx)
    */
  def runOne(
      xa: Transactor[IO],
      id: Long,
      worker: Long,
      leaseNanos: Long,
      now: () => Long,
      sidecar: Kind => Either[Reason, Output],
      emit: Option[Kind => Event] = None,
      log: String => Unit
  )(using Ox): Option[JobState] =
    // (1) CLAIM — one transaction.
    run(Db.claim(id, worker, leaseNanos, now()), xa) match
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
              if !run(Db.heartbeatRenew(id, worker, now()), xa) then alive = false
              else sleep(scala.concurrent.duration.DurationInt(5).millis)
            ()

          // (3) SIDECAR — OUTSIDE any DB transaction. The external service runs
          // here; no connection is held. Then map outcome -> Out* event.
          val event: Event =
            emit match
              case Some(f) => f(kind)
              case None    => outcomeToEvent(worker, kind, sidecar(kind))

          // load the CURRENT state (point-query) to fold the event through the
          // verified core. step() computes newState; saveState persists it.
          val result =
            if !run(Db.ownsRunning(id, worker), xa) then
              log(f"  [w$worker%d] CLAIMLOST row=$id%-2d -> aborting write (no terminal state)")
              None
            else
              run(Db.loadById(id), xa) match
                case None => None
                case Some(cur) =>
                  val post = step(cur, event) // <-- VERIFIED step, unchanged
                  // (4) SAVE — its own tx, owner-revalidated UPDATE.
                  val ok = run(Db.saveState(id, worker, post), xa)
                  if ok then
                    log(f"  [w$worker%d] WRITE   row=$id%-2d $event%-26s -> ${post.status}")
                    Some(post)
                  else
                    log(f"  [w$worker%d] WRITELOST row=$id%-2d (owner changed) -> no write")
                    None

          beat.cancel()
          result
