package io.linewise.jobfm

import io.linewise.jobfm.generated.JobModel.*

/* =============================================================================
 * SIDECAR ADAPTER — the only remaining hand-written worker glue.
 *
 * The worker LOOP is no longer here: it is GENERATED from the verified source
 * (verify/WorkerCore.scala -> generated.WorkerCore.runOne), running against the
 * doobie `generated.WorkerModel.World`. What stays hand-written is the adapter
 * from the external sidecar's result ADT to a runner `Event` — genuinely shell
 * (the sidecar is an external service), fed to the generated runOne as the
 * `sidecar: Kind => Event` capability.
 * ========================================================================== */

// honest sidecar outcome tokens
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
