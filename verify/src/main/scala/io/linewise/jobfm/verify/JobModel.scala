package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}

/* =============================================================================
 * THE SIMPLIFIED-BUT-FAITHFUL JOB SYSTEM — BUSINESS CORE (source of truth)
 *
 * A PURE state machine over 7 finite states + a static cross-kind dependency
 * DAG modeled as a DATA value. This mirrors the production worker-pool whose
 * row-as-source-of-truth design makes the FM core and the production
 * transition the SAME pure function: `step(JobState, Event): JobState`.
 *
 * THIS FILE IS THE SINGLE VERIFIABLE SOURCE OF TRUTH for the business logic.
 * The production core at src/main/scala/io/linewise/jobfm/generated/JobModel.scala
 * is GENERATED from this file by the `transpiler` module (do not hand-edit it).
 * The PROOFS that this model satisfies its specification live in JobProofs.scala,
 * which imports this object.
 *
 * Grounding (production code):
 *   - jobs/workerpool/JobState.scala:79   -> 7-status enum
 *   - jobs/workerpool/RunnerError.scala   -> the outcome ADT we branch on
 *   - docs/job-system-architecture.md     -> the lifecycle edges
 *
 * Concurrency (claim, heartbeat, stale-reclaim) lives in the worker-pool shell
 * and is modeled here SEQUENTIALLY via an ownership guard; interleaving is out
 * of scope (stated explicitly in CLAIM/STALE SAFETY in JobProofs.scala).
 * ========================================================================== */

object JobModel {

  // --- 7 STATES (mirror JobStatus enum) ----------------------------------
  sealed trait Status
  case object PENDING        extends Status
  case object RUNNING        extends Status
  case object DONE           extends Status
  case object FAILED         extends Status
  case object NOT_APPLICABLE extends Status
  case object PARTIAL        extends Status
  case object BLOCKED        extends Status

  // terminal-by-classification: NOT_APPLICABLE.
  // terminal-by-failure:        FAILED, DONE.
  def isTerminal(s: Status): Boolean = s match
    case DONE | FAILED | NOT_APPLICABLE => true
    case _                              => false

  def isNonTerminal(s: Status): Boolean = !isTerminal(s)

  // --- KINDS (simplified set with real DAG edges) ------------------------
  sealed trait Kind
  case object Transcode extends Kind  // produces "videoMeta.duration"
  case object Embedding extends Kind  // consumes it; produces "embedding.ready"
  case object Masking   extends Kind  // consumes "embedding.ready"
  case object Import    extends Kind  // produces "gcs_uri"
  case object RagIndex  extends Kind  // consumes "gcs_uri"

  // --- EVENTS ------------------------------------------------------------
  // Named upstream signals that unblock downstream kinds, plus the lifecycle
  // drivers (claim / heartbeat / runner outcome / resets / sweeper).
  sealed trait Event

  // ownership / lifecycle drivers
  case class Claim(worker: FMLong) extends Event      // PENDING|PARTIAL -> RUNNING (claim after runAt)
  case class Heartbeat(worker: FMLong) extends Event  // RUNNING -> RUNNING (renew lease, same owner)
  case class GracefulShutdown(worker: FMLong) extends Event // RUNNING -> PENDING (drain)

  // runner outcomes (the honest sidecar / runner result, see RunnerError.scala)
  case class OutDone(worker: FMLong) extends Event                 // -> DONE
  case class OutNotApplicable(worker: FMLong) extends Event        // -> NOT_APPLICABLE (terminal classification)
  case class OutUnrecoverable(worker: FMLong) extends Event        // -> FAILED immediately
  case class OutInputRejected(worker: FMLong) extends Event        // -> FAILED immediately (no Sentry)
  case class OutTransient(worker: FMLong) extends Event            // attempts++ ; PENDING or FAILED at max
  case class OutBlocked(worker: FMLong, waitFor: Event2) extends Event // -> BLOCKED parked on a named event
  case class OutPartial(worker: FMLong) extends Event              // -> PARTIAL (embedding only, Handled)

  // upstream / sweeper / producer drivers
  case class UpstreamEvent(name: Event2) extends Event // a named producer event fired -> unblock matching BLOCKED
  case object SweeperUpstreamFailed extends Event      // upstream FAILED: BLOCKED -> NOT_APPLICABLE
  case object ProducerReset extends Event              // terminal -> PENDING (re-enqueue)

  // --- NAMED upstream events (the DAG's edge labels) ---------------------
  // Distinct from Event (the transition driver). These are the data the DAG
  // is built from. Kept as a tiny finite enum so equality is decidable.
  sealed trait Event2
  case object VideoMetaDuration extends Event2  // Transcode.DONE produces
  case object EmbeddingReady    extends Event2  // Embedding.DONE produces
  case object GcsUri            extends Event2  // Import.DONE produces

  // --- JobState value (status + owner + attempts + kind + blockedOn) -----
  case class JobState(
      status: Status,
      owner: Option[FMLong],     // WorkerId of the claiming worker
      attempts: FMInt,           // retry counter; >= 0
      kind: Kind,
      blockedOn: Option[Event2]  // the named upstream event a BLOCKED row waits for
  ) {
    require(attempts >= FMInt(BigInt(0)) && attempts <= maxAttempts)
  }

  val maxAttempts: FMInt = FMInt(BigInt(5))

  /* ---------------------------------------------------------------------
   * OWNERSHIP GUARD (CLAIM/STALE SAFETY, sequential model)
   *
   * A worker may write a terminal/outcome state for a RUNNING row only if it
   * is the current owner. A worker whose ownership was lost (ClaimLost) must
   * NOT write terminal state. We model "is w the current owner" as a guard on
   * the runner-outcome events; if the guard fails the row is left untouched
   * (the stale write is a no-op), which is exactly the production rule.
   * ------------------------------------------------------------------- */
  def isOwner(s: JobState, w: FMLong): Boolean =
    s.owner == Some[FMLong](w)

  /* ---------------------------------------------------------------------
   * THE PURE TRANSITION FUNCTION  step(s, e): JobState
   *
   * This is the SAME function the production dispatcher computes after a
   * runner returns. Total over all (state, event) pairs: any event that is
   * not legal for the current status leaves the row unchanged (a no-op),
   * matching the production "filter terminal in claim SQL + guard on owner".
   * ------------------------------------------------------------------- */
  def step(s: JobState, e: Event): JobState = {
    e match
      // --- claim: PENDING|PARTIAL -> RUNNING, set owner ---
      case Claim(w) =>
        s.status match
          case PENDING | PARTIAL =>
            s.copy(status = RUNNING, owner = Some[FMLong](w))
          case _ => s

      // --- heartbeat: RUNNING -> RUNNING, only by current owner ---
      case Heartbeat(w) =>
        if s.status == RUNNING && isOwner(s, w) then s else s

      // --- graceful shutdown: RUNNING -> PENDING, release owner ---
      case GracefulShutdown(w) =>
        if s.status == RUNNING && isOwner(s, w) then
          s.copy(status = PENDING, owner = None[FMLong]())
        else s

      // --- runner outcome: DONE (owner-guarded) ---
      case OutDone(w) =>
        if s.status == RUNNING && isOwner(s, w) then
          s.copy(status = DONE, owner = None[FMLong]())
        else s

      // --- runner outcome: NOT_APPLICABLE terminal classification ---
      case OutNotApplicable(w) =>
        if s.status == RUNNING && isOwner(s, w) then
          s.copy(status = NOT_APPLICABLE, owner = None[FMLong]())
        else s

      // --- runner outcome: Unrecoverable -> FAILED immediately ---
      case OutUnrecoverable(w) =>
        if s.status == RUNNING && isOwner(s, w) then
          s.copy(status = FAILED, owner = None[FMLong]())
        else s

      // --- runner outcome: InputRejected -> FAILED immediately ---
      case OutInputRejected(w) =>
        if s.status == RUNNING && isOwner(s, w) then
          s.copy(status = FAILED, owner = None[FMLong]())
        else s

      // --- runner outcome: Transient -> PENDING (attempts++) or FAILED at max ---
      case OutTransient(w) =>
        if s.status == RUNNING && isOwner(s, w) then
          if s.attempts + FMInt(BigInt(1)) >= maxAttempts then
            s.copy(status = FAILED, owner = None[FMLong](), attempts = maxAttempts)
          else
            s.copy(status = PENDING, owner = None[FMLong](), attempts = s.attempts + FMInt(BigInt(1)))
        else s

      // --- runner outcome: Blocked -> BLOCKED parked on a named event ---
      case OutBlocked(w, waitFor) =>
        if s.status == RUNNING && isOwner(s, w) then
          s.copy(status = BLOCKED, owner = None[FMLong](), blockedOn = Some[Event2](waitFor))
        else s

      // --- runner outcome: Partial (embedding only) -> PARTIAL ---
      case OutPartial(w) =>
        if s.status == RUNNING && isOwner(s, w) && s.kind == Embedding then
          s.copy(status = PARTIAL, owner = None[FMLong]())
        else s

      // --- upstream named event fired: BLOCKED -> PENDING iff it matches ---
      case UpstreamEvent(name) =>
        if s.status == BLOCKED && s.blockedOn == Some[Event2](name) then
          s.copy(status = PENDING, blockedOn = None[Event2]())
        else s

      // --- sweeper sees upstream FAILED: BLOCKED -> NOT_APPLICABLE ---
      case SweeperUpstreamFailed =>
        if s.status == BLOCKED then
          s.copy(status = NOT_APPLICABLE, blockedOn = None[Event2]())
        else s

      // --- producer reset: any terminal -> PENDING (re-enqueue, attempts cleared) ---
      case ProducerReset =>
        if isTerminal(s.status) then
          s.copy(status = PENDING, owner = None[FMLong](), attempts = FMInt(BigInt(0)), blockedOn = None[Event2]())
        else s
  }.ensuring(res => res.attempts >= FMInt(BigInt(0)) && res.attempts <= maxAttempts)

  /* =====================================================================
   * THE CROSS-KIND DEPENDENCY DAG, modeled as a DATA VALUE.
   *
   * Two relations as association lists:
   *   produces : Kind  -> Event2   (the upstream kind's DONE produces this event)
   *   waitsOn  : Kind  -> Event2   (the downstream kind seeds BLOCKED on this)
   * ===================================================================== */
  case class Dag(
      produces: List[(Kind, Event2)], // kind's DONE produces event
      waitsOn:  List[(Kind, Event2)]  // kind seeds BLOCKED waiting for event
  )

  // The real (simplified) wiring:
  //   Transcode.DONE produces videoMeta.duration -> unblocks Embedding
  //   Embedding.DONE produces embedding.ready     -> unblocks Masking
  //   Import.DONE    produces gcs_uri             -> unblocks RagIndex
  val fullDag: Dag = Dag(
    produces = List[(Kind, Event2)](
      (Transcode, VideoMetaDuration),
      (Embedding, EmbeddingReady),
      (Import,    GcsUri)
    ),
    waitsOn = List[(Kind, Event2)](
      (Embedding, VideoMetaDuration), // Embedding seeded BLOCKED on transcode's event
      (Masking,   EmbeddingReady),    // Masking seeded BLOCKED on embedding's event
      (RagIndex,  GcsUri)             // RagIndex seeded BLOCKED on import's event
    )
  )

  // is there some kind whose DONE produces event `ev`? (the sweeper is the
  // implicit always-present fallback producer of NOT_APPLICABLE, but for the
  // headline coverage property we require a real upstream DONE producer.)
  def hasProducer(d: Dag, ev: Event2): Boolean =
    d.produces.exists((p: (Kind, Event2)) => p._2 == ev)

  // every (kind, waitEvent) edge has a producer of waitEvent.
  def coverage(d: Dag): Boolean =
    d.waitsOn.forall((w: (Kind, Event2)) => hasProducer(d, w._2))

  /* --- THE EVOLUTION GAP ------------------------------------------------
   * Scenario: REMOVE the Embedding kind from the DAG (a refactor drops the
   * embedding stage). Embedding was BOTH the consumer of Transcode's event
   * AND the producer of "embedding.ready" that Masking waits for. Dropping it
   * removes the producer row (Embedding, EmbeddingReady) but leaves Masking's
   * wait edge (Masking, EmbeddingReady) dangling. Now no kind's DONE produces
   * EmbeddingReady, so Masking is BLOCKED forever — an ORPHANED BLOCKED.
   * ------------------------------------------------------------------- */
  val embeddingRemovedDag: Dag = Dag(
    produces = List[(Kind, Event2)](
      (Transcode, VideoMetaDuration),
      // (Embedding, EmbeddingReady)  <-- REMOVED: this is the dangling edge cause
      (Import,    GcsUri)
    ),
    waitsOn = List[(Kind, Event2)](
      // (Embedding, VideoMetaDuration) <-- Embedding consumer also gone
      (Masking,  EmbeddingReady),  // <-- ORPHANED: nobody produces EmbeddingReady now
      (RagIndex, GcsUri)
    )
  )

  /* --- RESOLUTION -------------------------------------------------------
   * Re-wire Transcode.DONE to ALSO produce EmbeddingReady (or, equivalently,
   * re-add the Embedding kind). Either restores a producer for the orphaned
   * event and makes coverage hold again. JobProofs.scala proves the re-wire.
   * ------------------------------------------------------------------- */
  val rewiredDag: Dag = Dag(
    produces = List[(Kind, Event2)](
      (Transcode, VideoMetaDuration),
      (Transcode, EmbeddingReady), // <-- RE-WIRE: Transcode now also produces it
      (Import,    GcsUri)
    ),
    waitsOn = List[(Kind, Event2)](
      (Masking,  EmbeddingReady),
      (RagIndex, GcsUri)
    )
  )

  // The named event a kind's DONE produces, derived from the DAG value.
  def producedEventOf(d: Dag, k: Kind): Option[Event2] =
    d.produces.find((p: (Kind, Event2)) => p._1 == k).map(_._2)

  // The named event a kind seeds BLOCKED on, derived from the DAG value.
  def waitEventOf(d: Dag, k: Kind): Option[Event2] =
    d.waitsOn.find((w: (Kind, Event2)) => w._1 == k).map(_._2)
}
