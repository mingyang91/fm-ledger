package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._

/* =============================================================================
 * THE PROOFS — propositions about the business core in JobModel.scala.
 *
 * The BUSINESS CORE (the JobModel object: enums, JobState, step, the Dag and
 * its predicates) lives in JobModel.scala and is the single verifiable source
 * of truth that the production core is generated from. This file holds ONLY the
 * `.holds` propositions that prove the model satisfies its specification. It
 * imports JobModel._ so every proof references the verified definitions directly.
 *
 * EvolutionConflict.scala (the deliberately-INVALID counterexample) also imports
 * JobModel._; when run explicitly it must be passed ALONGSIDE JobModel.scala.
 * ========================================================================== */
object JobProofs {

  /* =====================================================================
   * PROPOSITION 1 — NO BLACK HOLE
   *
   * Every non-terminal state has a path forward to a terminal state or back
   * to PENDING. We prove the single-step escape edges exist:
   *   - PENDING  --Claim--> RUNNING            (progress)
   *   - RUNNING  --OutDone--> DONE             (terminal, owner)
   *   - PARTIAL  --Claim--> RUNNING            (re-claim after runAt)
   *   - BLOCKED  --UpstreamEvent--> PENDING    (named-event reclaim)
   *   - BLOCKED  --SweeperUpstreamFailed--> NA (sweeper always reclaims)
   * Plus: the retry budget is bounded — Transient terminates FAILED at max.
   * ===================================================================== */

  // PENDING always advances to RUNNING under a claim.
  def pendingProgresses(s: JobState, w: FMLong): Boolean = {
    require(s.status == PENDING)
    step(s, Claim(w)).status == RUNNING
  }.holds

  // RUNNING (owned) always reaches DONE under OutDone.
  def runningCanFinish(s: JobState, w: FMLong): Boolean = {
    require(s.status == RUNNING && isOwner(s, w))
    step(s, OutDone(w)).status == DONE
  }.holds

  // PARTIAL re-claims to RUNNING (never a dead end).
  def partialProgresses(s: JobState, w: FMLong): Boolean = {
    require(s.status == PARTIAL)
    step(s, Claim(w)).status == RUNNING
  }.holds

  // BLOCKED is ALWAYS reclaimable by the sweeper, regardless of what it waits
  // on — this is the "no orphaned BLOCKED at the framework level" guarantee.
  def blockedAlwaysReclaimableBySweeper(s: JobState): Boolean = {
    require(s.status == BLOCKED)
    val nxt = step(s, SweeperUpstreamFailed)
    isTerminal(nxt.status) // -> NOT_APPLICABLE
  }.holds

  // BLOCKED parked on a named event is promoted to PENDING when that exact
  // event fires (the happy-path reclaim).
  def blockedReclaimedByNamedEvent(s: JobState, ev: Event2): Boolean = {
    require(s.status == BLOCKED && s.blockedOn == Some[Event2](ev))
    step(s, UpstreamEvent(ev)).status == PENDING
  }.holds

  // RETRY BUDGET BOUNDED: a Transient outcome at the last attempt terminates
  // FAILED — the loop cannot spin forever.
  def transientTerminatesAtMax(s: JobState, w: FMLong): Boolean = {
    require(s.status == RUNNING && isOwner(s, w))
    require(s.attempts + FMInt(BigInt(1)) >= maxAttempts)
    step(s, OutTransient(w)).status == FAILED
  }.holds

  // RETRY BUDGET strictly counts up below max (no silent stall at PENDING
  // without consuming the budget).
  def transientConsumesBudget(s: JobState, w: FMLong): Boolean = {
    require(s.status == RUNNING && isOwner(s, w))
    require(s.attempts + FMInt(BigInt(1)) < maxAttempts)
    val nxt = step(s, OutTransient(w))
    nxt.status == PENDING && nxt.attempts == s.attempts + FMInt(BigInt(1))
  }.holds

  // NOT_APPLICABLE is the only terminal-by-classification state, and it IS
  // producer-resettable (not a permanent black hole at the producer level).
  def notApplicableIsResettable(s: JobState): Boolean = {
    require(s.status == NOT_APPLICABLE)
    step(s, ProducerReset).status == PENDING
  }.holds

  /* =====================================================================
   * PROPOSITION 2 — CLAIM / STALE SAFETY (sequential ownership guard)
   * ===================================================================== */

  // A terminal write happens ONLY by the current owner: a RUNNING row owned by
  // worker A is NOT moved to DONE by a different worker B's outcome.
  def terminalWriteOnlyByOwner(s: JobState, a: FMLong, b: FMLong): Boolean = {
    require(s.status == RUNNING && isOwner(s, a) && a != b)
    // worker B (not the owner) tries to mark DONE -> no-op, stays RUNNING.
    step(s, OutDone(b)).status == RUNNING
  }.holds

  // A ClaimLost worker cannot write terminal state of ANY kind. A ClaimLost
  // worker is, by definition, not the current owner (owner is someone else or
  // None). For every terminal-producing outcome event, a non-owner is a no-op.
  def claimLostCannotWriteTerminal(s: JobState, lost: FMLong): Boolean = {
    require(s.status == RUNNING && !isOwner(s, lost))
    val a = step(s, OutDone(lost)).status == RUNNING
    val b = step(s, OutUnrecoverable(lost)).status == RUNNING
    val c = step(s, OutInputRejected(lost)).status == RUNNING
    val d = step(s, OutNotApplicable(lost)).status == RUNNING
    val e = step(s, OutTransient(lost)).status == RUNNING
    val f = step(s, OutBlocked(lost, EmbeddingReady)).status == RUNNING
    val g = step(s, OutPartial(lost)).status == RUNNING
    a && b && c && d && e && f && g
  }.holds

  // A stale (heartbeat-expired) RUNNING row is safely reclaimable: production
  // marks an expired RUNNING row reclaimable, modeled here as a fresh claim by
  // a new worker overwriting ownership. The new owner replaces the old.
  def staleRunningReclaimable(s: JobState, stale: FMLong, fresh: FMLong): Boolean = {
    require(s.status == RUNNING && isOwner(s, stale) && stale != fresh)
    // The reclaim path first re-PENDINGs the stale row (sweeper / lease
    // expiry), then a fresh worker claims it.
    val repended = s.copy(status = PENDING, owner = None[FMLong]())
    val reclaimed = step(repended, Claim(fresh))
    reclaimed.status == RUNNING && isOwner(reclaimed, fresh) && !isOwner(reclaimed, stale)
  }.holds

  /* =====================================================================
   * PROPOSITION 3 — DAG HANDOFF COVERAGE + EVOLUTION GAP (THE HEADLINE)
   * ===================================================================== */

  // The full DAG is covered: every kind seeded BLOCKED on event E has a
  // producer of E.
  def fullDagIsCovered: Boolean = {
    coverage(fullDag)
  }.holds

  // Mechanized form: state it as a concrete equality so the solver evaluates
  // the data value directly.
  def fullDagCoverageConcrete: Boolean = {
    coverage(fullDag) == true
  }.holds

  // The coverage property goes INVALID for the embedding-removed DAG: there is
  // NO producer of EmbeddingReady, yet Masking still waits on it. We PROVE the
  // gap by proving coverage is FALSE for this DAG (a true theorem about the
  // broken value). The witness: Masking's wait edge has no producer.
  def evolutionGapDetected: Boolean = {
    // coverage must be FALSE -> the gap is real and detectable.
    !coverage(embeddingRemovedDag) &&
    // the specific orphaned consumer:
    !hasProducer(embeddingRemovedDag, EmbeddingReady) &&
    // sanity: Masking is still in the wait list, so it is the orphan.
    embeddingRemovedDag.waitsOn.exists((w: (Kind, Event2)) =>
      w._1 == Masking && w._2 == EmbeddingReady)
  }.holds

  // CONFLICT CHECK: asserting that the broken DAG IS covered is INVALID. This
  // is the failing VC we deliberately surface to show the property has teeth.
  // (Lives in a separate file `EvolutionConflict.scala` so this file verifies
  // clean; see that file for the INVALID counterexample.)

  // The re-wire resolution restores coverage.
  def rewireRestoresCoverage: Boolean = {
    coverage(rewiredDag)
  }.holds

  /* =====================================================================
   * PROPOSITION 4 — CLASSIFICATION SOUNDNESS
   *
   * A row that CAN satisfy its prerequisite via a named event (i.e. a genuine
   * Blocked condition) must never be classified as NOT_APPLICABLE (the
   * 540-video-latch incident) nor cycled as Transient (the no-gcsUri
   * retry-storm incident). We encode "can satisfy via a named event" as: the
   * runner's correct outcome for such a row is OutBlocked, and we prove that
   * outcome lands in BLOCKED (non-terminal, reclaimable), NOT in
   * NOT_APPLICABLE and NOT in a Transient retry cycle.
   * ===================================================================== */

  // A genuinely-blocked row routed through OutBlocked is BLOCKED, which is
  // non-terminal and (by Prop 1) always reclaimable. It is specifically NOT
  // NOT_APPLICABLE.
  def blockedNotMisclassifiedAsNotApplicable(s: JobState, w: FMLong, ev: Event2): Boolean = {
    require(s.status == RUNNING && isOwner(s, w))
    val nxt = step(s, OutBlocked(w, ev))
    nxt.status == BLOCKED &&
    nxt.status != NOT_APPLICABLE &&
    isNonTerminal(nxt.status)
  }.holds

  // The same row is NOT consumed by a Transient retry cycle: a BLOCKED row's
  // attempts counter does not advance, and it is parked OFF the claim path
  // (BLOCKED, not PENDING), so it cannot enter the retry storm.
  def blockedNotMisclassifiedAsTransient(s: JobState, w: FMLong, ev: Event2): Boolean = {
    require(s.status == RUNNING && isOwner(s, w))
    val nxt = step(s, OutBlocked(w, ev))
    nxt.status != PENDING &&            // not re-queued for an immediate retry
    nxt.attempts == s.attempts &&       // retry budget untouched
    nxt.blockedOn == Some[Event2](ev)   // parked on its named prerequisite
  }.holds

  // Contrast/witness: distinct outcomes for the SAME running row land in the
  // THREE distinct not-progressing states, proving they are not conflated.
  def threeNotProgressingStatesAreDistinct(s: JobState, w: FMLong): Boolean = {
    require(s.status == RUNNING && isOwner(s, w))
    require(s.attempts + FMInt(BigInt(1)) >= maxAttempts) // force Transient -> FAILED
    val na      = step(s, OutNotApplicable(w)).status        // terminal classification
    val transF  = step(s, OutTransient(w)).status            // transient exhausted -> FAILED
    val blocked = step(s, OutBlocked(w, GcsUri)).status      // blocked -> parked
    na == NOT_APPLICABLE &&
    transF == FAILED &&
    blocked == BLOCKED &&
    na != transF && na != blocked && transF != blocked
  }.holds
}
