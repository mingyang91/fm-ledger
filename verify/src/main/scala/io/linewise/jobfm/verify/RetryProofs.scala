package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import StoreCore.claimable

/* =============================================================================
 * RETRY-TO-COMPLETION — VERIFY-ONLY. The deterministic skeleton under the "99%
 * completion" goal, with the probability stripped out (1 - p^maxAttempts is then
 * grade-school arithmetic on top of these theorems).
 *
 * We model a job's life as folding a sequence of per-attempt outcomes (Boolean:
 * true = the process succeeded, false = a RETRYABLE transient failure) through
 * the real `step` machine. Each round is claim -> run -> outcome:
 *   success    => Claim then OutDone      => DONE (terminal)
 *   transient  => Claim then OutTransient => PENDING (attempts+1) below cap,
 *                                            or FAILED at the cap.
 * `runJob` folds rounds while the job is PENDING; a terminal state absorbs.
 *
 * The headline (R6a): if the outcome sequence contains a success WITHIN the
 * attempt budget (fewer than maxAttempts leading transient failures), the job's
 * terminal state is DONE. The re-pickup (R6b): a transient failure below the cap
 * returns the job to a CLAIMABLE PENDING state, and it is re-claimed by ANY
 * worker (possibly a different one). The cap (R6c): maxAttempts consecutive
 * transient failures from a fresh job reach FAILED.
 *
 * SCOPE / honest caveat: "false" here is the RETRYABLE failure (OutTransient).
 * Non-retryable errors (OutUnrecoverable / OutInputRejected) go straight to
 * FAILED and a later success cannot revive them — they are NOT modeled here, so
 * the "contains a success => DONE" theorem is scoped to workloads whose failures
 * are transient. CONCURRENCY: a re-PENDINGed retry row is an ordinary PENDING
 * row, so SeqMirrorProofs.raceExactlyOneWinner already covers "two workers racing
 * it re-claim it exactly once" (no lost / double-counted retry under any number
 * of workers, any order). Here we prove the per-job retry semantics; that race
 * proof is the no-double-count half.
 * ========================================================================== */
object RetryProofs {

  /* --- the outcome sequence: true = success, false = retryable transient. --- */

  def hasTrue(os: List[Boolean]): Boolean =
    os match
      case Nil()      => false
      case Cons(h, t) => h || hasTrue(t)

  // leading transient failures before the first success (or the whole length).
  def leadingFalses(os: List[Boolean]): BigInt = {
    os match
      case Nil()      => BigInt(0)
      case Cons(h, t) => if h then BigInt(0) else BigInt(1) + leadingFalses(t)
  }.ensuring(_ >= BigInt(0))

  /* --- ONE retry round on a PENDING job: claim, then succeed/fail. --- */
  def processOnce(s: JobState, succeeded: Boolean, w: FMLong): JobState = {
    require(s.status == PENDING)
    val running = step(s, Claim(w)) // PENDING -> RUNNING owned w
    if succeeded then step(running, OutDone(w)) // -> DONE
    else step(running, OutTransient(w)) // -> PENDING(attempts+1) or FAILED at cap
  }

  /* --- fold rounds while PENDING; a terminal state absorbs (stops the fold). --- */
  def runJob(s: JobState, outcomes: List[Boolean], w: FMLong): JobState = {
    require(s.status == PENDING)
    decreases(outcomes.size)
    outcomes match
      case Nil() => s
      case Cons(h, t) =>
        val r = processOnce(s, h, w)
        if r.status == PENDING then runJob(r, t, w) else r
  }

  /* =====================================================================
   * PER-ROUND BUILDING BLOCKS.
   * ===================================================================== */

  // a successful round from PENDING -> DONE.
  def roundSuccessDone(s: JobState, w: FMLong): Boolean = {
    require(s.status == PENDING)
    processOnce(s, true, w).status == DONE
  }.holds

  // a transient round BELOW the cap -> PENDING, attempts+1, and CLAIMABLE again.
  def roundTransientRePends(s: JobState, w: FMLong): Boolean = {
    require(s.status == PENDING && s.attempts.value + 1 < maxAttempts.value)
    val r = processOnce(s, false, w)
    r.status == PENDING && claimable(r, false) && r.attempts.value == s.attempts.value + 1
  }.holds

  // RE-PICKUP by ANY worker: the re-PENDINGed row is re-claimed by w2 (possibly
  // != w1) -> RUNNING owned by w2. "retryable Left gets picked up by a worker."
  def transientReclaimedByAnyWorker(s: JobState, w1: FMLong, w2: FMLong): Boolean = {
    require(s.status == PENDING && s.attempts.value + 1 < maxAttempts.value)
    val r = processOnce(s, false, w1)
    val reclaimed = step(r, Claim(w2))
    reclaimed.status == RUNNING && isOwner(reclaimed, w2)
  }.holds

  // a transient round AT the cap -> FAILED.
  def roundTransientAtCapFails(s: JobState, w: FMLong): Boolean = {
    require(s.status == PENDING && s.attempts.value + 1 >= maxAttempts.value)
    processOnce(s, false, w).status == FAILED
  }.holds

  /* =====================================================================
   * R6a — HEADLINE: a success within the attempt budget => terminal DONE.
   *
   * Induction on the outcome list: the first success absorbs to DONE; each
   * leading transient failure (within budget) re-PENDs and the budget shrinks.
   * The budget condition `leadingFalses + attempts < maxAttempts` is exactly
   * "the success arrives before maxAttempts transient failures cap the job".
   * ===================================================================== */
  @opaque @inlineOnce
  def doneIfTrueWithinBudget(s: JobState, outcomes: List[Boolean], w: FMLong): Unit = {
    require(s.status == PENDING)
    require(hasTrue(outcomes))
    require(leadingFalses(outcomes) + s.attempts.value < maxAttempts.value)
    decreases(outcomes.size)
    outcomes match
      case Nil() => () // hasTrue(Nil) == false: precondition unsatisfiable, vacuous
      case Cons(h, t) =>
        if h then { roundSuccessDone(s, w); () }
        else {
          roundTransientRePends(s, w) // r = processOnce(s,false,w) is PENDING, attempts+1
          doneIfTrueWithinBudget(processOnce(s, false, w), t, w)
        }
  }.ensuring(_ => runJob(s, outcomes, w).status == DONE)

  // Fresh-job corollary, matching the goal's phrasing directly: a fresh job whose
  // outcome sequence has a success within the first maxAttempts tries -> DONE.
  def freshDoneIfSuccessWithinCap(outcomes: List[Boolean], w: FMLong): Boolean = {
    require(hasTrue(outcomes) && leadingFalses(outcomes) < maxAttempts.value)
    val fresh = JobState(PENDING, None[FMLong](), FMInt(BigInt(0)), Transcode, None[Event2]())
    doneIfTrueWithinBudget(fresh, outcomes, w)
    runJob(fresh, outcomes, w).status == DONE
  }.holds

  /* =====================================================================
   * R6c — CAP: maxAttempts (= 5) consecutive transient failures from a fresh
   * job reach FAILED. Concrete instance demonstrating the cap fires (the
   * complement of R6a: FAILED is reachable only by exhausting the budget).
   * ===================================================================== */
  def freshAllTransientFails(w: FMLong): Boolean = {
    val fresh = JobState(PENDING, None[FMLong](), FMInt(BigInt(0)), Transcode, None[Event2]())
    val five  = Cons(false, Cons(false, Cons(false, Cons(false, Cons(false, Nil[Boolean]())))))
    runJob(fresh, five, w).status == FAILED
  }.holds
}
