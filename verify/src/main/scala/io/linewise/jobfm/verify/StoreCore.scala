package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._

/* =============================================================================
 * STORE CORE — the PROOF-FREE EXECUTABLE subset of the store + worker workflow.
 *
 * THIS FILE IS A TRANSPILER INPUT. Everything here is plain executable code:
 * data types, per-row business predicates, per-row transforms, and the four
 * worker ops as pure `JobStore => JobStore`. It contains NO proof scaffolding —
 * no `@opaque`/`@inlineOnce` lemmas, no `.holds`, no `@law`, no `require`, no
 * `ensuring`, and no `Nil()`/`Cons()` constructors. All of that lives in the
 * verify-only proof files that import this one (StoreProofs.scala, StoreLaw.scala,
 * WorkerProofs.scala) and are NEVER transpiled.
 *
 * The transpiler erases the stainless imports + FMInt/FMLong (bounds proven, so
 * native Int/Long is sound) and emits io.linewise.jobfm.generated.StoreCore,
 * which the production trait `Store`'s in-memory `ListStore` and the differential
 * test both use. The doobie `Db` (store.scala) is the TRUSTED realization of the
 * SAME ops; the differential test machine-checks that the two agree row-for-row.
 *
 * Note: `isOrphanedBlocked` is written with a wildcard `case _` (not a bare
 * `case None()`) so it transpiles cleanly — the transpiler's R4 only rewrites the
 * bracketed `None[T]()` constructor form, not a bare `None()` pattern.
 * ========================================================================== */
object StoreCore {

  /* --- A ROW: the id + its JobState (row-as-source-of-truth). --- */
  case class JobRow(id: FMLong, st: JobState)

  /* --- THE STORE: a list of rows. --- */
  case class JobStore(rows: List[JobRow])

  /* The store-contents OBSERVATION the abstract-store @law (StoreLaw.scala) and
   * the differential test both read. Trivial accessor; transpiles to `.rows`. */
  def view(s: JobStore): List[JobRow] = s.rows

  /* =====================================================================
   * PER-ROW BUSINESS PREDICATES (executable; storeInv itself lives in the
   * proof file because it folds in distinctIds, which uses Nil()/Cons()).
   * ===================================================================== */

  def attemptsBounded(s: JobState): Boolean =
    s.attempts >= FMInt(BigInt(0)) && s.attempts <= maxAttempts

  def blockedHasWaitEdge(s: JobState): Boolean =
    s.status != BLOCKED || s.blockedOn.isDefined

  def rowInv(r: JobRow): Boolean =
    attemptsBounded(r.st) && blockedHasWaitEdge(r.st)

  /* A row a worker may claim: PENDING/PARTIAL directly, or a stale RUNNING row
   * (lease expired — `stale` is the trusted lease-expiry input). */
  def claimable(s: JobState, stale: Boolean): Boolean =
    s.status == PENDING || s.status == PARTIAL || (s.status == RUNNING && stale)

  /* A BLOCKED row whose blockedOn event has NO producer in the dag. Written
   * pattern-free of a bare `None()` (wildcard `case _`) for clean transpile. */
  def isOrphanedBlocked(dag: Dag, s: JobState): Boolean =
    s.status == BLOCKED && (s.blockedOn match
      case Some(ev) => !hasProducer(dag, ev)
      case _        => false)

  /* =====================================================================
   * PER-ROW TRANSFORMS — what each op does to a single row.
   * ===================================================================== */

  def claimRow(id: FMLong, worker: FMLong, stale: Boolean)(r: JobRow): JobRow =
    if r.id == id then
      if claimable(r.st, stale) then
        val base = if r.st.status == RUNNING then r.st.copy(status = PENDING, owner = None[FMLong]()) else r.st
        JobRow(r.id, step(base, Claim(worker)))
      else r
    else r

  def outcomeRow(id: FMLong, outcome: Event)(r: JobRow): JobRow =
    if r.id == id then JobRow(r.id, step(r.st, outcome)) else r

  def handoffRow(ev: Event2)(r: JobRow): JobRow =
    JobRow(r.id, step(r.st, UpstreamEvent(ev)))

  def sweepRow(dag: Dag)(r: JobRow): JobRow =
    if isOrphanedBlocked(dag, r.st) then JobRow(r.id, step(r.st, SweeperUpstreamFailed)) else r

  /* =====================================================================
   * THE FOUR WORKER OPS as pure JobStore => JobStore — the PLAIN, proof-free
   * bodies (`s.rows.map(perRow)`). Invariant-preservation (R1) is proven over
   * these in StoreProofs.scala via the reusable mapPreservesStoreInv lemma.
   * ===================================================================== */

  def claimOne(s: JobStore, id: FMLong, worker: FMLong, stale: Boolean): JobStore =
    JobStore(s.rows.map(claimRow(id, worker, stale)))

  def applyOutcome(s: JobStore, id: FMLong, outcome: Event): JobStore =
    JobStore(s.rows.map(outcomeRow(id, outcome)))

  def handoff(s: JobStore, ev: Event2): JobStore =
    JobStore(s.rows.map(handoffRow(ev)))

  def sweep(s: JobStore, dag: Dag): JobStore =
    JobStore(s.rows.map(sweepRow(dag)))
}
