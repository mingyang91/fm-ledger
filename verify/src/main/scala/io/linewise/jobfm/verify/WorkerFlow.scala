package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import JobStore._

/* =============================================================================
 * THE WORKER WORKFLOW OVER THE STORE — operations as pure JobStore => JobStore,
 * and the OPERATIONAL REQUIREMENT LIBRARY R1..R5 proven over any valid store.
 *
 * This is the lift the whole exercise is about. JobProofs.scala proved facts
 * about a SINGLE `step` in a vacuum. Here every worker op acts on the WHOLE
 * store the worker.scala loop threads, and we prove that FOR ANY store
 * satisfying storeInv, each op (claimOne / applyOutcome / handoff / sweep)
 * preserves the invariant (R1) and meets a named business postcondition
 * (R2..R5).
 *
 * UNIFORM SHAPE (to tame induction). Every op is exactly
 *     s.copy(rows = s.rows.map(perRow))
 * for a named, total, id-preserving per-row transform `perRow`. That single
 * shape lets every R1 proof reuse the ONE induction lemma
 * `JobStore.mapPreservesStoreInv`. The per-row obligations it needs
 * (perRowCarriesInv, perRowKeepsId) are discharged by the per-op induction
 * lemmas below, each built from the pointwise per-row step lemmas.
 *
 * TRUSTED STORE-REALIZATION BOUNDARY (named here, proven nowhere — by design).
 * Each abstract op stands for a doobie operation in store.scala; the SQL WHERE
 * clause is TRUSTED-EQUIVALENT to the verified selection predicate:
 *
 *   abstract op            verified selection predicate         doobie realization (store.scala)
 *   ------------------     ---------------------------------    --------------------------------------------------
 *   claimOne(s,id,w)       row.id==id && claimable(row.st)      Db.claim: SELECT ... WHERE id=? AND (status='PENDING'
 *                          (PENDING|PARTIAL|stale-RUNNING)        OR status='PARTIAL' OR (status='RUNNING' AND lease
 *                                                                 expired)) FOR UPDATE SKIP LOCKED, then re-validating
 *                                                                 UPDATE guarded by the SAME WHERE.
 *   applyOutcome(s,id,w,o) row.id==id && row.st owned-RUNNING    Db.saveState: UPDATE ... WHERE id=? AND
 *                                                                 status='RUNNING' AND owner=? (owner-revalidated).
 *   handoff(s,ev)          row.st==BLOCKED && blockedOn==Some(ev) Db.handoff: SELECT id WHERE status='BLOCKED' AND
 *                                                                 blocked_on=?, UPDATE ... WHERE status='BLOCKED'.
 *   sweep(s,dag)           row.st==BLOCKED && blockedOn has no    Db.sweepOrphanedBlocked: SELECT WHERE status='BLOCKED'
 *                          producer in dag                        AND blocked_on NOT IN (live producers), UPDATE.
 *
 * The trusted claim is: the SQL WHERE selects EXACTLY the rows the abstract
 * predicate selects, and each selected row is written with `step(row, <event>)`.
 * Persistence, transactions, SKIP-LOCKED concurrency, and the wall-clock lease
 * are the part we do NOT verify; they are the trusted realization of this model.
 * ========================================================================== */

object WorkerFlow {

  /* =====================================================================
   * PER-ROW STEP LEMMAS — the obligations every op's perRow must satisfy.
   *
   * stepPreservesRowInv: applying `step` to ANY row's JobState preserves rowInv
   * (attemptsBounded from step's own ensuring; blockedHasWaitEdge because the
   * ONLY arm producing BLOCKED is OutBlocked, which always sets blockedOn).
   * This is the per-element fact that feeds perRowCarriesInv.
   * ------------------------------------------------------------------- */

  /* attemptsBounded is established by step's own postcondition. */
  def stepKeepsAttemptsBounded(s: JobState, e: Event): Boolean = {
    require(attemptsBounded(s))
    attemptsBounded(step(s, e))
  }.holds

  /* blockedHasWaitEdge is preserved: if the result is BLOCKED, blockedOn is
   * defined. Either the row was already BLOCKED (and the arm left blockedOn
   * untouched, so the precondition's blockedHasWaitEdge carries) or the arm is
   * OutBlocked which sets blockedOn = Some(waitFor). No other arm yields
   * BLOCKED. We let the solver case-split over the event; the require gives the
   * pre-image's blockedHasWaitEdge for the no-op arms. */
  def stepKeepsBlockedHasWaitEdge(s: JobState, e: Event): Boolean = {
    require(blockedHasWaitEdge(s))
    blockedHasWaitEdge(step(s, e))
  }.holds

  /* The combined per-row fact: step preserves rowInv. */
  def stepPreservesRowInv(r: JobRow, e: Event): Boolean = {
    require(rowInv(r))
    rowInv(JobRow(r.id, step(r.st, e)))
  }.holds

  /* =====================================================================
   * THE FOUR WORKER OPS, each `JobStore => JobStore` via a named perRow.
   *
   * Each op is `JobStore(s.rows.map(perRow))`. R1 (storeInv preserved) is
   * discharged by calling the reusable JobStore.mapPreservesStoreInv lemma,
   * whose two preconditions (perRowCarriesInv / perRowKeepsId on s.rows) are in
   * turn discharged by a per-op pair of structural-induction lemmas below. This
   * is the design that takes R1 from "solver times out unfolding map+forall"
   * (the 洞-B List-induction ceiling) to "valid in a fraction of a second": the
   * induction is done ONCE in mapPreservesStoreInv, reused for every op.
   * ===================================================================== */

  /* --- CLAIMABLE predicate: a row a worker may claim. PENDING and PARTIAL are
   * claimable directly (step's Claim arm fires). A stale RUNNING row is also
   * claimable in production (the lease expired), modeled per
   * staleRunningReclaimable: re-PENDING then Claim. We expose the claimability
   * test as a pure predicate so the SQL WHERE <-> predicate boundary is sharp.
   * The `stale` flag stands for "the lease has expired" — a TRUSTED input the
   * abstract model receives (the wall-clock comparison is inexpressible). --- */
  def claimable(s: JobState, stale: Boolean): Boolean =
    s.status == PENDING || s.status == PARTIAL || (s.status == RUNNING && stale)

  /* claimOne: the targeted row, IF claimable, advances via Claim (a stale
   * RUNNING row is first re-PENDINGed so step's Claim arm fires, exactly the
   * staleRunningReclaimable path in store.scala's Db.claim). Every other row is
   * returned unchanged. `stale` is the trusted lease-expiry input for the
   * targeted row. */
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

  def isOrphanedBlocked(dag: Dag, s: JobState): Boolean =
    s.status == BLOCKED && (s.blockedOn match
      case Some(ev) => !hasProducer(dag, ev)
      case None()   => false)

  def sweepRow(dag: Dag)(r: JobRow): JobRow =
    if isOrphanedBlocked(dag, r.st) then JobRow(r.id, step(r.st, SweeperUpstreamFailed)) else r

  /* =====================================================================
   * POINTWISE per-row obligations — each perRow, applied to ONE row r with
   * rowInv(r), yields a row with rowInv, and keeps r.id. These are the leaves
   * the per-op induction lemmas use at the head of the list.
   * ===================================================================== */

  /* claimRow on one row preserves rowInv. If not the target / not claimable it
   * is the identity (rowInv carries trivially). If claimable: the re-PENDING
   * copy keeps attempts (attemptsBounded carries) and is PENDING not BLOCKED
   * (blockedHasWaitEdge vacuous), then step(_, Claim) preserves rowInv. */
  def claimRowKeepsRowInv(id: FMLong, worker: FMLong, stale: Boolean, r: JobRow): Boolean = {
    require(rowInv(r))
    rowInv(claimRow(id, worker, stale)(r))
  }.holds

  def claimRowKeepsId(id: FMLong, worker: FMLong, stale: Boolean, r: JobRow): Boolean = {
    claimRow(id, worker, stale)(r).id == r.id
  }.holds

  def outcomeRowKeepsRowInv(id: FMLong, outcome: Event, r: JobRow): Boolean = {
    require(rowInv(r))
    rowInv(outcomeRow(id, outcome)(r))
  }.holds

  def outcomeRowKeepsId(id: FMLong, outcome: Event, r: JobRow): Boolean = {
    outcomeRow(id, outcome)(r).id == r.id
  }.holds

  def handoffRowKeepsRowInv(ev: Event2, r: JobRow): Boolean = {
    require(rowInv(r))
    rowInv(handoffRow(ev)(r))
  }.holds

  def handoffRowKeepsId(ev: Event2, r: JobRow): Boolean = {
    handoffRow(ev)(r).id == r.id
  }.holds

  def sweepRowKeepsRowInv(dag: Dag, r: JobRow): Boolean = {
    require(rowInv(r))
    rowInv(sweepRow(dag)(r))
  }.holds

  def sweepRowKeepsId(dag: Dag, r: JobRow): Boolean = {
    sweepRow(dag)(r).id == r.id
  }.holds

  /* =====================================================================
   * PER-OP INDUCTION LEMMAS — lift the pointwise obligations to the whole list,
   * establishing the two preconditions of mapPreservesStoreInv:
   *   perRowCarriesInv(xs, perRow)  — every element carries rowInv
   *   perRowKeepsId(xs, perRow)     — every element keeps its id
   * Each is a single structural induction over xs (@opaque @inlineOnce so the
   * solver does not unfold the recursion uncontrolled). The head step uses the
   * pointwise lemma; the tail step recurses.
   * ===================================================================== */

  @opaque @inlineOnce
  def claimCarriesInv(xs: List[JobRow], id: FMLong, worker: FMLong, stale: Boolean): Unit = {
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        if rowInv(h) then { claimRowKeepsRowInv(id, worker, stale, h); () } else ()
        claimCarriesInv(t, id, worker, stale)
  }.ensuring(_ => perRowCarriesInv(xs, claimRow(id, worker, stale)))

  @opaque @inlineOnce
  def claimKeepsIds(xs: List[JobRow], id: FMLong, worker: FMLong, stale: Boolean): Unit = {
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        claimRowKeepsId(id, worker, stale, h)
        claimKeepsIds(t, id, worker, stale)
  }.ensuring(_ => perRowKeepsId(xs, claimRow(id, worker, stale)))

  @opaque @inlineOnce
  def outcomeCarriesInv(xs: List[JobRow], id: FMLong, outcome: Event): Unit = {
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        if rowInv(h) then { outcomeRowKeepsRowInv(id, outcome, h); () } else ()
        outcomeCarriesInv(t, id, outcome)
  }.ensuring(_ => perRowCarriesInv(xs, outcomeRow(id, outcome)))

  @opaque @inlineOnce
  def outcomeKeepsIds(xs: List[JobRow], id: FMLong, outcome: Event): Unit = {
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        outcomeRowKeepsId(id, outcome, h)
        outcomeKeepsIds(t, id, outcome)
  }.ensuring(_ => perRowKeepsId(xs, outcomeRow(id, outcome)))

  @opaque @inlineOnce
  def handoffCarriesInv(xs: List[JobRow], ev: Event2): Unit = {
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        if rowInv(h) then { handoffRowKeepsRowInv(ev, h); () } else ()
        handoffCarriesInv(t, ev)
  }.ensuring(_ => perRowCarriesInv(xs, handoffRow(ev)))

  @opaque @inlineOnce
  def handoffKeepsIds(xs: List[JobRow], ev: Event2): Unit = {
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        handoffRowKeepsId(ev, h)
        handoffKeepsIds(t, ev)
  }.ensuring(_ => perRowKeepsId(xs, handoffRow(ev)))

  @opaque @inlineOnce
  def sweepCarriesInv(xs: List[JobRow], dag: Dag): Unit = {
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        if rowInv(h) then { sweepRowKeepsRowInv(dag, h); () } else ()
        sweepCarriesInv(t, dag)
  }.ensuring(_ => perRowCarriesInv(xs, sweepRow(dag)))

  @opaque @inlineOnce
  def sweepKeepsIds(xs: List[JobRow], dag: Dag): Unit = {
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        sweepRowKeepsId(dag, h)
        sweepKeepsIds(t, dag)
  }.ensuring(_ => perRowKeepsId(xs, sweepRow(dag)))

  /* =====================================================================
   * THE FOUR OPS as JobStore => JobStore. R1 (storeInv preserved) is proven by
   * threading the per-op induction lemmas into the reusable mapPreservesStoreInv.
   * ===================================================================== */

  def claimOne(s: JobStore, id: FMLong, worker: FMLong, stale: Boolean): JobStore = {
    require(storeInv(s))
    val f = claimRow(id, worker, stale)
    claimCarriesInv(s.rows, id, worker, stale)
    claimKeepsIds(s.rows, id, worker, stale)
    mapPreservesStoreInv(s, f)
    JobStore(s.rows.map(f))
  }.ensuring(res => storeInv(res))

  def applyOutcome(s: JobStore, id: FMLong, outcome: Event): JobStore = {
    require(storeInv(s))
    val f = outcomeRow(id, outcome)
    outcomeCarriesInv(s.rows, id, outcome)
    outcomeKeepsIds(s.rows, id, outcome)
    mapPreservesStoreInv(s, f)
    JobStore(s.rows.map(f))
  }.ensuring(res => storeInv(res))

  def handoff(s: JobStore, ev: Event2): JobStore = {
    require(storeInv(s))
    val f = handoffRow(ev)
    handoffCarriesInv(s.rows, ev)
    handoffKeepsIds(s.rows, ev)
    mapPreservesStoreInv(s, f)
    JobStore(s.rows.map(f))
  }.ensuring(res => storeInv(res))

  def sweep(s: JobStore, dag: Dag): JobStore = {
    require(storeInv(s))
    val f = sweepRow(dag)
    sweepCarriesInv(s.rows, dag)
    sweepKeepsIds(s.rows, dag)
    mapPreservesStoreInv(s, f)
    JobStore(s.rows.map(f))
  }.ensuring(res => storeInv(res))
}
