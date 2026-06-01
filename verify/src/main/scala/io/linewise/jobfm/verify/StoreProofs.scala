package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import StoreCore._

/* =============================================================================
 * STORE PROOFS — the VERIFY-ONLY proof scaffolding for the executable StoreCore.
 *
 * NEVER TRANSPILED. This file holds everything that is ghost relative to
 * production: the store invariant (storeInv, which folds in distinctIds and so
 * uses Nil()/Cons()), the reusable map-induction lemmas, the pointwise/per-op
 * lemmas, and the four R1 invariant-preservation wrappers that prove StoreCore's
 * plain ops preserve storeInv. `verify.scala` auto-discovers this file, so the
 * headline run still verifies all of it; the transpiler never sees it.
 *
 * TRUSTED STORE-REALIZATION BOUNDARY (named here, proven nowhere — by design).
 * Each StoreCore op stands for a doobie operation in store.scala; the SQL WHERE
 * clause is TRUSTED-EQUIVALENT to the verified selection predicate it realizes:
 *   claimOne(s,id,w,stale)  <-> Db.claim       (id + PENDING|PARTIAL|stale-RUNNING, FOR UPDATE SKIP LOCKED + re-validating UPDATE)
 *   applyOutcome(s,id,o)    <-> Db.saveState   (id + RUNNING + owner=?, owner-revalidated)
 *   handoff(s,ev)           <-> Db.handoff     (status=BLOCKED AND blocked_on=?)
 *   sweep(s,dag)            <-> Db.sweepOrphanedBlocked (BLOCKED AND blocked_on has no live producer)
 * Persistence, transactions, SKIP-LOCKED concurrency, and the wall-clock lease
 * are NOT verified; they are the trusted realization. The differential test
 * (DifferentialSpec) machine-checks that the doobie realization agrees with the
 * verified StoreCore ops row-for-row on concrete scenarios.
 * ========================================================================== */
object StoreProofs {

  /* =====================================================================
   * THE STORE INVARIANT — per-row rowInv (from StoreCore) + global distinctness.
   * ===================================================================== */

  def ids(rows: List[JobRow]): List[FMLong] =
    rows.map((r: JobRow) => r.id)

  def distinctIds(rows: List[JobRow]): Boolean =
    rows match
      case Nil()      => true
      case Cons(h, t) => !ids(t).contains(h.id) && distinctIds(t)

  def storeInv(s: JobStore): Boolean =
    s.rows.forall((r: JobRow) => rowInv(r)) && distinctIds(s.rows)

  /* =====================================================================
   * THE REUSABLE MAP-PRESERVES-INVARIANT LEMMA (洞-B: the List-induction core).
   * Done ONCE, reused by every op. Preserved verbatim from the original
   * JobStore.scala — do not redesign.
   * ===================================================================== */

  def perRowCarriesInv(xs: List[JobRow], perRow: JobRow => JobRow): Boolean =
    xs match
      case Nil() => true
      case Cons(h, t) =>
        (rowInv(h) ==> rowInv(perRow(h))) && perRowCarriesInv(t, perRow)

  @opaque @inlineOnce
  def mapPreservesForallInv(xs: List[JobRow], perRow: JobRow => JobRow): Unit = {
    require(xs.forall((r: JobRow) => rowInv(r)) && perRowCarriesInv(xs, perRow))
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        mapPreservesForallInv(t, perRow)
  }.ensuring(_ => xs.map(perRow).forall((r: JobRow) => rowInv(r)))

  def perRowKeepsId(xs: List[JobRow], perRow: JobRow => JobRow): Boolean =
    xs match
      case Nil() => true
      case Cons(h, t) => perRow(h).id == h.id && perRowKeepsId(t, perRow)

  @opaque @inlineOnce
  def mapKeepsIds(xs: List[JobRow], perRow: JobRow => JobRow): Unit = {
    require(perRowKeepsId(xs, perRow))
    xs match
      case Nil() => ()
      case Cons(h, t) => mapKeepsIds(t, perRow)
  }.ensuring(_ => ids(xs.map(perRow)) == ids(xs))

  @opaque @inlineOnce
  def mapPreservesDistinct(xs: List[JobRow], perRow: JobRow => JobRow): Unit = {
    require(perRowKeepsId(xs, perRow) && distinctIds(xs))
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        mapKeepsIds(t, perRow)
        mapPreservesDistinct(t, perRow)
  }.ensuring(_ => distinctIds(xs.map(perRow)))

  @opaque @inlineOnce
  def mapPreservesStoreInv(s: JobStore, perRow: JobRow => JobRow): Unit = {
    require(
      storeInv(s) &&
      perRowCarriesInv(s.rows, perRow) &&
      perRowKeepsId(s.rows, perRow)
    )
    mapPreservesForallInv(s.rows, perRow)
    mapPreservesDistinct(s.rows, perRow)
  }.ensuring(_ => storeInv(JobStore(s.rows.map(perRow))))

  /* =====================================================================
   * PER-ROW STEP LEMMAS — step preserves rowInv. Verbatim from WorkerFlow.scala.
   * ===================================================================== */

  def stepKeepsAttemptsBounded(s: JobState, e: Event): Boolean = {
    require(attemptsBounded(s))
    attemptsBounded(step(s, e))
  }.holds

  def stepKeepsBlockedHasWaitEdge(s: JobState, e: Event): Boolean = {
    require(blockedHasWaitEdge(s))
    blockedHasWaitEdge(step(s, e))
  }.holds

  def stepPreservesRowInv(r: JobRow, e: Event): Boolean = {
    require(rowInv(r))
    rowInv(JobRow(r.id, step(r.st, e)))
  }.holds

  /* =====================================================================
   * POINTWISE per-row obligations — each perRow preserves rowInv and keeps id.
   * ===================================================================== */

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
   * establishing the two preconditions of mapPreservesStoreInv. Verbatim from
   * WorkerFlow.scala.
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
   * R1 — INVARIANT-PRESERVATION wrappers. Each proves a StoreCore plain op
   * preserves storeInv, by threading the per-op induction lemmas into the
   * reusable mapPreservesStoreInv. Same VC content as the original in-op
   * ensurings; phrased as Unit lemmas over the externally-defined ops.
   * ===================================================================== */

  def claimOnePreservesInv(s: JobStore, id: FMLong, worker: FMLong, stale: Boolean): Unit = {
    require(storeInv(s))
    val f = claimRow(id, worker, stale)
    claimCarriesInv(s.rows, id, worker, stale)
    claimKeepsIds(s.rows, id, worker, stale)
    mapPreservesStoreInv(s, f)
  }.ensuring(_ => storeInv(claimOne(s, id, worker, stale)))

  def applyOutcomePreservesInv(s: JobStore, id: FMLong, outcome: Event): Unit = {
    require(storeInv(s))
    val f = outcomeRow(id, outcome)
    outcomeCarriesInv(s.rows, id, outcome)
    outcomeKeepsIds(s.rows, id, outcome)
    mapPreservesStoreInv(s, f)
  }.ensuring(_ => storeInv(applyOutcome(s, id, outcome)))

  def handoffPreservesInv(s: JobStore, ev: Event2): Unit = {
    require(storeInv(s))
    val f = handoffRow(ev)
    handoffCarriesInv(s.rows, ev)
    handoffKeepsIds(s.rows, ev)
    mapPreservesStoreInv(s, f)
  }.ensuring(_ => storeInv(handoff(s, ev)))

  def sweepPreservesInv(s: JobStore, dag: Dag): Unit = {
    require(storeInv(s))
    val f = sweepRow(dag)
    sweepCarriesInv(s.rows, dag)
    sweepKeepsIds(s.rows, dag)
    mapPreservesStoreInv(s, f)
  }.ensuring(_ => storeInv(sweep(s, dag)))
}
