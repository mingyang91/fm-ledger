package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._

/* =============================================================================
 * THE ABSTRACT STORE + ITS INVARIANT — lifting the proofs OFF a single JobState
 * and ONTO the operational world the worker threads.
 *
 * WHAT THIS IS. The production worker does not act on one JobState in a vacuum;
 * it acts on a STORE of rows (the `job` table). `step` is the per-row business
 * core, already proven in JobProofs.scala. This file models the store the worker
 * threads as a PURE VALUE — `JobStore(rows: List[JobRow])` — and defines the
 * STORE INVARIANT `storeInv` as a clean conjunction of per-row `forall`
 * predicates plus a global distinctness predicate. WorkerFlow.scala defines the
 * worker ops (claimOne / applyOutcome / handoff / sweep) as `JobStore => JobStore`
 * and proves R1..R5 over `require(storeInv(s))`.
 *
 * WHAT THIS IS NOT. We do NOT model persistence: doobie, SQL, transactions,
 * FOR UPDATE SKIP LOCKED, heartbeats-as-wall-clock, and true concurrent
 * interleaving are inexpressible in this pure first-order setting and are OUT OF
 * SCOPE. The real DB is a TRUSTED REALIZATION of this abstract store: each SQL
 * WHERE clause is trusted-equivalent to the verified selection predicate it
 * stands in for (the trusted boundary is enumerated in WorkerFlow.scala). We
 * prove the WORKFLOW GIVEN a valid store; we do not prove the store is persisted
 * faithfully.
 * ========================================================================== */

object JobStore {

  /* --- A ROW: the id + its JobState. The row IS the job (row-as-source-of-
   * truth), exactly as in store.scala where the `job` table's columns ARE the
   * five JobState fields keyed by id. ----------------------------------------*/
  case class JobRow(id: FMLong, st: JobState)

  /* --- THE STORE: a list of rows. stainless.collection.List, not scala List,
   * so `forall` / `map` / `exists` have the inductive structure the solver can
   * reason about. --------------------------------------------------------- */
  case class JobStore(rows: List[JobRow])

  /* =====================================================================
   * THE PER-ROW BUSINESS INVARIANT — `rowInv(r)`.
   *
   * Each clause is a named business requirement on a single row:
   *
   *   (a) ATTEMPTS BOUNDED:   0 <= attempts <= maxAttempts. This is the runtime
   *       echo of the `attempts BIGINT check (attempts >= 0 and attempts <= 5)`
   *       column constraint in store.scala's DDL. JobState's own `require`
   *       already enforces this for any constructible JobState, so it is always
   *       true; restating it here names it as a store-level business predicate
   *       and lets the solver see it as part of storeInv.
   *
   *   (b) BLOCKED HAS A WAIT EDGE: a BLOCKED row's `blockedOn` is defined. A
   *       BLOCKED row with no event to wait for would be stranded with no
   *       handoff and no orphan-sweep target. This is the load-bearing parking
   *       discipline: `step` only ever produces BLOCKED via
   *       `OutBlocked(w, waitFor)`, which always sets `blockedOn = Some(waitFor)`,
   *       so the core establishes this; the invariant carries it across ops.
   * ------------------------------------------------------------------- */

  def attemptsBounded(s: JobState): Boolean =
    s.attempts >= FMInt(BigInt(0)) && s.attempts <= maxAttempts

  def blockedHasWaitEdge(s: JobState): Boolean =
    s.status != BLOCKED || s.blockedOn.isDefined

  // The per-row invariant: the conjunction of the per-row business predicates.
  def rowInv(r: JobRow): Boolean =
    attemptsBounded(r.st) && blockedHasWaitEdge(r.st)

  /* =====================================================================
   * GLOBAL DISTINCTNESS — ids are unique across the store.
   *
   * The `job` table has `id BIGINT primary key`, so two rows never share an id.
   * We model this as distinctness of the projected id list. Distinctness is the
   * one NON-per-row clause; it is preserved by any op that does not change ids
   * (every WorkerFlow op is `rows.map(perRow)` with `perRow` id-preserving, so
   * the id list is byte-identical after a map — see idsPreservedByMap).
   * ------------------------------------------------------------------- */

  def ids(rows: List[JobRow]): List[FMLong] =
    rows.map((r: JobRow) => r.id)

  // distinct = no element appears later in the tail (stainless.collection.List
  // has `.contains`; we fold it into a structural recursion the solver unfolds).
  def distinctIds(rows: List[JobRow]): Boolean =
    rows match
      case Nil() => true
      case Cons(h, t) => !ids(t).contains(h.id) && distinctIds(t)

  /* =====================================================================
   * THE STORE INVARIANT — `storeInv(s)`.
   *
   * A clean conjunction: every row satisfies the per-row business invariant,
   * AND ids are globally distinct. This is the predicate every WorkerFlow op
   * takes as a precondition and re-establishes as a postcondition (R1).
   * ------------------------------------------------------------------- */
  def storeInv(s: JobStore): Boolean =
    s.rows.forall((r: JobRow) => rowInv(r)) && distinctIds(s.rows)

  /* =====================================================================
   * THE REUSABLE MAP-PRESERVES-INVARIANT LEMMA (洞-B: the List-induction core).
   *
   * Every WorkerFlow op has the UNIFORM shape `s.copy(rows = s.rows.map(perRow))`
   * with `perRow: JobRow => JobRow`. To prove R1 (each op preserves storeInv) we
   * need ONE lemma about `map` + `forall`, proven by induction over the list,
   * then reused for every op. The lemma is parameterised by the per-row
   * obligation `perRowPreservesInv`: if `perRow` carries rowInv from input to
   * output for EVERY element of `xs`, then `xs.map(perRow)` satisfies
   * `forall rowInv`.
   *
   * We state the per-element obligation as a Boolean hypothesis over the list
   * (forall x in xs: rowInv(x) ==> rowInv(perRow(x))) and conclude
   * forall(map) rowInv. @opaque @inlineOnce keeps the solver from unfolding the
   * recursion uncontrolled; the explicit induction does the work.
   * ------------------------------------------------------------------- */

  // The per-element hypothesis, as a decidable forall over the concrete list.
  def perRowCarriesInv(xs: List[JobRow], perRow: JobRow => JobRow): Boolean =
    xs match
      case Nil() => true
      case Cons(h, t) =>
        (rowInv(h) ==> rowInv(perRow(h))) && perRowCarriesInv(t, perRow)

  /* THE LEMMA. Given storeInv's forall-half on the input (every row has rowInv)
   * AND that perRow carries rowInv on every element, the mapped list satisfies
   * forall rowInv. Proven by structural induction on xs. */
  @opaque @inlineOnce
  def mapPreservesForallInv(xs: List[JobRow], perRow: JobRow => JobRow): Unit = {
    require(xs.forall((r: JobRow) => rowInv(r)) && perRowCarriesInv(xs, perRow))
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        // head: rowInv(h) holds (from forall) and perRow carries it, so
        // rowInv(perRow(h)) holds. tail: recurse.
        mapPreservesForallInv(t, perRow)
  }.ensuring(_ => xs.map(perRow).forall((r: JobRow) => rowInv(r)))

  /* DISTINCTNESS PRESERVATION. If perRow preserves the id of every element
   * (perRow(r).id == r.id), the projected id list is unchanged by map, so
   * distinctIds is preserved. We prove `ids(xs.map(perRow)) == ids(xs)` by
   * induction, from which distinctIds(xs) ==> distinctIds(xs.map(perRow)). */

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

  /* distinctIds depends ONLY on the id projection, so equal id lists give equal
   * distinctIds. We prove distinctIds is a function of `ids(rows)` by relating
   * the two structural recursions. Rather than thread an abstract equality
   * (which would need a congruence lemma over distinctIds' own shape), we prove
   * the combined statement directly: if perRow keeps every id and the input is
   * distinct, the mapped store is distinct. Induction on xs; the head step uses
   * mapKeepsIds on the tail to show the head's id is still absent from the
   * mapped tail's id list. */
  @opaque @inlineOnce
  def mapPreservesDistinct(xs: List[JobRow], perRow: JobRow => JobRow): Unit = {
    require(perRowKeepsId(xs, perRow) && distinctIds(xs))
    xs match
      case Nil() => ()
      case Cons(h, t) =>
        // ids(t.map(perRow)) == ids(t): the head's id (unchanged by perRow) was
        // absent from ids(t), so it is absent from ids(t.map(perRow)).
        mapKeepsIds(t, perRow)
        mapPreservesDistinct(t, perRow)
  }.ensuring(_ => distinctIds(xs.map(perRow)))

  /* THE COMBINED OP-LEVEL LEMMA the WorkerFlow ops call. Given a perRow that
   * (1) carries rowInv on every element and (2) keeps every id, a `map` over a
   * store satisfying storeInv yields a store satisfying storeInv. This is the
   * single induction every op reuses for R1. */
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
}
