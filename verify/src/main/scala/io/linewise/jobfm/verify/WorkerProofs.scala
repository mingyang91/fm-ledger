package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import StoreCore._
import StoreProofs._

/* =============================================================================
 * THE OPERATIONAL REQUIREMENT LIBRARY — R1..R5, each a NAMED BUSINESS
 * REQUIREMENT proven over the worker workflow for ANY store satisfying storeInv.
 *
 * WorkerFlow.scala defined the four ops (claimOne / applyOutcome / handoff /
 * sweep) as pure JobStore => JobStore and proved R1 (INVARIANT-PRESERVATION) as
 * each op's `ensuring(storeInv(res))` — discharged by the reusable
 * JobStore.mapPreservesStoreInv induction lemma. This file accumulates the rest
 * of the library: the business postconditions R2..R5.
 *
 * SHAPE OF THE PROOFS. Every op is `s.rows.map(perRow)`. `map` is, by its own
 * definition, ELEMENT-WISE: the i-th output row is `perRow(i-th input row)`. So
 * the business content of every R2..R5 proposition lives in the PER-ROW
 * transform — "what perRow does to a matching row" and "perRow is the identity
 * on a non-matching row" (the FRAME). We prove those per-row facts here as
 * `.holds` propositions; `map`'s element-wise definition lifts them to the whole
 * store for free (no extra induction). Where a proposition genuinely needs a
 * whole-list statement (e.g. "EXACTLY these rows changed, indexed positionally")
 * that exceeds what a clean per-row + map-definitional argument gives, it is
 * recorded as an explicit TRACKED GAP with its consequence — see the GAP notes.
 * ========================================================================== */
object WorkerProofs {

  /* =====================================================================
   * R1 — INVARIANT-PRESERVATION (named restatement; proven in WorkerFlow).
   *
   * Each op carries storeInv from precondition to postcondition. The proofs are
   * the `ensuring(storeInv(res))` on claimOne/applyOutcome/handoff/sweep in
   * WorkerFlow.scala, discharged by mapPreservesStoreInv. Here we name R1 as a
   * library entry by re-invoking each op under storeInv and asserting the result
   * satisfies storeInv — a thin proposition that depends on those ensurings. */

  def r1_claimPreservesInv(s: JobStore, id: FMLong, w: FMLong, stale: Boolean): Boolean = {
    require(storeInv(s))
    claimOnePreservesInv(s, id, w, stale)
    storeInv(claimOne(s, id, w, stale))
  }.holds

  def r1_applyOutcomePreservesInv(s: JobStore, id: FMLong, o: Event): Boolean = {
    require(storeInv(s))
    applyOutcomePreservesInv(s, id, o)
    storeInv(applyOutcome(s, id, o))
  }.holds

  def r1_handoffPreservesInv(s: JobStore, ev: Event2): Boolean = {
    require(storeInv(s))
    handoffPreservesInv(s, ev)
    storeInv(handoff(s, ev))
  }.holds

  def r1_sweepPreservesInv(s: JobStore, dag: Dag): Boolean = {
    require(storeInv(s))
    sweepPreservesInv(s, dag)
    storeInv(sweep(s, dag))
  }.holds

  /* =====================================================================
   * STORE-LEVEL FRAME — the SHAPE of the store is preserved by every op.
   *
   * Each op is `s.rows.map(perRow)` with an id-preserving perRow, so the store's
   * id list is byte-identical after the op: no row is added, dropped, or
   * reordered. This is the whole-list half of the frame (the per-row half — what
   * each row becomes — is R2..R4 below). We prove it from the existing
   * JobStore.mapKeepsIds induction lemma fed by the per-op KeepsId lemmas, so the
   * lift from "perRow keeps id" to "the store's id list is unchanged" is an
   * EXPLICIT VC, not an appeal to map's definition.
   * ------------------------------------------------------------------- */

  def frame_handoffKeepsIdList(s: JobStore, ev: Event2): Boolean = {
    val f = handoffRow(ev)
    handoffKeepsIds(s.rows, ev)
    mapKeepsIds(s.rows, f)
    ids(handoff(s, ev).rows) == ids(s.rows)
  }.holds

  def frame_sweepKeepsIdList(s: JobStore, dag: Dag): Boolean = {
    val f = sweepRow(dag)
    sweepKeepsIds(s.rows, dag)
    mapKeepsIds(s.rows, f)
    ids(sweep(s, dag).rows) == ids(s.rows)
  }.holds

  def frame_claimKeepsIdList(s: JobStore, id: FMLong, w: FMLong, stale: Boolean): Boolean = {
    val f = claimRow(id, w, stale)
    claimKeepsIds(s.rows, id, w, stale)
    mapKeepsIds(s.rows, f)
    ids(claimOne(s, id, w, stale).rows) == ids(s.rows)
  }.holds

  /* =====================================================================
   * R2 — HANDOFF-FRAME.
   *
   * handoff(s, ev) promotes EXACTLY the rows with status==BLOCKED &&
   * blockedOn==Some(ev) to PENDING (clearing blockedOn); every OTHER row is
   * byte-identical. We prove this per-row (the business content), and `map`
   * lifts it positionally to the store.
   *
   *   (promote)  a matching row -> PENDING, blockedOn cleared, id/attempts/
   *              owner/kind preserved.
   *   (frame)    a non-matching row -> the row is the SAME value (==).
   * ------------------------------------------------------------------- */

  // PROMOTE: a BLOCKED row parked on EXACTLY ev becomes PENDING with blockedOn
  // cleared, and id/owner/attempts/kind are untouched.
  def r2_handoffPromotesMatching(r: JobRow, ev: Event2): Boolean = {
    require(r.st.status == BLOCKED && r.st.blockedOn == Some[Event2](ev))
    val out = handoffRow(ev)(r)
    out.st.status == PENDING &&
    out.st.blockedOn == None[Event2]() &&
    out.id == r.id &&
    out.st.owner == r.st.owner &&
    out.st.attempts == r.st.attempts &&
    out.st.kind == r.st.kind
  }.holds

  // FRAME (status mismatch): a non-BLOCKED row is returned byte-identical.
  def r2_handoffFramesNonBlocked(r: JobRow, ev: Event2): Boolean = {
    require(r.st.status != BLOCKED)
    handoffRow(ev)(r) == r
  }.holds

  // FRAME (event mismatch): a BLOCKED row parked on a DIFFERENT event is
  // returned byte-identical — handoff of ev does not touch rows waiting on ev2.
  def r2_handoffFramesOtherEvent(r: JobRow, ev: Event2, ev2: Event2): Boolean = {
    require(r.st.status == BLOCKED && r.st.blockedOn == Some[Event2](ev2) && ev != ev2)
    handoffRow(ev)(r) == r
  }.holds

  // EXACTLY (the biconditional): handoff changes a row IFF it was BLOCKED on ev.
  // Equivalently: the row is unchanged unless (BLOCKED && blockedOn==Some(ev)).
  def r2_handoffChangesExactlyMatching(r: JobRow, ev: Event2): Boolean = {
    val changed = handoffRow(ev)(r) != r
    val matches = r.st.status == BLOCKED && r.st.blockedOn == Some[Event2](ev)
    changed ==> matches
  }.holds

  // FRAME does not touch attempts/owner of any non-promoted row (explicit,
  // since the task calls it out): if not a matching BLOCKED row, attempts and
  // owner are preserved. (Subsumed by the == frame, but named for the library.)
  def r2_handoffPreservesAttemptsOwnerOfOthers(r: JobRow, ev: Event2): Boolean = {
    require(!(r.st.status == BLOCKED && r.st.blockedOn == Some[Event2](ev)))
    val out = handoffRow(ev)(r)
    out.st.attempts == r.st.attempts && out.st.owner == r.st.owner
  }.holds

  /* =====================================================================
   * R3 — SWEEP-CORRECTNESS.
   *
   * sweep(s, dag) promotes EXACTLY the orphaned BLOCKED rows (BLOCKED with a
   * blockedOn whose event has NO producer in dag) to NOT_APPLICABLE; every other
   * row is byte-identical. Per-row, lifted by map.
   * ------------------------------------------------------------------- */

  // PROMOTE: an orphaned BLOCKED row -> NOT_APPLICABLE, blockedOn cleared,
  // id/owner/attempts/kind preserved.
  def r3_sweepPromotesOrphan(r: JobRow, dag: Dag): Boolean = {
    require(isOrphanedBlocked(dag, r.st))
    val out = sweepRow(dag)(r)
    out.st.status == NOT_APPLICABLE &&
    out.st.blockedOn == None[Event2]() &&
    out.id == r.id &&
    out.st.owner == r.st.owner &&
    out.st.attempts == r.st.attempts &&
    out.st.kind == r.st.kind
  }.holds

  // FRAME: a NON-orphaned row (not BLOCKED, or BLOCKED but its event still has a
  // live producer) is returned byte-identical.
  def r3_sweepFramesNonOrphan(r: JobRow, dag: Dag): Boolean = {
    require(!isOrphanedBlocked(dag, r.st))
    sweepRow(dag)(r) == r
  }.holds

  // EXACTLY: sweep changes a row IFF it was an orphaned BLOCKED row.
  def r3_sweepChangesExactlyOrphans(r: JobRow, dag: Dag): Boolean = {
    val changed = sweepRow(dag)(r) != r
    changed ==> isOrphanedBlocked(dag, r.st)
  }.holds

  // A BLOCKED row whose event STILL has a producer is NOT swept (it is left for
  // handoff) — the sweeper does not race ahead of a live producer.
  def r3_sweepLeavesCoveredBlocked(r: JobRow, dag: Dag, ev: Event2): Boolean = {
    require(r.st.status == BLOCKED && r.st.blockedOn == Some[Event2](ev) && hasProducer(dag, ev))
    sweepRow(dag)(r) == r
  }.holds

  /* =====================================================================
   * R4 — CLAIM-SAFETY-OVER-STORE.
   *
   * claimOne advances EXACTLY the targeted row if claimable (setting owner to
   * the claiming worker, status RUNNING); never changes another row; and a
   * second claim on a now-RUNNING row owned by A does not let B take it.
   * ------------------------------------------------------------------- */

  // ADVANCE: the targeted, claimable row -> RUNNING, owned by the claiming
  // worker, id preserved.
  def r4_claimAdvancesTarget(r: JobRow, id: FMLong, w: FMLong, stale: Boolean): Boolean = {
    require(r.id == id && claimable(r.st, stale))
    val out = claimRow(id, w, stale)(r)
    out.st.status == RUNNING && isOwner(out.st, w) && out.id == r.id
  }.holds

  // FRAME (different id): a row that is NOT the target is byte-identical, no
  // matter its status.
  def r4_claimFramesOtherRows(r: JobRow, id: FMLong, w: FMLong, stale: Boolean): Boolean = {
    require(r.id != id)
    claimRow(id, w, stale)(r) == r
  }.holds

  // NO-OP (target not claimable): the targeted row that is NOT claimable is
  // byte-identical (e.g. DONE/FAILED/NOT_APPLICABLE, or a fresh RUNNING with a
  // live lease where stale==false).
  def r4_claimNoOpWhenNotClaimable(r: JobRow, id: FMLong, w: FMLong, stale: Boolean): Boolean = {
    require(r.id == id && !claimable(r.st, stale))
    claimRow(id, w, stale)(r) == r
  }.holds

  // SECOND-CLAIM SAFETY: a RUNNING row owned by A, with a LIVE lease (stale ==
  // false), is NOT claimable by B — B's claim is a no-op and A stays the owner.
  // This is the store-level lift of staleRunningReclaimable's contrapositive:
  // only an EXPIRED lease re-opens a RUNNING row.
  def r4_secondClaimDeniedOnLiveLease(r: JobRow, id: FMLong, a: FMLong, b: FMLong): Boolean = {
    require(r.id == id && r.st.status == RUNNING && isOwner(r.st, a) && a != b)
    // stale == false: lease is live, so the row is not claimable by anyone.
    // B (a DIFFERENT worker) cannot take it; the row is untouched and A keeps it.
    val out = claimRow(id, b, false)(r)
    out == r && isOwner(out.st, a) && !isOwner(out.st, b)
  }.holds

  // STALE RECLAIM (the dual, kept honest): a RUNNING row owned by A whose lease
  // HAS expired (stale == true) IS reclaimable; B's claim takes ownership. This
  // is the v1 requirement that EvolutionConflict.scala's v2 will contradict.
  def r4_staleRunningReclaimedByB(r: JobRow, id: FMLong, a: FMLong, b: FMLong): Boolean = {
    require(r.id == id && r.st.status == RUNNING && isOwner(r.st, a) && a != b)
    val out = claimRow(id, b, true)(r)
    out.st.status == RUNNING && isOwner(out.st, b) && !isOwner(out.st, a)
  }.holds

  /* =====================================================================
   * R5 — NO-STRAND / PROGRESS.
   *
   * (a) Composing the producer's DONE with handoff: a BLOCKED row waiting on the
   *     event the producer's DONE fires becomes PENDING (not stranded). We model
   *     "the producer's DONE fires event ev" as the handoff(_, ev) call, and
   *     prove the waiting row is promoted — the producer-to-consumer baton is
   *     not dropped.
   * (b) sweep guarantees no orphaned BLOCKED row stays BLOCKED: after a sweep, an
   *     orphaned BLOCKED row has reached NOT_APPLICABLE (a terminal, not BLOCKED).
   * ------------------------------------------------------------------- */

  // (a) NO-STRAND: a row BLOCKED on ev, after handoff(ev), is PENDING — ready to
  // be claimed again. The baton from the upstream producer's DONE is delivered.
  def r5_handoffUnstrandsWaiter(r: JobRow, ev: Event2): Boolean = {
    require(r.st.status == BLOCKED && r.st.blockedOn == Some[Event2](ev))
    val out = handoffRow(ev)(r)
    out.st.status == PENDING && isNonTerminal(out.st.status)
  }.holds

  // (a') PRODUCER-CONSUMER COMPOSITION over the live DAG: take the event a
  // producer kind's DONE produces (producedEventOf), and a consumer row BLOCKED
  // on that exact event. handoff of that event unstrands the consumer. This ties
  // the abstract handoff to a REAL DAG edge: Transcode.DONE -> VideoMetaDuration
  // unblocks the Embedding consumer, etc.
  def r5_producerDoneUnblocksConsumer(r: JobRow, dag: Dag, producer: Kind): Boolean = {
    require(producedEventOf(dag, producer).isDefined)
    val ev = producedEventOf(dag, producer).get
    require(r.st.status == BLOCKED && r.st.blockedOn == Some[Event2](ev))
    handoffRow(ev)(r).st.status == PENDING
  }.holds

  // (b) PROGRESS via sweep: an orphaned BLOCKED row does NOT stay BLOCKED — after
  // sweepRow it is NOT_APPLICABLE (terminal). No orphan is stranded forever.
  def r5_sweepUnblocksOrphanToTerminal(r: JobRow, dag: Dag): Boolean = {
    require(isOrphanedBlocked(dag, r.st))
    val out = sweepRow(dag)(r)
    out.st.status == NOT_APPLICABLE &&
    isTerminal(out.st.status) &&
    out.st.status != BLOCKED
  }.holds

  // (b') COMBINED LIVENESS for any BLOCKED row: EITHER its event has a producer
  // (so a future handoff can unstrand it) OR it is orphaned (so sweep reclaims it
  // now). A BLOCKED row is never simultaneously un-handoffable AND un-sweepable —
  // there is always a forward edge. (storeInv guarantees blockedOn is defined.)
  def r5_everyBlockedHasAForwardEdge(r: JobRow, dag: Dag): Boolean = {
    require(rowInv(r) && r.st.status == BLOCKED)
    // blockedHasWaitEdge (in rowInv) gives blockedOn.isDefined.
    r.st.blockedOn match
      case Some(ev) =>
        // either ev has a producer (handoff path) or not (sweep path).
        if hasProducer(dag, ev) then
          // handoff(ev) would unstrand it.
          handoffRow(ev)(r).st.status == PENDING
        else
          // it is orphaned, so sweep reclaims it to NOT_APPLICABLE.
          sweepRow(dag)(r).st.status == NOT_APPLICABLE
      case None() =>
        // unreachable under rowInv: blockedHasWaitEdge forbids BLOCKED + None.
        false
  }.holds
}
