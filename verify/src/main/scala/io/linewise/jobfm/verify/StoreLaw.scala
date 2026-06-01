package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import StoreCore._
import StoreProofs._

/* =============================================================================
 * THE ABSTRACT STORE + ITS @law CONTRACT — the interface both the in-memory and
 * the doobie store implement, with the business contract stated ON the interface.
 *
 * This is the structural co-location that minimizes drift (the user's option B):
 * the worker's invariant-preservation requirement lives as a Stainless `@law` on
 * the abstract `AbstractStore`. Stainless CHECKS the law for the in-memory
 * `ListStoreModel` (below); the doobie store is TRUSTED to satisfy the SAME law,
 * and the differential test (DifferentialSpec) machine-checks that trust.
 *
 * VERIFY-ONLY, NEVER TRANSPILED. `@law` is ghost; production gets a separate,
 * tiny, hand-written signatures-only `trait Store` (src/Store.scala) that the
 * executable `ListStore` and `JdbcStore` implement. Keeping `@law` out of the
 * transpiler's path is why production has its own plain trait.
 *
 * The contract is stated over a GHOST observation `view: List[JobRow]` — the
 * store's contents — so it can talk about per-row state even though the doobie
 * realization has no materialized list. Each law is invariant-PRESERVATION:
 *   storeInv(view-before) ==> storeInv(view-after-op).
 *
 * DISCHARGE IDIOM (spike-proven, 52/52): the in-memory op invokes the reused
 * induction lemmas ONLY under the `if storeInv(...)` hypothesis the law carries —
 * an unconditional lemma call fails on an already-invalid store (the lemma's own
 * precondition is storeInv). The lemmas (claimCarriesInv / mapPreservesStoreInv /
 * …) are reused verbatim from StoreProofs — not redesigned.
 * ========================================================================== */
object StoreLaw {

  abstract class AbstractStore {
    def view: List[JobRow]
    def claimOne(id: FMLong, worker: FMLong, stale: Boolean): AbstractStore
    def applyOutcome(id: FMLong, outcome: Event): AbstractStore
    def handoff(ev: Event2): AbstractStore
    def sweep(dag: Dag): AbstractStore

    // a point read (no @law — its result only selects the worker's branch; the
    // storeInv proof comes from the op laws regardless of what findById returns).
    def findById(id: FMLong): Option[JobState]

    /* THE CONTRACT — invariant-preservation, one law per op, over `view`. Every
     * concrete store MUST satisfy these; Stainless generates a discharge VC per
     * subclass. (The doobie store's discharge is the trusted/differential-tested
     * one; it is not a Stainless subclass.) */
    @law def claimLaw(id: FMLong, worker: FMLong, stale: Boolean): Boolean =
      storeInv(JobStore(view)) ==> storeInv(JobStore(claimOne(id, worker, stale).view))

    @law def applyOutcomeLaw(id: FMLong, outcome: Event): Boolean =
      storeInv(JobStore(view)) ==> storeInv(JobStore(applyOutcome(id, outcome).view))

    @law def handoffLaw(ev: Event2): Boolean =
      storeInv(JobStore(view)) ==> storeInv(JobStore(handoff(ev).view))

    @law def sweepLaw(dag: Dag): Boolean =
      storeInv(JobStore(view)) ==> storeInv(JobStore(sweep(dag).view))
  }

  /* THE IN-MEMORY MODEL — view = the list; each op = map over StoreCore's per-row
   * transform. Discharges every @law via the spike idiom (lemmas under the
   * storeInv guard, .ensuring the per-op implication). */
  case class ListStoreModel(items: List[JobRow]) extends AbstractStore {
    def view: List[JobRow] = items

    def findById(id: FMLong): Option[JobState] =
      items.find((r: JobRow) => r.id == id).map((r: JobRow) => r.st)

    def claimOne(id: FMLong, worker: FMLong, stale: Boolean): AbstractStore = {
      val f = claimRow(id, worker, stale)
      if storeInv(JobStore(items)) then {
        claimCarriesInv(items, id, worker, stale)
        claimKeepsIds(items, id, worker, stale)
        mapPreservesStoreInv(JobStore(items), f)
      }
      ListStoreModel(items.map(f))
    }.ensuring((res: AbstractStore) => storeInv(JobStore(items)) ==> storeInv(JobStore(res.view)))

    def applyOutcome(id: FMLong, outcome: Event): AbstractStore = {
      val f = outcomeRow(id, outcome)
      if storeInv(JobStore(items)) then {
        outcomeCarriesInv(items, id, outcome)
        outcomeKeepsIds(items, id, outcome)
        mapPreservesStoreInv(JobStore(items), f)
      }
      ListStoreModel(items.map(f))
    }.ensuring((res: AbstractStore) => storeInv(JobStore(items)) ==> storeInv(JobStore(res.view)))

    def handoff(ev: Event2): AbstractStore = {
      val f = handoffRow(ev)
      if storeInv(JobStore(items)) then {
        handoffCarriesInv(items, ev)
        handoffKeepsIds(items, ev)
        mapPreservesStoreInv(JobStore(items), f)
      }
      ListStoreModel(items.map(f))
    }.ensuring((res: AbstractStore) => storeInv(JobStore(items)) ==> storeInv(JobStore(res.view)))

    def sweep(dag: Dag): AbstractStore = {
      val f = sweepRow(dag)
      if storeInv(JobStore(items)) then {
        sweepCarriesInv(items, dag)
        sweepKeepsIds(items, dag)
        mapPreservesStoreInv(JobStore(items), f)
      }
      ListStoreModel(items.map(f))
    }.ensuring((res: AbstractStore) => storeInv(JobStore(items)) ==> storeInv(JobStore(res.view)))
  }
}
