package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import StoreCore._

/* =============================================================================
 * REQUIREMENT-EVOLUTION CONFLICTS — the deliberately-INVALID VCs.
 *
 * This file exists ONLY to demonstrate that the propositions have TEETH: a v2
 * requirement that CONTRADICTS an established v1 requirement is machine-detected
 * as INVALID at proposition-introduction time, with a counterexample. It is
 * EXCLUDED from the headline `./mill verify.scala` run (the runner filters it
 * out) so the headline reports 0 invalid; run it explicitly to see the conflicts:
 *
 *   ./mill verify.scala \
 *     verify/stainless-lib/FMTypes.scala \
 *     verify/src/main/scala/io/linewise/jobfm/verify/JobModel.scala \
 *     verify/src/main/scala/io/linewise/jobfm/verify/StoreCore.scala \
 *     verify/src/main/scala/io/linewise/jobfm/verify/EvolutionConflict.scala
 *
 * TWO conflicts are surfaced:
 *   (1) DAG-COVERAGE conflict (single-step level): the embedding-removed DAG is
 *       falsely claimed covered.
 *   (2) STORE-LEVEL claim conflict: a v2 "a RUNNING row is NEVER re-PENDINGed by
 *       a claim" requirement contradicts the v1 stale-reclaim requirement
 *       (r4_staleRunningReclaimedByB), which DOES re-PENDING-then-reclaim a stale
 *       RUNNING row. Stainless finds the stale-but-RUNNING counterexample.
 * ========================================================================== */
object EvolutionConflict {

  /* --- CONFLICT (1): DAG-COVERAGE -------------------------------------------
   * INVALID: claims the broken DAG is covered. coverage(embeddingRemovedDag)
   * is FALSE (Masking waits on EmbeddingReady, which nobody produces), so this
   * postcondition is unprovable. Stainless emits the counterexample (the
   * orphaned (Masking, EmbeddingReady) edge). */
  def brokenDagFalselyClaimedCovered: Boolean = {
    coverage(embeddingRemovedDag) == true
  }.holds

  /* --- CONFLICT (2): STORE-LEVEL CLAIM / STALE-RECLAIM ----------------------
   * v1 REQUIREMENT (established, valid — WorkerProofs.r4_staleRunningReclaimedByB):
   *   "a STALE RUNNING row IS reclaimable: claimRow(id, b, stale=true) on a
   *    RUNNING row owned by A re-PENDINGs it and hands ownership to B."
   *
   * v2 REQUIREMENT (newly proposed, CONTRADICTORY):
   *   "a RUNNING row is NEVER re-PENDINGed / never changes owner under a claim —
   *    once RUNNING and owned, a claim must be a total no-op on owner."
   *
   * These cannot both hold. For a stale RUNNING row owned by A, claimRow(id, b,
   * true) sets status=RUNNING and owner=Some(b) != Some(a) — so asserting the v2
   * postcondition `claimRow(...).st.owner == r.st.owner` is FALSE. Stainless
   * finds the counterexample: a RUNNING row, stale=true, b != a.
   *
   * This is the requirement-evolution conflict the vision asks for, surfaced at
   * the STORE op level: introducing v2 turns the build INVALID, which is the
   * machine telling us the new requirement contradicts the shipped one. The
   * resolution is a product decision (keep stale-reclaim, or forbid it and pay
   * the cost of stranded leases), not a proof tweak. */
  def runningRowNeverRePendingedByClaim(r: JobRow, id: FMLong, a: FMLong, b: FMLong): Boolean = {
    require(r.id == id && r.st.status == RUNNING && isOwner(r.st, a) && a != b)
    // claim with stale==true: v1 reclaims (owner becomes b). v2 forbids any
    // owner change. The postcondition encodes v2; it is FALSE for the stale
    // RUNNING row, so this VC is INVALID with a counterexample.
    val out = claimRow(id, b, true)(r)
    out.st.owner == r.st.owner
  }.holds
}
