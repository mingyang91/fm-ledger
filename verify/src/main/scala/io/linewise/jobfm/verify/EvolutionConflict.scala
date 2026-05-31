package io.linewise.verify.fm.jobsystem

import stainless.lang._
import JobModel._

/* =============================================================================
 * EVOLUTION CONFLICT — the deliberately-INVALID VC.
 *
 * This file exists ONLY to demonstrate that the DAG coverage property is not
 * vacuous: when the Embedding kind is removed, asserting that the broken DAG
 * is STILL covered is FALSE, and Stainless finds the counterexample.
 *
 * Run this file SEPARATELY from JobProofs.scala. It is expected to report
 * INVALID with a counterexample on `brokenDagFalselyClaimedCovered`. The
 * counterexample is the orphaned edge (Masking, EmbeddingReady): coverage
 * evaluates to FALSE because hasProducer(embeddingRemovedDag, EmbeddingReady)
 * is FALSE, so the postcondition `== true` cannot hold.
 * ========================================================================== */
object EvolutionConflict {

  // INVALID: claims the broken DAG is covered. coverage(embeddingRemovedDag)
  // is FALSE (Masking waits on EmbeddingReady, which nobody produces), so this
  // postcondition is unprovable. Stainless emits the counterexample.
  def brokenDagFalselyClaimedCovered: Boolean = {
    coverage(embeddingRemovedDag) == true
  }.holds
}
