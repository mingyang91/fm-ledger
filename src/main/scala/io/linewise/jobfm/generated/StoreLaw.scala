package io.linewise.jobfm.generated

import io.linewise.jobfm.generated.JobModel.*
import io.linewise.jobfm.generated.StoreCore.JobRow

/* =============================================================================
 * PRODUCTION counterpart of verify/StoreLaw.scala's AbstractStore — SIGNATURES
 * ONLY. HAND-WRITTEN, in the `generated` package so the transpiled
 * generated.WorkerCore's `import StoreLaw.AbstractStore` resolves here. The @law
 * contracts and the `inv` observer are verify-only (dropped in transpilation);
 * production keeps just the interface that ListStore and JdbcStore (Stores.scala)
 * implement — the ONE store the worker runs against and the differential test
 * checks. No separate World.
 * ========================================================================== */
object StoreLaw {
  trait AbstractStore {
    def view: List[JobRow]
    def claimOne(id: Long, worker: Long, stale: Boolean): AbstractStore
    def applyOutcome(id: Long, outcome: Event): AbstractStore
    def handoff(ev: Event2): AbstractStore
    def sweep(dag: Dag): AbstractStore
    def findById(id: Long): Option[JobState]
  }
}
