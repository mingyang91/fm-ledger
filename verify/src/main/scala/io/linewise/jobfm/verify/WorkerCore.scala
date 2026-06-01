package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import StoreCore.JobStore
import StoreProofs.storeInv
import StoreLaw.AbstractStore

/* =============================================================================
 * THE WORKER LOGIC — TRANSPILED. Run-one-row over the ONE shared store
 * abstraction `AbstractStore` (StoreLaw): the worker is VERIFIED against the
 * store's @law contracts (claimLaw / applyOutcomeLaw), and in production the SAME
 * code runs against whatever implements that interface — `JdbcStore` (doobie) at
 * runtime, `ListStore` in the differential test. There is no separate `World`:
 * the worker and the differential test share one store, so the test covers the
 * worker's actual path.
 *
 * Flow: claimOne -> findById (get the kind / detect the win) -> sidecar(kind) ->
 * applyOutcome. The @law contracts are invoked EXPLICITLY as hints (Stainless
 * does not auto-chain laws); they are proof scaffolding and the transpiler drops
 * the `assert(...)` lines. storeInv-preservation is the proven postcondition,
 * discharged purely from claimLaw + applyOutcomeLaw.
 *
 * The sidecar (Kind => Event) is a capability: external IO stays outside the
 * verified logic; runOne is verified for ANY sidecar. The heartbeat is NOT here —
 * it is a production concern layered onto the sidecar (a daemon that renews the
 * lease while the sidecar runs), the canonical trusted/Ox leaf.
 * ========================================================================== */
object WorkerCore {

  def runOne(s: AbstractStore, id: FMLong, worker: FMLong, sidecar: Kind => Event): AbstractStore = {
    require(storeInv(JobStore(s.view)))
    assert(s.claimLaw(id, worker, false)) // hint: storeInv(s.view) ==> storeInv(s1.view)
    val s1 = s.claimOne(id, worker, false)
    s1.findById(id) match
      case Some(st) if st.status == RUNNING && isOwner(st, worker) =>
        val ev = sidecar(st.kind)
        assert(s1.applyOutcomeLaw(id, ev)) // hint: storeInv(s1.view) ==> storeInv(result.view)
        s1.applyOutcome(id, ev)
      case _ => s1
  }.ensuring(res => storeInv(JobStore(res.view)))
}
