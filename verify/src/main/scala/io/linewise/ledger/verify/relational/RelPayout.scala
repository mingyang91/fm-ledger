package io.linewise.verify.fm.ledger.relational

import stainless.lang.*
import stainless.collection.*
import stainless.annotation.*
import io.linewise.verify.effect.FMLong

/* =============================================================================
 * RELATIONAL FM — Phase 3: the PAYOUT-LIFECYCLE slice as tables.
 *   PayoutWorld already kept intents/dispatches/events/reconciliations as flat rows, but bolted two
 *   AGGREGATES on (`withdrawals: List[Withdrawal]`, `ledger: List[LedgerTx]`). Here the payout state
 *   is pure tables: INTENT, DISPATCH (outbox, one row per intent), PROVIDER_EVENT (unique by
 *   provider+eventId), RECONCILIATION. PK distinctness + intra-slice referential integrity
 *   (dispatch/recon → intent) are proven; the dispatch outbox PK is intentId. The cross-slice FK
 *   (intent → withdrawal, event → withdrawal) lives in RelWorld, where both tables coexist — the
 *   graph-shaped invariant a tree-shaped aggregate cannot express.
 *
 *   Status enums mirror PayoutLifecycleProofs (the relational namespace owns its own tags rather
 *   than dragging the aggregate-bearing proofs file into the verify).
 * ========================================================================== */
object RelPayout {

  enum DispatchStatus       { case Pending, InFlight, Dispatched, Failed }
  enum ProviderOutcome      { case Settled, Failed }
  enum ReconciliationResult { case Matched, FeeVariance }

  // ---- TABLES (rows) ----
  case class IntentRow(
      id: BigInt, withdrawalId: BigInt, userUid: String, provider: String, destinationId: String,
      gross: FMLong, quotedFee: FMLong, expectedNet: FMLong) {
    require(gross.value >= BigInt(0) && quotedFee.value >= BigInt(0))
  }
  case class DispatchRow(
      intentId: BigInt, withdrawalId: BigInt, amountMinor: FMLong, idempotencyKey: String,
      status: DispatchStatus, attempts: BigInt, providerTransferRef: Option[String]) {
    require(attempts >= BigInt(0))
  }
  case class EventRow(provider: String, eventId: String, withdrawalId: BigInt, outcome: ProviderOutcome, observedFee: FMLong)
  case class ReconRow(intentId: BigInt, eventId: String, expectedFee: FMLong, observedFee: FMLong, result: ReconciliationResult)

  case class PayoutTables(
      intents:    List[IntentRow],
      dispatches: List[DispatchRow],
      events:     List[EventRow],
      recons:     List[ReconRow])

  // ---- id projections + distinctness / refIntegrity predicates (first-order recursive) ----
  def intentIds(is: List[IntentRow]): List[BigInt] = is.map(i => i.id)

  def noIntentWith(is: List[IntentRow], id: BigInt): Boolean = is match
    case Nil()      => true
    case Cons(i, t) => i.id != id && noIntentWith(t, id)
  def distinctIntents(is: List[IntentRow]): Boolean = is match
    case Nil()      => true
    case Cons(i, t) => noIntentWith(t, i.id) && distinctIntents(t)

  // outbox PK: one dispatch row per intent
  def noDispatchWith(ds: List[DispatchRow], iid: BigInt): Boolean = ds match
    case Nil()      => true
    case Cons(d, t) => d.intentId != iid && noDispatchWith(t, iid)
  def distinctDispatches(ds: List[DispatchRow]): Boolean = ds match
    case Nil()      => true
    case Cons(d, t) => noDispatchWith(t, d.intentId) && distinctDispatches(t)

  // PROVIDER_EVENT uniqueness by (provider, eventId) — the webhook idempotency key
  def noEventWith(es: List[EventRow], provider: String, eid: String): Boolean = es match
    case Nil()      => true
    case Cons(e, t) => !(e.provider == provider && e.eventId == eid) && noEventWith(t, provider, eid)
  def distinctEvents(es: List[EventRow]): Boolean = es match
    case Nil()      => true
    case Cons(e, t) => noEventWith(t, e.provider, e.eventId) && distinctEvents(t)

  // intra-slice referential integrity: every dispatch / recon points at an existing intent
  def dispatchRefOk(ds: List[DispatchRow], iids: List[BigInt]): Boolean = ds match
    case Nil()      => true
    case Cons(d, t) => iids.contains(d.intentId) && dispatchRefOk(t, iids)
  def reconRefOk(rs: List[ReconRow], iids: List[BigInt]): Boolean = rs match
    case Nil()      => true
    case Cons(r, t) => iids.contains(r.intentId) && reconRefOk(t, iids)

  def refIntegrity(w: PayoutTables): Boolean =
    dispatchRefOk(w.dispatches, intentIds(w.intents)) && reconRefOk(w.recons, intentIds(w.intents))
  def distinct(w: PayoutTables): Boolean =
    distinctIntents(w.intents) && distinctDispatches(w.dispatches) && distinctEvents(w.events)

  // ---- operations (each appends to one table) ----
  def submitIntent(w: PayoutTables, i: IntentRow): PayoutTables   = w.copy(intents = i :: w.intents)
  def recordDispatch(w: PayoutTables, d: DispatchRow): PayoutTables = w.copy(dispatches = d :: w.dispatches)
  def recordEvent(w: PayoutTables, e: EventRow): PayoutTables     = w.copy(events = e :: w.events)
  def reconcile(w: PayoutTables, r: ReconRow): PayoutTables       = w.copy(recons = r :: w.recons)

  // weakening: adding an intent id can only help the dispatch/recon FK checks
  @induct def dispatchRefOkConsId(ds: List[DispatchRow], iids: List[BigInt], y: BigInt): Unit = {
    require(dispatchRefOk(ds, iids)); ()
  }.ensuring(_ => dispatchRefOk(ds, y :: iids))
  @induct def reconRefOkConsId(rs: List[ReconRow], iids: List[BigInt], y: BigInt): Unit = {
    require(reconRefOk(rs, iids)); ()
  }.ensuring(_ => reconRefOk(rs, y :: iids))

  // ---- PROOF: submitting a FRESH intent preserves distinctness + refIntegrity ----
  def submitIntentPreservesDistinct(w: PayoutTables, i: IntentRow): Boolean = {
    require(distinct(w)); require(noIntentWith(w.intents, i.id))
    distinct(submitIntent(w, i))
  }.holds
  def submitIntentPreservesRefIntegrity(w: PayoutTables, i: IntentRow): Boolean = {
    require(refIntegrity(w))
    dispatchRefOkConsId(w.dispatches, intentIds(w.intents), i.id)
    reconRefOkConsId(w.recons, intentIds(w.intents), i.id)
    refIntegrity(submitIntent(w, i))
  }.holds

  // ---- PROOF: recording a dispatch for an EXISTING intent (fresh outbox key) preserves both ----
  def recordDispatchPreservesRefIntegrity(w: PayoutTables, d: DispatchRow): Boolean = {
    require(refIntegrity(w)); require(intentIds(w.intents).contains(d.intentId))
    refIntegrity(recordDispatch(w, d))
  }.holds
  def recordDispatchPreservesDistinct(w: PayoutTables, d: DispatchRow): Boolean = {
    require(distinct(w)); require(noDispatchWith(w.dispatches, d.intentId))
    distinct(recordDispatch(w, d))
  }.holds

  // ---- PROOF: a fresh provider event preserves uniqueness (webhook idempotency) ----
  def recordEventPreservesDistinct(w: PayoutTables, e: EventRow): Boolean = {
    require(distinct(w)); require(noEventWith(w.events, e.provider, e.eventId))
    distinct(recordEvent(w, e))
  }.holds
}
