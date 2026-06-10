package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import LedgerInvariants._

/* =============================================================================
 * PAYOUT PROOFS — VERIFY-ONLY. The payout-lifecycle slice on the relational DB (INTENT / DISPATCH /
 * PROVIDER_EVENT / RECONCILIATION). The lifecycle is verify-only (production payout is the hand-
 * written PayoutDispatcher), so these are DB ops plus the structural invariant proofs: PK
 * distinctness, intra-slice referential integrity (dispatch/recon -> intent), provider-event
 * uniqueness, and the CROSS-SLICE FK (every intent references a live withdrawal — the graph-shaped
 * invariant a tree-shaped aggregate cannot express).
 * ========================================================================== */
object PayoutTableProofs {

  // ---- ops (proof-only model of the payout writes) ----
  def submitIntent(db: DB, i: IntentRow): DB   = db.copy(intents = i :: db.intents)
  def recordDispatch(db: DB, d: DispatchRow): DB = db.copy(dispatches = d :: db.dispatches)
  def recordEvent(db: DB, e: EventRow): DB     = db.copy(events = e :: db.events)
  def reconcile(db: DB, r: ReconRow): DB       = db.copy(recons = r :: db.recons)

  // ---- weakening lemmas: adding an intent / withdrawal can only help the FK checks ----
  @induct def dispatchRefOkConsI(ds: List[DispatchRow], is: List[IntentRow], i: IntentRow): Unit = {
    require(dispatchRefOk(ds, is)); ()
  }.ensuring(_ => dispatchRefOk(ds, i :: is))
  @induct def reconRefOkConsI(rs: List[ReconRow], is: List[IntentRow], i: IntentRow): Unit = {
    require(reconRefOk(rs, is)); ()
  }.ensuring(_ => reconRefOk(rs, i :: is))
  @induct def intentsRefWithdrawalConsW(is: List[IntentRow], ws: List[WithdrawalRow], w: WithdrawalRow): Unit = {
    require(allIntentsRefWithdrawal(is, ws)); ()
  }.ensuring(_ => allIntentsRefWithdrawal(is, w :: ws))

  // ---- submitting a fresh intent for a live withdrawal preserves payout validity + the cross-FK ----
  def submitIntentPreservesValidPayout(db: DB, i: IntentRow): Unit = {
    require(validPayout(db))
    require(noIntentWith(db.intents, i.id))
    dispatchRefOkConsI(db.dispatches, db.intents, i)
    reconRefOkConsI(db.recons, db.intents, i)
  }.ensuring(_ => validPayout(submitIntent(db, i)))

  def submitIntentPreservesCrossFk(db: DB, i: IntentRow): Unit = {
    require(allIntentsRefWithdrawal(db.intents, db.withdrawals))
    require(hasWithdrawal(db.withdrawals, i.withdrawalId))
  }.ensuring(_ => allIntentsRefWithdrawal(submitIntent(db, i).intents, db.withdrawals))

  // ---- recording a dispatch / recon for an existing intent preserves referential integrity ----
  def recordDispatchPreservesValidPayout(db: DB, d: DispatchRow): Unit = {
    require(validPayout(db)); require(hasIntent(db.intents, d.intentId))
  }.ensuring(_ => validPayout(recordDispatch(db, d)))
  def reconcilePreservesValidPayout(db: DB, r: ReconRow): Unit = {
    require(validPayout(db)); require(hasIntent(db.intents, r.intentId))
  }.ensuring(_ => validPayout(reconcile(db, r)))

  // ---- a fresh provider event preserves uniqueness (webhook idempotency) ----
  def recordEventPreservesValidPayout(db: DB, e: EventRow): Unit = {
    require(validPayout(db)); require(noEventWith(db.events, e.provider, e.eventId))
  }.ensuring(_ => validPayout(recordEvent(db, e)))
}
