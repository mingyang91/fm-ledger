package io.linewise.verify.fm.ledger.relational

import stainless.lang.*
import stainless.collection.*
import stainless.annotation.*
import io.linewise.verify.fm.ledger.LedgerModel.WithdrawalStatus

/* =============================================================================
 * RELATIONAL FM — the unified WORLD: ALL fm state as one set of tables (no aggregates), with a
 * whole-world validity invariant conjoining every table's structural guarantee AND the cross-table
 * foreign keys that span slices. Top-level operations preserve `valid` by reducing to the per-slice
 * lemmas — and because an op that touches one slice leaves the other slices' fields untouched.
 *
 * This replaces both aggregate worlds at once: the core `World(ledgerRepo, withdrawalRepo,
 * proposalRepo, obligationRepo)` and `PayoutWorld(withdrawals, intents, dispatches, events,
 * reconciliations, ledger)`. The two embedded aggregates PayoutWorld carried (`List[Withdrawal]`,
 * `List[LedgerTx]`) are gone — withdrawals and the ledger are now the same tables the rest of the
 * World uses, and the payout intents/events reference them by the cross-slice FK below. That FK is
 * the whole point: a withdrawal's payout intent lives in a DIFFERENT table than the withdrawal, so
 * the integrity constraint is graph-shaped — unrepresentable as a tree-shaped aggregate.
 * ========================================================================== */
object RelWorld {

  case class W(
      // ledger slice
      txHeaders:   List[RelLedger.TxHeaderRow],
      legs:        List[RelLedger.LegRow],
      // withdrawal slice
      withdrawals: List[RelWithdrawal.WithdrawalRow],
      wstatuses:   List[RelWithdrawal.WStatusRow],
      // proposal slice
      proposals:   List[RelProposal.ProposalRow],
      pstatuses:   List[RelProposal.PStatusRow],
      // obligation slice
      obligations: List[RelObligation.ObligationRow],
      // payout-lifecycle slice
      intents:     List[RelPayout.IntentRow],
      dispatches:  List[RelPayout.DispatchRow],
      events:      List[RelPayout.EventRow],
      recons:      List[RelPayout.ReconRow])

  def ledgerOf(w: W): RelLedger.LedgerTables = RelLedger.LedgerTables(w.txHeaders, w.legs)
  def wdOf(w: W): RelWithdrawal.WithdrawalTables = RelWithdrawal.WithdrawalTables(w.withdrawals, w.wstatuses)
  def propOf(w: W): RelProposal.ProposalTables = RelProposal.ProposalTables(w.proposals, w.pstatuses)
  def oblOf(w: W): RelObligation.ObligationTables = RelObligation.ObligationTables(w.obligations)
  def payOf(w: W): RelPayout.PayoutTables = RelPayout.PayoutTables(w.intents, w.dispatches, w.events, w.recons)

  // ---- the CROSS-SLICE foreign key: every payout intent references an existing withdrawal ----
  def allIntentsRefWithdrawal(is: List[RelPayout.IntentRow], wids: List[BigInt]): Boolean = is match
    case Nil()      => true
    case Cons(i, t) => wids.contains(i.withdrawalId) && allIntentsRefWithdrawal(t, wids)
  // weakening: more withdrawal ids can only help the FK
  @induct def allIntentsRefWithdrawalConsId(is: List[RelPayout.IntentRow], wids: List[BigInt], y: BigInt): Unit = {
    require(allIntentsRefWithdrawal(is, wids)); ()
  }.ensuring(_ => allIntentsRefWithdrawal(is, y :: wids))

  // the whole-world invariant: every table's structural guarantee + the cross-slice FK.
  def valid(w: W): Boolean =
    // ledger
    RelLedger.refIntegrity(ledgerOf(w)) && RelLedger.distinctHeaders(ledgerOf(w)) && RelLedger.conservation(ledgerOf(w)) &&
      // withdrawal
      RelWithdrawal.refIntegrity(wdOf(w)) && RelWithdrawal.distinctWithdrawals(wdOf(w)) &&
      // proposal
      RelProposal.refIntegrity(propOf(w)) && RelProposal.distinctProposals(propOf(w)) &&
      // obligation
      RelObligation.distinct(oblOf(w)) &&
      // payout (intra-slice)
      RelPayout.refIntegrity(payOf(w)) && RelPayout.distinct(payOf(w)) &&
      // cross-slice FK: payout intent → withdrawal
      allIntentsRefWithdrawal(w.intents, RelWithdrawal.withdrawalIds(w.withdrawals))

  // ---- top-level operations (each touches one slice; the rest of the World is untouched) ----
  def postTx(w: W, h: RelLedger.TxHeaderRow, newLegs: List[RelLedger.LegRow]): W =
    w.copy(txHeaders = h :: w.txHeaders, legs = newLegs ++ w.legs)
  def addWithdrawal(w: W, wr: RelWithdrawal.WithdrawalRow, init: WithdrawalStatus): W =
    w.copy(withdrawals = wr :: w.withdrawals, wstatuses = RelWithdrawal.WStatusRow(wr.id, init) :: w.wstatuses)
  def submitIntent(w: W, i: RelPayout.IntentRow): W =
    w.copy(intents = i :: w.intents)

  // ---- PROOF: posting a ledger tx preserves the WHOLE-WORLD invariant ----
  def postTxPreservesValid(w: W, h: RelLedger.TxHeaderRow, newLegs: List[RelLedger.LegRow]): Boolean = {
    require(valid(w))
    require(RelLedger.allSameTx(newLegs, h.id))
    require(RelLedger.admissible(newLegs))
    require(RelLedger.noHeaderWith(w.txHeaders, h.id))
    require(RelLedger.noLegWith(w.legs, h.id))
    val l0 = ledgerOf(w)
    RelLedger.postPreservesRefIntegrity(l0, h, newLegs)
    RelLedger.postPreservesDistinct(l0, h, newLegs)
    RelLedger.postPreservesConservation(l0, h, newLegs)
    valid(postTx(w, h, newLegs))
  }.holds

  // ---- PROOF: requesting a (fresh) withdrawal preserves the WHOLE-WORLD invariant ----
  //   note the cross-slice obligation: growing the withdrawal-id set must preserve every intent's FK.
  def addWithdrawalPreservesValid(w: W, wr: RelWithdrawal.WithdrawalRow, init: WithdrawalStatus): Boolean = {
    require(valid(w))
    require(RelWithdrawal.noWithdrawalWith(w.withdrawals, wr.id))
    val d0 = wdOf(w)
    RelWithdrawal.addWithdrawalPreservesRefIntegrity(d0, wr, init)
    RelWithdrawal.addWithdrawalPreservesDistinct(d0, wr, init)
    allIntentsRefWithdrawalConsId(w.intents, RelWithdrawal.withdrawalIds(w.withdrawals), wr.id)
    valid(addWithdrawal(w, wr, init))
  }.holds

  // ---- PROOF: submitting a payout intent for an EXISTING withdrawal preserves the WHOLE-WORLD
  //   invariant — including the cross-slice FK (the new intent points at a live withdrawal). ----
  def submitIntentPreservesValid(w: W, i: RelPayout.IntentRow): Boolean = {
    require(valid(w))
    require(RelPayout.noIntentWith(w.intents, i.id))
    require(RelWithdrawal.withdrawalIds(w.withdrawals).contains(i.withdrawalId))
    val p0 = payOf(w)
    RelPayout.submitIntentPreservesDistinct(p0, i)
    RelPayout.submitIntentPreservesRefIntegrity(p0, i)
    valid(submitIntent(w, i))
  }.holds
}
