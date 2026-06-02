package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import LedgerCore._
import LedgerProofs._

/* =============================================================================
 * CONSERVATION / APPEND-ONLY CONFLICTS — the deliberately-INVALID VCs.
 *
 * This file exists ONLY to show the propositions have TEETH: a definition that
 * drops a guard, or a v2 requirement that contradicts an established v1 one, is
 * machine-detected as INVALID with a counterexample. It is EXCLUDED from the
 * headline `./mill verify.scala` run (the runner filters it out) so the headline
 * reports 0 invalid; run it explicitly to see the conflicts:
 *
 *   ./mill verify.scala \
 *     verify/stainless-lib/FMTypes.scala \
 *     verify/stainless-lib/SafeArith.scala \
 *     verify/src/main/scala/io/linewise/ledgerfm/verify/LedgerModel.scala \
 *     verify/src/main/scala/io/linewise/ledgerfm/verify/LedgerCore.scala \
 *     verify/src/main/scala/io/linewise/ledgerfm/verify/LedgerProofs.scala \
 *     verify/src/main/scala/io/linewise/ledgerfm/verify/ConservationConflict.scala
 *
 * TWO conflicts are surfaced.
 * ========================================================================== */
object ConservationConflict {

  /* --- CONFLICT (1): DROPPING THE BALANCE GUARD BREAKS CONSERVATION ----------
   * `postUnchecked` prepends ANY transaction, skipping the admissible guard
   * (freshness + at-least-two + balanced). Claiming it preserves the invariant is
   * FALSE: an unbalanced (or duplicate) transaction breaks forall(txWellFormed)
   * (or distinctTxIds). Stainless emits the counterexample. This is the
   * conservation analog of OverflowCounterexample — it proves the balance check in
   * `post` is load-bearing, not decorative. */
  def postUnchecked(l: Ledger, tx: Tx): Ledger =
    Ledger(tx :: l.txs, l.total)

  def uncheckedPostPreservesInv(l: Ledger, tx: Tx): Boolean = {
    require(ledgerInv(l))
    // FALSE: postUnchecked admits an unbalanced/duplicate tx, breaking the invariant.
    ledgerInv(postUnchecked(l, tx))
  }.holds

  /* --- CONFLICT (2): OVERWRITE-IN-PLACE vs APPEND-ONLY (requirement evolution) -
   * v1 REQUIREMENT (established, valid — LedgerProofs.postNeverMutatesHistory):
   *   "the journal is append-only: after a post the previous journal is preserved,
   *    either unchanged (rejection) or exactly the tail of the new journal (accept)."
   *
   * v2 REQUIREMENT (newly proposed, CONTRADICTORY):
   *   "allow correction in place — posting a transaction whose id already exists
   *    REPLACES the old entry."  `postOrReplace` drops any existing same-id entry
   *    and prepends the new one.
   *
   * These cannot both hold. For a ledger that already contains tx.id, postOrReplace
   * removes the old entry, so the previous journal is NOT preserved — asserting the
   * v1 append-only postcondition for postOrReplace is FALSE, and Stainless finds the
   * counterexample (a ledger containing tx.id). The resolution is a product decision
   * — a correction is a NEW reversing entry, never an overwrite — not a proof tweak.
   * This is the requirement-evolution conflict the vision asks for, surfaced at the
   * ledger op level. */
  def postOrReplace(l: Ledger, tx: Tx): Ledger =
    Ledger(tx :: l.txs.filter((t: Tx) => t.id != tx.id), l.total)

  def overwriteViolatesAppendOnly(l: Ledger, tx: Tx): Boolean = {
    require(ledgerInv(l))
    val after = postOrReplace(l, tx)
    // v1 append-only: the previous journal is preserved (unchanged or as the tail).
    // FALSE for a ledger that already contains tx.id — the old entry is dropped.
    (after.txs == l.txs) || (!after.txs.isEmpty && after.txs.tail == l.txs)
  }.holds
}
