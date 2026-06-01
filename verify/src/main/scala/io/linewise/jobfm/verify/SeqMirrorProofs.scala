package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import JobModel._
import StoreCore._
import WorkerProofs._

/* =============================================================================
 * SEQ-MIRROR PROOFS — VERIFY-ONLY. The propositions that justify mirroring Ox
 * concurrency with the sequential SeqMirror combinators.
 *
 * TWO TIERS OF SOUNDNESS (the explicit, tracked trust boundary):
 *
 *   (1) INDEPENDENT parallelism — the mirror is EXACT and fully verified.
 *       mapPar(xs, f) == xs.map(f) by definition; for row-local pure f the
 *       parallel and sequential results coincide, so the verified `xs.map(f)`
 *       IS the production `xs.mapPar(f)`. par(a,b) == (a(), b()) likewise.
 *
 *   (2) SHARED-RESOURCE RACE — the claim race on ONE row. Here the sequential
 *       mirror is a single interleaving, so it is NOT a free mirror. Production
 *       correctness rests on:
 *         - TRUSTED: the DB `FOR UPDATE SKIP LOCKED` serializes the two racing
 *           claim transactions to SOME total order (inexpressible in this pure
 *           model — the same trusted boundary as store.scala's concurrency).
 *         - VERIFIED (here): EVERY serial order is safe — `raceExactlyOneWinner`
 *           proves both A-then-B and B-then-A end with the row RUNNING owned by
 *           EXACTLY ONE worker (the first in that order), the loser a no-op.
 *       So whichever order SKIP-LOCKED picks, the outcome is correct. The GAP is
 *       precisely "which order is picked" (trusted); "all orders safe" (verified).
 *
 * The race proof reuses R4 (WorkerProofs.r4_claimAdvancesTarget +
 * r4_secondClaimDeniedOnLiveLease) — it does not redesign claim safety.
 * ========================================================================== */
object SeqMirrorProofs {

  /* --- TIER 1: the mirror equivalences (trivial, by definition). --- */

  def mapParIsMap[A, B](xs: List[A], f: A => B): Boolean = {
    SeqMirror.mapPar(xs, f) == xs.map(f)
  }.holds

  def parIsTuple[A, B](a: () => A, b: () => B): Boolean = {
    SeqMirror.par(a, b) == (a(), b())
  }.holds

  /* --- TIER 2: the claim race on one row id, both serial orders. --- */

  // The targeted row after worker `first` claims, then worker `second` tries
  // (both with a LIVE lease, stale=false) — the serialized two-claim sequence.
  def raceRow(r: JobRow, id: FMLong, first: FMLong, second: FMLong): JobRow =
    claimRow(id, second, false)(claimRow(id, first, false)(r))

  // EXACTLY-ONE-WINNER, ORDER-INDEPENDENT. For a PENDING row and a != b, BOTH
  // serial orders end RUNNING owned by the FIRST claimer, with the second denied.
  // SKIP-LOCKED picks one order (trusted); this proves every order is safe.
  def raceExactlyOneWinner(r: JobRow, id: FMLong, a: FMLong, b: FMLong): Boolean = {
    require(r.id == id && r.st.status == PENDING && a != b)

    // order A-then-B
    val rA = claimRow(id, a, false)(r)
    r4_claimAdvancesTarget(r, id, a, false)        // rA RUNNING, owned a, id==id
    r4_secondClaimDeniedOnLiveLease(rA, id, a, b)  // claimRow(id,b,false)(rA) == rA

    // order B-then-A
    val rB = claimRow(id, b, false)(r)
    r4_claimAdvancesTarget(r, id, b, false)        // rB RUNNING, owned b, id==id
    r4_secondClaimDeniedOnLiveLease(rB, id, b, a)  // claimRow(id,a,false)(rB) == rB

    val ab = raceRow(r, id, a, b)  // == claimRow(id,b,false)(rA)
    val ba = raceRow(r, id, b, a)  // == claimRow(id,a,false)(rB)
    ab.st.status == RUNNING && isOwner(ab.st, a) && !isOwner(ab.st, b) &&
    ba.st.status == RUNNING && isOwner(ba.st, b) && !isOwner(ba.st, a)
  }.holds
}
