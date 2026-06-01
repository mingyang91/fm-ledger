package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._

/* =============================================================================
 * SEQUENTIAL MIRROR of Ox structured-concurrency combinators — a TRANSPILER
 * INPUT (executable, proof-free).
 *
 * The verified worker model expresses its parallel control flow with these
 * combinators, given a PURE SEQUENTIAL semantics so Stainless can reason about
 * it. The production side (src/SeqMirrorOx.scala, hand-written) provides the SAME
 * signatures backed by real Ox `fork`/`join`; the differential test binds the two.
 *
 * SOUNDNESS (two tiers, see SeqMirrorProofs.scala for the tracked gap):
 *   - mapPar (parallel `.map`): the sequential mirror `xs.map(f)` is OBSERVABLY
 *     EQUAL to Ox `mapPar` when the per-element work is independent (no shared
 *     mutable state) — pure `f`, row-local. Fully faithful, fully verified.
 *   - par (fork/join of two thunks): the sequential mirror `(a(), b())` is exact
 *     for INDEPENDENT thunks. For a shared-resource race (the claim race), the
 *     mirror is one interleaving; production correctness then rests on the DB
 *     serializing to SOME order (TRUSTED) and every order being safe (VERIFIED in
 *     SeqMirrorProofs.raceExactlyOneWinner).
 * ========================================================================== */
object SeqMirror {

  /* Parallel map. Ox `xs.mapPar(f)` in production; sequentially just `xs.map(f)`. */
  def mapPar[A, B](xs: List[A], f: A => B): List[B] = xs.map(f)

  /* Fork two independent thunks and join. Ox `fork`/`join` in production;
   * sequentially evaluate both and tuple. */
  def par[A, B](a: () => A, b: () => B): (A, B) = (a(), b())
}
