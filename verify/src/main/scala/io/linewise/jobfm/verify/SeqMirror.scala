package io.linewise.verify.fm.jobsystem

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.ox._

/* =============================================================================
 * MIRROR of Ox structured-concurrency combinators — a TRANSPILER INPUT
 * (executable, proof-free).
 *
 * These combinators are written in terms of the SHADOW Ox primitives
 * (io.linewise.verify.ox: `supervised`/`fork`/`join`), whose names match the
 * real Ox API one-for-one. Under verification the shadow gives them a PURE,
 * EAGER, SOURCE-ORDER semantics so Stainless can reason about them; the
 * transpiler swaps the shadow import for `import ox.*`, so the SAME source
 * generates the production `io.linewise.jobfm.generated.SeqMirror` running real
 * Ox `fork`/`join`. There is no hand-written Ox realization anymore — the
 * generated code IS the production realization.
 *
 * SOUNDNESS (two tiers, see SeqMirrorProofs.scala for the tracked gap):
 *   - mapPar (one fork per element): the eager mirror reduces to `xs.map(f)`,
 *     OBSERVABLY EQUAL to the Ox parallel map when the per-element work is
 *     independent (pure, row-local f). Fully faithful, fully verified.
 *   - par (fork/join of two thunks): the eager mirror reduces to `(a(), b())`,
 *     exact for INDEPENDENT thunks. For a shared-resource race (the claim race),
 *     the mirror is one interleaving; production correctness then rests on the DB
 *     serializing to SOME order (TRUSTED) and every order being safe (VERIFIED in
 *     SeqMirrorProofs.raceExactlyOneWinner).
 * ========================================================================== */
object SeqMirror {

  /* Parallel map — one fork per element, joined in order. Ox `fork`/`join` in
   * production; eagerly the forks reduce away and this is `xs.map(f)`. */
  def mapPar[A, B](xs: List[A], f: A => B): List[B] =
    supervised {
      xs.map(x => fork(f(x))).map(_.join())
    }

  /* Fork two independent thunks and join. Ox `fork`/`join` in production;
   * eagerly evaluate both and tuple. */
  def par[A, B](a: () => A, b: () => B): (A, B) =
    supervised {
      val fa = fork(a())
      val fb = fork(b())
      (fa.join(), fb.join())
    }
}
