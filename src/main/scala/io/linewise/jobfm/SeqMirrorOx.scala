package io.linewise.jobfm

import ox.*

/* =============================================================================
 * PRODUCTION Ox REALIZATION of the verified sequential-mirror combinators.
 *
 * Same signatures as the generated io.linewise.jobfm.generated.SeqMirror (the
 * SEQUENTIAL SPEC); these run the real Ox `fork`/`join`. The verified side
 * reasons about `SeqMirror.mapPar`/`par` sequentially (SeqMirror.scala +
 * SeqMirrorProofs.scala); production runs them concurrently here. The two agree
 * for independent work (mapPar over disjoint elements; par over disjoint state),
 * which is the soundness tier the proofs establish.
 * ========================================================================== */
object SeqMirrorOx:

  /** Parallel map — one fork per element, joined in order. Result is order-equal
    * to the sequential `xs.map(f)` (the verified SeqMirror.mapPar). */
  def mapPar[A, B](xs: List[A], f: A => B): List[B] =
    supervised:
      xs.map(x => fork(f(x))).map(_.join())

  /** Fork two independent thunks and join — the Ox realization of the verified
    * SeqMirror.par. Same shape as the claim-race fork/join in main.scala. */
  def par[A, B](a: () => A, b: () => B): (A, B) =
    supervised:
      val fa = fork(a())
      val fb = fork(b())
      (fa.join(), fb.join())
