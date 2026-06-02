package io.linewise.verify.ox

import stainless.lang._
import stainless.annotation._

/* =============================================================================
 * SHADOW MODEL of Ox (com.softwaremill.ox) structured concurrency — a
 * VERIFICATION-ONLY library, analogous to IO.scala / EitherT.scala.
 *
 * The names here are IDENTICAL to Ox's (`supervised`, `fork`, `forkUser`,
 * `Fork`, `.join()`, `par`), so verified business code reads exactly like the
 * production Ox code. The transpiler's only job for concurrency is to swap the
 * import `io.linewise.verify.ox.*` -> `ox.*`; the call sites need no rewriting.
 * This file is NEVER transpiled (it is the verification substitute that the real
 * Ox library replaces in production, just like IO -> cats-effect).
 *
 * SEMANTICS IN THE MODEL (the soundness contract):
 *   - `supervised { body }` is the IDENTITY scope: it runs `body` directly.
 *   - `fork { e }` / `forkUser { e }` evaluate `e` EAGERLY, at the fork site, in
 *     SOURCE ORDER, and hand back a `Fork` already holding the value.
 *   - `Fork#join()` returns that already-computed value.
 *   - `par(a, b)` evaluates `a` then `b` (source order) and tuples them.
 *
 * So the model collapses concurrency to eager, source-order, sequential
 * evaluation. Stainless therefore proves: "IF every fork runs atomically AND in
 * program order, THEN the result is X." It explores NO interleavings.
 *
 * The split of responsibility (the user owns concurrency safety):
 *   - the RUNTIME guarantees each forked step is atomic;
 *   - the PROOF AUTHOR must only assert properties that are INVARIANT under
 *     reordering/interleaving of the forks (e.g. "exactly one claimer wins", not
 *     "worker 21 wins"). Atomicity alone does not pin down order, so an
 *     order-DEPENDENT property proven here would NOT hold in the real concurrent
 *     run. See SeqMirrorProofs.raceExactlyOneWinner for the reorder-invariant
 *     style this model is sound for.
 *
 * `inline` is load-bearing: it lets Scala-3 beta-reduce the thunk away before
 * Stainless's AntiAliasing phase, so a forked body that captures a mutable
 * object becomes a direct call-site effect rather than an illegal mutable
 * capture. (A non-inline wrapper, or Stainless's own @inline, is rejected at
 * AntiAliasing — @inline fires too late.)
 *
 * NOT MODELLED HERE (deliberately): `race`/`raceResult`/`timeout`. Those DISCARD
 * results based on timing, so a sound model needs genuine nondeterministic
 * choice, not eager source-order pick — an eager placeholder would let you prove
 * "the first branch always wins," which is exactly the unsound thing. Deferred.
 * ========================================================================== */

/** A completed fork handle. In production this is Ox's `Fork[T]`; in the model
  * it eagerly holds the already-computed value. */
case class Fork[T](value: T) {
  def join(): T = value
}

/** Structured-concurrency scope. In production: Ox `supervised`. In the model:
  * the identity scope — run the body directly. */
inline def supervised[T](inline body: => T): T = body

/** Start a daemon fork. In production: Ox `fork` (needs `using Ox`, provided by
  * the enclosing `supervised`). In the model: evaluate eagerly, wrap the value. */
inline def fork[T](inline f: => T): Fork[T] = Fork(f)

/** Start a user fork (keeps the scope alive until it completes). In production:
  * Ox `forkUser`. In the model: same eager evaluation as `fork`. */
inline def forkUser[T](inline f: => T): Fork[T] = Fork(f)

/** Run two computations "in parallel" and tuple their results. In production:
  * Ox `par`. In the model: evaluate in source order and tuple. */
inline def par[A, B](inline a: => A, inline b: => B): (A, B) = (a, b)

/** Three-way `par`. */
inline def par[A, B, C](inline a: => A, inline b: => B, inline c: => C): (A, B, C) =
  (a, b, c)
