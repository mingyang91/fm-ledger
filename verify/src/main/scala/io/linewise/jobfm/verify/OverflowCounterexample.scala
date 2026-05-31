package io.linewise.verify.fm.jobsystem

import stainless.lang._
import io.linewise.verify.effect.{FMInt, FMLong}

/** =============================================================================
  * TIER-CONNECTING COUNTEREXAMPLE — proof that Tier 1 HAS TEETH.
  *
  * This file is the deliberately-INVALID companion to the FMInt overflow VC. It
  * is EXCLUDED from the default `./mill verify.scala` run (like
  * EvolutionConflict.scala) so the headline run reports 0 invalid. To see it
  * fail, pass it explicitly alongside FMTypes:
  *
  *   ./mill verify.scala \
  *     verify/stainless-lib/FMTypes.scala \
  *     verify/src/main/scala/io/linewise/jobfm/verify/OverflowCounterexample.scala
  *
  * WHAT IT SHOWS. In JobModel the `+` on `attempts` is SAFE because the JobState
  * invariant bounds attempts to [0, maxAttempts=5], so `attempts + 1 <= 6` is
  * provably in the Int range and the `+` precondition is DISCHARGED — that bound
  * is exactly why Tier 1 may erase FMInt to a native Int with no runtime check.
  *
  * Here, by contrast, two UNCONSTRAINED FMInts are added with NO guard. Each can
  * be as large as INT_MAX, so the true sum can be up to 2*INT_MAX, which exceeds
  * the Int range. Stainless cannot discharge FMInt.`+`'s precondition
  *   require(value + other.value <= INT_MAX)
  * and reports the VC INVALID with a counterexample (e.g. a = b = INT_MAX).
  *
  * This is the meaning of "Tier 1 has teeth": if production's native `+` could
  * overflow, the proof catches it at the verified source. It ALSO marks exactly
  * where you must switch to Tier 2: the fix is NOT to add a require here (the
  * inputs are genuinely unbounded) but to use the Tier-2 `safeAdd`, which turns
  * the overflow into a handled Left(Overflow) VALUE.
  * ============================================================================= */
object OverflowCounterexample {

  // INVALID: unguarded FMInt `+`. No invariant bounds a or b, so the solver
  // finds a = b = INT_MAX (sum = 2*INT_MAX > INT_MAX) violating the `+`
  // precondition. This VC must report INVALID.
  def unguardedAddOverflows(a: FMInt, b: FMInt): FMInt = {
    a + b
  }

  // INVALID variant for FMLong: two unconstrained FMLongs, each up to LONG_MAX,
  // summed without a guard -> the FMLong constructor's range invariant cannot be
  // discharged. (FMLong has no `+`, so we go through the wrapper directly to
  // exercise the same overflow shape at the Long width.)
  def unguardedLongAddOverflows(a: FMLong, b: FMLong): FMLong = {
    FMLong(a.value + b.value)
  }
}
