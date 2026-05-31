package io.linewise.verify.fm.jobsystem

import stainless.lang._
import io.linewise.verify.effect.{FMLong, FMInt, ArithError, LONG_MAX}
import io.linewise.verify.effect.SafeArith._

/** =============================================================================
  * TIER 2 DEMO — overflow-as-handled-VALUE, confined to the add site.
  *
  * This is a STANDALONE example. It is deliberately NOT wired into the job
  * transition (the job system's arithmetic is Tier 1: bounded with a discharged
  * no-overflow proof). The point here is to show the Tier-2 shape working end to
  * end: an unbounded computation composed from `safeAdd`, where overflow is a
  * reachable, handled `Left(Overflow)` rather than a thrown exception or a
  * silent wrap. The proofs below show BOTH branches are real:
  *   - on inputs whose true sum exceeds the machine range, sumThree returns Left;
  *   - on small inputs, sumThree returns Right carrying the exact sum.
  * ============================================================================= */
object SafeArithDemo {

  /** Sum three FMLongs by composing two safeAdds. The first overflow short-
    * circuits to Left(Overflow); otherwise the partial sum feeds the next add,
    * which may itself overflow. Overflow is confined to the add sites — the
    * error VALUE flows out, nothing throws. */
  def sumThree(a: FMLong, b: FMLong, c: FMLong): Either[ArithError, FMLong] = {
    a.safeAdd(b) match
      case Left(e)   => Left[ArithError, FMLong](e)
      case Right(ab) => ab.safeAdd(c)
  }

  /* ---------------------------------------------------------------------------
   * PROOF: the Left branch is REACHABLE and HANDLED.
   *
   * Witness inputs whose true sum is out of range. a = b = c = FMLong(LONG_MAX):
   * a + b already exceeds LONG_MAX, so the FIRST safeAdd returns Left(Overflow),
   * and sumThree propagates it. The Left is therefore not dead code.
   * ------------------------------------------------------------------------- */
  def sumThreeOverflowsToLeft: Boolean = {
    val big = FMLong(LONG_MAX)
    sumThree(big, big, big) == Left[ArithError, FMLong](ArithError.Overflow)
  }.holds

  /* ---------------------------------------------------------------------------
   * PROOF: on in-range inputs, sumThree returns Right with the exact sum.
   *
   * Witness small constants 1 + 2 + 3 = 6, comfortably in range, so neither
   * intermediate add overflows and the result is Right(FMLong(6)).
   * ------------------------------------------------------------------------- */
  def sumThreeInRangeToRight: Boolean = {
    val r = sumThree(FMLong(BigInt(1)), FMLong(BigInt(2)), FMLong(BigInt(3)))
    r == Right[ArithError, FMLong](FMLong(BigInt(6)))
  }.holds

  /* ---------------------------------------------------------------------------
   * PROOF (general): whenever sumThree returns Right(s), the wrapped value is
   * the true three-way BigInt sum — composition does not corrupt the result.
   * This ties the confined add-site errors back to a faithful overall spec.
   * ------------------------------------------------------------------------- */
  def sumThreeRightIsExactSum(a: FMLong, b: FMLong, c: FMLong): Boolean = {
    sumThree(a, b, c) match
      case Right(s) => s.value == a.value + b.value + c.value
      case Left(_)  => true // vacuously: the Left branch is covered elsewhere
  }.holds
}
