package dev.mingyang91.verify.effect

import stainless.lang._

/** =============================================================================
  * VERIFIED safeAddI PRIMITIVE.
  *
  * This helper uses Scala/Stainless Int directly. It returns an ADT value when
  * adding two Ints would overflow instead of relying on a failed precondition.
  * ============================================================================= */

/** The arithmetic-error ADT. The same shape is mirrored on the production side. */
sealed trait ArithError
object ArithError {
  case object Overflow extends ArithError
}

object SafeArith {

  /** True iff `a + b` fits in the machine-Int range. */
  def intAdditionFits(a: Int, b: Int): Boolean =
    !((b > 0 && a > Int.MaxValue - b) || (b < 0 && a < Int.MinValue - b))

  extension (a: Int)
    /** Int variant — returns Overflow exactly when the sum would not fit. */
    def safeAddI(b: Int): Either[ArithError, Int] = {
      if b > 0 && a > Int.MaxValue - b then Left[ArithError, Int](ArithError.Overflow)
      else if b < 0 && a < Int.MinValue - b then Left[ArithError, Int](ArithError.Overflow)
      else Right[ArithError, Int](a + b)
    }

  /** The Int variant carries the same faithful-witness guarantee. */
  def safeAddIRightIffInRange(a: Int, b: Int): Boolean = {
    a.safeAddI(b).isRight == intAdditionFits(a, b)
  }.holds
}
