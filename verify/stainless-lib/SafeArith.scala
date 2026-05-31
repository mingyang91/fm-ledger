package io.linewise.verify.effect

import stainless.lang._

/** =============================================================================
  * TIER 2 — VERIFIED safeAdd PRIMITIVE (the verify side of the trusted pair).
  *
  * Tier 1 (FMInt/FMLong in JobModel) handles arithmetic whose no-overflow bound
  * is PROVABLE from the surrounding invariants, so the `+` precondition is
  * discharged and the value erases to a native machine int with no runtime cost.
  *
  * Tier 2 is for arithmetic where overflow is GENUINELY possible — the inputs
  * are unbounded, so no static bound discharges the `+` precondition. Instead of
  * a proof obligation, overflow becomes a HANDLED ADT-error VALUE confined to the
  * add site: `safeAdd` returns `Either[ArithError, _]`, never throwing and never
  * silently wrapping.
  *
  * TRUSTED-EQUIVALENCE CONTRACT (read together with the production side at
  * src/main/scala/io/linewise/jobfm/SafeArith.scala):
  *   - THIS verify side is the SPEC. It computes the sum in unbounded BigInt
  *     (which cannot overflow), checks the result against the machine range, and
  *     returns Right(wrapped) when in range or Left(Overflow) when out of range.
  *     The spec lemmas below PROVE the Either faithfully reflects overflow:
  *     Right(_) IFF the true sum is in range, Left(Overflow) otherwise.
  *   - The PRODUCTION side computes `Math.addExact(a, b)` and turns the JVM's
  *     ArithmeticException into Left(Overflow). `Math.addExact` returns the exact
  *     sum iff it fits in the machine type and throws otherwise — which is the
  *     SAME boundary the BigInt range check encodes here. The two are therefore
  *     trusted-equivalent: same spec (Right in range / Left(Overflow) out of
  *     range), different implementations (BigInt range check vs Math.addExact).
  *   - The transpiler does NOT translate safeAdd's body (the impls differ on
  *     purpose); only CALL-SITES pass through, resolving against the hand-written
  *     production safeAdd.
  * ============================================================================= */

/** The arithmetic-error ADT. The same shape is mirrored on the production side. */
sealed trait ArithError
object ArithError {
  case object Overflow extends ArithError
}

object SafeArith {

  // The machine-Long bounds, named for the range check. LONG_MAX is defined in
  // FMTypes; the lower bound is its negation (the FMLong invariant uses the same
  // expression).
  val LONG_MIN: BigInt = BigInt(0) - LONG_MAX

  // The machine-Int bounds (INT_MIN/INT_MAX are defined in FMTypes).

  /** True iff `n` fits in the machine-Long range. */
  def inLongRange(n: BigInt): Boolean = n >= LONG_MIN && n <= LONG_MAX

  /** True iff `n` fits in the machine-Int range. */
  def inIntRange(n: BigInt): Boolean = n >= INT_MIN && n <= INT_MAX

  extension (a: FMLong)
    /** Add two FMLongs, returning Left(Overflow) instead of violating FMLong's
      * range invariant. The sum is computed in unbounded BigInt (no overflow),
      * then range-checked; the FMLong is only constructed on the in-range branch,
      * so its `require` is discharged by the guard. */
    def safeAdd(b: FMLong): Either[ArithError, FMLong] = {
      val sum = a.value + b.value
      if inLongRange(sum) then Right[ArithError, FMLong](FMLong(sum))
      else Left[ArithError, FMLong](ArithError.Overflow)
    }

  extension (a: FMInt)
    /** FMInt variant — same spec against the machine-Int range. */
    def safeAddI(b: FMInt): Either[ArithError, FMInt] = {
      val sum = a.value + b.value
      if inIntRange(sum) then Right[ArithError, FMInt](FMInt(sum))
      else Left[ArithError, FMInt](ArithError.Overflow)
    }

  /* ---------------------------------------------------------------------------
   * SPEC LEMMAS — the Either faithfully reflects overflow.
   * ------------------------------------------------------------------------- */

  /** Right IFF in range: safeAdd returns Right exactly when the true sum fits. */
  def safeAddRightIffInRange(a: FMLong, b: FMLong): Boolean = {
    a.safeAdd(b).isRight == inLongRange(a.value + b.value)
  }.holds

  /** Left IFF out of range: the complementary direction. Together with the
    * above, the Either is a faithful witness of overflow — no false Right, no
    * spurious Left. */
  def safeAddLeftIffOutOfRange(a: FMLong, b: FMLong): Boolean = {
    a.safeAdd(b).isLeft == !inLongRange(a.value + b.value)
  }.holds

  /** When out of range, the Left payload is specifically Overflow (not some
    * other ArithError) — the error value is precise. */
  def safeAddLeftIsOverflow(a: FMLong, b: FMLong): Boolean = {
    require(!inLongRange(a.value + b.value))
    a.safeAdd(b) == Left[ArithError, FMLong](ArithError.Overflow)
  }.holds

  /** When in range, the Right payload is exactly the (wrapped) true sum — the
    * success value is the faithful arithmetic result. */
  def safeAddRightIsExactSum(a: FMLong, b: FMLong): Boolean = {
    require(inLongRange(a.value + b.value))
    a.safeAdd(b) == Right[ArithError, FMLong](FMLong(a.value + b.value))
  }.holds

  /** The FMInt variant carries the same faithful-witness guarantee. */
  def safeAddIRightIffInRange(a: FMInt, b: FMInt): Boolean = {
    a.safeAddI(b).isRight == inIntRange(a.value + b.value)
  }.holds
}
