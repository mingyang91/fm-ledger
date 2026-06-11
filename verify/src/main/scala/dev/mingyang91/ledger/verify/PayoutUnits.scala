package dev.mingyang91.verify.fm.ledger

import stainless.lang._

/* =============================================================================
 * PAYOUT UNITS — VERIFY-ONLY (#5). Points (the ledger unit) and Minor (the provider/currency
 * smallest unit, e.g. cents) are DISTINCT types, so mixing them is a TYPE ERROR in the verified
 * model — the minor/points confusion is unrepresentable, not merely discouraged. The ratio is
 * `points per one minor unit`, so points = minor * ratio and minor = points / ratio. The shell
 * mirrors `toPoints` whenever it ingests a provider-reported fee (which arrives in Minor) before
 * posting or reconciling it against a quote (which is in Points).
 * ========================================================================== */
object PayoutUnits {

  case class Ratio(pointsPerMinor: BigInt) {
    require(pointsPerMinor > BigInt(0))
  }
  case class Points(value: BigInt)
  case class Minor(value: BigInt)

  def toPoints(m: Minor, r: Ratio): Points = Points(m.value * r.pointsPerMinor)
  def divisible(p: Points, r: Ratio): Boolean = p.value % r.pointsPerMinor == BigInt(0)
  def toMinor(p: Points, r: Ratio): Minor = Minor(p.value / r.pointsPerMinor)

  // (1) round-trip: minor -> points -> minor is identity for any ratio > 0.
  def minorRoundTrips(m: Minor, r: Ratio): Boolean = {
    toMinor(toPoints(m, r), r) == m
  }.holds

  // (2) round-trip the other way holds exactly for divisible points (the shell rejects
  //     non-divisible payout amounts at submit, so this precondition is enforced upstream).
  def pointsRoundTripWhenDivisible(p: Points, r: Ratio): Boolean = {
    require(divisible(p, r))
    toPoints(toMinor(p, r), r) == p
  }.holds

  // unit-consistent reconciliation: a quoted fee (Points) is compared against an observed fee that
  // arrives in Minor only AFTER converting the observed fee to Points — never Minor vs Points.
  def feeMatchesInPoints(quoted: Points, observedMinor: Minor, r: Ratio): Boolean =
    quoted == toPoints(observedMinor, r)

  // (3) DOCUMENTS THE BUG: when the real money agrees (quotedPoints == observedMinor * ratio) but
  //     ratio > 1, the unit-consistent check says "matched" while the naive raw comparison of the
  //     two integer VALUES (points-value vs minor-value) says "not matched". Converting first is the
  //     only correct comparison — exactly what #5 was getting wrong.
  def rawMinorComparisonIsUnitInconsistent(quotedPoints: BigInt, observedMinorVal: BigInt, r: Ratio): Boolean = {
    require(r.pointsPerMinor > BigInt(1))
    require(observedMinorVal > BigInt(0))
    require(quotedPoints == observedMinorVal * r.pointsPerMinor)
    feeMatchesInPoints(Points(quotedPoints), Minor(observedMinorVal), r) &&
      quotedPoints != observedMinorVal
  }.holds
}
