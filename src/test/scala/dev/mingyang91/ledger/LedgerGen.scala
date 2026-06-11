package dev.mingyang91.ledger

import org.scalacheck.{Arbitrary, Gen}

trait LedgerGen:
  final case class UserUid(value: String)
  final case class SourceKey(kind: String, id: String)
  final case class ClientRequestId(value: String)
  final case class IncentiveTraceId(value: String)
  final case class PositivePoints(value: Long)
  final case class HugePoints(value: Long) // above the 100000 single-ledger limit
  final case class FundedWithdrawal(funded: Long, withdrawn: Long)
  final case class FundedDebit(funded: Long, debit: Long)

  private def ident(prefix: String): Gen[String] =
    Gen.chooseNum(1, 100000000).map(n => s"$prefix-$n")

  given Arbitrary[UserUid] = Arbitrary(ident("u").map(UserUid.apply))
  given Arbitrary[SourceKey] = Arbitrary(
    for
      kind <- Gen.oneOf("annotation_job", "incentive_event", "bonus_batch")
      id <- ident("src")
    yield SourceKey(kind, id)
  )
  given Arbitrary[ClientRequestId] = Arbitrary(ident("cr").map(ClientRequestId.apply))
  given Arbitrary[IncentiveTraceId] = Arbitrary(ident("trace").map(IncentiveTraceId.apply))
  given Arbitrary[PositivePoints] = Arbitrary(Gen.chooseNum(1L, 20000L).map(PositivePoints.apply))
  given Arbitrary[HugePoints] = Arbitrary(Gen.chooseNum(100001L, 5000000L).map(HugePoints.apply))
  given Arbitrary[FundedWithdrawal] = Arbitrary(
    for
      funded <- Gen.chooseNum(1L, 20000L)
      withdrawn <- Gen.chooseNum(1L, funded)
    yield FundedWithdrawal(funded, withdrawn)
  )
  given Arbitrary[FundedDebit] = Arbitrary(
    for
      funded <- Gen.chooseNum(1L, 20000L)
      debit <- Gen.chooseNum(funded + 1L, funded + 20000L)
    yield FundedDebit(funded, debit)
  )
