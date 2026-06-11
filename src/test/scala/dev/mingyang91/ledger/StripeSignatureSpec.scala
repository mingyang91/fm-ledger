package dev.mingyang91.ledger

/* =============================================================================
 * Unit tests for the real Stripe webhook signature scheme — pure, no DB, no container.
 * ========================================================================== */
class StripeSignatureSpec extends munit.FunSuite:
  private val secret = "whsec_test"
  private val body = """{"id":"evt_1","type":"transfer.paid"}"""
  private val t = 1_700_000_000L

  test("a fresh signature within tolerance verifies") {
    val h = StripeSignature.sign(secret, body, t)
    assertEquals(StripeSignature.verify(body, h, secret, t), Right(()))
    assertEquals(StripeSignature.verify(body, h, secret, t + 200), Right(()))
  }

  test("a timestamp beyond the tolerance window is rejected") {
    val h = StripeSignature.sign(secret, body, t)
    assert(StripeSignature.verify(body, h, secret, t + 1000).isLeft)
  }

  test("a tampered body fails") {
    val h = StripeSignature.sign(secret, body, t)
    assert(StripeSignature.verify(body + " ", h, secret, t).isLeft)
  }

  test("a wrong secret fails") {
    val h = StripeSignature.sign(secret, body, t)
    assert(StripeSignature.verify(body, h, "whsec_other", t).isLeft)
  }

  test("a header with no v1 is malformed") {
    assert(StripeSignature.verify(body, s"t=$t", secret, t).isLeft)
  }

  test("rotation: any matching v1 among several verifies") {
    val good = StripeSignature.hmacHex(secret, s"$t.$body")
    assertEquals(StripeSignature.verify(body, s"t=$t,v1=deadbeef,v1=$good", secret, t), Right(()))
  }
