package dev.mingyang91.ledger

/* =============================================================================
 * Test doubles + fixtures for the Stripe integration. The hermetic suites use these so
 * CI needs no network: FakeGateway stands in for outbound transfers, StripeEvents builds
 * a real-HMAC-signed event body the way Stripe would, so the inbound signature path is
 * exercised for real against a known whsec test secret.
 * ========================================================================== */

/** Deterministic gateway: every create succeeds with tr_<idempotencyKey>, and records the
  * requests it saw so a test can assert on the amount / destination / metadata sent. */
final class FakeGateway extends PayoutGateway:
  @volatile var calls: List[TransferRequest] = Nil
  def createTransfer(req: TransferRequest): Either[GatewayError, TransferResult] =
    synchronized { calls = calls :+ req }
    Right(TransferResult(s"tr_${req.idempotencyKey}", s"""{"id":"tr_${req.idempotencyKey}"}"""))

/** A gateway that always fails — `retryable` selects the transient (stay pending) vs permanent
  * (mark failed) outbox branch. */
final class FailingGateway(retryable: Boolean) extends PayoutGateway:
  def createTransfer(req: TransferRequest): Either[GatewayError, TransferResult] =
    Left(GatewayError(retryable, if retryable then 503 else 400, "boom", "gateway down"))

/** Concrete self-typed subclass so Scala doesn't infer GenericContainer[Nothing] (the recursive
  * SELF type parameter can't be inferred from a bare `new GenericContainer(...)`). */
final class StripeMockContainer
    extends org.testcontainers.containers.GenericContainer[StripeMockContainer](
      org.testcontainers.utility.DockerImageName.parse("stripe/stripe-mock:latest"))

object StripeEvents:
  /** A Stripe-style event JSON. `metadata.withdrawal_id` routes it back to our withdrawal;
    * `fee` is the observed provider fee the reconciliation compares against the quote. */
  def event(eventId: String, eventType: String, objectId: String, withdrawalId: Long, fee: Long): String =
    s"""{"id":"$eventId","type":"$eventType","data":{"object":{"id":"$objectId","metadata":{"withdrawal_id":"$withdrawalId"},"fee":$fee}}}"""

  /** Sign a body with the current clock (inside Stripe's tolerance), returning (header, body). */
  def signed(secret: String, body: String): (String, String) =
    val t = System.currentTimeMillis() / 1000L
    (StripeSignature.sign(secret, body, t), body)
