package io.linewise.ledger

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Duration

/* =============================================================================
 * PAYOUT PROVIDER SEAM — shell only, off the verified classpath. The proven accounting
 * decision (PayoutProofs.recipientPaysFeeSettlement) never touches this; it is reached
 * later, from the webhook. The dispatcher (PayoutDispatcher) drives createTransfer over
 * the outbox; Stripe is one implementation, a fake (tests) is another.
 * ========================================================================== */

/** A request to move `amountMinor` (smallest currency unit) to `destination` (a Stripe
  * connected account, `acct_…`). `idempotencyKey` makes a retried dispatch safe; `metadata`
  * carries our `withdrawal_id` so the inbound webhook can route the event back. */
final case class TransferRequest(
    amountMinor: Long,
    currency: String,
    destination: String,
    idempotencyKey: String,
    metadata: Map[String, String],
    transferGroup: Option[String])

final case class TransferResult(providerTransferRef: String, raw: String)

/** `retryable` separates a transient failure (5xx / 429 / network — leave the outbox row
  * pending for the next tick) from a permanent one (4xx — mark failed, a human must look). */
final case class GatewayError(retryable: Boolean, httpStatus: Int, code: String, message: String)

trait PayoutGateway:
  def createTransfer(req: TransferRequest): Either[GatewayError, TransferResult]

/** Real Stripe Connect transfer over java.net.http — no extra production dependency on JDK 25.
  * Form-encodes the body the way Stripe expects, sends the secret key as a Bearer token and an
  * Idempotency-Key header, and parses the `id` (tr_…) out of the JSON response with ujson. */
final class StripeGateway(apiKey: String, baseUrl: String, apiVersion: String) extends PayoutGateway:
  private val http = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build()

  private def enc(s: String): String = URLEncoder.encode(s, UTF_8)

  private def form(req: TransferRequest): String =
    val pairs =
      List("amount" -> req.amountMinor.toString, "currency" -> req.currency, "destination" -> req.destination) ++
        req.transferGroup.map("transfer_group" -> _).toList ++
        req.metadata.toList.map((k, v) => s"metadata[$k]" -> v)
    pairs.map((k, v) => s"${enc(k)}=${enc(v)}").mkString("&")

  def createTransfer(req: TransferRequest): Either[GatewayError, TransferResult] =
    try
      val httpReq = HttpRequest.newBuilder()
        .uri(URI.create(s"$baseUrl/v1/transfers"))
        .timeout(Duration.ofSeconds(30))
        .header("Authorization", s"Bearer $apiKey")
        .header("Idempotency-Key", req.idempotencyKey)
        .header("Stripe-Version", apiVersion)
        .header("Content-Type", "application/x-www-form-urlencoded")
        .POST(HttpRequest.BodyPublishers.ofString(form(req), UTF_8))
        .build()
      val resp = http.send(httpReq, HttpResponse.BodyHandlers.ofString())
      val code = resp.statusCode()
      val body = resp.body()
      if code >= 200 && code < 300 then
        scala.util.Try(ujson.read(body).obj.get("id").flatMap(_.strOpt)).toOption.flatten match
          case Some(id) if id.nonEmpty => Right(TransferResult(id, body))
          case _                       => Left(GatewayError(retryable = false, code, "no_id", "transfer response missing id"))
      else
        val errObj = scala.util.Try(ujson.read(body).obj("error").obj).toOption
        val errCode = errObj.flatMap(o => o.get("code").orElse(o.get("type"))).flatMap(_.strOpt).getOrElse("stripe_error")
        val errMsg  = errObj.flatMap(_.get("message")).flatMap(_.strOpt).getOrElse(s"http $code")
        // 429 (rate limit) and 5xx are transient; other 4xx are permanent.
        Left(GatewayError(retryable = code >= 500 || code == 429, code, errCode, errMsg))
    catch
      case e: java.io.IOException =>
        Left(GatewayError(retryable = true, 0, "io", String.valueOf(e.getMessage)))
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        Left(GatewayError(retryable = true, 0, "interrupted", "interrupted"))
