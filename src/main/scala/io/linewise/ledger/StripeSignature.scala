package io.linewise.ledger

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/* =============================================================================
 * STRIPE WEBHOOK SIGNATURE — the real scheme, replacing the old internal 5-tuple HMAC.
 * Header:  `t=<unixSeconds>,v1=<hexHmacSha256>` (several v1= may appear during secret rotation).
 * signed_payload = "<t>.<rawBody>";  expected = hex(HmacSHA256(whsec, signed_payload)).
 * Accept iff some v1 matches (constant-time) AND |now - t| <= tolerance (replay protection).
 * Pure and side-effect-free, so it is unit-testable and could later carry a Stainless lemma.
 * ========================================================================== */
object StripeSignature:

  def hmacHex(secret: String, payload: String): String =
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(SecretKeySpec(secret.getBytes(UTF_8), "HmacSHA256"))
    mac.doFinal(payload.getBytes(UTF_8)).map(b => f"$b%02x").mkString

  /** Build a `Stripe-Signature` header the way Stripe would — used by tests to sign fixtures. */
  def sign(secret: String, payload: String, t: Long): String =
    s"t=$t,v1=${hmacHex(secret, s"$t.$payload")}"

  def verify(payload: String, header: String, secret: String, nowEpochSeconds: Long, toleranceSeconds: Long = 300L): Either[String, Unit] =
    val parts = header.split(',').toList.flatMap { kv =>
      kv.split("=", 2) match
        case Array(k, v) => Some(k.trim -> v.trim)
        case _           => None
    }
    val t   = parts.collectFirst { case ("t", v) => v }.flatMap(s => scala.util.Try(s.toLong).toOption)
    val v1s = parts.collect { case ("v1", v) => v }
    t match
      case None => Left("malformed: no timestamp")
      case Some(ts) =>
        if v1s.isEmpty then Left("malformed: no v1 signature")
        else if math.abs(nowEpochSeconds - ts) > toleranceSeconds then Left("timestamp outside tolerance")
        else
          val expected = hmacHex(secret, s"$ts.$payload").getBytes(UTF_8)
          if v1s.exists(v => MessageDigest.isEqual(v.getBytes(UTF_8), expected)) then Right(())
          else Left("no matching v1 signature")
