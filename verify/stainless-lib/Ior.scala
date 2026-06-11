package dev.mingyang91.verify.effect

import stainless.lang._

/** Inclusive-or type: mirrors cats.data.Ior for Stainless.
  *
  * Three variants: Left (error only), Right (success only), Both (partial success).
  *
  * Used by VideoClipService caption pipeline where Gemini can return
  * partial results (some chunks succeed, some fail).
  *
  * Generator substitution: cats.data.Ior[A, B] → Ior[A, B]
  */
sealed trait Ior[A, B]

object Ior {
  case class IorLeft[A, B](value: A) extends Ior[A, B]
  case class IorRight[A, B](value: B) extends Ior[A, B]
  case class IorBoth[A, B](left: A, right: B) extends Ior[A, B]
}
