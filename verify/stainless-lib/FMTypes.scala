package dev.mingyang91.verify.effect

import stainless.lang._

/** Small verify-only placeholder types that do not have a richer model yet. */

case class FMString(str: String, length: BigInt, blank: Boolean) {
  require(length >= BigInt(0))
  require((length == BigInt(0)) == (str == ""))
  def isBlank: Boolean = blank
}

case class FMJsonObject()
