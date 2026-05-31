package io.linewise.verify.effect

import stainless.lang._

/** Bounded primitive type wrappers for Stainless FM.
  *
  * Stainless has no Long type. Int/Long are modeled as bounded BigInt with
  * operator overloads for cross-type comparison (Long >= Int).
  *
  * Generator emits: type Int = FMInt type Long = FMLong type String = FMString
  * so production types resolve to these automatically.
  */

val INT_MAX: BigInt = BigInt(2147483647)
val INT_MIN: BigInt = BigInt(-2147483648)
val LONG_MAX: BigInt = BigInt(2147483647) * BigInt(2147483647)

case class FMInt(value: BigInt) {
  require(value >= INT_MIN && value <= INT_MAX)
  def >=(other: FMInt): Boolean = value >= other.value
  def <=(other: FMInt): Boolean = value <= other.value
  def >(other: FMInt): Boolean = value > other.value
  def <(other: FMInt): Boolean = value < other.value
  def ==(other: FMInt): Boolean = value == other.value
  def !=(other: FMInt): Boolean = value != other.value
  def +(other: FMInt): FMInt = {
    require(value + other.value >= INT_MIN && value + other.value <= INT_MAX)
    FMInt(value + other.value)
  }
  def -(other: FMInt): FMInt = {
    require(value - other.value >= INT_MIN && value - other.value <= INT_MAX)
    FMInt(value - other.value)
  }
  def toLong: FMLong = FMLong(value)
}

case class FMLong(value: BigInt) {
  require(value >= BigInt(0) - LONG_MAX && value <= LONG_MAX)
  def >=(other: FMLong): Boolean = value >= other.value
  def >=(other: FMInt): Boolean = value >= other.value
  def <=(other: FMLong): Boolean = value <= other.value
  def >(other: FMLong): Boolean = value > other.value
  def <(other: FMLong): Boolean = value < other.value
  def ==(other: FMLong): Boolean = value == other.value
  def !=(other: FMLong): Boolean = value != other.value
  def toInt: FMInt = {
    require(value >= INT_MIN && value <= INT_MAX)
    FMInt(value)
  }
}

case class FMString(str: String, length: BigInt, blank: Boolean) {
  require(length >= BigInt(0))
  require((length == BigInt(0)) == (str == ""))
  def isBlank: Boolean = blank
}

case class FMJsonObject()
