package dev.mingyang91.verify.effect

import stainless.lang._
import stainless.collection.{List => SList}

/** Non-empty list: mirrors cats.data.NonEmptyList for Stainless.
  *
  * Invariant: head always exists, tail may be empty.
  * Guarantees non-emptiness at construction time.
  *
  * Generator substitution: cats.data.NonEmptyList[A] → NonEmptyList[A]
  */
case class NonEmptyList[A](head: A, tail: SList[A]) {

  def size: BigInt = BigInt(1) + tail.size

  def toList: SList[A] = head :: tail

  def map[B](f: A => B): NonEmptyList[B] =
    NonEmptyList(f(head), tail.map(f))

  def exists(p: A => Boolean): Boolean =
    p(head) || tail.exists(p)

  def forall(p: A => Boolean): Boolean =
    p(head) && tail.forall(p)
}

object NonEmptyList {
  def one[A](a: A): NonEmptyList[A] =
    NonEmptyList(a, SList.empty[A])
}
