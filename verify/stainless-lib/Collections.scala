package io.linewise.verify.verify

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import stainless.proof._

/** Stainless-native implementation of AssocMap.
  *
  * THIS FILE IS HAND-WRITTEN — NOT GENERATED.
  * It replaces core/verify/Collections.scala in the verification environment.
  *
  * In production: AssocMap[K,V] = Map[K,V] (zero-cost type alias).
  * In verification: AssocMap[K,V] = List[(K,V)] (assoc list, Stainless-compatible).
  *
  * All operations + lemmas proved correct by Stainless.
  */

type AssocMap[K, V] = List[(K, V)]

object AssocMap:
  def empty[K, V]: AssocMap[K, V] = Nil[(K, V)]()

  extension [K, V](m: AssocMap[K, V])

    def has(k: K): Boolean =
      m match
        case Nil()              => false
        case Cons((key, _), tail) => if (key == k) true else tail.has(k)

    def lookup(k: K): V =
      require(m.has(k))
      m match
        case Cons((key, v), tail) => if (key == k) v else tail.lookup(k)

    def put(k: K, v: V): AssocMap[K, V] =
      m match
        case Nil()                          => Cons((k, v), Nil())
        case Cons((key, _), tail) if key == k => Cons((k, v), tail)
        case Cons(head, tail)               => Cons(head, tail.put(k, v))

    def remove(k: K): AssocMap[K, V] =
      m match
        case Nil()                          => Nil()
        case Cons((key, _), tail) if key == k => tail.remove(k) // remove ALL occurrences
        case Cons(head, tail)               => Cons(head, tail.remove(k))

    def isMapEmpty: Boolean = m.isEmpty

  // --- Bridge lemmas ---

  /** After put(k, v), has(k) is true and lookup(k) == v. */
  @opaque @inlineOnce
  def putThenGet[K, V](m: AssocMap[K, V], k: K, v: V): Unit = {
    m match
      case Nil()                          => ()
      case Cons((key, _), _) if key == k  => ()
      case Cons(_, tail)                  => putThenGet(tail, k, v)
  }.ensuring(_ =>
    m.put(k, v).has(k) &&
    m.put(k, v).lookup(k) == v
  )

  /** After remove(k), has(k) is false. remove() removes ALL occurrences. */
  @opaque @inlineOnce
  def removeThenNotHas[K, V](m: AssocMap[K, V], k: K): Unit = {
    m match
      case Nil()                          => ()
      case Cons((key, _), tail) if key == k => removeThenNotHas(tail, k)
      case Cons(_, tail)                  => removeThenNotHas(tail, k)
  }.ensuring(_ =>
    !m.remove(k).has(k)
  )

  /** Check if a key appears more than once. */
  def hasDuplicate[K, V](m: AssocMap[K, V], k: K): Boolean =
    m match
      case Nil() => false
      case Cons((key, _), tail) if key == k => tail.has(k)
      case Cons(_, tail) => hasDuplicate(tail, k)
