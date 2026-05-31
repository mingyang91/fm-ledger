package io.linewise.verify.effect

import stainless.lang._

/** EitherT monad transformer over IO state monad.
  *
  * Models: cats.data.EitherT[F, E, A] where F = IO[W, _]
  *
  * Three-level error taxonomy:
  *   - None           = halt (JVM crash, fiber cancellation)
  *   - Some(Left(e))  = typed business error (recoverable)
  *   - Some(Right(a)) = success
  *
  * In production: EitherT[F, E, A] (cats-data)
  * In verification: EitherT[W, E, A] = IO[W, Either[E, A]] (pure, Stainless-verifiable)
  *
  * Generator substitution: EitherT[F, E, A] → EitherT[World, E, A]
  */
case class EitherT[W, E, A](value: IO[W, Either[E, A]]) {

  /** Monadic bind: halt propagates, Left short-circuits, Right continues. */
  def flatMap[B](f: A => EitherT[W, E, B]): EitherT[W, E, B] =
    EitherT[W, E, B](IO[W, Either[E, B]]((w: W) => {
      val (w1, optEa) = value.run(w)
      optEa match {
        case None() => (w1, None[Either[E, B]]())
        case Some(ea) => ea match {
          case Left(e)  => (w1, Some(Left[E, B](e)))
          case Right(a) => f(a).value.run(w1)
        }
      }
    }))

  /** Functor map on the Right value. Halt propagates. */
  def map[B](f: A => B): EitherT[W, E, B] =
    EitherT[W, E, B](IO[W, Either[E, B]]((w: W) => {
      val (w1, optEa) = value.run(w)
      optEa match {
        case None() => (w1, None[Either[E, B]]())
        case Some(ea) => ea match {
          case Left(e)  => (w1, Some(Left[E, B](e)))
          case Right(a) => (w1, Some(Right[E, B](f(a))))
        }
      }
    }))

  /** Map the error type. Halt propagates. */
  def leftMap[E2](f: E => E2): EitherT[W, E2, A] =
    EitherT[W, E2, A](IO[W, Either[E2, A]]((w: W) => {
      val (w1, optEa) = value.run(w)
      optEa match {
        case None() => (w1, None[Either[E2, A]]())
        case Some(ea) => ea match {
          case Left(e)  => (w1, Some(Left[E2, A](f(e))))
          case Right(a) => (w1, Some(Right[E2, A](a)))
        }
      }
    }))

  /** Apply an effectful function on the Right value. Halt propagates from both IO layers. */
  def semiflatMap[B](f: A => IO[W, B]): EitherT[W, E, B] =
    EitherT[W, E, B](IO[W, Either[E, B]]((w: W) => {
      val (w1, optEa) = value.run(w)
      optEa match {
        case None() => (w1, None[Either[E, B]]())
        case Some(ea) => ea match {
          case Left(e) => (w1, Some(Left[E, B](e)))
          case Right(a) =>
            val (w2, optB) = f(a).run(w1)
            optB match {
              case None()  => (w2, None[Either[E, B]]())
              case Some(b) => (w2, Some(Right[E, B](b)))
            }
        }
      }
    }))
}

object EitherT {
  /** Lift a pure Right value. */
  def pure[W, E, A](a: A): EitherT[W, E, A] =
    EitherT[W, E, A](IO.pure[W, Either[E, A]](Right[E, A](a)))

  /** Lift an IO into EitherT (always Right, halt propagates). */
  def liftF[W, E, A](fa: IO[W, A]): EitherT[W, E, A] =
    EitherT[W, E, A](fa.map((a: A) => Right[E, A](a)))

  /** Conditional: if test then Right(right) else Left(left). */
  def cond[W, E, A](test: Boolean, right: A, left: E): EitherT[W, E, A] =
    if (test) EitherT[W, E, A](IO.pure[W, Either[E, A]](Right[E, A](right)))
    else EitherT[W, E, A](IO.pure[W, Either[E, A]](Left[E, A](left)))

  /** Lift a pure Either into EitherT. */
  def fromEither[W, E, A](e: Either[E, A]): EitherT[W, E, A] =
    EitherT[W, E, A](IO.pure[W, Either[E, A]](e))

  /** Lift an IO[Option[A]] into EitherT, using ifNone as the Left value.
    * Note: this Option is a BUSINESS option (data missing), not a halt.
    * Halt propagates from the IO layer.
    */
  def fromOptionF[W, E, A](fa: IO[W, Option[A]], ifNone: E): EitherT[W, E, A] =
    EitherT[W, E, A](fa.map((opt: Option[A]) => opt match {
      case Some(a) => Right[E, A](a)
      case None()  => Left[E, A](ifNone)
    }))
}
