package io.linewise.verify.effect

import stainless.lang._

/** State monad with halt: IO[W, A] = W => (W, Option[A]).
  *
  * Models effectful computation as pure state threading.
  * W is the "world" (all service state). A is the result.
  *
  * - Some(a) = computation completed normally
  * - None    = computation halted (JVM crash, fiber cancellation)
  *
  * flatMap short-circuits on None — continuation is NEVER called.
  * Halt lives in the monad (Option[A]), not in the World state.
  *
  * In production: F[A] (cats-effect IO, any effect type — always cancellable)
  * In verification: IO[W, A] = W => (W, Option[A]) (pure, Stainless-verifiable)
  *
  * Generator substitution: F[A] → IO[World, A]
  */
case class IO[W, A](run: W => (W, Option[A])) {

  /** flatMap: short-circuit on None (halted), continue on Some. */
  inline def flatMap[B](inline f: A => IO[W, B]): IO[W, B] =
    IO[W, B]((w: W) => {
      val (w1, optA) = run(w)
      optA match {
        case Some(a) => f(a).run(w1)
        case None()  => (w1, None[B]())
      }
    })

  inline def map[B](inline f: A => B): IO[W, B] =
    IO[W, B]((w: W) => {
      val (w1, optA) = run(w)
      optA match {
        case Some(a) => (w1, Some(f(a)))
        case None()  => (w1, None[B]())
      }
    })
  
  inline def tap(inline f: A => Unit): IO[W, A] =
    IO[W, A]((w: W) => {
      val (w1, optA) = run(w)
      optA match {
        case Some(a) => (w1, { f(a); Some(a) })
        case None()  => (w1, None[A]())
      }
    })

  /** Sequence: run this, ignore result, run next. */
  inline def >>[B](inline next: IO[W, B]): IO[W, B] =
    flatMap((_: A) => next)

  /** Transform the result while keeping state unchanged. */
  def as[B](b: B): IO[W, B] = map((_: A) => b)
}

object IO {
  /** Lift a pure value (no state change, no halt). */
  def pure[W, A](a: A): IO[W, A] =
    IO[W, A]((w: W) => (w, Some(a)))

  /** Halt: freeze world, no result. */
  def halt[W, A]: IO[W, A] =
    IO[W, A]((w: W) => (w, None[A]()))

  /** Access the current state (no halt). */
  def get[W]: IO[W, W] =
    IO[W, W]((w: W) => (w, Some(w)))

  /** Replace the state (no halt). */
  def set[W](w: W): IO[W, Unit] =
    IO[W, Unit]((_: W) => (w, Some(())))

  /** Modify the state with a function (no halt). */
  def modify[W](f: W => W): IO[W, Unit] =
    IO[W, Unit]((w: W) => (f(w), Some(())))

  /** Read a projection from state (no halt, no mutation). */
  def read[W, A](f: W => A): IO[W, A] =
    IO[W, A]((w: W) => (w, Some(f(w))))

  /** Conditional halt: if survived, apply f; otherwise halt. */
  def guard[W](survived: Boolean, f: W => W): IO[W, Unit] =
    IO[W, Unit]((w: W) =>
      if (survived) (f(w), Some(()))
      else (w, None[Unit]())
    )
}
