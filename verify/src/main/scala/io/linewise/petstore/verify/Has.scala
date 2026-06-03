package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._

/* =============================================================================
 * Has[W, R] — a LENS / capability into a World. A service is polymorphic in the
 * world type W and is handed a `Has[W, R]` to read and modify its slice R of W:
 *
 *   val w1 = hasUserRepo(w).write(userRepo => userRepo.save(newUser))
 *
 * `has(w)` returns a cursor focused at `w`; `.read` gets the slice, `.write(f)`
 * applies `f` to the slice and returns the new world. The two lens laws are stated
 * as Stainless `@law`s, so EVERY concrete lens (the World accessors) is checked to
 * be lawful — get-after-set returns what was set, set-of-get is a no-op. (The
 * transpiler strips @law; production keeps the plain interface.)
 * ========================================================================== */
case class HasCursor[W, R](lens: Has[W, R], w: W) {
  def read: R = lens.get(w)
  def write(f: R => R): W = lens.set(w, f(lens.get(w)))
}

abstract class Has[W, R] {
  def get(w: W): R
  def set(w: W, r: R): W

  /** `has(w)` — focus the lens at a world, giving a read/write cursor. */
  def apply(w: W): HasCursor[W, R] = HasCursor[W, R](this, w)

  @law def lawGetSet(w: W, r: R): Boolean = get(set(w, r)) == r
  @law def lawSetGet(w: W): Boolean = set(w, get(w)) == w
}
