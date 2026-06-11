package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.annotation._

/* =============================================================================
 * Has[W, R] — a small LENS / capability for verified services. A service is
 * polymorphic in the world type W and is handed a `Has[W, R]` to read
 * and write its slice R of W. The two lens laws are stated as @laws, so every
 * concrete lens (the World accessors) is checked lawful. The transpiler strips @law;
 * production keeps the plain interface.
 * ========================================================================== */
case class HasCursor[W, R](lens: Has[W, R], w: W) {
  def read: R = lens.get(w)
  def write(f: R => R): W = lens.set(w, f(lens.get(w)))
}

abstract class Has[W, R] {
  def get(w: W): R
  def set(w: W, r: R): W

  def apply(w: W): HasCursor[W, R] = HasCursor[W, R](this, w)

  @law def lawGetSet(w: W, r: R): Boolean = get(set(w, r)) == r
  @law def lawSetGet(w: W): Boolean = set(w, get(w)) == w
}
