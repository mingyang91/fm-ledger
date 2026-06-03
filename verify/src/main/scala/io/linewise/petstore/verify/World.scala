package io.linewise.verify.fm.petstore

import stainless.lang._

/* =============================================================================
 * THE WORLD — the aggregate state, plus the concrete lenses into it. A service is
 * generic in its world type; this is the concrete World the proofs (and the
 * production demo) instantiate the services with. Each lens is checked LAWFUL by
 * the Has @laws (get-after-set / set-of-get). Transpile-clean; transpiler input.
 * ========================================================================== */
case class World(
    pets: PetRepository,
    orders: OrderRepository,
    users: UserRepository,
)

case class HasPets() extends Has[World, PetRepository] {
  def get(w: World): PetRepository = w.pets
  def set(w: World, r: PetRepository): World = w.copy(pets = r)
}

case class HasOrders() extends Has[World, OrderRepository] {
  def get(w: World): OrderRepository = w.orders
  def set(w: World, r: OrderRepository): World = w.copy(orders = r)
}

case class HasUsers() extends Has[World, UserRepository] {
  def get(w: World): UserRepository = w.users
  def set(w: World, r: UserRepository): World = w.copy(users = r)
}
