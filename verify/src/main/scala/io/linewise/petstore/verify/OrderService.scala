package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * ORDER — THE SERVICE LAYER, polymorphic in W via a Has lens (no validation, as
 * in the original). placeOrder writes through the lens; get reads; delete is
 * idempotent. Transpile-clean.
 * ========================================================================== */
case class OrderService[W](has: Has[W, OrderRepository]) {

  def placeOrder(w: W, order: Order, freshId: FMLong): (W, Order) = {
    val saved = order.copy(id = Some[FMLong](freshId))
    val w1 = has(w).write((r: OrderRepository) => r.save(saved))
    (w1, saved)
  }

  def get(w: W, id: FMLong): Either[ValidationError, Order] =
    has.get(w).get(id) match
      case Some(o) => Right[ValidationError, Order](o)
      case _       => Left[ValidationError, Order](OrderNotFoundError)

  def delete(w: W, id: FMLong): W =
    has(w).write((r: OrderRepository) => r.delete(id))
}
