package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._
import OrderRepository.OrderTable

/* =============================================================================
 * ORDER — THE SERVICE LAYER (OrderService). The original has no validation here:
 * placeOrder delegates straight to the repository; get -> Either; delete is
 * idempotent. Transpile-clean; transpiler input.
 * ========================================================================== */
object OrderService {

  def placeOrder(t: OrderTable, order: Order, freshId: FMLong): (OrderTable, Order) =
    OrderRepository.create(t, order, freshId)

  def get(t: OrderTable, id: FMLong): Either[ValidationError, Order] =
    OrderRepository.get(t, id) match
      case Some(o) => Right[ValidationError, Order](o)
      case _       => Left[ValidationError, Order](OrderNotFoundError)

  def delete(t: OrderTable, id: FMLong): OrderTable =
    OrderRepository.delete(t, id)._1
}
