package io.linewise.petstore

import io.linewise.petstore.generated.PetStoreModel.*

/* =============================================================================
 * ORDER — THE SERVICE LAYER, production (mirrors the original OrderService: no
 * validation). placeOrder delegates to the repository; get -> Either; delete is
 * idempotent. No cats-effect.
 * ========================================================================== */
final class OrderService(repo: OrderRepository):

  def placeOrder(order: Order): Order = repo.create(order)

  def get(id: Long): Either[ValidationError, Order] =
    repo.get(id) match
      case Some(o) => Right(o)
      case None    => Left(OrderNotFoundError)

  def delete(id: Long): Unit = repo.delete(id)
