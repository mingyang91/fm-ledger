package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._
import io.linewise.petstore.Db

/* =============================================================================
 * ORDER — THE REPOSITORY LAYER as a sealed abstract type with a @ghost `rows` model.
 *   - InMemOrderRepository: a real list (the verification ORACLE / differential ref).
 *   - JdbcOrderRepository: FIELD-LESS (immutable, so the World/Has lens still
 *     verifies); @ghost stub rows; each op is @extern @pure and delegates to the
 *     production Quill `Db` facade. TRUSTED via the saveGet / deleteGet axioms it
 *     carries on its ensurings; the in-memory-vs-JDBC differential test is the guard.
 * ========================================================================== */
sealed abstract class OrderRepository {
  @ghost def rows: List[Order]

  def save(order: Order): OrderRepository
  def get(id: FMLong): Option[Order]
  def delete(id: FMLong): OrderRepository

  @law def saveGet(order: Order, id: FMLong): Boolean =
    (order.id == Some[FMLong](id)) ==> (save(order).get(id) == Some[Order](order))
  @law def deleteGet(id: FMLong): Boolean =
    delete(id).get(id) == None[Order]()
}

/* IN-MEMORY oracle — a real list; discharges the axioms by head-match / lemma. */
case class InMemOrderRepository(items: List[Order]) extends OrderRepository {
  @ghost def rows: List[Order] = items
  def save(order: Order): OrderRepository = InMemOrderRepository(order :: items)
  def get(id: FMLong): Option[Order] = items.find((o: Order) => o.id == Some[FMLong](id))
  def delete(id: FMLong): OrderRepository =
    InMemOrderRepository(items.filter((o: Order) => o.id != Some[FMLong](id)))
      .ensuring((res: OrderRepository) => { OrderProofs.orderDeleteGetLemma(items, id); res.get(id) == None[Order]() })
}

/* JDBC realization — FIELD-LESS (immutable); each op @extern, delegating to the
 * production Quill `Db` facade; @ghost stub rows; trusted via the axioms. */
case class JdbcOrderRepository() extends OrderRepository {
  @ghost def rows: List[Order] = Nil[Order]()

  @extern @pure
  def save(order: Order): OrderRepository = { Db.insertOrder(order); JdbcOrderRepository() }.ensuring((res: OrderRepository) =>
    forall((id: FMLong) => (order.id == Some[FMLong](id)) ==> (res.get(id) == Some[Order](order))))

  @extern @pure
  def get(id: FMLong): Option[Order] = Db.orderById(id)

  @extern @pure
  def delete(id: FMLong): OrderRepository =
    { Db.deleteOrder(id); JdbcOrderRepository() }.ensuring((res: OrderRepository) => res.get(id) == None[Order]())
}
