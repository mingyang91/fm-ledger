package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * ORDER PROOFS — VERIFY-ONLY. OrderService capabilities proven over the World +
 * HasOrders lens and the ABSTRACT OrderRepository, via the saveGet / deleteGet
 * axioms. placePreservesDistinct (distinct ids) moves to the ORDERS PRIMARY KEY in
 * production (the @ghost id list is unobservable in the JDBC realization).
 * ========================================================================== */
object OrderProofs {

  /* The filter-then-find lemma the in-memory delete's ensuring uses to discharge the
   * `deleteGet` axiom. Verify-only (never transpiled). */
  @opaque @inlineOnce
  def orderDeleteGetLemma(rows: List[Order], id: FMLong): Unit = {
    rows match
      case Nil()      => ()
      case Cons(h, t) => orderDeleteGetLemma(t, id)
  }.ensuring(_ =>
    rows.filter((o: Order) => o.id != Some[FMLong](id)).find((o: Order) => o.id == Some[FMLong](id)) == None[Order]())

  def placeThenGet(w: World, order: Order, freshId: FMLong): Boolean = {
    val svc = OrderService[World](HasOrders())
    val (w1, saved) = svc.placeOrder(w, order, freshId)
    assert(w.orders.saveGet(saved, freshId)) // hint: save(saved).get(freshId)==Some(saved)
    saved.id == Some[FMLong](freshId) && svc.get(w1, freshId) == Right[ValidationError, Order](saved)
  }.holds

  def getMissingFails(w: World, id: FMLong): Boolean = {
    require(w.orders.get(id) == None[Order]())
    val svc = OrderService[World](HasOrders())
    svc.get(w, id) == Left[ValidationError, Order](OrderNotFoundError)
  }.holds

  def deleteRemoves(w: World, id: FMLong): Boolean = {
    assert(w.orders.deleteGet(id)) // hint: delete(id).get(id) == None
    val svc = OrderService[World](HasOrders())
    svc.get(svc.delete(w, id), id) == Left[ValidationError, Order](OrderNotFoundError)
  }.holds
}
