package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * ORDER PROOFS — VERIFY-ONLY. Every OrderService capability proven over the World
 * + HasOrders lens, plus the distinct-id invariant.
 * ========================================================================== */
object OrderProofs {

  def orderIds(rows: List[Order]): List[FMLong] =
    rows match
      case Nil()      => Nil[FMLong]()
      case Cons(h, t) => h.id match
          case Some(i) => i :: orderIds(t)
          case _       => orderIds(t)

  def distinctL(xs: List[FMLong]): Boolean =
    xs match
      case Nil()      => true
      case Cons(h, t) => !t.contains(h) && distinctL(t)

  def repoInv(repo: OrderRepository): Boolean = distinctL(orderIds(repo.rows))

  @opaque @inlineOnce
  def deleteRemovesLemma(rows: List[Order], id: FMLong): Unit = {
    rows match
      case Nil()      => ()
      case Cons(h, t) => deleteRemovesLemma(t, id)
  }.ensuring(_ =>
    rows.filter((o: Order) => o.id != Some[FMLong](id)).find((o: Order) => o.id == Some[FMLong](id)) == None[Order]())

  def placeThenGet(w: World, order: Order, freshId: FMLong): Boolean = {
    val svc = OrderService[World](HasOrders())
    val (w1, saved) = svc.placeOrder(w, order, freshId)
    saved.id == Some[FMLong](freshId) && svc.get(w1, freshId) == Right[ValidationError, Order](saved)
  }.holds

  def getMissingFails(w: World, id: FMLong): Boolean = {
    require(w.orders.get(id) == None[Order]())
    val svc = OrderService[World](HasOrders())
    svc.get(w, id) == Left[ValidationError, Order](OrderNotFoundError)
  }.holds

  def deleteRemoves(w: World, id: FMLong): Boolean = {
    deleteRemovesLemma(w.orders.rows, id)
    val svc = OrderService[World](HasOrders())
    svc.get(svc.delete(w, id), id) == Left[ValidationError, Order](OrderNotFoundError)
  }.holds

  def placePreservesDistinct(w: World, order: Order, freshId: FMLong): Boolean = {
    require(repoInv(w.orders) && !orderIds(w.orders.rows).contains(freshId))
    val svc = OrderService[World](HasOrders())
    repoInv(svc.placeOrder(w, order, freshId)._1.orders)
  }.holds
}
