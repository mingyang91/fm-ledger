package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._
import OrderRepository.OrderTable

/* =============================================================================
 * ORDER PROOFS — VERIFY-ONLY. Every OrderService capability proven over the
 * Repository / Service layers, plus the distinct-id store invariant.
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

  def tableInv(t: OrderTable): Boolean = distinctL(orderIds(t.rows))

  @opaque @inlineOnce
  def deleteRemovesLemma(rows: List[Order], id: FMLong): Unit = {
    rows match
      case Nil()      => ()
      case Cons(h, t) => deleteRemovesLemma(t, id)
  }.ensuring(_ =>
    rows.filter((o: Order) => o.id != Some[FMLong](id)).find((o: Order) => o.id == Some[FMLong](id)) == None[Order]())

  def placeThenGet(t: OrderTable, order: Order, freshId: FMLong): Boolean = {
    val (t2, saved) = OrderService.placeOrder(t, order, freshId)
    saved.id == Some[FMLong](freshId) && OrderRepository.get(t2, freshId) == Some[Order](saved)
  }.holds

  def getMissingFails(t: OrderTable, id: FMLong): Boolean = {
    require(OrderRepository.get(t, id) == None[Order]())
    OrderService.get(t, id) == Left[ValidationError, Order](OrderNotFoundError)
  }.holds

  def getPresentSucceeds(t: OrderTable, id: FMLong, o: Order): Boolean = {
    require(OrderRepository.get(t, id) == Some[Order](o))
    OrderService.get(t, id) == Right[ValidationError, Order](o)
  }.holds

  def deleteRemoves(t: OrderTable, id: FMLong): Boolean = {
    deleteRemovesLemma(t.rows, id)
    OrderRepository.get(OrderService.delete(t, id), id) == None[Order]()
  }.holds

  def placePreservesDistinct(t: OrderTable, order: Order, freshId: FMLong): Boolean = {
    require(tableInv(t) && !orderIds(t.rows).contains(freshId))
    tableInv(OrderService.placeOrder(t, order, freshId)._1)
  }.holds
}
