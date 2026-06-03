package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * ORDER — THE REPOSITORY LAYER (OrderRepositoryAlgebra, realized purely).
 * Data-access only: create/get/delete over the in-memory OrderTable.
 * Transpile-clean; transpiler input.
 * ========================================================================== */
object OrderRepository {

  case class OrderTable(rows: List[Order])

  def create(t: OrderTable, order: Order, freshId: FMLong): (OrderTable, Order) = {
    val saved = order.copy(id = Some[FMLong](freshId))
    (OrderTable(saved :: t.rows), saved)
  }

  def get(t: OrderTable, id: FMLong): Option[Order] =
    t.rows.find((o: Order) => o.id == Some[FMLong](id))

  def delete(t: OrderTable, id: FMLong): (OrderTable, Option[Order]) =
    (OrderTable(t.rows.filter((o: Order) => o.id != Some[FMLong](id))),
     t.rows.find((o: Order) => o.id == Some[FMLong](id)))
}
