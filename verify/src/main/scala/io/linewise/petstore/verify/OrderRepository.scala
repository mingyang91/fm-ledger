package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * ORDER — THE REPOSITORY LAYER as a VALUE with methods. Transpile-clean.
 * ========================================================================== */
case class OrderRepository(rows: List[Order]) {
  def save(order: Order): OrderRepository = OrderRepository(order :: rows)
  def get(id: FMLong): Option[Order] = rows.find((o: Order) => o.id == Some[FMLong](id))
  def delete(id: FMLong): OrderRepository = OrderRepository(rows.filter((o: Order) => o.id != Some[FMLong](id)))
}
