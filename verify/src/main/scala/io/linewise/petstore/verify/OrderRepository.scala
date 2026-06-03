package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._
import JdbcSupport.{fmFromLong, longOfFM}
import io.linewise.petstore.ConnProvider

/* =============================================================================
 * ORDER — THE REPOSITORY LAYER as a sealed abstract type with a @ghost `rows` model.
 *   - InMemOrderRepository: a real list (the verification ORACLE / differential ref).
 *   - JdbcOrderRepository: FIELD-LESS (immutable, so the World/Has lens still
 *     verifies); @ghost stub rows; each op is @extern @pure REAL PER-OP SQL against
 *     the ambient connection. TRUSTED via the saveGet / deleteGet axioms it carries
 *     on its ensurings; the in-memory-vs-JDBC differential test is the guard.
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

/* JDBC realization — FIELD-LESS (immutable); real per-op SQL against the ambient
 * connection; @ghost stub rows; trusted via the axioms. */
case class JdbcOrderRepository() extends OrderRepository {
  @ghost def rows: List[Order] = Nil[Order]()

  @extern @pure
  def save(order: Order): OrderRepository = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "INSERT INTO ORDERS (ID, PET_ID, SHIP_DATE, STATUS, COMPLETE, USER_ID) VALUES (?,?,?,?,?,?)")
    ps.setLong(1, longOfFM(order.id.getOrElse(FMLong(BigInt(0)))))
    ps.setLong(2, longOfFM(order.petId))
    order.shipDate match
      case Some(ms) => ps.setTimestamp(3, new java.sql.Timestamp(longOfFM(ms)))
      case _        => ps.setNull(3, java.sql.Types.TIMESTAMP)
    ps.setString(4, orderStatusToStr(order.status))
    ps.setBoolean(5, order.complete)
    ps.setLong(6, longOfFM(order.userId.getOrElse(FMLong(BigInt(0)))))
    ps.executeUpdate()
    JdbcOrderRepository()
  }.ensuring((res: OrderRepository) =>
    forall((id: FMLong) => (order.id == Some[FMLong](id)) ==> (res.get(id) == Some[Order](order))))

  @extern @pure
  def get(id: FMLong): Option[Order] = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "SELECT ID, PET_ID, SHIP_DATE, STATUS, COMPLETE, USER_ID FROM ORDERS WHERE ID = ?")
    ps.setLong(1, longOfFM(id))
    val rs = ps.executeQuery()
    if rs.next() then Some[Order](rowToOrder(rs)) else None[Order]()
  }

  @extern @pure
  def delete(id: FMLong): OrderRepository = {
    val ps = ConnProvider.conn().underlying.prepareStatement("DELETE FROM ORDERS WHERE ID = ?")
    ps.setLong(1, longOfFM(id)); ps.executeUpdate()
    JdbcOrderRepository()
  }.ensuring((res: OrderRepository) => res.get(id) == None[Order]())

  @extern @pure
  private def rowToOrder(rs: java.sql.ResultSet): Order = {
    val ts = rs.getTimestamp("SHIP_DATE")
    val shipDate = if ts == null then None[FMLong]() else Some[FMLong](fmFromLong(ts.getTime))
    Order(fmFromLong(rs.getLong("PET_ID")), shipDate, strToOrderStatus(rs.getString("STATUS")),
      rs.getBoolean("COMPLETE"), Some[FMLong](fmFromLong(rs.getLong("ID"))),
      Some[FMLong](fmFromLong(rs.getLong("USER_ID"))))
  }
}

/* Pure status <-> VARCHAR converters (real logic; transpiled). */
def orderStatusToStr(s: OrderStatus): String = s match
  case Approved  => "Approved"
  case Delivered => "Delivered"
  case Placed    => "Placed"

def strToOrderStatus(s: String): OrderStatus =
  if s == "Approved" then Approved else if s == "Delivered" then Delivered else Placed
