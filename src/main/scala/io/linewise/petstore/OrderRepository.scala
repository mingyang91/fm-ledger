package io.linewise.petstore

import java.sql.{Connection, ResultSet, Timestamp, Types}
import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.OrderRepository as GenOrderRepository
import io.linewise.petstore.generated.OrderRepository.OrderTable

/* =============================================================================
 * ORDER — THE REPOSITORY LAYER, production (data access). InMemoryOrderRepository
 * single-sourced from the generated OrderRepository; JdbcOrderRepository over the
 * isomorphic ORDERS table (PET_ID / USER_ID FKs; shipDate epoch <-> TIMESTAMP).
 * ========================================================================== */
trait OrderRepository:
  def snapshot: OrderTable
  def create(order: Order): Order
  def get(id: Long): Option[Order]
  def delete(id: Long): Unit

final class InMemoryOrderRepository extends OrderRepository:
  private var t: OrderTable = OrderTable(Nil)
  private var nextId: Long = 1L
  def snapshot: OrderTable = t
  def create(order: Order): Order =
    val (t2, saved) = GenOrderRepository.create(t, order, nextId); t = t2; nextId += 1; saved
  def get(id: Long): Option[Order] = GenOrderRepository.get(t, id)
  def delete(id: Long): Unit = t = GenOrderRepository.delete(t, id)._1

object OrderCodec:
  def statusStr(s: OrderStatus): String = s match
    case Approved  => "Approved"
    case Delivered => "Delivered"
    case Placed    => "Placed"
  def strStatus(s: String): OrderStatus = s match
    case "Approved"  => Approved
    case "Delivered" => Delivered
    case "Placed"    => Placed
    case other       => throw new IllegalStateException(s"unknown order status in row: $other")

final class JdbcOrderRepository(c: Connection) extends OrderRepository:
  def initSchema(): Unit = Jdbc.initSchema(c)

  private def rowToOrder(rs: ResultSet): Order =
    val ts = rs.getTimestamp("SHIP_DATE")
    Order(
      rs.getLong("PET_ID"),
      if ts == null then None else Some(ts.getTime),
      OrderCodec.strStatus(rs.getString("STATUS")),
      rs.getBoolean("COMPLETE"),
      Some(rs.getLong("ID")),
      Some(rs.getLong("USER_ID")),
    )

  def snapshot: OrderTable =
    OrderTable(Jdbc.query(c, "SELECT ID, PET_ID, SHIP_DATE, STATUS, COMPLETE, USER_ID FROM ORDERS ORDER BY ID DESC")(_ => ())(rowToOrder))

  def create(order: Order): Order =
    val id = Jdbc.insertReturningId(
      c, "INSERT INTO ORDERS (PET_ID, SHIP_DATE, STATUS, COMPLETE, USER_ID) VALUES (?,?,?,?,?)") { ps =>
      ps.setLong(1, order.petId)
      order.shipDate match
        case Some(ms) => ps.setTimestamp(2, new Timestamp(ms))
        case None     => ps.setNull(2, Types.TIMESTAMP)
      ps.setString(3, OrderCodec.statusStr(order.status))
      ps.setBoolean(4, order.complete)
      ps.setLong(5, order.userId.getOrElse(0L))
    }
    order.copy(id = Some(id))

  def get(id: Long): Option[Order] = GenOrderRepository.get(snapshot, id)

  def delete(id: Long): Unit =
    Jdbc.update(c, "DELETE FROM ORDERS WHERE ID=?")(_.setLong(1, id)); ()
