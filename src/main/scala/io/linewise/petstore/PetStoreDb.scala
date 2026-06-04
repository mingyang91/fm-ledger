package io.linewise.petstore

import javax.sql.DataSource
import java.sql.Timestamp

import io.getquill.*

import io.linewise.petstore.generated.PetStoreModel.*

/* =============================================================================
 * QUILL persistence for the pet store — the production realization the verified,
 * field-less @extern repositories delegate to (via the `Db` facade). Replaces the raw
 * java.sql plumbing: Quill owns the DataSource (HikariCP / a supplied pool) and binds a
 * connection per `run`, so it is thread-safe and there is no shared java.sql.Connection.
 * Queries compile to SQL at COMPILE TIME (protoquill macros).
 *
 * Mapping is the recommended "flat ROW case class + plain-Scala conversion" shape: the
 * Row types mirror the columns 1:1 (List[String] and the enums packed/encoded as
 * VARCHAR), and a NamingStrategy(SnakeCase, UpperCase) maps camelCase fields to the
 * UPPER_SNAKE columns; only the table names are overridden via querySchema.
 * ========================================================================== */
final case class PetRow(id: Long, name: String, category: String, bio: String,
    status: String, photoUrls: String, tags: String)
final case class UserRow(id: Long, userName: String, firstName: String, lastName: String,
    email: String, hash: String, phone: String, role: String)
final case class OrderRow(id: Long, petId: Long, shipDate: Option[Timestamp],
    status: String, complete: Boolean, userId: Long)

final class PetStoreDb(ds: DataSource):
  private val ctx = new H2JdbcContext(NamingStrategy(SnakeCase, UpperCase), ds)
  import ctx.*

  private inline def pets   = quote { querySchema[PetRow]("PET") }
  private inline def users  = quote { querySchema[UserRow]("USERS") }
  private inline def orders = quote { querySchema[OrderRow]("ORDERS") }

  // --- domain <-> row (plain Scala) ---
  private def petStatusStr(s: PetStatus): String = s match
    case Available => "Available"; case Pending => "Pending"; case Adopted => "Adopted"
  private def strPetStatus(s: String): PetStatus = s match
    case "Available" => Available; case "Pending" => Pending; case "Adopted" => Adopted
    case other => throw new IllegalStateException(s"Unknown PET.STATUS value: '$other'")
  private def orderStatusStr(s: OrderStatus): String = s match
    case Approved => "Approved"; case Delivered => "Delivered"; case Placed => "Placed"
  private def strOrderStatus(s: String): OrderStatus = s match
    case "Approved" => Approved; case "Delivered" => Delivered; case "Placed" => Placed
    case other => throw new IllegalStateException(s"Unknown ORDERS.STATUS value: '$other'")

  private def petToRow(p: Pet): PetRow =
    PetRow(p.id.getOrElse(0L), p.name, p.category, p.bio, petStatusStr(p.status),
      Jdbc.encodeList(p.photoUrls), Jdbc.encodeList(p.tags))
  private def rowToPet(r: PetRow): Pet =
    Pet(r.name, r.category, r.bio, strPetStatus(r.status), Jdbc.decodeList(r.tags),
      Jdbc.decodeList(r.photoUrls), Some(r.id))
  private def userToRow(u: User): UserRow =
    UserRow(u.id.getOrElse(0L), u.userName, u.firstName, u.lastName, u.email, u.hash, u.phone, u.role.roleRepr)
  private def rowToUser(r: UserRow): User =
    User(r.userName, r.firstName, r.lastName, r.email, r.hash, r.phone, Some(r.id), Role(r.role))
  private def orderToRow(o: Order): OrderRow =
    OrderRow(o.id.getOrElse(0L), o.petId, o.shipDate.map(ms => new Timestamp(ms)),
      orderStatusStr(o.status), o.complete, o.userId.getOrElse(0L))
  private def rowToOrder(r: OrderRow): Order =
    Order(r.petId, r.shipDate.map(_.getTime), strOrderStatus(r.status), r.complete, Some(r.id), Some(r.userId))

  // --- pets ---
  def insertPet(p: Pet): Unit = { val r = petToRow(p); ctx.run(quote { pets.insertValue(lift(r)) }); () }
  def petById(id: Long): Option[Pet] = ctx.run(quote { pets.filter(_.id == lift(id)) }).headOption.map(rowToPet)
  def updatePet(p: Pet): Unit = { val r = petToRow(p); ctx.run(quote { pets.filter(_.id == lift(r.id)).updateValue(lift(r)) }); () }
  def deletePet(id: Long): Unit = { ctx.run(quote { pets.filter(_.id == lift(id)).delete }); () }
  def petsByNameAndCategory(name: String, category: String): List[Pet] =
    ctx.run(quote { pets.filter(p => p.name == lift(name) && p.category == lift(category)) }).map(rowToPet)
  def petsByStatus(statuses: List[PetStatus]): List[Pet] =
    if statuses.isEmpty then Nil
    else
      val ns = statuses.map(petStatusStr)
      ctx.run(quote { pets.filter(p => liftQuery(ns).contains(p.status)) }).map(rowToPet)
  def petsByTag(tags: List[String]): List[Pet] =
    ctx.run(quote { pets }).map(rowToPet).filter(p => tags.exists(t => p.tags.contains(t)))
  def petsList(pageSize: Int, offset: Int): List[Pet] =
    ctx.run(quote { pets.sortBy(p => p.id)(using Ord.desc).drop(lift(offset)).take(lift(pageSize)) }).map(rowToPet)

  // --- users ---
  def insertUser(u: User): Unit = { val r = userToRow(u); ctx.run(quote { users.insertValue(lift(r)) }); () }
  def userById(id: Long): Option[User] = ctx.run(quote { users.filter(_.id == lift(id)) }).headOption.map(rowToUser)
  def userByName(name: String): Option[User] = ctx.run(quote { users.filter(_.userName == lift(name)) }).headOption.map(rowToUser)
  def updateUser(u: User): Unit = { val r = userToRow(u); ctx.run(quote { users.filter(_.id == lift(r.id)).updateValue(lift(r)) }); () }
  def deleteUser(id: Long): Unit = { ctx.run(quote { users.filter(_.id == lift(id)).delete }); () }
  def deleteUserByName(name: String): Unit = { ctx.run(quote { users.filter(_.userName == lift(name)).delete }); () }
  def usersList(pageSize: Int, offset: Int): List[User] =
    ctx.run(quote { users.sortBy(u => u.id)(using Ord.desc).drop(lift(offset)).take(lift(pageSize)) }).map(rowToUser)

  // --- orders ---
  def insertOrder(o: Order): Unit = { val r = orderToRow(o); ctx.run(quote { orders.insertValue(lift(r)) }); () }
  def orderById(id: Long): Option[Order] = ctx.run(quote { orders.filter(_.id == lift(id)) }).headOption.map(rowToOrder)
  def deleteOrder(id: Long): Unit = { ctx.run(quote { orders.filter(_.id == lift(id)).delete }); () }
