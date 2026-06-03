package io.linewise.petstore

import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.{World, HasPets, HasOrders, HasUsers}
import io.linewise.petstore.generated.{PetRepository, OrderRepository, UserRepository}
import io.linewise.petstore.generated.{PetService, OrderService, UserService}

/* =============================================================================
 * PET-STORE DEMO — the World + Has-lens design, driven end-to-end with NO
 * cats-effect and NO http4s. Each service is polymorphic in the world W and wired
 * by a lens; here W is the in-memory `World` value, threaded through the calls.
 * Every PetService/OrderService/UserService capability is exercised.
 *
 * Run: ./mill runMain io.linewise.petstore.PetStoreMain
 * ========================================================================== */
object PetStoreMain:
  def main(args: Array[String]): Unit =
    var w: World = World(PetRepository(Nil), OrderRepository(Nil), UserRepository(Nil))
    val users  = UserService[World](HasUsers())
    val pets   = PetService[World](HasPets())
    val orders = OrderService[World](HasOrders())

    var nextId = 1L
    def fresh(): Long = { val id = nextId; nextId += 1; id }

    def line(s: String): Unit = println(s)
    def rule(): Unit = println("-" * 78)

    line("=" * 78)
    line("SCALA-PET-STORE — WORLD + Has-lens design  (no cats-effect, no http4s)")
    line("  service[W](has: Has[W,Repo]) reads has.get(w), writes has(w).write(_.save(..))")
    line("  W = the in-memory World value, threaded through the calls")
    line("=" * 78)

    val (w1, ra) = users.createUser(w, User("alice", "Alice", "Smith", "a@x", "h1", "555", None, Customer), fresh()); w = w1
    val (w2, rb) = users.createUser(w, User("bob", "Bob", "Stone", "b@x", "h2", "666", None, Admin), fresh()); w = w2
    val (w3, rd) = users.createUser(w, User("alice", "Al", "S", "a2@x", "h", "0", None, Customer), fresh()); w = w3
    line(s"UserService:  createUser(alice)=${shortU(ra)}  createUser(bob)=${shortU(rb)}")
    line(s"              createUser(alice again) -> ${shortU(rd)}  (validation)")
    line(s"              getUserByName(bob)=${shortU(users.getUserByName(w, "bob"))}  list=${users.list(w, 10, 0).length}")
    rule()

    val (w4, rp) = pets.create(w, Pet("Rex", "Dog", "good boy", Available, List("friendly"), List("u1"), None), fresh()); w = w4
    val (w5, rc) = pets.create(w, Pet("Whiskers", "Cat", "aloof", Pending, List("indoor"), Nil, None), fresh()); w = w5
    val (w6, rdp) = pets.create(w, Pet("Rex", "Dog", "good boy", Adopted, Nil, Nil, None), fresh()); w = w6
    line(s"PetService:   create(Rex)=${shortP(rp)}  create(Whiskers)=${shortP(rc)}")
    line(s"              create(Rex again) -> ${shortP(rdp)}  (validation)")
    line(s"              findByStatus([Available])=${pets.findByStatus(w, List(Available)).map(_.name).mkString(",")}")
    line(s"              findByTag([indoor])=${pets.findByTag(w, List("indoor")).map(_.name).mkString(",")}")
    rule()

    val (w7, order) = orders.placeOrder(w, Order(4L, Some(1700000000000L), Placed, false, None, Some(1L)), fresh()); w = w7
    line(s"OrderService: placeOrder(pet=4, user=1)=${shortO(order)}")
    line(s"              get(${order.id.getOrElse(0L)})=${shortOE(orders.get(w, order.id.getOrElse(0L)))}  get(99)=${orders.get(w, 99L)}")
    val w8 = orders.delete(w, order.id.getOrElse(0L)); w = w8
    line(s"              after delete: get=${orders.get(w, order.id.getOrElse(0L))}")
    rule()

    val ok = ra.isRight && rb.isRight && rd.isLeft && rp.isRight && rdp.isLeft &&
      orders.get(w, order.id.getOrElse(0L)) == Left(OrderNotFoundError)
    line(s"RESULT: every service capability ran by threading the World through lenses,")
    line(s"        no cats-effect/http4s -> ${if ok then "OK" else "WRONG"}")

  private def shortP(e: Either[ValidationError, Pet]): String = e match
    case Right(p) => s"Pet#${p.id.getOrElse(0L)}(${p.name})"
    case Left(er) => s"REJECTED(${er.getClass.getSimpleName.stripSuffix("$")})"
  private def shortU(e: Either[ValidationError, User]): String = e match
    case Right(u) => s"User#${u.id.getOrElse(0L)}(${u.userName})"
    case Left(er) => s"REJECTED(${er.getClass.getSimpleName.stripSuffix("$")})"
  private def shortOE(e: Either[ValidationError, Order]): String = e match
    case Right(o) => s"Order#${o.id.getOrElse(0L)}(pet=${o.petId},${o.status})"
    case Left(er) => s"REJECTED(${er.getClass.getSimpleName.stripSuffix("$")})"
  private def shortO(o: Order): String = s"Order#${o.id.getOrElse(0L)}(pet=${o.petId},${o.status})"
