package io.linewise.petstore

import io.linewise.petstore.generated.PetStoreModel.*

/* =============================================================================
 * PET-STORE DEMO — the translated scala-pet-store driven end-to-end with NO
 * cats-effect and NO http4s. The SERVICE layer (PetService/OrderService/
 * UserService) composes the verified validation over plain-JDBC repositories on
 * the isomorphic tables. Exercises every service capability.
 *
 * Run: ./mill runMain io.linewise.petstore.PetStoreMain
 * ========================================================================== */
object PetStoreMain:
  def main(args: Array[String]): Unit =
    val conn    = Jdbc.h2("petstore_demo")
    val petRepo = JdbcPetRepository(conn); petRepo.initSchema()
    val pets    = PetService(petRepo)
    val users   = UserService(JdbcUserRepository(conn))
    val orders  = OrderService(JdbcOrderRepository(conn))

    def line(s: String): Unit = println(s)
    def rule(): Unit = println("-" * 78)

    line("=" * 78)
    line("SCALA-PET-STORE, TRANSLATED TO STAINLESS  (no cats-effect, no http4s)")
    line("  Service layer (PetService/OrderService/UserService) -> verified validation")
    line("  -> Repository layer (in-memory verified core | plain-JDBC isomorphic tables)")
    line("=" * 78)
    line("DDL: created PET, USERS, ORDERS, JWT (the post-migration schema, verbatim).")
    rule()

    val alice = users.createUser(User("alice", "Alice", "Smith", "alice@x", "hash1", "555", None, Customer))
    val bob   = users.createUser(User("bob", "Bob", "Stone", "bob@x", "hash2", "666", None, Admin))
    val dupU  = users.createUser(User("alice", "Al", "S", "a2@x", "h", "0", None, Customer))
    line(s"UserService:  createUser(alice)=${shortU(alice)}  createUser(bob)=${shortU(bob)}")
    line(s"              createUser(alice again) -> ${shortU(dupU)}  (validation)")
    line(s"              getUserByName(bob)=${shortU(users.getUserByName("bob"))}  list=${users.list(10, 0).length}")
    rule()

    val rex  = pets.create(Pet("Rex", "Dog", "good boy", Available, List("friendly", "trained"), List("u1"), None))
    val cat  = pets.create(Pet("Whiskers", "Cat", "aloof", Pending, List("indoor"), Nil, None))
    val dupP = pets.create(Pet("Rex", "Dog", "good boy", Adopted, Nil, Nil, None))
    line(s"PetService:   create(Rex)=${shortP(rex)}  create(Whiskers)=${shortP(cat)}")
    line(s"              create(Rex again) -> ${shortP(dupP)}  (validation)")
    line(s"              findByStatus([Available])=${pets.findByStatus(List(Available)).map(_.name).mkString(",")}")
    line(s"              findByTag([indoor])=${pets.findByTag(List("indoor")).map(_.name).mkString(",")}")
    rule()

    val order = orders.placeOrder(Order(1L, Some(1700000000000L), Placed, false, None, Some(1L)))
    line(s"OrderService: placeOrder(pet=1, user=1)=${shortO(order)}")
    line(s"              get(1)=${shortOE(orders.get(1L))}  get(99)=${orders.get(99L)}")
    orders.delete(1L)
    line(s"              after delete(1): get(1)=${orders.get(1L)}")
    rule()

    val ok =
      alice.isRight && bob.isRight && dupU.isLeft &&
      rex.isRight && dupP.isLeft && order.id == Some(1L) && orders.get(1L) == Left(OrderNotFoundError)
    line(s"RESULT: every PetService/OrderService/UserService capability ran on the")
    line(s"        isomorphic tables with no cats-effect/http4s -> ${if ok then "OK" else "WRONG"}")

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
