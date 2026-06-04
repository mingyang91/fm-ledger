package io.linewise.petstore

import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.{World, HasPets, HasOrders, HasUsers}
import io.linewise.petstore.generated.{InMemPetRepository, InMemOrderRepository, InMemUserRepository}
import io.linewise.petstore.generated.{JdbcPetRepository, JdbcOrderRepository, JdbcUserRepository}
import io.linewise.petstore.generated.{PetService, OrderService, UserService}
import io.linewise.petstore.generated.JdbcSupport.Conn

/* =============================================================================
 * PET-STORE DIFFERENTIAL SPEC — the machine-checked DRIFT GATE for the trusted
 * @extern per-op JDBC.
 *
 * Drives the SAME service-call sequence over BOTH realizations of the abstract,
 * transpiler-GENERATED repositories — the in-memory ORACLE (InMem*Repository, a real
 * list) and the TRUSTED ambient @extern per-op JDBC (Jdbc*Repository over a fresh H2)
 * — and asserts they agree at every step: the same accept/reject verdict and the same
 * rows read back. EVERY repository op is exercised so a wrong column, malformed clause,
 * or divergent shape in any @extern SQL turns this red: save (create/place), get,
 * UPDATE, DELETE, findByNameAndCategory, findByStatus, findByTag, and list — plus the
 * nullable ORDERS.SHIP_DATE both as Some(ts) and as None (the setNull / null->None path).
 *
 * The JDBC repositories are field-less; their connection is supplied per call by
 * ConnProvider.withConn. Steps respect the schema's foreign keys (a user and a pet exist
 * before an order references them; orders are deleted before their pet/user).
 * ========================================================================== */
class PetStoreDifferentialSpec extends munit.FunSuite:

  private val users  = UserService[World](HasUsers())
  private val pets   = PetService[World](HasPets())
  private val orders = OrderService[World](HasOrders())

  private def freshConn(): Conn =
    val c = Jdbc.h2(s"petdiff_${java.util.UUID.randomUUID}")
    Jdbc.initSchema(c)
    Conn(c)

  test("User/Pet/Order services agree on the in-memory oracle and the ambient @extern JDBC") {
    val jdbcW: World = World(JdbcPetRepository(), JdbcOrderRepository(), JdbcUserRepository())
    val conn = freshConn()
    var memW: World = World(InMemPetRepository(Nil), InMemOrderRepository(Nil), InMemUserRepository(Nil))

    // a write returning Either: run on the threaded in-memory world and on the
    // (stateless) JDBC world under the ambient connection; assert identical verdicts.
    def stepU(f: World => (World, Either[ValidationError, User])): Unit =
      val (mw, mr) = f(memW); memW = mw
      assertEquals(mr, ConnProvider.withConn(conn)(f(jdbcW)._2), "User write verdict diverged")
    def stepP(f: World => (World, Either[ValidationError, Pet])): Unit =
      val (mw, mr) = f(memW); memW = mw
      assertEquals(mr, ConnProvider.withConn(conn)(f(jdbcW)._2), "Pet write verdict diverged")
    def placeBoth(order: Order, id: Long): Unit =
      val (mw, mo) = orders.placeOrder(memW, order, id); memW = mw
      assertEquals(mo, ConnProvider.withConn(conn)(orders.placeOrder(jdbcW, order, id)._2), "Order place result diverged")

    def getU(id: Long): Unit =
      assertEquals(users.getUser(memW, id), ConnProvider.withConn(conn)(users.getUser(jdbcW, id)), s"getUser($id) diverged")
    def getP(id: Long): Unit =
      assertEquals(pets.get(memW, id), ConnProvider.withConn(conn)(pets.get(jdbcW, id)), s"getPet($id) diverged")
    def getO(id: Long): Unit =
      assertEquals(orders.get(memW, id), ConnProvider.withConn(conn)(orders.get(jdbcW, id)), s"getOrder($id) diverged")

    // query ops return lists; their order is unspecified for JDBC (no ORDER BY on the
    // findBy*), so compare sorted by id.
    def petsEq(label: String)(f: World => List[Pet]): Unit =
      assertEquals(f(memW).sortBy(_.id.getOrElse(0L)),
        ConnProvider.withConn(conn)(f(jdbcW)).sortBy(_.id.getOrElse(0L)), s"$label diverged")
    def usersEq(label: String)(f: World => List[User]): Unit =
      assertEquals(f(memW).sortBy(_.id.getOrElse(0L)),
        ConnProvider.withConn(conn)(f(jdbcW)).sortBy(_.id.getOrElse(0L)), s"$label diverged")

    val alice = User("alice", "Alice", "Smith", "a@x", "h1", "555", None, Customer)
    val bob   = User("bob", "Bob", "Stone", "b@x", "h2", "666", None, Admin)
    val rex      = Pet("Rex", "Dog", "good boy", Available, List("friendly"), List("u1"), None)
    val whiskers = Pet("Whiskers", "Cat", "aloof", Pending, List("indoor"), Nil, None)

    // create users (1,2); a duplicate userName is rejected the same way on both
    stepU(w => users.createUser(w, alice, 1L))
    stepU(w => users.createUser(w, alice, 99L)) // both -> Left(UserAlreadyExistsError)
    stepU(w => users.createUser(w, bob, 2L))
    // create pets (10,11); a duplicate name+category is rejected the same way on both
    stepP(w => pets.create(w, rex, 10L))
    stepP(w => pets.create(w, rex, 98L)) // both -> Left(PetAlreadyExistsError)
    stepP(w => pets.create(w, whiskers, 11L))

    // UPDATE pet 10 and user 1, then read back (exercises the UPDATE ... WHERE ID SQL)
    stepP(w => pets.update(w, rex.copy(id = Some(10L), bio = "very good boy")))
    getP(10L)
    stepU(w => users.update(w, alice.copy(id = Some(1L), lastName = "Smithe")))
    getU(1L)

    // place order 20 (shipDate present) and order 21 (shipDate None -> setNull / null->None)
    placeBoth(Order(10L, Some(1700000000000L), Placed, false, None, Some(1L)), 20L)
    placeBoth(Order(11L, None, Approved, true, None, Some(2L)), 21L)

    // reads of present and absent ids agree (round-tripped row == oracle row)
    getU(1L); getU(99L)
    getP(10L); getP(77L)
    getO(20L); getO(21L); getO(88L)

    // query ops agree (UPDATE moved nothing out of these result sets)
    petsEq("findByNameAndCategory")(w => w.pets.findByNameAndCategory("Rex", "Dog"))
    petsEq("findByStatus")(w => pets.findByStatus(w, List(Available)))
    petsEq("findByTag")(w => pets.findByTag(w, List("friendly")))
    petsEq("listPets")(w => pets.list(w, 50, 0))
    usersEq("listUsers")(w => users.list(w, 50, 0))

    // delete orders first (FK), then pets, then users; both agree
    memW = orders.delete(memW, 20L); ConnProvider.withConn(conn)(orders.delete(jdbcW, 20L)); getO(20L)
    memW = orders.delete(memW, 21L); ConnProvider.withConn(conn)(orders.delete(jdbcW, 21L)); getO(21L)
    memW = pets.delete(memW, 10L); ConnProvider.withConn(conn)(pets.delete(jdbcW, 10L)); getP(10L)
    memW = users.deleteUser(memW, 1L); ConnProvider.withConn(conn)(users.deleteUser(jdbcW, 1L)); getU(1L)
  }
