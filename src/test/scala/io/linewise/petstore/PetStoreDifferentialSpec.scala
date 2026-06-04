package io.linewise.petstore

import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.{World, HasPets, HasOrders, HasUsers}
import io.linewise.petstore.generated.{InMemPetRepository, InMemOrderRepository, InMemUserRepository}
import io.linewise.petstore.generated.{JdbcPetRepository, JdbcOrderRepository, JdbcUserRepository}
import io.linewise.petstore.generated.{PetService, OrderService, UserService}

/* =============================================================================
 * PET-STORE DIFFERENTIAL SPEC — the machine-checked DRIFT GATE for the trusted
 * @extern-over-Quill JDBC realization.
 *
 * Drives the SAME service-call sequence over BOTH realizations of the abstract,
 * transpiler-GENERATED repositories — the in-memory ORACLE (InMem*Repository, a real
 * list) and the TRUSTED Jdbc*Repository, whose @extern bodies delegate to the production
 * Quill `Db` (bound here over a fresh in-memory H2) — and asserts they agree at every
 * step. EVERY repository op is exercised so a wrong column, malformed clause, or
 * divergent shape in the Quill queries turns this red: save (create/place), get, UPDATE,
 * DELETE, findByNameAndCategory, findByStatus, findByTag, list — plus the nullable
 * ORDERS.SHIP_DATE both as Some(ts) and as None.
 *
 * Quill owns the connection (pool-per-run); there is no ambient connection to bind.
 * Steps respect the schema's foreign keys (a user and a pet exist before an order).
 * ========================================================================== */
class PetStoreDifferentialSpec extends munit.FunSuite:

  private val users  = UserService[World](HasUsers())
  private val pets   = PetService[World](HasPets())
  private val orders = OrderService[World](HasOrders())

  /** A fresh isolated H2 + schema, bound as the Quill DAO behind the JDBC repos. */
  private def freshDb(): Unit =
    val ds = Jdbc.dataSource(s"petdiff_${java.util.UUID.randomUUID}")
    val c0 = ds.getConnection()
    try Jdbc.initSchema(c0) finally c0.close()
    Db.init(ds)

  test("User/Pet/Order services agree on the in-memory oracle and the Quill JDBC realization") {
    freshDb()
    val jdbcW: World = World(JdbcPetRepository(), JdbcOrderRepository(), JdbcUserRepository())
    var memW: World = World(InMemPetRepository(Nil), InMemOrderRepository(Nil), InMemUserRepository(Nil))

    // a write returning Either: run on the threaded in-memory world and on the JDBC
    // world (which delegates to Quill); assert identical verdicts.
    def stepU(f: World => (World, Either[ValidationError, User])): Unit =
      val (mw, mr) = f(memW); memW = mw
      assertEquals(mr, f(jdbcW)._2, "User write verdict diverged")
    def stepP(f: World => (World, Either[ValidationError, Pet])): Unit =
      val (mw, mr) = f(memW); memW = mw
      assertEquals(mr, f(jdbcW)._2, "Pet write verdict diverged")
    def placeBoth(order: Order, id: Long): Unit =
      val (mw, mo) = orders.placeOrder(memW, order, id); memW = mw
      assertEquals(mo, orders.placeOrder(jdbcW, order, id)._2, "Order place result diverged")

    def getU(id: Long): Unit = assertEquals(users.getUser(memW, id), users.getUser(jdbcW, id), s"getUser($id) diverged")
    def getP(id: Long): Unit = assertEquals(pets.get(memW, id), pets.get(jdbcW, id), s"getPet($id) diverged")
    def getO(id: Long): Unit = assertEquals(orders.get(memW, id), orders.get(jdbcW, id), s"getOrder($id) diverged")

    // query ops return lists; JDBC order is unspecified, so compare sorted by id.
    def petsEq(label: String)(f: World => List[Pet]): Unit =
      assertEquals(f(memW).sortBy(_.id.getOrElse(0L)), f(jdbcW).sortBy(_.id.getOrElse(0L)), s"$label diverged")
    def usersEq(label: String)(f: World => List[User]): Unit =
      assertEquals(f(memW).sortBy(_.id.getOrElse(0L)), f(jdbcW).sortBy(_.id.getOrElse(0L)), s"$label diverged")

    val alice = User("alice", "Alice", "Smith", "a@x", "h1", "555", None, Customer)
    val bob   = User("bob", "Bob", "Stone", "b@x", "h2", "666", None, Admin)
    val rex      = Pet("Rex", "Dog", "good boy", Available, List("friendly"), List("u1"), None)
    val whiskers = Pet("Whiskers", "Cat", "aloof", Pending, List("indoor"), List(""), None) // photoUrls=List("") exercises the empty-element codec edge

    // create users (1,2); a duplicate userName is rejected the same way on both
    stepU(w => users.createUser(w, alice, 1L))
    stepU(w => users.createUser(w, alice, 99L)) // both -> Left(UserAlreadyExistsError)
    stepU(w => users.createUser(w, bob, 2L))
    // create pets (10,11); a duplicate name+category is rejected the same way on both
    stepP(w => pets.create(w, rex, 10L))
    stepP(w => pets.create(w, rex, 98L)) // both -> Left(PetAlreadyExistsError)
    stepP(w => pets.create(w, whiskers, 11L))

    // UPDATE pet 10 and user 1, then read back (exercises the UPDATE queries)
    stepP(w => pets.update(w, rex.copy(id = Some(10L), bio = "very good boy")))
    getP(10L)
    stepU(w => users.update(w, alice.copy(id = Some(1L), lastName = "Smithe")))
    getU(1L)

    // place order 20 (shipDate present) and order 21 (shipDate None -> SHIP_DATE NULL)
    placeBoth(Order(10L, Some(1700000000000L), Placed, false, None, Some(1L)), 20L)
    placeBoth(Order(11L, None, Approved, true, None, Some(2L)), 21L)

    // reads of present and absent ids agree (round-tripped row == oracle row)
    getU(1L); getU(99L)
    getP(10L); getP(11L); getP(77L)
    getO(20L); getO(21L); getO(88L)

    // query ops agree
    petsEq("findByNameAndCategory")(w => w.pets.findByNameAndCategory("Rex", "Dog"))
    petsEq("findByStatus")(w => pets.findByStatus(w, List(Available)))
    petsEq("findByTag")(w => pets.findByTag(w, List("friendly")))
    petsEq("listPets")(w => pets.list(w, 50, 0))
    usersEq("listUsers")(w => users.list(w, 50, 0))

    // delete orders first (FK), then pets, then users; both agree
    memW = orders.delete(memW, 20L); orders.delete(jdbcW, 20L); getO(20L)
    memW = orders.delete(memW, 21L); orders.delete(jdbcW, 21L); getO(21L)
    memW = pets.delete(memW, 10L); pets.delete(jdbcW, 10L); getP(10L)
    memW = users.deleteUser(memW, 1L); users.deleteUser(jdbcW, 1L); getU(1L)
  }
