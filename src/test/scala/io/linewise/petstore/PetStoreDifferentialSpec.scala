package io.linewise.petstore

import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.{World, HasPets, HasOrders, HasUsers}
import io.linewise.petstore.generated.{InMemPetRepository, InMemOrderRepository, InMemUserRepository}
import io.linewise.petstore.generated.{JdbcPetRepository, JdbcOrderRepository, JdbcUserRepository}
import io.linewise.petstore.generated.{PetService, OrderService, UserService}
import io.linewise.petstore.generated.JdbcSupport.Conn

/* =============================================================================
 * PET-STORE DIFFERENTIAL SPEC — the machine-checked DRIFT GATE.
 *
 * Drives the SAME service-call sequence over BOTH realizations of the abstract,
 * transpiler-GENERATED repositories — the in-memory ORACLE (InMem*Repository, a real
 * list) and the TRUSTED ambient @extern per-op JDBC (Jdbc*Repository over a fresh H2)
 * — and asserts they produce the IDENTICAL result at every step: the same accept /
 * reject verdict (with the same ValidationError) and the same row read back by id.
 *
 * The JDBC repositories are field-less; their connection is supplied per call by
 * ConnProvider.withConn (an ambient/pooled connection in production). The verified
 * service logic (validation, branching) is shared; this gate proves the @extern JDBC
 * realization agrees with the verified oracle. Steps respect the schema's foreign
 * keys (a user and a pet exist before an order references them).
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

    // a write step returning Either: run on the threaded in-memory world and on the
    // (stateless) JDBC world under the ambient connection; assert identical verdicts.
    def stepU(f: World => (World, Either[ValidationError, User])): Unit =
      val (mw, mr) = f(memW); memW = mw
      val dr = ConnProvider.withConn(conn)(f(jdbcW)._2)
      assertEquals(mr, dr, "User write verdict diverged")
    def stepP(f: World => (World, Either[ValidationError, Pet])): Unit =
      val (mw, mr) = f(memW); memW = mw
      val dr = ConnProvider.withConn(conn)(f(jdbcW)._2)
      assertEquals(mr, dr, "Pet write verdict diverged")

    def getU(id: Long): Unit =
      assertEquals(users.getUser(memW, id), ConnProvider.withConn(conn)(users.getUser(jdbcW, id)), s"getUser($id) diverged")
    def getP(id: Long): Unit =
      assertEquals(pets.get(memW, id), ConnProvider.withConn(conn)(pets.get(jdbcW, id)), s"getPet($id) diverged")
    def getO(id: Long): Unit =
      assertEquals(orders.get(memW, id), ConnProvider.withConn(conn)(orders.get(jdbcW, id)), s"getOrder($id) diverged")

    val alice = User("alice", "Alice", "Smith", "a@x", "h1", "555", None, Customer)
    val rex   = Pet("Rex", "Dog", "good boy", Available, List("friendly"), List("u1"), None)

    // create user(1); a duplicate userName is rejected the same way on both
    stepU(w => users.createUser(w, alice, 1L))
    stepU(w => users.createUser(w, alice, 99L))   // both -> Left(UserAlreadyExistsError(alice))
    // create pet(2); a duplicate name+category is rejected the same way on both
    stepP(w => pets.create(w, rex, 2L))
    stepP(w => pets.create(w, rex, 98L))          // both -> Left(PetAlreadyExistsError(rex))
    // place order(3) referencing the existing pet 2 and user 1 (FK-safe)
    val placedMem = orders.placeOrder(memW, Order(2L, Some(1700000000000L), Placed, false, None, Some(1L)), 3L)
    memW = placedMem._1
    ConnProvider.withConn(conn)(orders.placeOrder(jdbcW, Order(2L, Some(1700000000000L), Placed, false, None, Some(1L)), 3L))

    // reads of present and absent ids agree (round-tripped row == oracle row)
    getU(1L); getU(99L)
    getP(2L); getP(77L)
    getO(3L); getO(88L)

    // delete the order, then the pet, then the user (no FK dependents left); both agree
    memW = orders.delete(memW, 3L); ConnProvider.withConn(conn)(orders.delete(jdbcW, 3L))
    getO(3L)
    memW = pets.delete(memW, 2L); ConnProvider.withConn(conn)(pets.delete(jdbcW, 2L))
    getP(2L)
    memW = users.deleteUser(memW, 1L); ConnProvider.withConn(conn)(users.deleteUser(jdbcW, 1L))
    getU(1L)
  }
