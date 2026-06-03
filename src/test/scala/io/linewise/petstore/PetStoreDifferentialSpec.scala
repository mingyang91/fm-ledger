package io.linewise.petstore

import io.linewise.petstore.generated.PetStoreModel.*

/* =============================================================================
 * PET-STORE DIFFERENTIAL SPEC — the drift gate, now over the layered design. The
 * SAME service (PetService / OrderService / UserService) is run over BOTH the
 * verified in-memory repository (oracle) and the plain-JDBC repository (isomorphic
 * tables), asserting they agree at every step. No cats-effect. Fresh H2 per test.
 * ========================================================================== */
class PetStoreDifferentialSpec extends munit.FunSuite:

  test("Pet: PetService agrees over in-memory and JDBC repositories") {
    val mem = PetService(InMemoryPetRepository())
    val jr  = JdbcPetRepository(Jdbc.h2("pet_" + java.util.UUID.randomUUID)); jr.initSchema()
    val jdb = PetService(jr)
    def agree[A](f: PetService => A): A = { val a = f(mem); val b = f(jdb); assertEquals(a, b); a }

    val rex = Pet("Rex", "Dog", "good boy", Available, List("friendly", "trained"), List("u1"), None)
    assert(agree(_.create(rex)).isRight)                                   // id = 1 on both
    assertEquals(agree(_.create(rex)), Left(PetAlreadyExistsError(rex)))   // duplicate refused
    assert(agree(_.create(Pet("Whiskers", "Cat", "aloof", Pending, List("indoor"), Nil, None))).isRight) // id = 2

    assert(agree(_.get(1L)).isRight)
    assertEquals(agree(_.get(99L)), Left(PetNotFoundError))

    val rexUpdated = rex.copy(bio = "great boy", id = Some(1L))
    assertEquals(agree(_.update(rexUpdated)), Right(rexUpdated))
    assertEquals(agree(_.update(rex.copy(id = Some(99L)))), Left(PetNotFoundError))

    agree(_.findByStatus(List(Available)))
    agree(_.findByStatus(List(Pending, Adopted)))
    agree(_.findByTag(List("indoor")))
    agree(_.list(10, 0))

    agree(_.delete(1L))
    assertEquals(agree(_.get(1L)), Left(PetNotFoundError))
    agree(_.list(10, 0))
  }

  test("Order: OrderService agrees over in-memory and JDBC repositories") {
    val conn = Jdbc.h2("order_" + java.util.UUID.randomUUID)
    val petRepo = JdbcPetRepository(conn); petRepo.initSchema()
    petRepo.create(Pet("Rex", "Dog", "gb", Available, Nil, Nil, None))                 // PET id 1 (FK)
    JdbcUserRepository(conn).create(User("alice", "A", "L", "a@x", "h", "p", None, Customer)) // USERS id 1 (FK)

    val mem = OrderService(InMemoryOrderRepository())
    val jdb = OrderService(JdbcOrderRepository(conn))
    def agree[A](f: OrderService => A): A = { val a = f(mem); val b = f(jdb); assertEquals(a, b); a }

    val o1 = Order(1L, Some(1000L), Placed, false, None, Some(1L))
    assertEquals(agree(_.placeOrder(o1)).id, Some(1L))
    assert(agree(_.get(1L)).isRight)
    assertEquals(agree(_.get(99L)), Left(OrderNotFoundError))
    agree(_.delete(1L))
    assertEquals(agree(_.get(1L)), Left(OrderNotFoundError))
  }

  test("User: UserService agrees over in-memory and JDBC repositories") {
    val mem = UserService(InMemoryUserRepository())
    val jr  = JdbcUserRepository(Jdbc.h2("user_" + java.util.UUID.randomUUID)); jr.initSchema()
    val jdb = UserService(jr)
    def agree[A](f: UserService => A): A = { val a = f(mem); val b = f(jdb); assertEquals(a, b); a }

    val alice = User("alice", "Alice", "L", "a@x", "hash", "555", None, Customer)
    assert(agree(_.createUser(alice)).isRight)                                  // id = 1
    assertEquals(agree(_.createUser(alice)), Left(UserAlreadyExistsError(alice))) // duplicate userName
    assert(agree(_.createUser(User("bob", "Bob", "B", "b@x", "h", "666", None, Admin))).isRight) // id = 2

    assert(agree(_.getUser(1L)).isRight)
    assert(agree(_.getUserByName("bob")).isRight)
    assertEquals(agree(_.getUserByName("nobody")), Left(UserNotFoundError))

    val aliceUpd = alice.copy(email = "alice@y", id = Some(1L))
    assertEquals(agree(_.update(aliceUpd)), Right(aliceUpd))
    assertEquals(agree(_.update(alice.copy(id = Some(99L)))), Left(UserNotFoundError))

    agree(_.list(10, 0))
    agree(_.deleteByUserName("bob"))
    assertEquals(agree(_.getUserByName("bob")), Left(UserNotFoundError))
    agree(_.list(10, 0))
  }
