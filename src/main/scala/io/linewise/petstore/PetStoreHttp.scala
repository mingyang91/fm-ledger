package io.linewise.petstore

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import sttp.model.StatusCode
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.json.pickler.*
import sttp.tapir.server.ServerEndpoint

import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.{World, HasPets, HasOrders, HasUsers}
import io.linewise.petstore.generated.{PetService, OrderService, UserService}
import io.linewise.petstore.generated.{InMemPetRepository, InMemOrderRepository, InMemUserRepository}
import io.linewise.petstore.generated.{JdbcPetRepository, JdbcOrderRepository, JdbcUserRepository}
import io.linewise.petstore.generated.JdbcSupport.Conn

/* =============================================================================
 * PET-STORE WEB SERVER — direct-style (Ox / Identity) tapir, mirroring the original
 * scala-pet-store HTTP API. Every endpoint handler delegates its business logic to
 * the STAINLESS-TRANSPILED services (PetService / OrderService / UserService) through
 * the [W] / Has lens; only the HTTP shell (routing, JSON, status mapping) and the
 * auth layer live here. No cats-effect, no http4s.
 *
 * The service is polymorphic in the world W, so the SAME endpoints run over either
 * backing: a JDBC-backed World (production persistence, via the ambient ConnProvider)
 * or an in-memory World held in an AtomicReference (used by the translated endpoint
 * specs, exactly as the originals ran over in-memory repository interpreters).
 *
 * Auth: a "backed bearer token" — login mints an opaque token bound to the user's id;
 * a request resolves the token back to the (still-existing) user. A deleted user's
 * token no longer authenticates (the backed-identity semantic the originals rely on).
 * This is server infrastructure, deliberately NOT part of the verified core.
 * ========================================================================== */
object PetStoreJson:
  given Pickler[PetStatus]     = Pickler.derived
  given Pickler[OrderStatus]   = Pickler.derived
  given Pickler[Role]          = Pickler.derived
  given Pickler[Pet]           = Pickler.derived
  given Pickler[Order]         = Pickler.derived
  given Pickler[User]          = Pickler.derived
  given Pickler[SignupRequest] = Pickler.derived
  given Pickler[LoginRequest]  = Pickler.derived

/** The BARE endpoints (no logic, capability `Any`): shared by the server (which
  * attaches logic) and the tests (which interpret them as a typed sttp client, so
  * client and server use the identical codecs). `Err = (status, message)`. */
object PetStoreEndpoints:
  import PetStoreJson.given
  type Err = (StatusCode, String)
  private val errOut = statusCode.and(stringBody)
  private val bearer = auth.bearer[String]()

  // pets — literal /findBy* are listed before /{id} when registered (see serverEndpoints)
  val createPet       = endpoint.securityIn(bearer).post.in("pets").in(jsonBody[Pet]).out(jsonBody[Pet]).errorOut(errOut)
  val listPets        = endpoint.securityIn(bearer).get.in("pets").in(query[Option[Int]]("pageSize")).in(query[Option[Int]]("offset")).out(jsonBody[List[Pet]]).errorOut(errOut)
  val findPetsByStatus = endpoint.securityIn(bearer).get.in("pets" / "findByStatus").in(query[List[String]]("status")).out(jsonBody[List[Pet]]).errorOut(errOut)
  val findPetsByTag   = endpoint.securityIn(bearer).get.in("pets" / "findByTags").in(query[List[String]]("tags")).out(jsonBody[List[Pet]]).errorOut(errOut)
  val getPet          = endpoint.securityIn(bearer).get.in("pets" / path[Long]("id")).out(jsonBody[Pet]).errorOut(errOut)
  val updatePet       = endpoint.securityIn(bearer).put.in("pets" / path[Long]("id")).in(jsonBody[Pet]).out(jsonBody[Pet]).errorOut(errOut)
  val deletePet       = endpoint.securityIn(bearer).delete.in("pets" / path[Long]("id")).errorOut(errOut)

  // orders
  val placeOrder = endpoint.securityIn(bearer).post.in("orders").in(jsonBody[Order]).out(jsonBody[Order]).errorOut(errOut)
  val getOrder   = endpoint.securityIn(bearer).get.in("orders" / path[Long]("id")).out(jsonBody[Order]).errorOut(errOut)
  val deleteOrder = endpoint.securityIn(bearer).delete.in("orders" / path[Long]("id")).errorOut(errOut)

  // users — signup + login public; the rest admin-only (auth enforced in the logic)
  val signup        = endpoint.post.in("users").in(jsonBody[SignupRequest]).out(jsonBody[User]).errorOut(errOut)
  val login         = endpoint.post.in("users" / "login").in(jsonBody[LoginRequest]).out(jsonBody[User]).out(header[String]("Authorization")).errorOut(errOut)
  val listUsers     = endpoint.securityIn(bearer).get.in("users").in(query[Option[Int]]("pageSize")).in(query[Option[Int]]("offset")).out(jsonBody[List[User]]).errorOut(errOut)
  val getUserByName = endpoint.securityIn(bearer).get.in("users" / path[String]("userName")).out(jsonBody[User]).errorOut(errOut)
  val updateUser    = endpoint.securityIn(bearer).put.in("users" / path[String]("userName")).in(jsonBody[User]).out(jsonBody[User]).errorOut(errOut)
  val deleteUser    = endpoint.securityIn(bearer).delete.in("users" / path[String]("userName")).errorOut(errOut)

/** How a request reaches the World: read it, write-and-return a value, or mutate it.
  * The transpiled service calls run INSIDE these. */
trait Backend:
  def read[A](f: World => A): A
  def write[A](f: World => (World, A)): A
  def mutate(f: World => World): Unit

/** In-memory backing: one shared World in an AtomicReference (the analogue of the
  * originals' in-memory repository interpreters; no DB, no foreign keys). */
final class InMemBackend extends Backend:
  private val ref = AtomicReference[World](
    World(InMemPetRepository(Nil), InMemOrderRepository(Nil), InMemUserRepository(Nil)))
  def read[A](f: World => A): A = f(ref.get())
  def write[A](f: World => (World, A)): A = synchronized {
    val (w2, a) = f(ref.get()); ref.set(w2); a
  }
  def mutate(f: World => World): Unit = synchronized { ref.set(f(ref.get())) }

/** JDBC backing: a fresh field-less World per call. Each operation BORROWS its own
  * connection from the pool and binds it for the duration (java.sql.Connection is not
  * thread-safe, so connections must never be shared across the sync server's request
  * threads). The new World a write returns is discarded — the durable state is the DB. */
final class JdbcBackend(ds: javax.sql.DataSource) extends Backend:
  private val world = World(JdbcPetRepository(), JdbcOrderRepository(), JdbcUserRepository())
  private def run[A](body: => A): A =
    val c = ds.getConnection()
    try ConnProvider.withConn(new Conn(c))(body) finally c.close()
  def read[A](f: World => A): A = run(f(world))
  def write[A](f: World => (World, A)): A = run(f(world)._2)
  def mutate(f: World => World): Unit = run { f(world); () }

class PetStoreApi(backend: Backend):
  import PetStoreJson.given
  import PetStoreEndpoints.*
  private given MonadError[Identity] = IdentityMonad

  // natural capability `Any` (no streams); NettySyncServer.addEndpoints accepts it,
  // and the sync stub interpreter requires exactly ServerEndpoint[Any, Identity].
  type SE = ServerEndpoint[Any, Identity]

  private val pets   = PetService[World](HasPets())
  private val orders = OrderService[World](HasOrders())
  private val users  = UserService[World](HasUsers())
  private val nextId = AtomicLong(1L)
  private def freshId(): Long = nextId.getAndIncrement()

  // --- backed-token auth (server infrastructure, NOT from the transpiled core) ---
  private val tokens = ConcurrentHashMap[String, Long]()
  def mintToken(userId: Long): String =
    val t = java.util.UUID.randomUUID.toString; tokens.put(t, userId); t
  def hashPassword(p: String): String =
    val md = java.security.MessageDigest.getInstance("SHA-256")
    md.digest(p.getBytes("UTF-8")).map(b => f"$b%02x").mkString

  private val unauthorized: Err = (StatusCode.Unauthorized, "")
  /** Resolve a bearer token to its still-existing user (a deleted user -> 401). */
  private def resolve(token: String): Either[Err, User] =
    Option(tokens.get(token)) match
      case Some(id) =>
        backend.read(users.getUser(_, id)) match
          case Right(u) => Right(u)
          case Left(_)  => Left(unauthorized)
      case None => Left(unauthorized)
  private def resolveAdmin(token: String): Either[Err, User] =
    resolve(token).flatMap(u => if u.role == Admin then Right(u) else Left(unauthorized))

  /** Test/programmatic-login helper (mirrors the originals' AuthTest.embedToken):
    * create the user and mint a bearer token, returning the BARE token (the client
    * interpreter adds the "Bearer " scheme itself). */
  def tokenFor(user: User): String =
    val id = backend.write(users.createUser(_, user.copy(id = None), freshId())) match
      case Right(u) => u.id.getOrElse(-1L)
      case Left(_)  => backend.read(users.getUserByName(_, user.userName)).toOption.flatMap(_.id).getOrElse(-1L)
    mintToken(id)

  private val petNotFound:   Err = (StatusCode.NotFound, "The pet was not found")
  private val orderNotFound: Err = (StatusCode.NotFound, "The order was not found")
  private val userNotFound:  Err = (StatusCode.NotFound, "The user was not found")

  /** All server endpoints (logic attached). Order matters: literal /pets/findBy* and
    * /users/login are registered before the /{id} and /{userName} variable routes. */
  def serverEndpoints: List[SE] = List(
    createPet.handleSecurity(resolve).handle(_ => (pet: Pet) =>
      backend.write(pets.create(_, pet, freshId())) match
        case Right(saved) => Right(saved)
        case Left(PetAlreadyExistsError(p)) =>
          Left((StatusCode.Conflict, s"The pet ${p.name} of category ${p.category} already exists"))
        case Left(_) => Left((StatusCode.BadRequest, "bad request"))),

    findPetsByStatus.handleSecurity(resolve).handle(_ => (raw: List[String]) =>
      if raw.isEmpty then Left((StatusCode.BadRequest, "status parameter not specified"))
      else
        val parsed = raw.map(parsePetStatus)
        if parsed.exists(_.isEmpty) then Left((StatusCode.BadRequest, "invalid status value"))
        else Right(backend.read(pets.findByStatus(_, parsed.flatten)))),

    findPetsByTag.handleSecurity(resolve).handle(_ => (tags: List[String]) =>
      if tags.isEmpty then Left((StatusCode.BadRequest, "tag parameter not specified"))
      else Right(backend.read(pets.findByTag(_, tags)))),

    listPets.handleSecurity(resolve).handle(_ => (in: (Option[Int], Option[Int])) =>
      Right(backend.read(pets.list(_, in._1.getOrElse(10), in._2.getOrElse(0))))),

    getPet.handleSecurity(resolve).handle(_ => (id: Long) =>
      backend.read(pets.get(_, id)).left.map(_ => petNotFound)),

    updatePet.handleSecurity(resolveAdmin).handle(_ => (in: (Long, Pet)) =>
      backend.write(pets.update(_, in._2.copy(id = Some(in._1)))) match
        case Right(saved) => Right(saved)
        case Left(_)      => Left(petNotFound)),

    deletePet.handleSecurity(resolveAdmin).handle(_ => (id: Long) =>
      { backend.mutate(pets.delete(_, id)); Right(()) }),

    placeOrder.handleSecurity(resolve).handle(u => (order: Order) =>
      Right(backend.write(w => orders.placeOrder(w, order.copy(userId = u.id), freshId())))),

    getOrder.handleSecurity(resolve).handle(_ => (id: Long) =>
      backend.read(orders.get(_, id)).left.map(_ => orderNotFound)),

    deleteOrder.handleSecurity(resolveAdmin).handle(_ => (id: Long) =>
      { backend.mutate(orders.delete(_, id)); Right(()) }),

    login.handle((req: LoginRequest) =>
      backend.read(users.getUserByName(_, req.userName)) match
        case Right(u) if u.hash == hashPassword(req.password) && u.id.isDefined =>
          Right((u, s"Bearer ${mintToken(u.id.get)}"))
        case _ => Left((StatusCode.BadRequest, s"Authentication failed for user ${req.userName}"))),

    signup.handle((req: SignupRequest) =>
      val user = req.asUser(hashPassword(req.password))
      backend.write(users.createUser(_, user, freshId())) match
        case Right(saved) => Right(saved)
        case Left(UserAlreadyExistsError(u)) =>
          Left((StatusCode.Conflict, s"The user with user name ${u.userName} already exists"))
        case Left(_) => Left((StatusCode.BadRequest, "bad request"))),

    listUsers.handleSecurity(resolveAdmin).handle(_ => (in: (Option[Int], Option[Int])) =>
      Right(backend.read(users.list(_, in._1.getOrElse(10), in._2.getOrElse(0))))),

    getUserByName.handleSecurity(resolveAdmin).handle(_ => (name: String) =>
      backend.read(users.getUserByName(_, name)).left.map(_ => userNotFound)),

    updateUser.handleSecurity(resolveAdmin).handle(_ => (in: (String, User)) =>
      backend.write(users.update(_, in._2.copy(userName = in._1))) match
        case Right(saved) => Right(saved)
        case Left(_)      => Left(userNotFound)),

    deleteUser.handleSecurity(resolveAdmin).handle(_ => (name: String) =>
      { backend.mutate(users.deleteByUserName(_, name)); Right(()) }),
  )

  private def parsePetStatus(s: String): Option[PetStatus] = s match
    case "Available" => Some(Available)
    case "Pending"   => Some(Pending)
    case "Adopted"   => Some(Adopted)
    case _           => None
