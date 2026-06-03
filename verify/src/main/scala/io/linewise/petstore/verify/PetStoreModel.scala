package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong

/* =============================================================================
 * SCALA-PET-STORE — the DATA LAYER, translated to Stainless (transpiler input #1).
 *
 * A faithful, structurally ISOMORPHIC translation of the entities, enums, DTOs,
 * and the ValidationError ADT of github.com/pauljamescleary/scala-pet-store
 * (domain/{pets,orders,users,authentication}). Field names and order match the
 * originals one-for-one; the production core generated from this file
 * (io.linewise.petstore.generated.PetStoreModel) has the SAME shapes with the
 * bounded FM types erased to native ones.
 *
 * TWO representation choices, forced by Stainless's type theory and documented as
 * the only deviations from byte-for-byte isomorphism:
 *   - `id: Option[Long]`        -> `Option[FMLong]`  (BIGSERIAL; erases back to Long)
 *   - `tags/photoUrls: Set[String]` -> `List[String]`  (Stainless's SMT Set cannot
 *       be iterated/folded, which the queries need; a List of the same strings is
 *       the faithful model, and the DB column is a single VARCHAR either way)
 *   - `shipDate: Option[Instant]` -> `Option[FMLong]` (epoch millis; Stainless has
 *       no time type, the column is TIMESTAMP)
 *
 * OUT OF SCOPE (like the http4s router, by the goal): the tsec/JWT crypto in
 * Auth.scala and the AuthorizationInfo/SimpleAuthEnum tsec instances. Role is
 * translated as its plain value (roleRepr) with Admin/Customer; the JWT TABLE is
 * mirrored isomorphically in the production store.
 * ========================================================================== */
object PetStoreModel {

  /* --- PETS (domain/pets/Pet.scala, PetStatus.scala) --- */
  sealed trait PetStatus
  case object Available extends PetStatus
  case object Pending   extends PetStatus
  case object Adopted   extends PetStatus

  case class Pet(
      name: String,
      category: String,
      bio: String,
      status: PetStatus,
      tags: List[String],
      photoUrls: List[String],
      id: Option[FMLong],
  )

  /* --- ORDERS (domain/orders/Order.scala, OrderStatus.scala) --- */
  sealed trait OrderStatus
  case object Approved  extends OrderStatus
  case object Delivered extends OrderStatus
  case object Placed    extends OrderStatus

  case class Order(
      petId: FMLong,
      shipDate: Option[FMLong], // epoch millis (Instant in the original)
      status: OrderStatus,
      complete: Boolean,
      id: Option[FMLong],
      userId: Option[FMLong],
  )

  /* --- USERS (domain/users/User.scala, Role.scala) --- */
  case class Role(roleRepr: String)
  val Admin: Role    = Role("Admin")
  val Customer: Role = Role("Customer")

  case class User(
      userName: String,
      firstName: String,
      lastName: String,
      email: String,
      hash: String,
      phone: String,
      id: Option[FMLong],
      role: Role,
  )

  /* --- AUTH DTOs (domain/authentication/LoginRequest.scala) --- */
  case class LoginRequest(userName: String, password: String)

  case class SignupRequest(
      userName: String,
      firstName: String,
      lastName: String,
      email: String,
      password: String,
      phone: String,
      role: Role,
  ) {
    // hashing happens outside the pure core (tsec, out of scope); the already-
    // hashed password is passed in — the pure shape of the original `asUser`.
    def asUser(hashedPassword: String): User =
      User(userName, firstName, lastName, email, hashedPassword, phone, None[FMLong](), role)
  }

  /* --- VALIDATION ERRORS (domain/ValidationError.scala) — isomorphic ADT. --- */
  sealed trait ValidationError
  case class PetAlreadyExistsError(pet: Pet) extends ValidationError
  case object PetNotFoundError extends ValidationError
  case object OrderNotFoundError extends ValidationError
  case object UserNotFoundError extends ValidationError
  case class UserAlreadyExistsError(user: User) extends ValidationError
  case class UserAuthenticationFailedError(userName: String) extends ValidationError
}
