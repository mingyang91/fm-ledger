package io.linewise.petstore

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import io.linewise.petstore.generated.PetStoreModel.*

/* =============================================================================
 * Scalacheck generators — the translation of the original Arbitraries.scala onto our
 * generated DTOs (Set[String] -> List[String], Option[Instant] -> Option[Long] millis,
 * Option[Long] ids). AdminUser / CustomerUser are role-pinned wrappers, as upstream.
 * ========================================================================== */
trait PetStoreGen:
  val userNameGen: Gen[String] = Gen.listOfN(16, Gen.alphaChar).map(_.mkString)

  given Arbitrary[PetStatus]   = Arbitrary(Gen.oneOf(Available, Pending, Adopted))
  given Arbitrary[OrderStatus] = Arbitrary(Gen.oneOf(Approved, Delivered, Placed))
  given Arbitrary[Role]        = Arbitrary(Gen.oneOf(Admin, Customer))

  given Arbitrary[Pet] = Arbitrary(
    for
      name      <- Gen.nonEmptyListOf(Gen.asciiPrintableChar).map(_.mkString)
      category  <- arbitrary[String]
      bio       <- arbitrary[String]
      status    <- arbitrary[PetStatus]
      n         <- Gen.choose(0, 10)
      tags      <- Gen.listOfN(n, Gen.alphaStr)
      photoUrls <- Gen.listOfN(n, Gen.alphaStr).map(_.map(x => s"http://$x.com"))
      id        <- Gen.option(Gen.posNum[Long])
    yield Pet(name, category, bio, status, tags, photoUrls, id))

  given Arbitrary[Order] = Arbitrary(
    for
      petId    <- Gen.posNum[Long]
      shipDate <- Gen.option(Gen.posNum[Long])
      status   <- arbitrary[OrderStatus]
      complete <- arbitrary[Boolean]
      id       <- Gen.option(Gen.posNum[Long])
    yield Order(petId, shipDate, status, complete, id, None))

  given Arbitrary[User] = Arbitrary(
    for
      userName  <- userNameGen
      firstName <- arbitrary[String]
      lastName  <- arbitrary[String]
      email     <- arbitrary[String]
      hash      <- arbitrary[String]
      phone     <- arbitrary[String]
      id        <- Gen.option(Gen.posNum[Long])
      role      <- arbitrary[Role]
    yield User(userName, firstName, lastName, email, hash, phone, id, role))

  given Arbitrary[SignupRequest] = Arbitrary(
    for
      userName  <- userNameGen
      firstName <- arbitrary[String]
      lastName  <- arbitrary[String]
      email     <- arbitrary[String]
      password  <- arbitrary[String]
      phone     <- arbitrary[String]
      role      <- arbitrary[Role]
    yield SignupRequest(userName, firstName, lastName, email, password, phone, role))

  case class AdminUser(value: User)
  case class CustomerUser(value: User)
  given Arbitrary[AdminUser]    = Arbitrary(arbitrary[User].map(u => AdminUser(u.copy(role = Admin))))
  given Arbitrary[CustomerUser] = Arbitrary(arbitrary[User].map(u => CustomerUser(u.copy(role = Customer))))
