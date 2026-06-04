package io.linewise.petstore

import org.scalacheck.Prop.forAll
import sttp.model.StatusCode
import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.PetStoreEndpoints as E

/* =============================================================================
 * Translation of the original PetEndpointsSpec (http4s + scalatest) onto our tapir
 * server, driven through the stub backend with a typed sttp client. Every business
 * call inside the endpoints is a Stainless-transpiled PetService function.
 * ========================================================================== */
class PetEndpointsSpec extends PetStoreHttpSuite:

  property("create pet") {
    // (a) unauthenticated create is rejected
    forAll { (pet: Pet) =>
      val be = stubOf(newApi())
      assertEquals(secure(E.createPet, be, "no-such-token", pet).code, StatusCode.Unauthorized)
    } && {
      // (b) an authenticated create succeeds, and (c) the pet is then retrievable by id
      forAll { (pet: Pet, user: User) =>
        val api = newApi(); val be = stubOf(api)
        val token = api.tokenFor(user)
        val created = secure(E.createPet, be, token, pet)
        assertEquals(created.code, StatusCode.Ok)
        created.body match
          case Right(saved) =>
            val got = secure(E.getPet, be, token, saved.id.get)
            assertEquals(got.code, StatusCode.Ok)
            assertEquals(got.body.map(_.id), Right(saved.id))
          case Left(e) => fail(s"expected created pet, got $e")
      }
    }
  }

  property("update pet") {
    forAll { (pet: Pet, admin: AdminUser) =>
      val api = newApi(); val be = stubOf(api)
      val token = api.tokenFor(admin.value)
      secure(E.createPet, be, token, pet).body match
        case Right(created) =>
          val toUpdate = created.copy(name = created.name.reverse)
          val updated = secure(E.updatePet, be, token, (created.id.get, toUpdate))
          assertEquals(updated.code, StatusCode.Ok)
          assertEquals(updated.body.map(_.name), Right(pet.name.reverse))
        case Left(e) => fail(s"expected created pet, got $e")
    }
  }

  property("find by tag") {
    forAll { (pet: Pet, admin: AdminUser) =>
      val api = newApi(); val be = stubOf(api)
      val token = api.tokenFor(admin.value)
      secure(E.createPet, be, token, pet).body match
        case Right(created) =>
          created.tags.headOption match
            case Some(tag) =>
              val found = secure(E.findPetsByTag, be, token, List(tag))
              assertEquals(found.code, StatusCode.Ok)
              assert(found.body.fold(_ => false, _.exists(_.id == created.id)),
                s"findByTags($tag) should contain the created pet")
            case None => () // no tags generated; nothing to assert
        case Left(e) => fail(s"expected created pet, got $e")
    }
  }

  property("find by status requires a status parameter") {
    forAll { (admin: AdminUser) =>
      val api = newApi(); val be = stubOf(api)
      val token = api.tokenFor(admin.value)
      assertEquals(secure(E.findPetsByStatus, be, token, Nil).code, StatusCode.BadRequest)
    }
  }
