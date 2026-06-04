package io.linewise.petstore

import org.scalacheck.Prop.forAll
import sttp.model.StatusCode
import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.PetStoreEndpoints as E

/* =============================================================================
 * Translation of the original UserEndpointsSpec + LoginTest onto our tapir server.
 * signup / login are public; get / update / delete users are admin-only (as upstream).
 * Login mints a backed bearer token; a deleted user's token no longer authenticates.
 * ========================================================================== */
class UserEndpointsSpec extends PetStoreHttpSuite:

  property("create user and log in") {
    forAll { (signup: SignupRequest) =>
      val be = stubOf(newApi())
      val (_, token) = signUpAndLogIn(signup, be)
      assert(token.isDefined, "login should yield a bearer token")
    }
  }

  property("update user") {
    forAll { (signup: SignupRequest) =>
      val be = stubOf(newApi())
      val (Some(created), Some(token)) = signUpAndLogInAsAdmin(signup, be): @unchecked
      val toUpdate = created.copy(lastName = created.lastName.reverse)
      val updated = secure(E.updateUser, be, token, (created.userName, toUpdate))
      assertEquals(updated.code, StatusCode.Ok)
      assertEquals(updated.body.map(_.lastName), Right(created.lastName.reverse))
      assertEquals(updated.body.map(_.id), Right(created.id))
    }
  }

  property("get user by userName") {
    forAll { (signup: SignupRequest) =>
      val be = stubOf(newApi())
      val (Some(created), Some(token)) = signUpAndLogInAsAdmin(signup, be): @unchecked
      val got = secure(E.getUserByName, be, token, created.userName)
      assertEquals(got.code, StatusCode.Ok)
      assertEquals(got.body.map(_.userName), Right(created.userName))
    }
  }

  property("delete user by userName") {
    // a customer may not delete a user
    forAll { (signup: SignupRequest) =>
      val be = stubOf(newApi())
      val (Some(created), Some(token)) = signUpAndLogInAsCustomer(signup, be): @unchecked
      assertEquals(secure(E.deleteUser, be, token, created.userName).code, StatusCode.Unauthorized)
    } && {
      // an admin may; afterwards the (self-deleted) admin's token no longer authenticates
      forAll { (signup: SignupRequest) =>
        val be = stubOf(newApi())
        val (Some(created), Some(token)) = signUpAndLogInAsAdmin(signup, be): @unchecked
        assertEquals(secure(E.deleteUser, be, token, created.userName).code, StatusCode.Ok)
        assertEquals(secure(E.getUserByName, be, token, created.userName).code, StatusCode.Unauthorized)
      }
    }
  }
