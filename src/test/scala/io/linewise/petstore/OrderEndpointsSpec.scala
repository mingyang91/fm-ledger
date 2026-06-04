package io.linewise.petstore

import org.scalacheck.Prop.forAll
import sttp.model.StatusCode
import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.PetStoreEndpoints as E

/* =============================================================================
 * Translation of the original OrderEndpointsSpec onto our tapir server. placeOrder /
 * getOrder / deleteOrder delegate to the Stainless-transpiled OrderService.
 * ========================================================================== */
class OrderEndpointsSpec extends PetStoreHttpSuite:

  property("place and get order") {
    forAll { (order: Order, admin: AdminUser) =>
      val api = newApi(); val be = stubOf(api)
      val token = api.tokenFor(admin.value)
      val placed = secure(E.placeOrder, be, token, order)
      assertEquals(placed.code, StatusCode.Ok)
      placed.body match
        case Right(saved) =>
          assertEquals(saved.petId, order.petId)
          val got = secure(E.getOrder, be, token, saved.id.get)
          assertEquals(got.code, StatusCode.Ok)
          assert(got.body.fold(_ => false, _.userId.isDefined), "the placed order's userId is set to the caller")
        case Left(e) => fail(s"expected placed order, got $e")
    }
  }

  property("user roles: only an admin may delete an order") {
    forAll { (customer: CustomerUser) =>
      val api = newApi(); val be = stubOf(api)
      val token = api.tokenFor(customer.value)
      assertEquals(secure(E.deleteOrder, be, token, 1L).code, StatusCode.Unauthorized)
    } && {
      forAll { (admin: AdminUser) =>
        val api = newApi(); val be = stubOf(api)
        val token = api.tokenFor(admin.value)
        assertEquals(secure(E.deleteOrder, be, token, 1L).code, StatusCode.Ok)
      }
    }
  }
