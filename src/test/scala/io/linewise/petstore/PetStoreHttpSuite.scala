package io.linewise.petstore

import sttp.client4.*
import sttp.model.Uri
import sttp.tapir.Endpoint
import sttp.tapir.client.sttp4.SttpClientInterpreter
import sttp.tapir.server.stub4.TapirSyncStubInterpreter
import io.linewise.petstore.generated.PetStoreModel.{User, Role, Admin, Customer, SignupRequest, LoginRequest}
import io.linewise.petstore.PetStoreEndpoints as E

/* =============================================================================
 * Base for the translated endpoint specs. Each test stands up a fresh in-memory-backed
 * PetStoreApi (the analogue of the originals' in-memory repository interpreters), wraps
 * its server endpoints in a tapir stub backend (no port bound), and drives them with a
 * typed sttp client derived from the SAME bare endpoints — so client and server share
 * the exact codecs. `resp.code` is the HTTP status; `resp.body` is Either[Err, O].
 * ========================================================================== */
trait PetStoreHttpSuite extends munit.ScalaCheckSuite with PetStoreGen:
  val baseUri: Uri = uri"http://test.local"
  private val client = SttpClientInterpreter()

  def newApi(): PetStoreApi = PetStoreApi(InMemBackend())

  def stubOf(api: PetStoreApi): SyncBackend =
    TapirSyncStubInterpreter()
      .whenServerEndpointsRunLogic(api.serverEndpoints)
      .backend()

  /** Call a secured endpoint with a bearer token. */
  def secure[A, I, E, O](ep: Endpoint[A, I, E, O, Any], be: SyncBackend, token: A, in: I): Response[Either[E, O]] =
    client.toSecureRequestThrowDecodeFailures(ep, Some(baseUri))(token)(in).send(be)

  /** Call a public endpoint (no auth). */
  def open[I, EE, O](ep: Endpoint[Unit, I, EE, O, Any], be: SyncBackend, in: I): Response[Either[EE, O]] =
    client.toRequestThrowDecodeFailures(ep, Some(baseUri))(in).send(be)

  /** Translation of LoginTest: sign up, then log in; returns the created user and the
    * BARE bearer token (the login endpoint emits "Bearer <token>" in Authorization). */
  def signUpAndLogIn(req: SignupRequest, be: SyncBackend): (Option[User], Option[String]) =
    val user  = open(E.signup, be, req).body.toOption
    val token = open(E.login, be, LoginRequest(req.userName, req.password)).body.toOption.map(_._2.stripPrefix("Bearer "))
    (user, token)
  def signUpAndLogInAsAdmin(req: SignupRequest, be: SyncBackend): (Option[User], Option[String]) =
    signUpAndLogIn(req.copy(role = Admin), be)
  def signUpAndLogInAsCustomer(req: SignupRequest, be: SyncBackend): (Option[User], Option[String]) =
    signUpAndLogIn(req.copy(role = Customer), be)
