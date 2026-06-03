package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * USER PROOFS — VERIFY-ONLY. UserService capabilities proven over the World +
 * HasUsers lens and the ABSTRACT UserRepository, via the repo's algebraic axioms
 * (saveFindName) and the service's own branching. delete/list/distinctness are
 * covered by the differential test (in-memory oracle vs @extern JDBC), not here.
 * ========================================================================== */
object UserProofs {

  /* The filter-then-find lemma the in-memory delete's ensuring uses to discharge the
   * `deleteGet` axiom. Verify-only (never transpiled), so its List recursion does not
   * leak into production; the JDBC delete is trusted via its own ensuring. */
  @opaque @inlineOnce
  def userDeleteGetLemma(rows: List[User], id: FMLong): Unit = {
    rows match
      case Nil()      => ()
      case Cons(h, t) => userDeleteGetLemma(t, id)
  }.ensuring(_ =>
    rows.filter((x: User) => x.id != Some[FMLong](id)).find((x: User) => x.id == Some[FMLong](id)) == None[User]())

  // CREATE then GET-BY-NAME: a created user is retrievable by its userName.
  def createThenGetByName(w: World, user: User, freshId: FMLong): Boolean = {
    require(UserValidation.doesNotExist(w.users, user))
    val svc = UserService[World](HasUsers())
    svc.createUser(w, user, freshId) match
      case (w1, Right(saved)) =>
        assert(w.users.saveFindName(saved)) // hint: save(saved).findByUserName(saved.userName)==Some(saved)
        saved.id == Some[FMLong](freshId) &&
        svc.getUserByName(w1, user.userName) == Right[ValidationError, User](saved)
      case (_, Left(_)) => false
  }.holds

  // CREATE rejects a duplicate userName with UserAlreadyExistsError (service logic).
  def createRejectsDuplicateName(w: World, user: User, freshId: FMLong): Boolean = {
    require(!UserValidation.doesNotExist(w.users, user))
    val svc = UserService[World](HasUsers())
    svc.createUser(w, user, freshId)._2 == Left[ValidationError, User](UserAlreadyExistsError(user))
  }.holds

  // GET / GET-BY-NAME of an absent key -> Left(UserNotFoundError) (service logic).
  def getMissingFails(w: World, id: FMLong): Boolean = {
    require(w.users.get(id) == None[User]())
    val svc = UserService[World](HasUsers())
    svc.getUser(w, id) == Left[ValidationError, User](UserNotFoundError)
  }.holds

  def getByNameMissingFails(w: World, userName: String): Boolean = {
    require(w.users.findByUserName(userName) == None[User]())
    val svc = UserService[World](HasUsers())
    svc.getUserByName(w, userName) == Left[ValidationError, User](UserNotFoundError)
  }.holds

  // UPDATE of an absent / id-less user -> Left(UserNotFoundError) (service logic).
  def updateMissingFails(w: World, user: User): Boolean = {
    require(!UserValidation.existsOpt(w.users, user.id))
    val svc = UserService[World](HasUsers())
    svc.update(w, user)._2 == Left[ValidationError, User](UserNotFoundError)
  }.holds

  // DELETE then GET fails: deleting an id makes it unreachable (deleteGet axiom).
  def deleteRemoves(w: World, id: FMLong): Boolean = {
    assert(w.users.deleteGet(id)) // hint: delete(id).get(id) == None
    val svc = UserService[World](HasUsers())
    svc.getUser(svc.deleteUser(w, id), id) == Left[ValidationError, User](UserNotFoundError)
  }.holds
}
