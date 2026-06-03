package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._
import UserRepository.UserTable

/* =============================================================================
 * USER PROOFS — VERIFY-ONLY. Every UserService / UserValidation capability proven
 * over the Repository / Validation / Service layers, plus the distinct-id invariant.
 * ========================================================================== */
object UserProofs {

  def userIds(rows: List[User]): List[FMLong] =
    rows match
      case Nil()      => Nil[FMLong]()
      case Cons(h, t) => h.id match
          case Some(i) => i :: userIds(t)
          case _       => userIds(t)

  def distinctL(xs: List[FMLong]): Boolean =
    xs match
      case Nil()      => true
      case Cons(h, t) => !t.contains(h) && distinctL(t)

  def tableInv(t: UserTable): Boolean = distinctL(userIds(t.rows))

  @opaque @inlineOnce
  def deleteRemovesLemma(rows: List[User], id: FMLong): Unit = {
    rows match
      case Nil()      => ()
      case Cons(h, t) => deleteRemovesLemma(t, id)
  }.ensuring(_ =>
    rows.filter((u: User) => u.id != Some[FMLong](id)).find((u: User) => u.id == Some[FMLong](id)) == None[User]())

  def createThenGetByName(t: UserTable, user: User, freshId: FMLong): Boolean = {
    require(UserValidation.doesNotExist(t, user))
    UserService.createUser(t, user, freshId) match
      case Right((t2, saved)) =>
        saved.id == Some[FMLong](freshId) && UserRepository.findByUserName(t2, user.userName) == Some[User](saved)
      case Left(_) => false
  }.holds

  def createRejectsDuplicateName(t: UserTable, user: User, freshId: FMLong): Boolean = {
    require(!UserValidation.doesNotExist(t, user))
    UserService.createUser(t, user, freshId) == Left[ValidationError, (UserTable, User)](UserAlreadyExistsError(user))
  }.holds

  def createPreservesDistinct(t: UserTable, user: User, freshId: FMLong): Boolean = {
    require(tableInv(t) && !userIds(t.rows).contains(freshId))
    tableInv(UserRepository.create(t, user, freshId)._1)
  }.holds

  def getMissingFails(t: UserTable, id: FMLong): Boolean = {
    require(UserRepository.get(t, id) == None[User]())
    UserService.getUser(t, id) == Left[ValidationError, User](UserNotFoundError)
  }.holds

  def getByNameMissingFails(t: UserTable, userName: String): Boolean = {
    require(UserRepository.findByUserName(t, userName) == None[User]())
    UserService.getUserByName(t, userName) == Left[ValidationError, User](UserNotFoundError)
  }.holds

  def updateMissingFails(t: UserTable, user: User): Boolean = {
    require(!UserValidation.existsOpt(t, user.id))
    UserService.update(t, user) == Left[ValidationError, (UserTable, User)](UserNotFoundError)
  }.holds

  def deleteRemoves(t: UserTable, id: FMLong): Boolean = {
    deleteRemovesLemma(t.rows, id)
    UserRepository.get(UserService.deleteUser(t, id), id) == None[User]()
  }.holds
}
