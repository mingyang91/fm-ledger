package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * USER PROOFS — VERIFY-ONLY. Every UserService / UserValidation capability proven
 * over the World + HasUsers lens, plus the distinct-id invariant.
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

  def repoInv(repo: UserRepository): Boolean = distinctL(userIds(repo.rows))

  @opaque @inlineOnce
  def deleteRemovesLemma(rows: List[User], id: FMLong): Unit = {
    rows match
      case Nil()      => ()
      case Cons(h, t) => deleteRemovesLemma(t, id)
  }.ensuring(_ =>
    rows.filter((u: User) => u.id != Some[FMLong](id)).find((u: User) => u.id == Some[FMLong](id)) == None[User]())

  def createThenGetByName(w: World, user: User, freshId: FMLong): Boolean = {
    require(UserValidation.doesNotExist(w.users, user))
    val svc = UserService[World](HasUsers())
    svc.createUser(w, user, freshId) match
      case (w1, Right(saved)) =>
        saved.id == Some[FMLong](freshId) && svc.getUserByName(w1, user.userName) == Right[ValidationError, User](saved)
      case (_, Left(_)) => false
  }.holds

  def createRejectsDuplicateName(w: World, user: User, freshId: FMLong): Boolean = {
    require(!UserValidation.doesNotExist(w.users, user))
    val svc = UserService[World](HasUsers())
    svc.createUser(w, user, freshId)._2 == Left[ValidationError, User](UserAlreadyExistsError(user))
  }.holds

  def createPreservesDistinct(w: World, user: User, freshId: FMLong): Boolean = {
    require(UserValidation.doesNotExist(w.users, user))
    require(repoInv(w.users) && !userIds(w.users.rows).contains(freshId))
    val svc = UserService[World](HasUsers())
    repoInv(svc.createUser(w, user, freshId)._1.users)
  }.holds

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

  def updateMissingFails(w: World, user: User): Boolean = {
    require(!UserValidation.existsOpt(w.users, user.id))
    val svc = UserService[World](HasUsers())
    svc.update(w, user)._2 == Left[ValidationError, User](UserNotFoundError)
  }.holds

  def deleteRemoves(w: World, id: FMLong): Boolean = {
    deleteRemovesLemma(w.users.rows, id)
    val svc = UserService[World](HasUsers())
    svc.getUser(svc.deleteUser(w, id), id) == Left[ValidationError, User](UserNotFoundError)
  }.holds
}
