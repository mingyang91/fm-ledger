package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._

/* =============================================================================
 * USER — THE SERVICE LAYER, polymorphic in W via a Has lens. Composes validation
 * with the repository reached through the lens. Write ops return (W, result);
 * reads return the result. Transpile-clean.
 * ========================================================================== */
case class UserService[W](has: Has[W, UserRepository]) {

  def createUser(w: W, user: User, freshId: FMLong): (W, Either[ValidationError, User]) = {
    val repo = has.get(w)
    if UserValidation.doesNotExist(repo, user) then
      val saved = user.copy(id = Some[FMLong](freshId))
      val w1 = has(w).write((r: UserRepository) => r.save(saved))
      (w1, Right[ValidationError, User](saved))
    else
      (w, Left[ValidationError, User](UserAlreadyExistsError(user)))
  }

  def getUser(w: W, id: FMLong): Either[ValidationError, User] =
    has.get(w).get(id) match
      case Some(u) => Right[ValidationError, User](u)
      case _       => Left[ValidationError, User](UserNotFoundError)

  def getUserByName(w: W, userName: String): Either[ValidationError, User] =
    has.get(w).findByUserName(userName) match
      case Some(u) => Right[ValidationError, User](u)
      case _       => Left[ValidationError, User](UserNotFoundError)

  def update(w: W, user: User): (W, Either[ValidationError, User]) = {
    val repo = has.get(w)
    if UserValidation.existsOpt(repo, user.id) then
      val w1 = has(w).write((r: UserRepository) => r.update(user))
      (w1, Right[ValidationError, User](user))
    else
      (w, Left[ValidationError, User](UserNotFoundError))
  }

  def deleteUser(w: W, id: FMLong): W =
    has(w).write((r: UserRepository) => r.delete(id))

  def deleteByUserName(w: W, userName: String): W =
    has(w).write((r: UserRepository) => r.deleteByUserName(userName))

  def list(w: W, pageSize: FMInt, offset: FMInt): List[User] =
    has.get(w).list(pageSize, offset)
}
