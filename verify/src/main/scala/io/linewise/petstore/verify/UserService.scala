package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._
import UserRepository.UserTable

/* =============================================================================
 * USER — THE SERVICE LAYER (UserService). Composes validation + repository, method
 * names mirroring the original (createUser/getUser/getUserByName/update/deleteUser/
 * deleteByUserName/list). EitherT/OptionT flatten to pure Either/Option — no
 * cats-effect. The id source is the trusted `freshId` parameter. Transpile-clean.
 * ========================================================================== */
object UserService {

  def createUser(t: UserTable, user: User, freshId: FMLong): Either[ValidationError, (UserTable, User)] =
    if UserValidation.doesNotExist(t, user) then
      Right[ValidationError, (UserTable, User)](UserRepository.create(t, user, freshId))
    else
      Left[ValidationError, (UserTable, User)](UserAlreadyExistsError(user))

  def getUser(t: UserTable, id: FMLong): Either[ValidationError, User] =
    UserRepository.get(t, id) match
      case Some(u) => Right[ValidationError, User](u)
      case _       => Left[ValidationError, User](UserNotFoundError)

  def getUserByName(t: UserTable, userName: String): Either[ValidationError, User] =
    UserRepository.findByUserName(t, userName) match
      case Some(u) => Right[ValidationError, User](u)
      case _       => Left[ValidationError, User](UserNotFoundError)

  def update(t: UserTable, user: User): Either[ValidationError, (UserTable, User)] =
    if UserValidation.existsOpt(t, user.id) then
      UserRepository.update(t, user) match
        case (t2, Some(u)) => Right[ValidationError, (UserTable, User)]((t2, u))
        case _             => Left[ValidationError, (UserTable, User)](UserNotFoundError)
    else Left[ValidationError, (UserTable, User)](UserNotFoundError)

  def deleteUser(t: UserTable, id: FMLong): UserTable =
    UserRepository.delete(t, id)._1

  def deleteByUserName(t: UserTable, userName: String): UserTable =
    UserRepository.deleteByUserName(t, userName)._1

  def list(t: UserTable, pageSize: FMInt, offset: FMInt): List[User] =
    UserRepository.list(t, pageSize, offset)
}
