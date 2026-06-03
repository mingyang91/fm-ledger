package io.linewise.petstore

import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.UserValidation as GenUserValidation

/* =============================================================================
 * USER — THE SERVICE LAYER, production (mirrors the original UserService). Composes
 * the generated, verified UserValidation with a UserRepository realization. No
 * cats-effect — pure Either.
 * ========================================================================== */
final class UserService(repo: UserRepository):

  def createUser(user: User): Either[ValidationError, User] =
    if GenUserValidation.doesNotExist(repo.snapshot, user) then Right(repo.create(user))
    else Left(UserAlreadyExistsError(user))

  def getUser(id: Long): Either[ValidationError, User] =
    repo.get(id) match
      case Some(u) => Right(u)
      case None    => Left(UserNotFoundError)

  def getUserByName(userName: String): Either[ValidationError, User] =
    repo.findByUserName(userName) match
      case Some(u) => Right(u)
      case None    => Left(UserNotFoundError)

  def update(user: User): Either[ValidationError, User] =
    if GenUserValidation.existsOpt(repo.snapshot, user.id) then
      repo.update(user) match
        case Some(u) => Right(u)
        case None    => Left(UserNotFoundError)
    else Left(UserNotFoundError)

  def deleteUser(id: Long): Unit = repo.delete(id)
  def deleteByUserName(userName: String): Unit = repo.deleteByUserName(userName)
  def list(pageSize: Int, offset: Int): List[User] = repo.list(pageSize, offset)
