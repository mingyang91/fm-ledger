package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._

/* =============================================================================
 * USER — THE REPOSITORY LAYER (UserRepositoryAlgebra, realized purely). The
 * original's OptionT[F, A] flattens to Option. Data-access only:
 * create/get/findByUserName/update/delete/deleteByUserName/list. Transpile-clean.
 * ========================================================================== */
object UserRepository {

  case class UserTable(rows: List[User])

  def create(t: UserTable, user: User, freshId: FMLong): (UserTable, User) = {
    val saved = user.copy(id = Some[FMLong](freshId))
    (UserTable(saved :: t.rows), saved)
  }

  def get(t: UserTable, id: FMLong): Option[User] =
    t.rows.find((u: User) => u.id == Some[FMLong](id))

  def findByUserName(t: UserTable, userName: String): Option[User] =
    t.rows.find((u: User) => u.userName == userName)

  def update(t: UserTable, user: User): (UserTable, Option[User]) =
    t.rows.find((u: User) => u.id == user.id) match
      case Some(_) =>
        (UserTable(t.rows.map((u: User) => if u.id == user.id then user else u)), Some[User](user))
      case _ => (t, None[User]())

  def delete(t: UserTable, id: FMLong): (UserTable, Option[User]) =
    (UserTable(t.rows.filter((u: User) => u.id != Some[FMLong](id))),
     t.rows.find((u: User) => u.id == Some[FMLong](id)))

  def deleteByUserName(t: UserTable, userName: String): (UserTable, Option[User]) =
    (UserTable(t.rows.filter((u: User) => u.userName != userName)),
     t.rows.find((u: User) => u.userName == userName))

  def list(t: UserTable, pageSize: FMInt, offset: FMInt): List[User] =
    t.rows.drop(offset.value).take(pageSize.value)
}
