package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._

/* =============================================================================
 * USER — THE REPOSITORY LAYER as a VALUE with methods (OptionT flattens to Option).
 * Transpile-clean.
 * ========================================================================== */
case class UserRepository(rows: List[User]) {
  def save(user: User): UserRepository = UserRepository(user :: rows)
  def get(id: FMLong): Option[User] = rows.find((u: User) => u.id == Some[FMLong](id))
  def findByUserName(userName: String): Option[User] = rows.find((u: User) => u.userName == userName)
  def update(user: User): UserRepository = UserRepository(rows.map((u: User) => if u.id == user.id then user else u))
  def delete(id: FMLong): UserRepository = UserRepository(rows.filter((u: User) => u.id != Some[FMLong](id)))
  def deleteByUserName(userName: String): UserRepository = UserRepository(rows.filter((u: User) => u.userName != userName))
  def list(pageSize: FMInt, offset: FMInt): List[User] = rows.drop(offset.value).take(pageSize.value)
}
