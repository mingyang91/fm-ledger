package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._
import io.linewise.petstore.Db

/* =============================================================================
 * USER — THE REPOSITORY LAYER as a sealed abstract type with a @ghost `rows` model.
 *
 *   - InMemUserRepository: a real list (the verification ORACLE / differential ref).
 *   - JdbcUserRepository: FIELD-LESS (so it stays immutable — a stored handle would
 *     taint the abstract hierarchy and the World mutable, which the Has lens forbids);
 *     `rows` is an erased ghost stub; each op is @extern @pure and delegates to the
 *     production `Db` facade (Quill, over a pooled DataSource). Quill is a third-party
 *     macro library NOT on the verify classpath, so it CANNOT live in the verified
 *     source; the @extern body is havoc'd in verification anyway, so it delegates to
 *     `Db` (a verify-only stub here; the real Quill DAO in production). TRUSTED by the
 *     algebraic axioms it carries on its ensurings; the differential test is the guard.
 *
 * Axioms (head-match, so the in-memory oracle discharges them; the JDBC realization is
 * trusted via its ensurings): saveGet, saveFindName, deleteGet.
 * ========================================================================== */
sealed abstract class UserRepository {
  @ghost def rows: List[User]

  def save(u: User): UserRepository
  def get(id: FMLong): Option[User]
  def findByUserName(name: String): Option[User]
  def update(u: User): UserRepository
  def delete(id: FMLong): UserRepository
  def deleteByUserName(name: String): UserRepository
  def list(pageSize: FMInt, offset: FMInt): List[User]

  @law def saveGet(u: User, id: FMLong): Boolean =
    (u.id == Some[FMLong](id)) ==> (save(u).get(id) == Some[User](u))
  @law def saveFindName(u: User): Boolean =
    save(u).findByUserName(u.userName) == Some[User](u)
  @law def deleteGet(id: FMLong): Boolean =
    delete(id).get(id) == None[User]()
}

/* IN-MEMORY oracle — a real list; discharges the axioms by head-match. */
case class InMemUserRepository(items: List[User]) extends UserRepository {
  @ghost def rows: List[User] = items
  def save(u: User): UserRepository = InMemUserRepository(u :: items)
  def get(id: FMLong): Option[User] = items.find((x: User) => x.id == Some[FMLong](id))
  def findByUserName(name: String): Option[User] = items.find((x: User) => x.userName == name)
  def update(u: User): UserRepository = InMemUserRepository(items.map((x: User) => if x.id == u.id then u else x))
  def delete(id: FMLong): UserRepository =
    InMemUserRepository(items.filter((x: User) => x.id != Some[FMLong](id)))
      .ensuring((res: UserRepository) => { UserProofs.userDeleteGetLemma(items, id); res.get(id) == None[User]() })
  def deleteByUserName(name: String): UserRepository = InMemUserRepository(items.filter((x: User) => x.userName != name))
  def list(pageSize: FMInt, offset: FMInt): List[User] = items.drop(offset.value).take(pageSize.value)
}

/* JDBC realization — FIELD-LESS (immutable); each op @extern, delegating to the
 * production Quill `Db` facade; @ghost stub rows; trusted via the axioms. */
case class JdbcUserRepository() extends UserRepository {
  @ghost def rows: List[User] = Nil[User]()

  @extern @pure
  def save(u: User): UserRepository = { Db.insertUser(u); JdbcUserRepository() }.ensuring((res: UserRepository) =>
    forall((id: FMLong) => (u.id == Some[FMLong](id)) ==> (res.get(id) == Some[User](u))) &&
    res.findByUserName(u.userName) == Some[User](u))

  @extern @pure
  def get(id: FMLong): Option[User] = Db.userById(id)

  @extern @pure
  def findByUserName(name: String): Option[User] = Db.userByName(name)

  @extern @pure
  def update(u: User): UserRepository = { Db.updateUser(u); JdbcUserRepository() }

  @extern @pure
  def delete(id: FMLong): UserRepository =
    { Db.deleteUser(id); JdbcUserRepository() }.ensuring((res: UserRepository) => res.get(id) == None[User]())

  @extern @pure
  def deleteByUserName(name: String): UserRepository = { Db.deleteUserByName(name); JdbcUserRepository() }

  @extern @pure
  def list(pageSize: FMInt, offset: FMInt): List[User] = Db.usersList(pageSize, offset)
}
