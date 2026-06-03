package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._
import JdbcSupport.{fmFromLong, longOfFM}
import io.linewise.petstore.ConnProvider

/* =============================================================================
 * USER — THE REPOSITORY LAYER as a sealed abstract type with a @ghost `rows` model.
 *
 *   - InMemUserRepository: a real list (the verification ORACLE / differential ref).
 *   - JdbcUserRepository: FIELD-LESS (so it stays immutable — a stored Conn would
 *     taint the abstract hierarchy and the World mutable, which the Has lens forbids);
 *     `rows` is an erased ghost stub; each op is @extern @pure REAL PER-OPERATION SQL
 *     (no whole-table load) that fetches the ambient connection from ConnProvider
 *     INSIDE its havoc'd body. TRUSTED by the algebraic axioms it carries on `save`'s
 *     ensuring; the in-memory-vs-JDBC differential test is the machine-checked guard.
 *
 * Axioms (head-match, so the in-memory oracle discharges them automatically; the
 * JDBC realization is trusted via save's ensuring): saveGet (a saved row is gotten
 * back by its id) and saveFindName (gotten back by its userName). delete/update/
 * list correctness is checked by the differential test, not an axiom.
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

/* JDBC realization — FIELD-LESS (immutable); real per-op SQL against the ambient
 * connection; @ghost stub rows; trusted via the axioms. */
case class JdbcUserRepository() extends UserRepository {
  @ghost def rows: List[User] = Nil[User]()

  @extern @pure
  def save(u: User): UserRepository = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "INSERT INTO USERS (ID, USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE, ROLE) VALUES (?,?,?,?,?,?,?,?)")
    ps.setLong(1, longOfFM(u.id.getOrElse(FMLong(BigInt(0)))))
    ps.setString(2, u.userName); ps.setString(3, u.firstName); ps.setString(4, u.lastName)
    ps.setString(5, u.email); ps.setString(6, u.hash); ps.setString(7, u.phone)
    ps.setString(8, u.role.roleRepr)
    ps.executeUpdate()
    JdbcUserRepository()
  }.ensuring((res: UserRepository) =>
    forall((id: FMLong) => (u.id == Some[FMLong](id)) ==> (res.get(id) == Some[User](u))) &&
    res.findByUserName(u.userName) == Some[User](u))

  @extern @pure
  def get(id: FMLong): Option[User] = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "SELECT ID, USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE, ROLE FROM USERS WHERE ID = ?")
    ps.setLong(1, longOfFM(id))
    val rs = ps.executeQuery()
    if rs.next() then Some[User](rowToUser(rs)) else None[User]()
  }

  @extern @pure
  def findByUserName(name: String): Option[User] = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "SELECT ID, USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE, ROLE FROM USERS WHERE USER_NAME = ?")
    ps.setString(1, name)
    val rs = ps.executeQuery()
    if rs.next() then Some[User](rowToUser(rs)) else None[User]()
  }

  @extern @pure
  def update(u: User): UserRepository = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "UPDATE USERS SET USER_NAME=?, FIRST_NAME=?, LAST_NAME=?, EMAIL=?, HASH=?, PHONE=?, ROLE=? WHERE ID=?")
    ps.setString(1, u.userName); ps.setString(2, u.firstName); ps.setString(3, u.lastName)
    ps.setString(4, u.email); ps.setString(5, u.hash); ps.setString(6, u.phone)
    ps.setString(7, u.role.roleRepr); ps.setLong(8, longOfFM(u.id.getOrElse(FMLong(BigInt(0)))))
    ps.executeUpdate()
    JdbcUserRepository()
  }

  @extern @pure
  def delete(id: FMLong): UserRepository = {
    val ps = ConnProvider.conn().underlying.prepareStatement("DELETE FROM USERS WHERE ID = ?")
    ps.setLong(1, longOfFM(id)); ps.executeUpdate()
    JdbcUserRepository()
  }.ensuring((res: UserRepository) => res.get(id) == None[User]())

  @extern @pure
  def deleteByUserName(name: String): UserRepository = {
    val ps = ConnProvider.conn().underlying.prepareStatement("DELETE FROM USERS WHERE USER_NAME = ?")
    ps.setString(1, name); ps.executeUpdate()
    JdbcUserRepository()
  }

  @extern @pure
  def list(pageSize: FMInt, offset: FMInt): List[User] = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "SELECT ID, USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE, ROLE FROM USERS ORDER BY ID DESC LIMIT ? OFFSET ?")
    ps.setLong(1, longOfFM(pageSize.toLong)); ps.setLong(2, longOfFM(offset.toLong))
    val rs = ps.executeQuery()
    var acc: List[User] = Nil[User]()
    while rs.next() do acc = rowToUser(rs) :: acc
    acc
  }

  @extern @pure
  private def rowToUser(rs: java.sql.ResultSet): User =
    User(rs.getString("USER_NAME"), rs.getString("FIRST_NAME"), rs.getString("LAST_NAME"),
      rs.getString("EMAIL"), rs.getString("HASH"), rs.getString("PHONE"),
      Some[FMLong](fmFromLong(rs.getLong("ID"))), Role(rs.getString("ROLE")))
}
