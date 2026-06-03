package io.linewise.petstore

import java.sql.{Connection, ResultSet}
import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.UserRepository as GenUserRepository
import io.linewise.petstore.generated.UserRepository.UserTable

/* =============================================================================
 * USER — THE REPOSITORY LAYER, production (data access). InMemoryUserRepository
 * single-sourced from the generated UserRepository; JdbcUserRepository over the
 * isomorphic USERS table (USER_NAME UNIQUE, ROLE default 'Customer'). Role <-> its
 * roleRepr VARCHAR. The id source is the BIGSERIAL key.
 * ========================================================================== */
trait UserRepository:
  def snapshot: UserTable
  def create(user: User): User
  def get(id: Long): Option[User]
  def findByUserName(userName: String): Option[User]
  def update(user: User): Option[User]
  def delete(id: Long): Unit
  def deleteByUserName(userName: String): Unit
  def list(pageSize: Int, offset: Int): List[User]

final class InMemoryUserRepository extends UserRepository:
  private var t: UserTable = UserTable(Nil)
  private var nextId: Long = 1L
  def snapshot: UserTable = t
  def create(user: User): User =
    val (t2, saved) = GenUserRepository.create(t, user, nextId); t = t2; nextId += 1; saved
  def get(id: Long): Option[User] = GenUserRepository.get(t, id)
  def findByUserName(userName: String): Option[User] = GenUserRepository.findByUserName(t, userName)
  def update(user: User): Option[User] =
    val (t2, r) = GenUserRepository.update(t, user); t = t2; r
  def delete(id: Long): Unit = t = GenUserRepository.delete(t, id)._1
  def deleteByUserName(userName: String): Unit = t = GenUserRepository.deleteByUserName(t, userName)._1
  def list(pageSize: Int, offset: Int): List[User] = GenUserRepository.list(t, pageSize, offset)

final class JdbcUserRepository(c: Connection) extends UserRepository:
  def initSchema(): Unit = Jdbc.initSchema(c)

  private def rowToUser(rs: ResultSet): User =
    User(
      rs.getString("USER_NAME"), rs.getString("FIRST_NAME"), rs.getString("LAST_NAME"),
      rs.getString("EMAIL"), rs.getString("HASH"), rs.getString("PHONE"),
      Some(rs.getLong("ID")), Role(rs.getString("ROLE")),
    )

  def snapshot: UserTable =
    UserTable(Jdbc.query(
      c, "SELECT ID, USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE, ROLE FROM USERS ORDER BY ID DESC")(_ => ())(rowToUser))

  def create(user: User): User =
    val id = Jdbc.insertReturningId(
      c, "INSERT INTO USERS (USER_NAME, FIRST_NAME, LAST_NAME, EMAIL, HASH, PHONE, ROLE) VALUES (?,?,?,?,?,?,?)") { ps =>
      ps.setString(1, user.userName); ps.setString(2, user.firstName); ps.setString(3, user.lastName)
      ps.setString(4, user.email); ps.setString(5, user.hash); ps.setString(6, user.phone)
      ps.setString(7, user.role.roleRepr)
    }
    user.copy(id = Some(id))

  def get(id: Long): Option[User] = GenUserRepository.get(snapshot, id)
  def findByUserName(userName: String): Option[User] = GenUserRepository.findByUserName(snapshot, userName)

  def update(user: User): Option[User] =
    user.id match
      case Some(uid) if GenUserRepository.get(snapshot, uid).isDefined =>
        Jdbc.update(
          c, "UPDATE USERS SET USER_NAME=?, FIRST_NAME=?, LAST_NAME=?, EMAIL=?, HASH=?, PHONE=?, ROLE=? WHERE ID=?") { ps =>
          ps.setString(1, user.userName); ps.setString(2, user.firstName); ps.setString(3, user.lastName)
          ps.setString(4, user.email); ps.setString(5, user.hash); ps.setString(6, user.phone)
          ps.setString(7, user.role.roleRepr); ps.setLong(8, uid)
        }
        Some(user)
      case _ => None

  def delete(id: Long): Unit =
    Jdbc.update(c, "DELETE FROM USERS WHERE ID=?")(_.setLong(1, id)); ()
  def deleteByUserName(userName: String): Unit =
    Jdbc.update(c, "DELETE FROM USERS WHERE USER_NAME=?")(_.setString(1, userName)); ()
  def list(pageSize: Int, offset: Int): List[User] = GenUserRepository.list(snapshot, pageSize, offset)
