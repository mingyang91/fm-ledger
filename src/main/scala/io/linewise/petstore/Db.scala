package io.linewise.petstore

import javax.sql.DataSource
import io.linewise.petstore.generated.PetStoreModel.*

/* =============================================================================
 * PRODUCTION persistence facade — the real counterpart of the verify-only `Db` stub
 * the generated field-less repositories delegate to. Holds the process-wide Quill DAO
 * (set once over a pooled DataSource via `init`) and forwards each op to it. This
 * replaces the old raw-connection `ConnProvider`: Quill owns and pools the connections,
 * so there is no shared java.sql.Connection and no per-request connection binding here.
 * ========================================================================== */
object Db:
  @volatile private var impl: PetStoreDb = null

  /** Bind the Quill DAO over a DataSource (the server / differential test calls this). */
  def init(ds: DataSource): Unit = impl = new PetStoreDb(ds)

  private def db: PetStoreDb =
    if impl == null then throw new IllegalStateException("Db not initialized — call Db.init(dataSource)") else impl

  // pets
  def insertPet(p: Pet): Unit = db.insertPet(p)
  def petById(id: Long): Option[Pet] = db.petById(id)
  def updatePet(p: Pet): Unit = db.updatePet(p)
  def deletePet(id: Long): Unit = db.deletePet(id)
  def petsByNameAndCategory(name: String, category: String): List[Pet] = db.petsByNameAndCategory(name, category)
  def petsByStatus(statuses: List[PetStatus]): List[Pet] = db.petsByStatus(statuses)
  def petsByTag(tags: List[String]): List[Pet] = db.petsByTag(tags)
  def petsList(pageSize: Int, offset: Int): List[Pet] = db.petsList(pageSize, offset)

  // users
  def insertUser(u: User): Unit = db.insertUser(u)
  def userById(id: Long): Option[User] = db.userById(id)
  def userByName(name: String): Option[User] = db.userByName(name)
  def updateUser(u: User): Unit = db.updateUser(u)
  def deleteUser(id: Long): Unit = db.deleteUser(id)
  def deleteUserByName(name: String): Unit = db.deleteUserByName(name)
  def usersList(pageSize: Int, offset: Int): List[User] = db.usersList(pageSize, offset)

  // orders
  def insertOrder(o: Order): Unit = db.insertOrder(o)
  def orderById(id: Long): Option[Order] = db.orderById(id)
  def deleteOrder(id: Long): Unit = db.deleteOrder(id)
