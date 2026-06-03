package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._
import JdbcSupport.{fmFromLong, longOfFM}
import io.linewise.petstore.ConnProvider
import io.linewise.petstore.Jdbc.{encodeList, decodeList}

/* =============================================================================
 * PET — THE REPOSITORY LAYER as a sealed abstract type with a @ghost `rows` model.
 *
 *   - InMemPetRepository: a real list (the verification ORACLE / differential ref).
 *   - JdbcPetRepository: FIELD-LESS (so it stays immutable — a stored Conn would
 *     taint the abstract hierarchy and the World mutable, which the Has lens forbids);
 *     `rows` is an erased ghost stub; each op is @extern @pure REAL PER-OPERATION SQL
 *     (no whole-table load) that fetches the ambient connection from ConnProvider
 *     INSIDE its havoc'd body. TRUSTED by the algebraic axioms it carries on its
 *     ensurings; the in-memory-vs-JDBC differential test is the machine-checked guard.
 *
 * Axioms (head-match, so the in-memory oracle discharges them; the JDBC realization
 * is trusted via ensurings): saveGet (a saved row is gotten back by its id) and
 * deleteGet (a deleted id is no longer gotten). The queries (findByNameAndCategory /
 * findByStatus / findByTag / list) and update are checked by the differential test;
 * the distinct-id invariant is enforced in production by the PET primary key.
 * ========================================================================== */
sealed abstract class PetRepository {
  @ghost def rows: List[Pet]

  def save(pet: Pet): PetRepository
  def get(id: FMLong): Option[Pet]
  def update(pet: Pet): PetRepository
  def delete(id: FMLong): PetRepository
  def findByNameAndCategory(name: String, category: String): List[Pet]
  def list(pageSize: FMInt, offset: FMInt): List[Pet]
  def findByStatus(statuses: List[PetStatus]): List[Pet]
  def findByTag(tags: List[String]): List[Pet]

  @law def saveGet(pet: Pet, id: FMLong): Boolean =
    (pet.id == Some[FMLong](id)) ==> (save(pet).get(id) == Some[Pet](pet))
  @law def deleteGet(id: FMLong): Boolean =
    delete(id).get(id) == None[Pet]()
}

/* IN-MEMORY oracle — a real list; discharges the axioms by head-match / lemma. */
case class InMemPetRepository(items: List[Pet]) extends PetRepository {
  @ghost def rows: List[Pet] = items
  def save(pet: Pet): PetRepository = InMemPetRepository(pet :: items)
  def get(id: FMLong): Option[Pet] = items.find((p: Pet) => p.id == Some[FMLong](id))
  def update(pet: Pet): PetRepository = InMemPetRepository(items.map((p: Pet) => if p.id == pet.id then pet else p))
  def delete(id: FMLong): PetRepository =
    InMemPetRepository(items.filter((p: Pet) => p.id != Some[FMLong](id)))
      .ensuring((res: PetRepository) => { PetProofs.petDeleteGetLemma(items, id); res.get(id) == None[Pet]() })
  def findByNameAndCategory(name: String, category: String): List[Pet] =
    items.filter((p: Pet) => p.name == name && p.category == category)
  def list(pageSize: FMInt, offset: FMInt): List[Pet] = items.drop(offset.value).take(pageSize.value)
  def findByStatus(statuses: List[PetStatus]): List[Pet] = items.filter((p: Pet) => statuses.contains(p.status))
  def findByTag(tags: List[String]): List[Pet] =
    items.filter((p: Pet) => tags.exists((tag: String) => p.tags.contains(tag)))
}

/* JDBC realization — FIELD-LESS (immutable); real per-op SQL against the ambient
 * connection; @ghost stub rows; trusted via the axioms. */
case class JdbcPetRepository() extends PetRepository {
  @ghost def rows: List[Pet] = Nil[Pet]()

  @extern @pure
  def save(pet: Pet): PetRepository = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "INSERT INTO PET (ID, NAME, CATEGORY, BIO, STATUS, PHOTO_URLS, TAGS) VALUES (?,?,?,?,?,?,?)")
    ps.setLong(1, longOfFM(pet.id.getOrElse(FMLong(BigInt(0)))))
    ps.setString(2, pet.name); ps.setString(3, pet.category); ps.setString(4, pet.bio)
    ps.setString(5, petStatusToStr(pet.status))
    ps.setString(6, encodeList(pet.photoUrls)); ps.setString(7, encodeList(pet.tags))
    ps.executeUpdate()
    JdbcPetRepository()
  }.ensuring((res: PetRepository) =>
    forall((id: FMLong) => (pet.id == Some[FMLong](id)) ==> (res.get(id) == Some[Pet](pet))))

  @extern @pure
  def get(id: FMLong): Option[Pet] = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "SELECT ID, NAME, CATEGORY, BIO, STATUS, PHOTO_URLS, TAGS FROM PET WHERE ID = ?")
    ps.setLong(1, longOfFM(id))
    val rs = ps.executeQuery()
    if rs.next() then Some[Pet](rowToPet(rs)) else None[Pet]()
  }

  @extern @pure
  def update(pet: Pet): PetRepository = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "UPDATE PET SET NAME=?, CATEGORY=?, BIO=?, STATUS=?, PHOTO_URLS=?, TAGS=? WHERE ID=?")
    ps.setString(1, pet.name); ps.setString(2, pet.category); ps.setString(3, pet.bio)
    ps.setString(4, petStatusToStr(pet.status))
    ps.setString(5, encodeList(pet.photoUrls)); ps.setString(6, encodeList(pet.tags))
    ps.setLong(7, longOfFM(pet.id.getOrElse(FMLong(BigInt(0)))))
    ps.executeUpdate()
    JdbcPetRepository()
  }

  @extern @pure
  def delete(id: FMLong): PetRepository = {
    val ps = ConnProvider.conn().underlying.prepareStatement("DELETE FROM PET WHERE ID = ?")
    ps.setLong(1, longOfFM(id)); ps.executeUpdate()
    JdbcPetRepository()
  }.ensuring((res: PetRepository) => res.get(id) == None[Pet]())

  @extern @pure
  def findByNameAndCategory(name: String, category: String): List[Pet] = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "SELECT ID, NAME, CATEGORY, BIO, STATUS, PHOTO_URLS, TAGS FROM PET WHERE NAME = ? AND CATEGORY = ?")
    ps.setString(1, name); ps.setString(2, category)
    drainPets(ps.executeQuery())
  }

  @extern @pure
  def list(pageSize: FMInt, offset: FMInt): List[Pet] = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "SELECT ID, NAME, CATEGORY, BIO, STATUS, PHOTO_URLS, TAGS FROM PET ORDER BY ID DESC LIMIT ? OFFSET ?")
    ps.setLong(1, longOfFM(pageSize.toLong)); ps.setLong(2, longOfFM(offset.toLong))
    drainPets(ps.executeQuery())
  }

  @extern @pure
  def findByStatus(statuses: List[PetStatus]): List[Pet] = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "SELECT ID, NAME, CATEGORY, BIO, STATUS, PHOTO_URLS, TAGS FROM PET")
    drainPets(ps.executeQuery()).filter((p: Pet) => statuses.contains(p.status))
  }

  @extern @pure
  def findByTag(tags: List[String]): List[Pet] = {
    val ps = ConnProvider.conn().underlying.prepareStatement(
      "SELECT ID, NAME, CATEGORY, BIO, STATUS, PHOTO_URLS, TAGS FROM PET")
    drainPets(ps.executeQuery()).filter((p: Pet) => tags.exists((tag: String) => p.tags.contains(tag)))
  }

  @extern @pure
  private def drainPets(rs: java.sql.ResultSet): List[Pet] = {
    var acc: List[Pet] = Nil[Pet]()
    while rs.next() do acc = rowToPet(rs) :: acc
    acc
  }

  @extern @pure
  private def rowToPet(rs: java.sql.ResultSet): Pet =
    Pet(rs.getString("NAME"), rs.getString("CATEGORY"), rs.getString("BIO"),
      strToPetStatus(rs.getString("STATUS")), decodeList(rs.getString("TAGS")),
      decodeList(rs.getString("PHOTO_URLS")), Some[FMLong](fmFromLong(rs.getLong("ID"))))
}

/* Pure status <-> VARCHAR converters (real logic; transpiled). Used inside the JDBC
 * @extern bodies; total so verification accepts them. */
def petStatusToStr(s: PetStatus): String = s match
  case Available => "Available"
  case Pending   => "Pending"
  case Adopted   => "Adopted"

def strToPetStatus(s: String): PetStatus =
  if s == "Available" then Available else if s == "Pending" then Pending else Adopted
