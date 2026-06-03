package io.linewise.petstore

import java.sql.{Connection, ResultSet}
import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.PetRepository as GenPetRepository
import io.linewise.petstore.generated.PetRepository.PetTable

/* =============================================================================
 * PET — THE REPOSITORY LAYER, production (data access; NO validation). Mirrors the
 * original PetRepositoryAlgebra with two interpreters:
 *   InMemoryPetRepository — single-sourced from the generated PetRepository ops
 *                           (the differential ORACLE); owns the id counter.
 *   JdbcPetRepository      — plain java.sql over the isomorphic PET table; the id
 *                           source is the BIGSERIAL key. Query/lookup logic is
 *                           delegated to the generated core over a loaded snapshot.
 * The service composition (validation) lives in PetService — not here.
 * ========================================================================== */
trait PetRepository:
  def snapshot: PetTable                  // the current table, for validation/queries
  def create(pet: Pet): Pet               // persist, assign id
  def get(id: Long): Option[Pet]
  def update(pet: Pet): Option[Pet]       // None if absent
  def delete(id: Long): Unit
  def findByNameAndCategory(name: String, category: String): List[Pet]
  def list(pageSize: Int, offset: Int): List[Pet]
  def findByStatus(statuses: List[PetStatus]): List[Pet]
  def findByTag(tags: List[String]): List[Pet]

final class InMemoryPetRepository extends PetRepository:
  private var t: PetTable = PetTable(Nil)
  private var nextId: Long = 1L
  def snapshot: PetTable = t
  def create(pet: Pet): Pet =
    val (t2, saved) = GenPetRepository.create(t, pet, nextId); t = t2; nextId += 1; saved
  def get(id: Long): Option[Pet] = GenPetRepository.get(t, id)
  def update(pet: Pet): Option[Pet] =
    val (t2, r) = GenPetRepository.update(t, pet); t = t2; r
  def delete(id: Long): Unit = t = GenPetRepository.delete(t, id)._1
  def findByNameAndCategory(name: String, category: String): List[Pet] = GenPetRepository.findByNameAndCategory(t, name, category)
  def list(pageSize: Int, offset: Int): List[Pet] = GenPetRepository.list(t, pageSize, offset)
  def findByStatus(statuses: List[PetStatus]): List[Pet] = GenPetRepository.findByStatus(t, statuses)
  def findByTag(tags: List[String]): List[Pet] = GenPetRepository.findByTag(t, tags)

object PetCodec:
  def statusStr(s: PetStatus): String = s match
    case Available => "Available"
    case Pending   => "Pending"
    case Adopted   => "Adopted"
  def strStatus(s: String): PetStatus = s match
    case "Available" => Available
    case "Pending"   => Pending
    case "Adopted"   => Adopted
    case other       => throw new IllegalStateException(s"unknown pet status in row: $other")

final class JdbcPetRepository(c: Connection) extends PetRepository:
  def initSchema(): Unit = Jdbc.initSchema(c)

  private def rowToPet(rs: ResultSet): Pet =
    Pet(
      rs.getString("NAME"), rs.getString("CATEGORY"), rs.getString("BIO"),
      PetCodec.strStatus(rs.getString("STATUS")),
      Jdbc.decodeList(rs.getString("TAGS")), Jdbc.decodeList(rs.getString("PHOTO_URLS")),
      Some(rs.getLong("ID")),
    )

  // ORDER BY ID DESC matches the verified core's prepend (newest-first), so query
  // results are identical to the in-memory oracle.
  def snapshot: PetTable =
    PetTable(Jdbc.query(c, "SELECT ID, NAME, CATEGORY, BIO, STATUS, PHOTO_URLS, TAGS FROM PET ORDER BY ID DESC")(_ => ())(rowToPet))

  def create(pet: Pet): Pet =
    val id = Jdbc.insertReturningId(
      c, "INSERT INTO PET (NAME, CATEGORY, BIO, STATUS, PHOTO_URLS, TAGS) VALUES (?,?,?,?,?,?)") { ps =>
      ps.setString(1, pet.name); ps.setString(2, pet.category); ps.setString(3, pet.bio)
      ps.setString(4, PetCodec.statusStr(pet.status))
      ps.setString(5, Jdbc.encodeList(pet.photoUrls)); ps.setString(6, Jdbc.encodeList(pet.tags))
    }
    pet.copy(id = Some(id))

  def get(id: Long): Option[Pet] = GenPetRepository.get(snapshot, id)

  def update(pet: Pet): Option[Pet] =
    pet.id match
      case Some(pid) if GenPetRepository.get(snapshot, pid).isDefined =>
        Jdbc.update(
          c, "UPDATE PET SET NAME=?, CATEGORY=?, BIO=?, STATUS=?, PHOTO_URLS=?, TAGS=? WHERE ID=?") { ps =>
          ps.setString(1, pet.name); ps.setString(2, pet.category); ps.setString(3, pet.bio)
          ps.setString(4, PetCodec.statusStr(pet.status))
          ps.setString(5, Jdbc.encodeList(pet.photoUrls)); ps.setString(6, Jdbc.encodeList(pet.tags))
          ps.setLong(7, pid)
        }
        Some(pet)
      case _ => None

  def delete(id: Long): Unit =
    Jdbc.update(c, "DELETE FROM PET WHERE ID=?")(_.setLong(1, id)); ()

  def findByNameAndCategory(name: String, category: String): List[Pet] =
    GenPetRepository.findByNameAndCategory(snapshot, name, category)
  def list(pageSize: Int, offset: Int): List[Pet] = GenPetRepository.list(snapshot, pageSize, offset)
  def findByStatus(statuses: List[PetStatus]): List[Pet] = GenPetRepository.findByStatus(snapshot, statuses)
  def findByTag(tags: List[String]): List[Pet] = GenPetRepository.findByTag(snapshot, tags)
