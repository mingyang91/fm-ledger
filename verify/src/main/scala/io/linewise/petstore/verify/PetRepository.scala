package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._
import io.linewise.petstore.Db

/* =============================================================================
 * PET — THE REPOSITORY LAYER as a sealed abstract type with a @ghost `rows` model.
 *
 *   - InMemPetRepository: a real list (the verification ORACLE / differential ref).
 *   - JdbcPetRepository: FIELD-LESS (so it stays immutable — a stored handle would
 *     taint the abstract hierarchy and the World mutable, which the Has lens forbids);
 *     `rows` is an erased ghost stub; each op is @extern @pure and delegates to the
 *     production `Db` facade (Quill, over a pooled DataSource). Quill is not on the
 *     verify classpath, so it cannot live in the verified source; the @extern body is
 *     havoc'd anyway, so it delegates to `Db` (a verify-only stub here; real Quill DAO
 *     in production). TRUSTED via the axioms it carries on its ensurings; the
 *     in-memory-vs-JDBC differential test is the machine-checked guard.
 *
 * Axioms (head-match, so the in-memory oracle discharges them; the JDBC realization is
 * trusted via ensurings): saveGet, deleteGet. The query ops and update are checked by
 * the differential test; the distinct-id invariant is the PET primary key in production.
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

/* JDBC realization — FIELD-LESS (immutable); each op @extern, delegating to the
 * production Quill `Db` facade; @ghost stub rows; trusted via the axioms. */
case class JdbcPetRepository() extends PetRepository {
  @ghost def rows: List[Pet] = Nil[Pet]()

  @extern @pure
  def save(pet: Pet): PetRepository = { Db.insertPet(pet); JdbcPetRepository() }.ensuring((res: PetRepository) =>
    forall((id: FMLong) => (pet.id == Some[FMLong](id)) ==> (res.get(id) == Some[Pet](pet))))

  @extern @pure
  def get(id: FMLong): Option[Pet] = Db.petById(id)

  @extern @pure
  def update(pet: Pet): PetRepository = { Db.updatePet(pet); JdbcPetRepository() }

  @extern @pure
  def delete(id: FMLong): PetRepository =
    { Db.deletePet(id); JdbcPetRepository() }.ensuring((res: PetRepository) => res.get(id) == None[Pet]())

  @extern @pure
  def findByNameAndCategory(name: String, category: String): List[Pet] = Db.petsByNameAndCategory(name, category)

  @extern @pure
  def list(pageSize: FMInt, offset: FMInt): List[Pet] = Db.petsList(pageSize, offset)

  @extern @pure
  def findByStatus(statuses: List[PetStatus]): List[Pet] = Db.petsByStatus(statuses)

  @extern @pure
  def findByTag(tags: List[String]): List[Pet] = Db.petsByTag(tags)
}
