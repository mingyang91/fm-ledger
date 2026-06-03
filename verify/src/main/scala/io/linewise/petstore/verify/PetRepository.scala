package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._

/* =============================================================================
 * PET — THE REPOSITORY LAYER (PetRepositoryAlgebra, realized purely).
 *
 * The data-access algebra: the in-memory store `PetTable` and its total ops
 * (create/get/update/delete/findByNameAndCategory/list/findByStatus/findByTag).
 * NO validation, NO service composition — that is PetService's job. The id source
 * (DB BIGSERIAL / in-memory AtomicLong) is the trusted `freshId` parameter.
 * Transpile-clean; transpiler input.
 * ========================================================================== */
object PetRepository {

  case class PetTable(rows: List[Pet])

  def create(t: PetTable, pet: Pet, freshId: FMLong): (PetTable, Pet) = {
    val saved = pet.copy(id = Some[FMLong](freshId))
    (PetTable(saved :: t.rows), saved)
  }

  def get(t: PetTable, id: FMLong): Option[Pet] =
    t.rows.find((p: Pet) => p.id == Some[FMLong](id))

  def update(t: PetTable, pet: Pet): (PetTable, Option[Pet]) =
    t.rows.find((p: Pet) => p.id == pet.id) match
      case Some(_) =>
        (PetTable(t.rows.map((p: Pet) => if p.id == pet.id then pet else p)), Some[Pet](pet))
      case _ => (t, None[Pet]())

  def delete(t: PetTable, id: FMLong): (PetTable, Option[Pet]) =
    (PetTable(t.rows.filter((p: Pet) => p.id != Some[FMLong](id))),
     t.rows.find((p: Pet) => p.id == Some[FMLong](id)))

  def findByNameAndCategory(t: PetTable, name: String, category: String): List[Pet] =
    t.rows.filter((p: Pet) => p.name == name && p.category == category)

  def list(t: PetTable, pageSize: FMInt, offset: FMInt): List[Pet] =
    t.rows.drop(offset.value).take(pageSize.value)

  def findByStatus(t: PetTable, statuses: List[PetStatus]): List[Pet] =
    t.rows.filter((p: Pet) => statuses.contains(p.status))

  def findByTag(t: PetTable, tags: List[String]): List[Pet] =
    t.rows.filter((p: Pet) => tags.exists((tag: String) => p.tags.contains(tag)))
}
