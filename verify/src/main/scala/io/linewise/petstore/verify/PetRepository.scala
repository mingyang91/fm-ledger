package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._

/* =============================================================================
 * PET — THE REPOSITORY LAYER as a VALUE with methods (PetRepositoryAlgebra). A
 * World holds a `PetRepository` value; a service reaches it through a Has lens and
 * rewrites it with `repo.save(pet)` etc. Pure data access, no validation.
 * Transpile-clean; transpiler input.
 * ========================================================================== */
case class PetRepository(rows: List[Pet]) {

  // save inserts a fully-formed pet (its id already assigned by the service from
  // the trusted BIGSERIAL sequence). `hasPets(w).write(_.save(saved))`.
  def save(pet: Pet): PetRepository = PetRepository(pet :: rows)

  def get(id: FMLong): Option[Pet] =
    rows.find((p: Pet) => p.id == Some[FMLong](id))

  def update(pet: Pet): PetRepository =
    PetRepository(rows.map((p: Pet) => if p.id == pet.id then pet else p))

  def delete(id: FMLong): PetRepository =
    PetRepository(rows.filter((p: Pet) => p.id != Some[FMLong](id)))

  def findByNameAndCategory(name: String, category: String): List[Pet] =
    rows.filter((p: Pet) => p.name == name && p.category == category)

  def list(pageSize: FMInt, offset: FMInt): List[Pet] =
    rows.drop(offset.value).take(pageSize.value)

  def findByStatus(statuses: List[PetStatus]): List[Pet] =
    rows.filter((p: Pet) => statuses.contains(p.status))

  def findByTag(tags: List[String]): List[Pet] =
    rows.filter((p: Pet) => tags.exists((tag: String) => p.tags.contains(tag)))
}
