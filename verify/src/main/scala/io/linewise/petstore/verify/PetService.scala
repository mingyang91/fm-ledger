package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._

/* =============================================================================
 * PET — THE SERVICE LAYER, polymorphic in the world W and wired by a Has lens.
 * It composes the validation with the repository it reaches THROUGH the lens:
 *   - reads:  has.get(w)
 *   - writes: has(w).write(repo => repo.save(...))  -> a new world
 * Write ops return (W, result); read ops return the result. No cats-effect; the
 * id source is the trusted `freshId`. Transpile-clean; transpiler input.
 * ========================================================================== */
case class PetService[W](has: Has[W, PetRepository]) {

  def create(w: W, pet: Pet, freshId: FMLong): (W, Either[ValidationError, Pet]) = {
    val repo = has.get(w)
    if PetValidation.doesNotExist(repo, pet) then
      val saved = pet.copy(id = Some[FMLong](freshId))
      val w1 = has(w).write((r: PetRepository) => r.save(saved))
      (w1, Right[ValidationError, Pet](saved))
    else
      (w, Left[ValidationError, Pet](PetAlreadyExistsError(pet)))
  }

  def update(w: W, pet: Pet): (W, Either[ValidationError, Pet]) = {
    val repo = has.get(w)
    if PetValidation.existsOpt(repo, pet.id) then
      val w1 = has(w).write((r: PetRepository) => r.update(pet))
      (w1, Right[ValidationError, Pet](pet))
    else
      (w, Left[ValidationError, Pet](PetNotFoundError))
  }

  def get(w: W, id: FMLong): Either[ValidationError, Pet] =
    has.get(w).get(id) match
      case Some(p) => Right[ValidationError, Pet](p)
      case _       => Left[ValidationError, Pet](PetNotFoundError)

  // delete is idempotent; returns the new world (PetService.delete is Unit-y).
  def delete(w: W, id: FMLong): W =
    has(w).write((r: PetRepository) => r.delete(id))

  def list(w: W, pageSize: FMInt, offset: FMInt): List[Pet] =
    has.get(w).list(pageSize, offset)

  def findByStatus(w: W, statuses: List[PetStatus]): List[Pet] =
    has.get(w).findByStatus(statuses)

  def findByTag(w: W, tags: List[String]): List[Pet] =
    has.get(w).findByTag(tags)
}
