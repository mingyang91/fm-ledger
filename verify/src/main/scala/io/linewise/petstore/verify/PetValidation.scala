package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * PET — THE VALIDATION LAYER (PetValidationInterpreter). Pure predicates over a
 * PetRepository value; no persistence. Transpile-clean; transpiler input.
 * ========================================================================== */
object PetValidation {

  def doesNotExist(repo: PetRepository, pet: Pet): Boolean =
    repo.findByNameAndCategory(pet.name, pet.category).forall((m: Pet) => m.bio != pet.bio)

  def exists(repo: PetRepository, id: FMLong): Boolean =
    repo.get(id).isDefined

  def existsOpt(repo: PetRepository, petId: Option[FMLong]): Boolean =
    petId match
      case Some(id) => exists(repo, id)
      case _        => false
}
