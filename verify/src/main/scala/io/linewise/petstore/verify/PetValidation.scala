package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._
import PetRepository.PetTable

/* =============================================================================
 * PET — THE VALIDATION LAYER (PetValidationInterpreter, realized purely).
 * Uses the repository to decide existence; no persistence of its own.
 * Transpile-clean; transpiler input.
 * ========================================================================== */
object PetValidation {

  // doesNotExist: no pet with the same name+category shares this bio.
  def doesNotExist(t: PetTable, pet: Pet): Boolean =
    PetRepository.findByNameAndCategory(t, pet.name, pet.category).forall((m: Pet) => m.bio != pet.bio)

  def exists(t: PetTable, id: FMLong): Boolean =
    PetRepository.get(t, id).isDefined

  def existsOpt(t: PetTable, petId: Option[FMLong]): Boolean =
    petId match
      case Some(id) => exists(t, id)
      case _        => false
}
