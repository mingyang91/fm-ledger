package io.linewise.petstore

import io.linewise.petstore.generated.PetStoreModel.*
import io.linewise.petstore.generated.PetValidation as GenPetValidation

/* =============================================================================
 * PET — THE SERVICE LAYER, production. The domain entry point (mirrors the
 * original PetService): it COMPOSES the validation (the generated, verified
 * PetValidation) with a PetRepository realization. No cats-effect — pure Either.
 * The differential test runs this same service over the in-memory and the JDBC
 * repository, confirming they agree.
 * ========================================================================== */
final class PetService(repo: PetRepository):

  def create(pet: Pet): Either[ValidationError, Pet] =
    if GenPetValidation.doesNotExist(repo.snapshot, pet) then Right(repo.create(pet))
    else Left(PetAlreadyExistsError(pet))

  def update(pet: Pet): Either[ValidationError, Pet] =
    if GenPetValidation.existsOpt(repo.snapshot, pet.id) then
      repo.update(pet) match
        case Some(p) => Right(p)
        case None    => Left(PetNotFoundError)
    else Left(PetNotFoundError)

  def get(id: Long): Either[ValidationError, Pet] =
    repo.get(id) match
      case Some(p) => Right(p)
      case None    => Left(PetNotFoundError)

  def delete(id: Long): Unit = repo.delete(id)

  def list(pageSize: Int, offset: Int): List[Pet] = repo.list(pageSize, offset)
  def findByStatus(statuses: List[PetStatus]): List[Pet] = repo.findByStatus(statuses)
  def findByTag(tags: List[String]): List[Pet] = repo.findByTag(tags)
