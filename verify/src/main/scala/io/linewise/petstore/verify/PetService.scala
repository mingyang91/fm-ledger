package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._
import PetRepository.PetTable

/* =============================================================================
 * PET — THE SERVICE LAYER (PetService). The domain entry point: it COMPOSES the
 * validation and the repository, exactly as the original PetService does. Method
 * names mirror the original (create/update/get/delete/list/findByStatus/findByTag).
 * `EitherT[F, E, A]` becomes pure `Either[ValidationError, (PetTable, A)]` — no
 * cats-effect. The id source is the trusted `freshId` parameter. Transpile-clean.
 * ========================================================================== */
object PetService {

  // create = validate doesNotExist, then repository.create.
  def create(t: PetTable, pet: Pet, freshId: FMLong): Either[ValidationError, (PetTable, Pet)] =
    if PetValidation.doesNotExist(t, pet) then
      Right[ValidationError, (PetTable, Pet)](PetRepository.create(t, pet, freshId))
    else
      Left[ValidationError, (PetTable, Pet)](PetAlreadyExistsError(pet))

  // update = validate exists, then repository.update (PetNotFoundError otherwise).
  def update(t: PetTable, pet: Pet): Either[ValidationError, (PetTable, Pet)] =
    if PetValidation.existsOpt(t, pet.id) then
      PetRepository.update(t, pet) match
        case (t2, Some(p)) => Right[ValidationError, (PetTable, Pet)]((t2, p))
        case _             => Left[ValidationError, (PetTable, Pet)](PetNotFoundError)
    else Left[ValidationError, (PetTable, Pet)](PetNotFoundError)

  def get(t: PetTable, id: FMLong): Either[ValidationError, Pet] =
    PetRepository.get(t, id) match
      case Some(p) => Right[ValidationError, Pet](p)
      case _       => Left[ValidationError, Pet](PetNotFoundError)

  // delete is idempotent; the original PetService.delete returns Unit -> we yield
  // the post-state table.
  def delete(t: PetTable, id: FMLong): PetTable =
    PetRepository.delete(t, id)._1

  // list / findByStatus / findByTag delegate to the repository (no validation).
  def list(t: PetTable, pageSize: FMInt, offset: FMInt): List[Pet] =
    PetRepository.list(t, pageSize, offset)

  def findByStatus(t: PetTable, statuses: List[PetStatus]): List[Pet] =
    PetRepository.findByStatus(t, statuses)

  def findByTag(t: PetTable, tags: List[String]): List[Pet] =
    PetRepository.findByTag(t, tags)
}
