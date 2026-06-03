package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._
import PetRepository.PetTable

/* =============================================================================
 * PET PROOFS — VERIFY-ONLY. Each PetService capability is proven over the layered
 * Repository / Validation / Service objects, plus the distinct-id store invariant.
 * Never transpiled.
 * ========================================================================== */
object PetProofs {

  def petIds(rows: List[Pet]): List[FMLong] =
    rows match
      case Nil()      => Nil[FMLong]()
      case Cons(h, t) => h.id match
          case Some(i) => i :: petIds(t)
          case _       => petIds(t)

  def distinctL(xs: List[FMLong]): Boolean =
    xs match
      case Nil()      => true
      case Cons(h, t) => !t.contains(h) && distinctL(t)

  def tableInv(t: PetTable): Boolean = distinctL(petIds(t.rows))

  @opaque @inlineOnce
  def filterSat(xs: List[Pet], q: Pet => Boolean): Unit = {
    xs match
      case Nil()      => ()
      case Cons(h, t) => filterSat(t, q)
  }.ensuring(_ => xs.filter(q).forall(q))

  @opaque @inlineOnce
  def deleteRemovesLemma(rows: List[Pet], id: FMLong): Unit = {
    rows match
      case Nil()      => ()
      case Cons(h, t) => deleteRemovesLemma(t, id)
  }.ensuring(_ =>
    rows.filter((p: Pet) => p.id != Some[FMLong](id)).find((p: Pet) => p.id == Some[FMLong](id)) == None[Pet]())

  /* --- SERVICE-LAYER capabilities (PetService), proven. --- */

  def createThenGet(t: PetTable, pet: Pet, freshId: FMLong): Boolean = {
    require(PetValidation.doesNotExist(t, pet))
    PetService.create(t, pet, freshId) match
      case Right((t2, saved)) =>
        saved.id == Some[FMLong](freshId) && PetRepository.get(t2, freshId) == Some[Pet](saved)
      case Left(_) => false
  }.holds

  def createRejectsDuplicate(t: PetTable, pet: Pet, freshId: FMLong): Boolean = {
    require(!PetValidation.doesNotExist(t, pet))
    PetService.create(t, pet, freshId) == Left[ValidationError, (PetTable, Pet)](PetAlreadyExistsError(pet))
  }.holds

  def createPreservesDistinct(t: PetTable, pet: Pet, freshId: FMLong): Boolean = {
    require(tableInv(t) && !petIds(t.rows).contains(freshId))
    tableInv(PetRepository.create(t, pet, freshId)._1)
  }.holds

  def getPresentSucceeds(t: PetTable, id: FMLong, p: Pet): Boolean = {
    require(PetRepository.get(t, id) == Some[Pet](p))
    PetService.get(t, id) == Right[ValidationError, Pet](p)
  }.holds

  def getMissingFails(t: PetTable, id: FMLong): Boolean = {
    require(PetRepository.get(t, id) == None[Pet]())
    PetService.get(t, id) == Left[ValidationError, Pet](PetNotFoundError)
  }.holds

  def updateMissingFails(t: PetTable, pet: Pet): Boolean = {
    require(!PetValidation.existsOpt(t, pet.id))
    PetService.update(t, pet) == Left[ValidationError, (PetTable, Pet)](PetNotFoundError)
  }.holds

  def deleteRemoves(t: PetTable, id: FMLong): Boolean = {
    deleteRemovesLemma(t.rows, id)
    PetRepository.get(PetService.delete(t, id), id) == None[Pet]()
  }.holds

  def findByStatusSound(t: PetTable, statuses: List[PetStatus]): Boolean = {
    filterSat(t.rows, (p: Pet) => statuses.contains(p.status))
    PetService.findByStatus(t, statuses).forall((p: Pet) => statuses.contains(p.status))
  }.holds

  def findByNameAndCategorySound(t: PetTable, name: String, category: String): Boolean = {
    filterSat(t.rows, (p: Pet) => p.name == name && p.category == category)
    PetRepository.findByNameAndCategory(t, name, category).forall((p: Pet) => p.name == name && p.category == category)
  }.holds
}
