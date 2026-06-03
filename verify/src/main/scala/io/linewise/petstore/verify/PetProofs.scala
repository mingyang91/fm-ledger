package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import PetStoreModel._

/* =============================================================================
 * PET PROOFS — VERIFY-ONLY. Every PetService capability is proven over the
 * concrete World and the HasPets lens — i.e. through the [W]-generic service wired
 * by a lawful lens — plus the distinct-id invariant on the pet repository value.
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

  def repoInv(repo: PetRepository): Boolean = distinctL(petIds(repo.rows))

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

  /* --- SERVICE capabilities over the World + HasPets lens. --- */

  def createThenGet(w: World, pet: Pet, freshId: FMLong): Boolean = {
    require(PetValidation.doesNotExist(w.pets, pet))
    val svc = PetService[World](HasPets())
    svc.create(w, pet, freshId) match
      case (w1, Right(saved)) =>
        saved.id == Some[FMLong](freshId) && svc.get(w1, freshId) == Right[ValidationError, Pet](saved)
      case (_, Left(_)) => false
  }.holds

  def createRejectsDuplicate(w: World, pet: Pet, freshId: FMLong): Boolean = {
    require(!PetValidation.doesNotExist(w.pets, pet))
    val svc = PetService[World](HasPets())
    svc.create(w, pet, freshId)._2 == Left[ValidationError, Pet](PetAlreadyExistsError(pet))
  }.holds

  def createPreservesDistinct(w: World, pet: Pet, freshId: FMLong): Boolean = {
    require(PetValidation.doesNotExist(w.pets, pet))
    require(repoInv(w.pets) && !petIds(w.pets.rows).contains(freshId))
    val svc = PetService[World](HasPets())
    repoInv(svc.create(w, pet, freshId)._1.pets)
  }.holds

  def getMissingFails(w: World, id: FMLong): Boolean = {
    require(w.pets.get(id) == None[Pet]())
    val svc = PetService[World](HasPets())
    svc.get(w, id) == Left[ValidationError, Pet](PetNotFoundError)
  }.holds

  def updateMissingFails(w: World, pet: Pet): Boolean = {
    require(!PetValidation.existsOpt(w.pets, pet.id))
    val svc = PetService[World](HasPets())
    svc.update(w, pet)._2 == Left[ValidationError, Pet](PetNotFoundError)
  }.holds

  def deleteRemoves(w: World, id: FMLong): Boolean = {
    deleteRemovesLemma(w.pets.rows, id)
    val svc = PetService[World](HasPets())
    svc.get(svc.delete(w, id), id) == Left[ValidationError, Pet](PetNotFoundError)
  }.holds

  def findByStatusSound(w: World, statuses: List[PetStatus]): Boolean = {
    filterSat(w.pets.rows, (p: Pet) => statuses.contains(p.status))
    val svc = PetService[World](HasPets())
    svc.findByStatus(w, statuses).forall((p: Pet) => statuses.contains(p.status))
  }.holds
}
