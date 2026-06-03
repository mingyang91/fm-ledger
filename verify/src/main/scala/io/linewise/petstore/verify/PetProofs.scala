package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * PET PROOFS — VERIFY-ONLY. PetService capabilities proven over the World + HasPets
 * lens and the ABSTRACT PetRepository, via the repo's algebraic axioms (saveGet,
 * deleteGet) and the service's own branching.
 *
 * Two b3ab2d1 properties no longer fit the @ghost-rows abstraction and move boundary:
 *   - createPreservesDistinct (distinct ids): the materialized id list is @ghost and
 *     unobservable in the JDBC realization; distinctness is enforced in production by
 *     the PET PRIMARY KEY (a save with a clashing id fails at the DB).
 *   - findByStatusSound (every result matches the filter): a property of the query
 *     SQL (WHERE/IN); covered by the in-memory-vs-JDBC differential test.
 * ========================================================================== */
object PetProofs {

  /* The filter-then-find lemma the in-memory delete's ensuring uses to discharge the
   * `deleteGet` axiom. Verify-only (never transpiled), so its List recursion does not
   * leak into production; the JDBC delete is trusted via its own ensuring. */
  @opaque @inlineOnce
  def petDeleteGetLemma(rows: List[Pet], id: FMLong): Unit = {
    rows match
      case Nil()      => ()
      case Cons(h, t) => petDeleteGetLemma(t, id)
  }.ensuring(_ =>
    rows.filter((p: Pet) => p.id != Some[FMLong](id)).find((p: Pet) => p.id == Some[FMLong](id)) == None[Pet]())

  /* --- SERVICE capabilities over the World + HasPets lens. --- */

  def createThenGet(w: World, pet: Pet, freshId: FMLong): Boolean = {
    require(PetValidation.doesNotExist(w.pets, pet))
    val svc = PetService[World](HasPets())
    svc.create(w, pet, freshId) match
      case (w1, Right(saved)) =>
        assert(w.pets.saveGet(saved, freshId)) // hint: save(saved).get(freshId)==Some(saved)
        saved.id == Some[FMLong](freshId) && svc.get(w1, freshId) == Right[ValidationError, Pet](saved)
      case (_, Left(_)) => false
  }.holds

  def createRejectsDuplicate(w: World, pet: Pet, freshId: FMLong): Boolean = {
    require(!PetValidation.doesNotExist(w.pets, pet))
    val svc = PetService[World](HasPets())
    svc.create(w, pet, freshId)._2 == Left[ValidationError, Pet](PetAlreadyExistsError(pet))
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
    assert(w.pets.deleteGet(id)) // hint: delete(id).get(id) == None
    val svc = PetService[World](HasPets())
    svc.get(svc.delete(w, id), id) == Left[ValidationError, Pet](PetNotFoundError)
  }.holds
}
