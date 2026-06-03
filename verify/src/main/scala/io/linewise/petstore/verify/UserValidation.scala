package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._

/* =============================================================================
 * USER — THE VALIDATION LAYER (UserValidationInterpreter). Pure predicates over a
 * UserRepository value. Transpile-clean.
 * ========================================================================== */
object UserValidation {

  def doesNotExist(repo: UserRepository, user: User): Boolean =
    repo.findByUserName(user.userName).isEmpty

  def exists(repo: UserRepository, id: FMLong): Boolean =
    repo.get(id).isDefined

  def existsOpt(repo: UserRepository, userId: Option[FMLong]): Boolean =
    userId match
      case Some(id) => exists(repo, id)
      case _        => false
}
