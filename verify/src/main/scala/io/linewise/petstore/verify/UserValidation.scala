package io.linewise.verify.fm.petstore

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import PetStoreModel._
import UserRepository.UserTable

/* =============================================================================
 * USER — THE VALIDATION LAYER (UserValidationInterpreter, realized purely).
 * doesNotExist: no user already has this userName. Transpile-clean.
 * ========================================================================== */
object UserValidation {

  def doesNotExist(t: UserTable, user: User): Boolean =
    UserRepository.findByUserName(t, user.userName).isEmpty

  def exists(t: UserTable, id: FMLong): Boolean =
    UserRepository.get(t, id).isDefined

  def existsOpt(t: UserTable, userId: Option[FMLong]): Boolean =
    userId match
      case Some(id) => exists(t, id)
      case _        => false
}
