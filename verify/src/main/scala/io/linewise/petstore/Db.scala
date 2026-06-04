package io.linewise.petstore

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.{FMInt, FMLong}
import io.linewise.verify.fm.petstore.PetStoreModel._

/* =============================================================================
 * VERIFY-ONLY STUB for the production Quill persistence facade. The field-less JDBC
 * repositories' @extern bodies delegate here. Quill is a third-party macro library that
 * is NOT on the Stainless verify classpath, so it cannot appear in the verified source;
 * these @extern signatures are havoc'd in verification and never executed (the repo
 * bodies that call them are themselves @extern). This file lives in the production-shaped
 * package `io.linewise.petstore` (NOT `io.linewise.verify.*`), so the generated code
 * references it verbatim and resolves to the HAND-WRITTEN production `Db` (real Quill DAO
 * over a pooled DataSource) — no transpiler import rule needed. Never transpiled.
 * ========================================================================== */
object Db {
  // pets
  @extern def insertPet(p: Pet): Unit = ???
  @extern def petById(id: FMLong): Option[Pet] = ???
  @extern def updatePet(p: Pet): Unit = ???
  @extern def deletePet(id: FMLong): Unit = ???
  @extern def petsByNameAndCategory(name: String, category: String): List[Pet] = ???
  @extern def petsByStatus(statuses: List[PetStatus]): List[Pet] = ???
  @extern def petsByTag(tags: List[String]): List[Pet] = ???
  @extern def petsList(pageSize: FMInt, offset: FMInt): List[Pet] = ???

  // users
  @extern def insertUser(u: User): Unit = ???
  @extern def userById(id: FMLong): Option[User] = ???
  @extern def userByName(name: String): Option[User] = ???
  @extern def updateUser(u: User): Unit = ???
  @extern def deleteUser(id: FMLong): Unit = ???
  @extern def deleteUserByName(name: String): Unit = ???
  @extern def usersList(pageSize: FMInt, offset: FMInt): List[User] = ???

  // orders
  @extern def insertOrder(o: Order): Unit = ???
  @extern def orderById(id: FMLong): Option[Order] = ???
  @extern def deleteOrder(id: FMLong): Unit = ???
}
