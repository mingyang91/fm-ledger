package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import io.linewise.ledger.Db

/* =============================================================================
 * LEDGER — THE REPOSITORY as a sealed abstract type with a @ghost `rows` model.
 *
 *   - InMemLedgerRepository: a real list (the verification ORACLE / differential ref);
 *     discharges the postGet axiom by head-match.
 *   - JdbcLedgerRepository: FIELD-LESS (so it stays immutable — a stored handle would
 *     taint the World mutable, which the Has lens forbids); `rows` is an erased ghost
 *     stub; each op is @extern @pure and delegates to the production Quill `Db` facade.
 *     Quill is not on the verify classpath, so it cannot live in verified source; the
 *     @extern body is havoc'd, so it delegates to `Db` (a verify-only stub here, the
 *     real Quill DAO in production). TRUSTED via the axiom on its ensuring; the
 *     in-memory-vs-JDBC differential test is the machine-checked guard.
 *
 * The ledger is APPEND-ONLY: `post` only ever adds; there is no update or delete. The
 * postGet axiom (post(tx).get(tx.id) == Some(tx)) is head-match dischargeable.
 * ========================================================================== */
sealed abstract class LedgerRepository {
  @ghost def rows: List[LedgerTx]

  def post(tx: LedgerTx): LedgerRepository
  def get(id: FMLong): Option[LedgerTx]
  def findBySource(kind: String, id: String): Option[LedgerTx]
  def all: List[LedgerTx]

  @law def postGet(tx: LedgerTx): Boolean =
    post(tx).get(tx.id) == Some[LedgerTx](tx)
}

/* IN-MEMORY oracle — a real list; discharges the axiom by head-match. */
case class InMemLedgerRepository(items: List[LedgerTx]) extends LedgerRepository {
  @ghost def rows: List[LedgerTx] = items
  def post(tx: LedgerTx): LedgerRepository = InMemLedgerRepository(tx :: items)
  def get(id: FMLong): Option[LedgerTx] = items.find((t: LedgerTx) => t.id == id)
  def findBySource(kind: String, id: String): Option[LedgerTx] =
    items.find((t: LedgerTx) => t.sourceKind == Some[String](kind) && t.sourceId == Some[String](id))
  def all: List[LedgerTx] = items
}

/* JDBC realization — FIELD-LESS (immutable); each op @extern, delegating to the
 * production Quill `Db` facade; @ghost stub rows; trusted via the axiom. */
case class JdbcLedgerRepository() extends LedgerRepository {
  @ghost def rows: List[LedgerTx] = Nil[LedgerTx]()

  @extern @pure
  def post(tx: LedgerTx): LedgerRepository = { Db.insertTx(tx); JdbcLedgerRepository() }.ensuring((res: LedgerRepository) =>
    res.get(tx.id) == Some[LedgerTx](tx))

  @extern @pure
  def get(id: FMLong): Option[LedgerTx] = Db.txById(id)

  @extern @pure
  def findBySource(kind: String, id: String): Option[LedgerTx] = Db.txBySource(kind, id)

  @extern @pure
  def all: List[LedgerTx] = Db.allTxs
}
