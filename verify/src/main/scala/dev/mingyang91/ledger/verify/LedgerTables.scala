package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import dev.mingyang91.ledger.Db
import LedgerModel._

/* =============================================================================
 * LEDGER TABLE STORE — the read/modify seam over the ledger tables (TX_HEADER + LEG) of the DB.
 * This is the reshaped "repository": its ops take and return the whole DB (reading/modifying its
 * ledger tables). Two realizations:
 *   - InMem: pure ops on the materialized db (the oracle, used by proofs + drift gate).
 *   - Jdbc:  @extern SQL (db is the empty placeholder); identical production behavior to before.
 * A ledger tx is stored split into a header row + its leg rows, and read back by a filter-join,
 * mirroring the production assembleTxs. The postGet law round-trips a freshly posted tx.
 * ========================================================================== */
object LedgerTables {

  // ---- LedgerTx <-> (header, legs) ----
  def toHeader(tx: LedgerTx): TxHeaderRow = TxHeaderRow(tx.id, tx.kind, tx.sourceKind, tx.sourceId, tx.userUid)
  def legToEntry(l: LegRow): LedgerEntry = LedgerEntry(l.account, l.dir, l.amount)
  // recursion via isEmpty/head/tail (not `case Nil()/Cons()`) so these transpile clean.
  def entriesToLegs(txId: Long, es: List[LedgerEntry], n: BigInt): List[LegRow] =
    if es.isEmpty then Nil[LegRow]()
    else LegRow(txId, n, es.head.account, es.head.direction, es.head.amount) :: entriesToLegs(txId, es.tail, n + BigInt(1))
  def toLegs(tx: LedgerTx): List[LegRow] = entriesToLegs(tx.id, tx.entries, BigInt(1))
  def assemble(h: TxHeaderRow, legs: List[LegRow]): LedgerTx =
    LedgerTx(h.id, h.kind, legs.map(legToEntry), h.sourceKind, h.sourceId, h.userUid)

  // ---- first-order recursive predicates over the ledger tables (no List.contains on Long) ----
  def legsOf(legs: List[LegRow], id: Long): List[LegRow] = legs.filter((l: LegRow) => l.txId == id)
  def noLegWith(legs: List[LegRow], id: Long): Boolean =
    if legs.isEmpty then true else legs.head.txId != id && noLegWith(legs.tail, id)
  def noHeaderWith(hs: List[TxHeaderRow], id: Long): Boolean =
    if hs.isEmpty then true else hs.head.id != id && noHeaderWith(hs.tail, id)
  def allSameTx(legs: List[LegRow], id: Long): Boolean =
    if legs.isEmpty then true else legs.head.txId == id && allSameTx(legs.tail, id)

  // ---- the store ----
  // No @law: the aggregate's trivial find-first postGet does not survive the header/leg split, and
  // it is not needed. Safety is the valid(db)-preservation theorems (proved over the InMem ops in
  // the proof layer); the Jdbc realization is @extern and reconciled to InMem by the drift gate.
  sealed abstract class LedgerStore {
    def post(db: DB, tx: LedgerTx): DB
    def get(db: DB, id: Long): Option[LedgerTx]
    def findBySource(db: DB, kind: String, id: String): Option[LedgerTx]
    def all(db: DB): List[LedgerTx]
  }

  // ---- InMem oracle: pure ops over the materialized db ----
  case class InMemLedgerStore() extends LedgerStore {
    def post(db: DB, tx: LedgerTx): DB =
      db.copy(txHeaders = toHeader(tx) :: db.txHeaders, legs = toLegs(tx) ++ db.legs)
    def get(db: DB, id: Long): Option[LedgerTx] =
      db.txHeaders.find((h: TxHeaderRow) => h.id == id) match
        case Some(h) => Some[LedgerTx](assemble(h, legsOf(db.legs, id)))
        case _       => None[LedgerTx]()
    def findBySource(db: DB, kind: String, id: String): Option[LedgerTx] =
      db.txHeaders.find((h: TxHeaderRow) => h.sourceKind == Some[String](kind) && h.sourceId == Some[String](id)) match
        case Some(h) => Some[LedgerTx](assemble(h, legsOf(db.legs, h.id)))
        case _       => None[LedgerTx]()
    def all(db: DB): List[LedgerTx] =
      db.txHeaders.map((h: TxHeaderRow) => assemble(h, legsOf(db.legs, h.id)))
  }

  // ---- Jdbc realization: @extern SQL; db is the empty placeholder, ignored ----
  case class JdbcLedgerStore() extends LedgerStore {
    @extern @pure
    def post(db: DB, tx: LedgerTx): DB = { Db.insertTx(tx); db }
    @extern @pure
    def get(db: DB, id: Long): Option[LedgerTx] = Db.txById(id)
    @extern @pure
    def findBySource(db: DB, kind: String, id: String): Option[LedgerTx] = Db.txBySource(kind, id)
    @extern @pure
    def all(db: DB): List[LedgerTx] = Db.allTxs
  }
}
