package io.linewise.ledgerfm

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doobie.*
import doobie.implicits.*
import io.linewise.ledgerfm.generated.LedgerModel.*
import io.linewise.ledgerfm.generated.LedgerCore
import io.linewise.ledgerfm.generated.LedgerCore.Ledger
import io.linewise.ledgerfm.generated.LedgerLaw.AbstractLedger

/* =============================================================================
 * THE TWO LEDGER IMPLEMENTATIONS of the ONE interface (generated.LedgerLaw.
 * AbstractLedger — the same shape the verified LedgerLaw.AbstractLedger carries a
 * @law over). There is no separate model: the driver and the differential test
 * both use this one abstraction.
 *
 *   ListLedger — IN-MEMORY, delegates EVERY op to the transpiler-GENERATED
 *                LedgerCore (single-sourced from the verified ops). The
 *                differential ORACLE.
 *   JdbcLedger — TRUSTED doobie over `Db` at a single `eval` boundary; the
 *                differential SUBJECT. It reuses the GENERATED LedgerCore for the
 *                balance / well-formedness checks (so that logic is single-sourced
 *                too) and adds ONLY persistence: id-freshness, the append-only
 *                INSERT, and snapshot reconstruction.
 *
 * LedgerDifferentialSpec runs the same post sequence over BOTH and asserts the
 * resulting journals, conservation total, and accept/reject verdicts agree —
 * machine-checking that the trusted doobie store realizes the verified contract.
 * ========================================================================== */

/* IN-MEMORY: every op delegates to the GENERATED LedgerCore — drift-proof logic. */
final case class ListLedger(l: Ledger) extends AbstractLedger:
  def snapshot: Ledger = l
  def post(tx: Tx): AbstractLedger = ListLedger(LedgerCore.post(l, tx))
  def postSafe(tx: Tx): Either[LedgerError, AbstractLedger] =
    LedgerCore.postSafe(l, tx) match
      case Right(l2) => Right(ListLedger(l2))
      case Left(e)   => Left(e)
  def findTx(id: Long): Option[Tx] = LedgerCore.findTx(l, id)
  def total: Long      = l.total
  def txCount: Int     = l.txs.size
  def entryCount: Int  = l.txs.map(_.postings.size).sum

object ListLedger:
  def empty: ListLedger = ListLedger(Ledger(List.empty[Tx], 0L))

/* TRUSTED doobie: wraps `Db` at a single eval boundary. The verified validation
 * (atLeastTwo / sumSafe == balanced) is reused from the GENERATED LedgerCore; only
 * id-freshness + the append-only INSERT + the snapshot reads are doobie. */
final case class JdbcLedger(xa: Transactor[IO]) extends AbstractLedger:

  // THE eval boundary — the cats-effect IO -> bare value bridge, confined here.
  private def eval[A](c: ConnectionIO[A]): A = c.transact(xa).unsafeRunSync()

  // postSafe MIRRORS LedgerCore.postSafe exactly: the same DuplicateTx / TooFew /
  // Overflow / Unbalanced partition, with id-freshness via the DB and the body via
  // an append-only INSERT. The balance math is the verified LedgerCore.sumSafe.
  def postSafe(tx: Tx): Either[LedgerError, AbstractLedger] =
    if eval(Db.txExists(tx.id)) then Left(LedgerError.DuplicateTx)
    else if !LedgerCore.atLeastTwo(tx.postings) then Left(LedgerError.TooFewPostings)
    else
      LedgerCore.sumSafe(tx.postings) match
        case Left(e)  => Left(e) // Overflow propagated
        case Right(s) =>
          if s == 0L then { eval(Db.insertTx(tx.id, tx.postings)); Right(this) }
          else Left(LedgerError.Unbalanced)

  // post is total: a no-op on rejection (the verified `post` shape).
  def post(tx: Tx): AbstractLedger =
    postSafe(tx) match
      case Right(l2) => l2
      case Left(_)   => this

  def findTx(id: Long): Option[Tx] = eval(Db.loadTx(id))

  // snapshot reconstructs the verified Ledger value; `total` is the REAL DB net,
  // so the differential check confirms the persisted sum is genuinely zero.
  def snapshot: Ledger = Ledger(eval(Db.loadAllTxs), eval(Db.sumAll))
  def total: Long      = eval(Db.sumAll)
  def txCount: Int     = eval(Db.countTxs)
  def entryCount: Int  = eval(Db.countPostings)

  // --- driver helper: create the append-only schema ---
  def initSchema(): Unit = { eval(Db.ddlLedgerTx); eval(Db.ddlPosting); () }
