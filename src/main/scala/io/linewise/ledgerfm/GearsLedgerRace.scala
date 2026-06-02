package io.linewise.ledgerfm

import doobie.*
import cats.effect.IO
import gears.async.*
import gears.async.default.given
import io.linewise.ledgerfm.generated.LedgerModel.*
import io.linewise.ledgerfm.generated.LedgerLaw.AbstractLedger

/* =============================================================================
 * GEARS CONCURRENT POSTING RACE — the ledger's structured-concurrency surface.
 *
 * The job system demonstrates its no-double-claim invariant under a real Ox race;
 * this is the ledger's analog under a real GEARS race (ch.epfl.lamp::gears,
 * direct-style structured concurrency over virtual threads). Many clerks post
 * CONCURRENTLY; the verified contract is that the append-only / no-duplicate-write
 * rule holds whatever order the database serializes them to:
 *
 *   - same id, N clerks racing => EXACTLY ONE is admitted, the rest get
 *     DuplicateTx. The `ledger_tx` PK serializes the writes to SOME order
 *     (TRUSTED); every order is safe (VERIFIED: LedgerProofs.sameIdRaceKeepsExactly
 *     One / postIdempotentOnSameTx). This is the two-tier trust of the SeqMirror
 *     story, realized for the ledger with Gears instead of Ox.
 *   - distinct ids, all balanced => all admitted; conservation (total = 0) holds.
 *
 * Each clerk runs in its own Gears Future (a virtual thread), and each post is its
 * own doobie transaction (its own H2 connection), so the race is genuine. The
 * Gears API surface used is exactly: `Async.blocking`, `Future(...)`, `.await`.
 *
 * Run: ./mill runMain io.linewise.ledgerfm.GearsLedgerRace
 * ========================================================================== */
object GearsLedgerRace:

  type Verdict = Either[LedgerError, AbstractLedger]

  /** Race `clerks` concurrent posts of the SAME transaction (the contended case).
    * Returns each clerk's verdict — exactly one Right, the rest Left(DuplicateTx). */
  def raceSameId(led: AbstractLedger, tx: Tx, clerks: Int): List[Verdict] =
    Async.blocking:
      val fs = (1 to clerks).toList.map(_ => Future(led.postSafe(tx)))
      fs.map(_.await)

  /** Post a batch of DISTINCT-id transactions concurrently — every balanced,
    * fresh-id entry is admitted and conservation still holds. */
  def raceDistinct(led: AbstractLedger, txs: List[Tx]): List[Verdict] =
    Async.blocking:
      val fs = txs.map(tx => Future(led.postSafe(tx)))
      fs.map(_.await)

  private def isDuplicate(v: Verdict): Boolean = v match
    case Left(LedgerError.DuplicateTx) => true
    case _                             => false

  def main(args: Array[String]): Unit =
    val xa = Transactor.fromDriverManager[IO](
      driver = "org.h2.Driver",
      url = "jdbc:h2:mem:ledger_gears;DB_CLOSE_DELAY=-1",
      user = "sa", password = "", logHandler = None
    )
    val ledger = JdbcLedger(xa)
    ledger.initSchema()

    def line(s: String): Unit = println(s)
    def rule(): Unit = println("-" * 78)

    line("=" * 78)
    line("GEARS CONCURRENT LEDGER RACE  (ch.epfl.lamp::gears; real virtual-thread futures)")
    line("  append-only / no-duplicate-write must hold under concurrent posting:")
    line("  exactly one winner per id, conservation total = 0 — whatever the order.")
    line("=" * 78)

    // --- contended: 8 clerks all race to post the SAME id 1 ---
    val tx1 = Tx(1L, List(Posting(Cash, 100L), Posting(Revenue, -100L)))
    val contended = raceSameId(ledger, tx1, 8)
    val winners = contended.count(_.isRight)
    val dups    = contended.count(isDuplicate)
    line(s"CONTENDED  8 clerks race id 1 concurrently:")
    line(s"  winners = $winners   duplicates-refused = $dups")

    // --- uncontended: 5 clerks race DISTINCT ids 2..6 concurrently ---
    val txs = (2 to 6).toList.map(i => Tx(i.toLong, List(Posting(Cash, i * 10L), Posting(Revenue, -(i * 10L)))))
    val distinct = raceDistinct(ledger, txs)
    line(s"UNCONTENDED  5 clerks race distinct ids 2..6 concurrently:")
    line(s"  winners = ${distinct.count(_.isRight)}")
    rule()

    line(s"FINAL JOURNAL:  txCount=${ledger.txCount}  entryCount=${ledger.entryCount}  total=${ledger.total}")
    val ok = winners == 1 && dups == 7 && ledger.total == 0L && ledger.txCount == 6 && ledger.entryCount == 12
    line(s"RESULT: ${if ok then "EXACTLY ONE WINNER per id + conservation holds under concurrency (correct)"
                       else "WRONG"}")
    rule()
    line("Each clerk ran in a Gears Future; each post was its own doobie transaction.")
    line("The ledger_tx PK serialized the contended writes; sameIdRaceKeepsExactlyOne")
    line("(verified) guarantees every serialization order leaves exactly one winner.")
