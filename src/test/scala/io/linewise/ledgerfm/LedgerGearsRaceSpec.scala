package io.linewise.ledgerfm

import cats.effect.IO
import doobie.*
import io.linewise.ledgerfm.generated.LedgerModel.*

/* =============================================================================
 * GEARS RACE SPEC — the concurrent counterpart of the differential drift gate.
 *
 * Drives REAL concurrent posts (Gears Futures on virtual threads) and asserts the
 * append-only / no-duplicate-write invariant holds under contention: exactly one
 * winner per id, the rest refused as DuplicateTx, and conservation (total = 0)
 * intact. The assertions are on the INVARIANT, never on WHICH clerk wins (that is
 * the trusted serialization order) — so the test is deterministic despite the
 * nondeterministic race. The verified justification is
 * LedgerProofs.sameIdRaceKeepsExactlyOne.
 * ========================================================================== */
class LedgerGearsRaceSpec extends munit.FunSuite:

  def freshJdbc(): JdbcLedger =
    val xa = Transactor.fromDriverManager[IO](
      driver = "org.h2.Driver",
      url = s"jdbc:h2:mem:ledgergears_${java.util.UUID.randomUUID};DB_CLOSE_DELAY=-1",
      user = "sa", password = "", logHandler = None
    )
    val j = JdbcLedger(xa); j.initSchema(); j

  def dupCount(vs: List[GearsLedgerRace.Verdict]): Int =
    vs.count {
      case Left(LedgerError.DuplicateTx) => true
      case _                             => false
    }

  test("8 clerks racing the same id: exactly one winner, the rest DuplicateTx") {
    val j  = freshJdbc()
    val tx = Tx(1L, List(Posting(Cash, 100L), Posting(Revenue, -100L)))
    val verdicts = GearsLedgerRace.raceSameId(j, tx, 8)
    assertEquals(verdicts.count(_.isRight), 1, "exactly one winner")
    assertEquals(dupCount(verdicts), 7, "the other seven are refused as duplicates")
    assertEquals(j.txCount, 1)
    assertEquals(j.entryCount, 2)
    assertEquals(j.total, 0L, "conservation holds")
  }

  test("distinct ids racing concurrently: all admitted, conservation holds") {
    val j   = freshJdbc()
    val txs = (1 to 10).toList.map(i => Tx(i.toLong, List(Posting(Cash, i * 10L), Posting(Revenue, -(i * 10L)))))
    val verdicts = GearsLedgerRace.raceDistinct(j, txs)
    assertEquals(verdicts.count(_.isRight), 10, "every fresh balanced entry is admitted")
    assertEquals(j.txCount, 10)
    assertEquals(j.entryCount, 20)
    assertEquals(j.total, 0L)
  }

  test("several ids each contended by four clerks: each id kept exactly once") {
    val j = freshJdbc()
    val verdicts = List(1L, 2L, 3L).flatMap { id =>
      GearsLedgerRace.raceSameId(j, Tx(id, List(Posting(Cash, 50L), Posting(Revenue, -50L))), 4)
    }
    assertEquals(verdicts.count(_.isRight), 3, "one winner per id")
    assertEquals(dupCount(verdicts), 9, "the other nine are duplicates")
    assertEquals(j.txCount, 3)
    assertEquals(j.entryCount, 6)
    assertEquals(j.total, 0L)
  }
