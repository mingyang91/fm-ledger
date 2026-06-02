package io.linewise.ledgerfm

import cats.effect.IO
import doobie.*
import io.linewise.ledgerfm.generated.LedgerModel.*
import io.linewise.ledgerfm.generated.LedgerLaw.AbstractLedger

/* =============================================================================
 * LEDGER DIFFERENTIAL SPEC — the machine-checked DRIFT GATE over the ONE ledger.
 *
 * Drives the SAME post sequence over BOTH implementations of AbstractLedger — the
 * verified, transpiler-GENERATED in-memory ListLedger (oracle) and the TRUSTED
 * doobie JdbcLedger (the append-only SQL the real ledger runs) — and asserts they
 * produce the identical journal (transactions + per-posting lines), the identical
 * conservation total, the identical posting/transaction counts, AND the identical
 * accept/reject verdict (with the same rejection reason) at every step.
 *
 * If the doobie realization drifts from the verified LedgerCore — a balance check
 * skipped, a duplicate admitted, history mutated — a deterministic scenario fails
 * and `./mill test` goes red. Each scenario uses a fresh isolated H2 (unique mem
 * URL) for the JDBC side.
 * ========================================================================== */
class LedgerDifferentialSpec extends munit.FunSuite:

  // comparable normal form: journal sorted by tx id (each tx's postings kept in
  // seq order), plus the conservation total and the tracked counts.
  type NormTx = (Long, List[(String, Long)])
  def norm(led: AbstractLedger): (List[NormTx], Long, Int, Int) =
    val txs = led.snapshot.txs
      .sortBy(_.id)
      .map(t => (t.id, t.postings.map(p => (Db.accountStr(p.account), p.amount))))
    (txs, led.total, led.txCount, led.entryCount)

  def freshJdbc(): JdbcLedger =
    val xa = Transactor.fromDriverManager[IO](
      driver = "org.h2.Driver",
      url = s"jdbc:h2:mem:ledgerdiff_${java.util.UUID.randomUUID};DB_CLOSE_DELAY=-1",
      user = "sa", password = "", logHandler = None
    )
    val j = JdbcLedger(xa); j.initSchema(); j

  def verdict(r: Either[LedgerError, AbstractLedger]): String = r match
    case Right(_) => "ACCEPT"
    case Left(e)  => e.toString

  // a mixed sequence exercising every accept and every rejection kind interleaved.
  val seq: List[Tx] = List(
    Tx(1L, List(Posting(Cash, 100L), Posting(Revenue, -100L))),                       // accept
    Tx(1L, List(Posting(Cash, 5L), Posting(Revenue, -5L))),                           // reject DuplicateTx
    Tx(2L, List(Posting(Expense, 30L), Posting(Cash, -30L))),                         // accept
    Tx(3L, List(Posting(Cash, 100L), Posting(Revenue, -50L))),                        // reject Unbalanced
    Tx(4L, List(Posting(Cash, 0L))),                                                  // reject TooFewPostings
    Tx(5L, List(Posting(Cash, Long.MaxValue), Posting(AccountsReceivable, Long.MaxValue))), // reject Overflow
    Tx(6L, List(Posting(AccountsReceivable, 50L), Posting(Revenue, -50L)))            // accept
  )

  test("post (total): verified ListLedger and doobie JdbcLedger agree on the journal") {
    val mem = seq.foldLeft[AbstractLedger](ListLedger.empty)((l, tx) => l.post(tx))
    val db  = seq.foldLeft[AbstractLedger](freshJdbc())((l, tx) => l.post(tx))
    assertEquals(norm(mem), norm(db))
    assertEquals(mem.total, 0L, "conservation total is zero")
    assertEquals(mem.txCount, 3, "exactly the three balanced, fresh-id txs are posted")
    assertEquals(mem.entryCount, 6, "six postings tracked")
  }

  test("postSafe verdicts agree step-for-step (same accept / same rejection reason)") {
    var mem: AbstractLedger = ListLedger.empty
    var db:  AbstractLedger = freshJdbc()
    seq.foreach { tx =>
      val rm = mem.postSafe(tx)
      val rd = db.postSafe(tx)
      assertEquals(verdict(rm), verdict(rd), s"verdict mismatch for tx ${tx.id}")
      rm.foreach(l => mem = l)
      rd.foreach(l => db = l)
    }
    assertEquals(norm(mem), norm(db))
  }

  test("append-only: a duplicate id never overwrites the original entry, on both") {
    val original = Tx(7L, List(Posting(Cash, 100L), Posting(Revenue, -100L)))
    val overwrite = Tx(7L, List(Posting(Expense, 999L), Posting(Cash, -999L))) // same id, different lines
    val ops: List[Tx] = List(original, overwrite)
    val mem = ops.foldLeft[AbstractLedger](ListLedger.empty)((l, tx) => l.post(tx))
    val db  = ops.foldLeft[AbstractLedger](freshJdbc())((l, tx) => l.post(tx))
    assertEquals(norm(mem), norm(db))
    // the original lines survive; the overwrite was refused.
    assertEquals(mem.findTx(7L).map(_.postings.map(_.amount)), Some(List(100L, -100L)))
    assertEquals(mem.entryCount, 2)
  }
