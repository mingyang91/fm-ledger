package io.linewise.ledgerfm

import doobie.*
import cats.effect.IO
import io.linewise.ledgerfm.generated.LedgerModel.*
import io.linewise.ledgerfm.generated.LedgerLaw.AbstractLedger

/* =============================================================================
 * DRIVER — post a few balanced journal entries through the VERIFIED LedgerCore
 * (realized by the doobie JdbcLedger), printing the journal and the conservation
 * total at each step, then demonstrate the four rejections (duplicate id,
 * unbalanced, too-few-postings, overflow) and show that the books stay balanced
 * (total = 0) and the history is immutable (append-only).
 *
 * The driver talks to the ledger through its BARE-VALUE API only — no doobie /
 * ConnectionIO / transact appears here. The transactor is owned by JdbcLedger,
 * which confines the IO->value eval boundary.
 *
 * Run: ./mill runMain io.linewise.ledgerfm.LedgerMain
 * ========================================================================== */
object LedgerMain:
  def main(args: Array[String]): Unit =
    val xa = Transactor.fromDriverManager[IO](
      driver = "org.h2.Driver",
      url = "jdbc:h2:mem:ledger;DB_CLOSE_DELAY=-1",
      user = "sa",
      password = "",
      logHandler = None
    )
    val ledger = JdbcLedger(xa)

    def line(s: String): Unit = println(s)
    def rule(): Unit = println("-" * 78)

    def dump(tag: String): Unit =
      line(s"  JOURNAL $tag:  txCount=${ledger.txCount}  entryCount=${ledger.entryCount}  total=${ledger.total}")
      ledger.snapshot.txs.sortBy(_.id).foreach { tx =>
        val ps = tx.postings.map(p => f"${Db.accountStr(p.account)}%-18s ${p.amount}%+d").mkString("  |  ")
        line(f"    tx ${tx.id}%-2d : $ps")
      }

    def attempt(label: String, tx: Tx): Unit =
      ledger.postSafe(tx) match
        case Right(_) => line(f"  POST $label%-44s -> ACCEPTED")
        case Left(e)  => line(f"  POST $label%-44s -> REJECTED (${rejectName(e)})")

    def rejectName(e: LedgerError): String = e match
      case LedgerError.DuplicateTx    => "DuplicateTx"
      case LedgerError.Unbalanced     => "Unbalanced"
      case LedgerError.TooFewPostings => "TooFewPostings"
      case LedgerError.Overflow       => "Overflow"

    line("=" * 78)
    line("DOUBLE-ENTRY LEDGER  (real doobie + embedded H2; verified LedgerCore.post)")
    line("  invariant: every entry balances (sum of postings = 0) => total = 0;")
    line("  append-only: a posted transaction is never overwritten or removed.")
    line("=" * 78)

    ledger.initSchema()
    line("DDL: created append-only tables ledger_tx (id pk) + posting (tx_id, seq, ...).")
    rule()

    // --- three balanced journal entries (each nets to zero) ---
    line("POST three balanced journal entries (debit positive, credit negative):")
    attempt("sale:     Cash +100 / Revenue -100", Tx(1L, List(
      Posting(Cash, 100L), Posting(Revenue, -100L))))
    attempt("expense:  Expense +30 / Cash -30",    Tx(2L, List(
      Posting(Expense, 30L), Posting(Cash, -30L))))
    attempt("credit:   A/R +50 / Revenue -50",     Tx(3L, List(
      Posting(AccountsReceivable, 50L), Posting(Revenue, -50L))))
    dump("after three balanced posts")
    line(s"  => conservation holds: total = ${ledger.total} (the books balance).")
    rule()

    // --- the four rejections; each must leave the journal untouched ---
    line("ATTEMPT four inadmissible posts — each is refused, journal unchanged:")
    attempt("DUPLICATE id 1 (already posted)",      Tx(1L, List(
      Posting(Cash, 5L), Posting(Revenue, -5L))))
    attempt("UNBALANCED Cash +100 / Revenue -50",   Tx(4L, List(
      Posting(Cash, 100L), Posting(Revenue, -50L))))
    attempt("TOO FEW   single posting Cash 0",      Tx(5L, List(
      Posting(Cash, 0L))))
    attempt("OVERFLOW  Cash +MAX / A/R +MAX",       Tx(6L, List(
      Posting(Cash, Long.MaxValue), Posting(AccountsReceivable, Long.MaxValue))))
    dump("after four rejected posts (identical to before)")
    rule()

    // --- conservation + append-only summary ---
    val conserved = ledger.total == 0L
    val historyIntact = ledger.txCount == 3 && ledger.entryCount == 6
    line(s"RESULT: conservation total=0 ? ${if conserved then "YES" else "NO (WRONG)"}")
    line(s"        append-only intact (3 txs / 6 postings) ? ${if historyIntact then "YES" else "NO (WRONG)"}")
    line(s"        findTx(1) = ${ledger.findTx(1L).map(t => s"tx ${t.id} with ${t.postings.size} postings").getOrElse("-")}")
    rule()
    line("DONE. LedgerCore.post was applied for every attempt; the DB layer only did")
    line("      id-freshness / append-only INSERT / snapshot around the verified ops.")
