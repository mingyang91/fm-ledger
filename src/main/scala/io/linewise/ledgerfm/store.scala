package io.linewise.ledgerfm

import doobie.*
import doobie.implicits.*
import cats.effect.IO
import io.linewise.ledgerfm.generated.LedgerModel.*

/* =============================================================================
 * THE DOOBIE LEDGER STORE — the REAL append-only persistence behind the verified
 * LedgerCore ops. ROW-AS-SOURCE-OF-TRUTH: the rows ARE the journal.
 *
 * APPEND-ONLY BY CONSTRUCTION: this layer only ever INSERTs. There is no UPDATE
 * and no DELETE — a posted transaction is immutable. The `ledger_tx` primary key
 * IS the no-duplicate-write rule: a second INSERT of the same id raises a PK
 * violation, which JdbcLedger turns into Left(DuplicateTx) (it checks existence
 * first, so the violation is belt-and-suspenders). This realizes the verified
 * distinctTxIds + postNeverMutatesHistory at the SQL layer.
 *
 * The BALANCE / well-formedness checks are NOT re-implemented here: JdbcLedger
 * reuses the verified, transpiler-GENERATED LedgerCore (atLeastTwo / sumSafe) for
 * those, so the only trusted thing this file adds is persistence (id-freshness +
 * INSERT + snapshot reconstruction). The differential test machine-checks that the
 * persisted result matches the in-memory verified store row-for-row.
 *
 * On H2 vs Postgres: the SQL is the production Postgres flavour; H2 2.3.232
 * accepts it verbatim. Amounts are BIGINT (native Long); the Tier-2 overflow guard
 * lives in the verified sum, not the column.
 * ========================================================================== */

/* PACKAGE-PRIVATE: the doobie ConnectionIO building blocks (the "Queries" layer).
 * ConnectionIO never escapes the package — JdbcLedger composes these and evaluates
 * them at a single boundary, so the shell (LedgerMain) only sees bare values. */
private[ledgerfm] object Db:

  // ---------------------------------------------------------------------------
  // CODEC: Account <-> varchar. Strict decode — an unknown token THROWS at the
  // doobie boundary (the row is the source of truth; a corrupt account is a hard
  // error, not a silent default), matching the job store's discipline.
  // ---------------------------------------------------------------------------
  given Meta[Account] = Meta[String].timap {
    case "Cash"               => Cash
    case "AccountsReceivable" => AccountsReceivable
    case "AccountsPayable"    => AccountsPayable
    case "Revenue"            => Revenue
    case "Expense"            => Expense
    case "Equity"             => Equity
    case other                => throw new IllegalStateException(s"unknown account in row: $other")
  } {
    case Cash               => "Cash"
    case AccountsReceivable => "AccountsReceivable"
    case AccountsPayable    => "AccountsPayable"
    case Revenue            => "Revenue"
    case Expense            => "Expense"
    case Equity             => "Equity"
  }

  def accountStr(a: Account): String = a match
    case Cash               => "Cash"
    case AccountsReceivable => "AccountsReceivable"
    case AccountsPayable    => "AccountsPayable"
    case Revenue            => "Revenue"
    case Expense            => "Expense"
    case Equity             => "Equity"

  // ---------------------------------------------------------------------------
  // DDL — the append-only journal. `ledger_tx` is one row per transaction (the id
  // PK = the no-duplicate-write rule). `posting` is the lines of each entry, keyed
  // (tx_id, seq) so insertion order is preserved and the FK ties lines to their tx.
  // ---------------------------------------------------------------------------
  val ddlLedgerTx: ConnectionIO[Int] =
    sql"""
      create table ledger_tx (
        id BIGINT primary key
      )
    """.update.run

  val ddlPosting: ConnectionIO[Int] =
    sql"""
      create table posting (
        tx_id   BIGINT       not null,
        seq     INT          not null,
        account VARCHAR(32)  not null,
        amount  BIGINT       not null,
        primary key (tx_id, seq),
        foreign key (tx_id) references ledger_tx(id)
      )
    """.update.run

  /** Does this transaction id already exist? (the append-only freshness check) */
  def txExists(id: Long): ConnectionIO[Boolean] =
    sql"select 1 from ledger_tx where id = $id".query[Int].option.map(_.isDefined)

  /** INSERT a transaction and its postings — the ONLY write. One tx: the ledger_tx
    * row then each posting in order (seq = 0,1,2,...). No UPDATE, no DELETE. */
  def insertTx(id: Long, postings: List[Posting]): ConnectionIO[Unit] =
    val insTx: ConnectionIO[Int] = sql"insert into ledger_tx (id) values ($id)".update.run
    postings.zipWithIndex.foldLeft(insTx) { (acc, pi) =>
      val (p, i) = pi
      acc.flatMap(_ =>
        sql"""insert into posting (tx_id, seq, account, amount)
              values ($id, $i, ${p.account}, ${p.amount})""".update.run)
    }.map(_ => ())

  /** Reconstruct one transaction by id (the point read behind findTx). */
  def loadTx(id: Long): ConnectionIO[Option[Tx]] =
    txExists(id).flatMap {
      case false => doobie.free.connection.pure(Option.empty[Tx])
      case true =>
        sql"select account, amount from posting where tx_id = $id order by seq"
          .query[(Account, Long)].to[List]
          .map(ps => Some(Tx(id, ps.map((a, amt) => Posting(a, amt)))))
    }

  /** Reconstruct the whole journal (newest first, mirroring the in-memory prepend;
    * the differential test sorts by id, so order is not load-bearing). */
  val loadAllTxs: ConnectionIO[List[Tx]] =
    sql"select id from ledger_tx order by id desc".query[Long].to[List].flatMap { ids =>
      ids.foldRight(doobie.free.connection.pure(List.empty[Tx])) { (id, acc) =>
        for
          ps   <- sql"select account, amount from posting where tx_id = $id order by seq"
                    .query[(Account, Long)].to[List]
          tail <- acc
        yield Tx(id, ps.map((a, amt) => Posting(a, amt))) :: tail
      }
    }

  /** The REAL net of every posting amount in the ledger — the conservation total
    * read straight from the DB (zero whenever every posted entry was balanced). */
  val sumAll: ConnectionIO[Long] =
    sql"select coalesce(sum(amount), 0) from posting".query[Long].unique

  /** Number of postings (记账总数) and transactions — the tracked projections. */
  val countPostings: ConnectionIO[Int] =
    sql"select count(*) from posting".query[Int].unique

  val countTxs: ConnectionIO[Int] =
    sql"select count(*) from ledger_tx".query[Int].unique
