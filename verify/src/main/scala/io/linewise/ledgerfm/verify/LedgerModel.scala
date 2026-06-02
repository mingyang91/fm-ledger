package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong

/* =============================================================================
 * THE DOUBLE-ENTRY LEDGER — BUSINESS MODEL (source of truth, transpiler input #1).
 *
 * A ledger is an APPEND-ONLY journal of balanced transactions. Each transaction
 * (a journal entry) posts signed amounts to two or more accounts; by the rule of
 * double-entry the signed amounts WITHIN a transaction sum to zero (a debit is a
 * positive amount, a credit a negative one), so the GLOBAL net across every
 * posting of every transaction is conserved at zero. Conservation `total = 0` is
 * therefore not an aggregate we recompute but an invariant maintained one
 * balanced transaction at a time.
 *
 * THIS FILE IS A TRANSPILER INPUT. It holds only the data types and the
 * arithmetic-error ADT — no proofs, no `Nil()`/`Cons()` constructors, no
 * `require`/`ensuring`. The executable store ops live in LedgerCore.scala; the
 * propositions (conservation, append-only, faithfulness) live in the verify-only
 * LedgerProofs.scala / LedgerLaw.scala, which import this object and are never
 * transpiled. The production core at
 * src/main/scala/io/linewise/ledgerfm/generated/LedgerModel.scala is GENERATED
 * from this file (do not hand-edit it).
 *
 * Grounding: this mirrors the row-as-source-of-truth discipline of the job
 * system (JobModel.scala) — the FM core and the production core compute the same
 * pure functions; persistence/concurrency live in the trusted doobie shell.
 * ========================================================================== */
object LedgerModel {

  /* --- ACCOUNTS: a small finite chart of accounts (decidable equality). ---
   * Real double-entry accounts. The sign convention is carried by the posting
   * amount (debit = positive, credit = negative); accounts themselves are just
   * the buckets a posting lands in. */
  sealed trait Account
  case object Cash               extends Account // an asset
  case object AccountsReceivable extends Account // an asset
  case object AccountsPayable    extends Account // a liability
  case object Revenue            extends Account // income
  case object Expense            extends Account // an expense
  case object Equity             extends Account // owner's equity

  /* --- A POSTING: a signed amount landing in one account. ---
   * amount is the SIGNED move: +100 debits the account by 100, -100 credits it.
   * Within a balanced journal entry the postings' amounts sum to exactly zero. */
  case class Posting(account: Account, amount: FMLong)

  /* --- A TRANSACTION (journal entry): an id + its postings. ---
   * The id is the append-only key: a ledger never writes the same transaction id
   * twice (no duplicate / no overwrite). A well-formed entry has >= 2 postings. */
  case class Tx(id: FMLong, postings: List[Posting])

  /* --- THE REJECTION ADT: why a `post` was refused. ---
   * Mirrored on the production side. The ledger refuses, with a precise reason,
   * any write it cannot admit without breaking the invariant. */
  sealed trait LedgerError
  object LedgerError {
    case object DuplicateTx     extends LedgerError // id already posted (append-only)
    case object Unbalanced      extends LedgerError // postings do not net to zero
    case object TooFewPostings  extends LedgerError // fewer than two postings
    case object Overflow        extends LedgerError // summing the postings overflowed (Tier-2)
  }
}
