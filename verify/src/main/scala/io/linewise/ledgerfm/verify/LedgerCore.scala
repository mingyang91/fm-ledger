package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import io.linewise.verify.effect.SafeArith._
import LedgerModel._

/* =============================================================================
 * LEDGER CORE — the PROOF-FREE EXECUTABLE store + its append (transpiler input #2).
 *
 * Everything here is plain executable code: the store value, the Tier-2 safe sum
 * of a transaction's postings, the well-formedness predicates, and the append
 * `post` (total, a no-op on rejection) plus the diagnostic `postSafe`. It carries
 * NO proof scaffolding and NONE of the tokens the leak-guard forbids:
 *   - list recursion is written with `isEmpty`/`head`/`tail`, never `Nil()`/`Cons()`;
 *   - the prepend is `tx :: l.txs`, never a `Cons(...)` constructor;
 *   - "at least two postings" is `!ps.isEmpty && !ps.tail.isEmpty`, never `.size >= 2`;
 *   - the only `BigInt` is inside `FMLong(BigInt(0))`, which the transpiler erases
 *     to the native literal `0L`.
 * `decreases` is verify-only termination metadata; the transpiler drops it.
 *
 * TIER-2 OVERFLOW: posting amounts are unbounded, so summing a journal entry can
 * genuinely overflow. `sumSafe` folds the postings through `safeAdd`, turning an
 * overflow into a handled `Left(Overflow)` VALUE rather than a silent wrap — so an
 * entry whose intermediate sum overflows is simply not balanced and is rejected.
 * This is the first real production wiring of the Tier-2 primitive; the transpiler
 * rewrites the verify-side `SafeArith` import to the production `safeAdd`.
 *
 * The proofs (conservation, append-only, faithfulness) live in the verify-only
 * LedgerProofs.scala / LedgerLaw.scala, which import this object and are never
 * transpiled. The doobie store (store.scala) is the TRUSTED realization of these
 * same ops; the differential test machine-checks they agree.
 * ========================================================================== */
object LedgerCore {

  /* --- THE STORE: the append-only journal + the maintained conservation total. ---
   * `txs` is every transaction posted (newest first — `post` prepends). `total`
   * is the running net; the invariant pins it to zero and every balanced post
   * keeps it there. The posting COUNT (记账总数) is a derived projection reported
   * by the production store and proven-monotonic in LedgerProofs, not a field
   * here (its growth is unbounded, so it is not a Tier-1 bounded quantity). */
  case class Ledger(txs: List[Tx], total: FMLong)

  /* --- map an overflowing safeAdd into the ledger's own Overflow reason. --- */
  def addOrOverflow(a: FMLong, b: FMLong): Either[LedgerError, FMLong] =
    a.safeAdd(b) match
      case Right(v) => Right[LedgerError, FMLong](v)
      case Left(_)  => Left[LedgerError, FMLong](LedgerError.Overflow)

  /* --- TIER-2 SAFE SUM of a transaction's postings (overflow => Left). ---
   * Recurses with isEmpty/head/tail (no Nil()/Cons()), so it transpiles cleanly. */
  def sumSafe(ps: List[Posting]): Either[LedgerError, FMLong] = {
    decreases(ps.size)
    if ps.isEmpty then Right[LedgerError, FMLong](FMLong(BigInt(0)))
    else
      sumSafe(ps.tail) match
        case Left(e)     => Left[LedgerError, FMLong](e)
        case Right(rest) => addOrOverflow(ps.head.amount, rest)
  }

  /* --- BUSINESS SEMANTICS predicates. --- */

  // a journal entry posts to at least two accounts (debit and credit).
  def atLeastTwo(ps: List[Posting]): Boolean =
    !ps.isEmpty && !ps.tail.isEmpty

  // double-entry: the signed postings net to exactly zero (and did not overflow).
  // Written as a MATCH (not an Either `==`) so it shares postSafe's exact shape —
  // this keeps the postSafe<->post bridge proofs first-order for the solver (an
  // Either-equality VC sends z3 into a model-build that the native binding crashes
  // on; the match form proves trivially and never reaches that path).
  def txBalanced(t: Tx): Boolean =
    sumSafe(t.postings) match
      case Right(s) => s == FMLong(BigInt(0))
      case Left(_)  => false

  // a well-formed entry: at least two postings AND balanced.
  def txWellFormed(t: Tx): Boolean =
    atLeastTwo(t.postings) && txBalanced(t)

  /* --- the append-only ids already in the journal. --- */
  def txIds(txs: List[Tx]): List[FMLong] =
    txs.map((t: Tx) => t.id)

  /* --- a transaction is admissible iff its id is fresh and it is well-formed. ---
   * Fresh id = the append-only / no-duplicate-write rule. */
  def admissible(l: Ledger, tx: Tx): Boolean =
    !txIds(l.txs).contains(tx.id) && txWellFormed(tx)

  /* --- POST: total, a NO-OP on rejection. ---
   * On an admissible transaction the ledger only PREPENDS it (history is never
   * mutated or removed) and the conservation total is unchanged (a balanced entry
   * nets to zero). On any rejection the ledger is returned byte-identical. This
   * single shape gives the @law (LedgerLaw), append-only immutability, and the
   * no-duplicate-write rule at once. */
  def post(l: Ledger, tx: Tx): Ledger =
    if admissible(l, tx) then Ledger(tx :: l.txs, l.total) else l

  /* --- POSTSAFE: the diagnostic API for production — the precise rejection reason.
   * Right iff admissible (bridged to `post` in LedgerProofs). --- */
  def postSafe(l: Ledger, tx: Tx): Either[LedgerError, Ledger] =
    if txIds(l.txs).contains(tx.id) then Left[LedgerError, Ledger](LedgerError.DuplicateTx)
    else if !atLeastTwo(tx.postings) then Left[LedgerError, Ledger](LedgerError.TooFewPostings)
    else
      sumSafe(tx.postings) match
        case Left(e)  => Left[LedgerError, Ledger](e) // Overflow propagated
        case Right(s) =>
          if s == FMLong(BigInt(0)) then Right[LedgerError, Ledger](Ledger(tx :: l.txs, l.total))
          else Left[LedgerError, Ledger](LedgerError.Unbalanced)

  /* --- a point read of a transaction by id. --- */
  def findTx(l: Ledger, id: FMLong): Option[Tx] =
    l.txs.find((t: Tx) => t.id == id)
}
