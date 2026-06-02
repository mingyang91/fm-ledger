package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import io.linewise.verify.effect.SafeArith._
import LedgerModel._
import LedgerCore._

/* =============================================================================
 * LEDGER PROOFS — the VERIFY-ONLY propositions about the executable LedgerCore.
 *
 * NEVER TRANSPILED. Holds the store invariant, the Tier-2 faithfulness lemma, the
 * append-preserves-invariant lemma, the HEADLINE conservation theorem, the
 * append-only / no-duplicate-write / count-tracking propositions, and the
 * postSafe<->post bridge. `verify.scala` auto-discovers this file; the transpiler
 * never sees it, so it freely uses Nil()/Cons() and BigInt.
 *
 * THE TRUSTED STORE-REALIZATION BOUNDARY (named here, proven nowhere — by design):
 * the doobie store (store.scala) is the trusted realization of LedgerCore's `post`
 * / `postSafe`; its append-only `ledger_tx` PK realizes distinctTxIds, its
 * INSERT-only writes realize the no-mutation-of-history property, and its
 * Math.addExact sum realizes `sumSafe`. The differential test (LedgerDifferential
 * Spec) machine-checks that the doobie realization agrees with these verified ops
 * row-for-row on concrete scenarios.
 * ========================================================================== */
object LedgerProofs {

  /* =====================================================================
   * THE STORE INVARIANT — per-tx well-formedness + distinct ids + conservation.
   * ===================================================================== */

  def distinctTxIds(txs: List[Tx]): Boolean =
    txs match
      case Nil()      => true
      case Cons(h, t) => !txIds(t).contains(h.id) && distinctTxIds(t)

  def ledgerInv(l: Ledger): Boolean =
    l.txs.forall((t: Tx) => txWellFormed(t)) &&
    distinctTxIds(l.txs) &&
    l.total == FMLong(BigInt(0))

  /* =====================================================================
   * TIER-2 FAITHFULNESS — when the safe fold is Right(v), v is the true sum.
   * ===================================================================== */

  // the true (unbounded) sum of a transaction's postings — the spec sumSafe is
  // checked against. BigInt is fine here (verify-only).
  def sumBig(ps: List[Posting]): BigInt =
    ps match
      case Nil()      => BigInt(0)
      case Cons(h, t) => h.amount.value + sumBig(t)

  // addOrOverflow's Right payload is the exact two-way sum (unfolds safeAdd).
  def addOrOverflowRightIsExactSum(a: FMLong, b: FMLong): Boolean = {
    addOrOverflow(a, b) match
      case Right(v) => v.value == a.value + b.value
      case Left(_)  => true
  }.holds

  // FAITHFULNESS: sumSafe(ps) == Right(v) ==> v.value == sumBig(ps). Induction
  // mirrors sumSafe's isEmpty/head/tail recursion.
  @opaque @inlineOnce
  def sumSafeFaithful(ps: List[Posting]): Unit = {
    decreases(ps.size)
    if ps.isEmpty then ()
    else {
      sumSafeFaithful(ps.tail)
      sumSafe(ps.tail) match
        case Right(rest) => { addOrOverflowRightIsExactSum(ps.head.amount, rest); () }
        case Left(_)     => ()
    }
  }.ensuring(_ =>
    sumSafe(ps) match
      case Right(v) => v.value == sumBig(ps)
      case Left(_)  => true
  )

  /* =====================================================================
   * L1 — POST PRESERVES THE INVARIANT (the @law's content; near-definitional
   * cons-prepend: forall/distinct unfold on cons, the admissible guard supplies
   * the head facts, total is unchanged).
   * ===================================================================== */
  def postPreservesInv(l: Ledger, tx: Tx): Unit = {
    require(ledgerInv(l))
  }.ensuring(_ => ledgerInv(post(l, tx)))

  /* =====================================================================
   * L2 — APPEND-ONLY / IMMUTABLE HISTORY.
   * post either leaves the journal byte-identical (rejection) or prepends exactly
   * the new transaction (acceptance) — existing entries are never mutated or
   * removed, and they keep their order as the tail.
   * ===================================================================== */

  def postRejectIsNoOp(l: Ledger, tx: Tx): Boolean = {
    require(!admissible(l, tx))
    post(l, tx) == l
  }.holds

  def postAcceptPrependsOnly(l: Ledger, tx: Tx): Boolean = {
    require(admissible(l, tx))
    post(l, tx).txs == (tx :: l.txs)
  }.holds

  // the immutability statement: after any post, the previous journal is preserved
  // — either unchanged (reject) or exactly the tail of the new journal (accept).
  def postNeverMutatesHistory(l: Ledger, tx: Tx): Boolean = {
    val after = post(l, tx)
    (after.txs == l.txs) || (!after.txs.isEmpty && after.txs.tail == l.txs)
  }.holds

  /* =====================================================================
   * L3 — NO DUPLICATE WRITE. An id already in the journal is rejected with
   * DuplicateTx and the ledger is unchanged (the append-only key rule).
   * ===================================================================== */
  def duplicateRejected(l: Ledger, tx: Tx): Boolean = {
    require(txIds(l.txs).contains(tx.id))
    postSafe(l, tx) == Left[LedgerError, Ledger](LedgerError.DuplicateTx) &&
    post(l, tx) == l
  }.holds

  /* =====================================================================
   * L4 — CONSERVATION (THE HEADLINE): every transaction balanced ==> the global
   * net of every posting across every transaction and account is exactly zero.
   * ===================================================================== */

  def globalNetBig(txs: List[Tx]): BigInt =
    txs match
      case Nil()      => BigInt(0)
      case Cons(h, t) => sumBig(h.postings) + globalNetBig(t)

  @opaque @inlineOnce
  def conservationFromBalanced(txs: List[Tx]): Unit = {
    require(txs.forall((t: Tx) => txWellFormed(t)))
    decreases(txs.size)
    txs match
      case Nil() => ()
      case Cons(h, t) =>
        sumSafeFaithful(h.postings) // txWellFormed(h) => sumSafe(h.postings)==Right(0) => sumBig==0
        conservationFromBalanced(t)
  }.ensuring(_ => globalNetBig(txs) == BigInt(0))

  // the store-level corollary: any ledger satisfying the invariant has a zero net.
  def ledgerConserves(l: Ledger): Boolean = {
    require(ledgerInv(l))
    conservationFromBalanced(l.txs)
    globalNetBig(l.txs) == BigInt(0)
  }.holds

  // FAITHFULNESS OF THE MATERIALIZED total: under the invariant, the maintained
  // `total` field equals the true global net (both zero) — the field is not a
  // disconnected counter, it is the conservation quantity.
  def totalReflectsGlobalNet(l: Ledger): Boolean = {
    require(ledgerInv(l))
    conservationFromBalanced(l.txs)
    l.total.value == globalNetBig(l.txs)
  }.holds

  /* =====================================================================
   * L5 — COUNT TRACKING (记账总数). The posting count grows by exactly the
   * transaction's postings on acceptance and is unchanged on rejection —
   * monotonic and exact, a faithful projection of the immutable journal.
   * ===================================================================== */

  def countPostings(txs: List[Tx]): BigInt =
    txs match
      case Nil()      => BigInt(0)
      case Cons(h, t) => h.postings.size + countPostings(t)

  def postGrowsCountOnAccept(l: Ledger, tx: Tx): Boolean = {
    require(admissible(l, tx))
    countPostings(post(l, tx).txs) == countPostings(l.txs) + tx.postings.size
  }.holds

  def postKeepsCountOnReject(l: Ledger, tx: Tx): Boolean = {
    require(!admissible(l, tx))
    countPostings(post(l, tx).txs) == countPostings(l.txs)
  }.holds

  /* =====================================================================
   * POSTSAFE <-> POST BRIDGE — the diagnostic Either agrees with the total op.
   * ===================================================================== */

  // Right iff admissible: the precise rejection partition exactly covers the
  // not-admissible cases, and success exactly the admissible one.
  def postSafeRightIffAdmissible(l: Ledger, tx: Tx): Boolean = {
    postSafe(l, tx).isRight == admissible(l, tx)
  }.holds

  // on success, the payload is the post result.
  def postSafeRightIsPost(l: Ledger, tx: Tx): Boolean = {
    require(admissible(l, tx))
    postSafe(l, tx) == Right[LedgerError, Ledger](post(l, tx))
  }.holds

  // on rejection, post is a no-op.
  def postSafeLeftIsNoOp(l: Ledger, tx: Tx): Boolean = {
    require(postSafe(l, tx).isLeft)
    post(l, tx) == l
  }.holds

  /* =====================================================================
   * RACE / ORDER-INDEPENDENCE — the verified justification for the concurrent
   * Gears posting race (GearsLedgerRace). The ledger analog of the job system's
   * SeqMirrorProofs.raceExactlyOneWinner: when two clerks race to post the SAME id
   * the DB serializes them to SOME order (the `ledger_tx` PK — TRUSTED), and EVERY
   * order is safe (VERIFIED here): exactly one is admitted (the first), the second
   * is refused as a duplicate, and the journal grows by exactly one either way.
   * ===================================================================== */

  // posting the same transaction twice is idempotent — the second is a no-op
  // (append-only: its id is already present). So replaying a post any number of
  // times, in any order, yields one copy.
  def postIdempotentOnSameTx(l: Ledger, tx: Tx): Boolean = {
    require(ledgerInv(l))
    post(post(l, tx), tx) == post(l, tx)
  }.holds

  // EXACTLY ONE WINNER, FIRST WINS, BOTH ORDERS: two well-formed transactions
  // sharing an id, posted in either order onto a ledger that has neither, leave
  // exactly the FIRST-applied one in the journal (the journal grows by one) and
  // refuse the second. Which one wins depends on the serialization order (trusted);
  // that exactly one wins does not (verified).
  def sameIdRaceKeepsExactlyOne(l: Ledger, a: Tx, b: Tx): Boolean = {
    require(ledgerInv(l) && a.id == b.id)
    require(!txIds(l.txs).contains(a.id))
    require(txWellFormed(a) && txWellFormed(b))
    val ab = post(post(l, a), b) // a then b
    val ba = post(post(l, b), a) // b then a
    findTx(ab, a.id) == Some[Tx](a) && ab.txs.size == l.txs.size + BigInt(1) &&
    findTx(ba, b.id) == Some[Tx](b) && ba.txs.size == l.txs.size + BigInt(1)
  }.holds

  /* =====================================================================
   * WITNESSES — the propositions are non-vacuous on concrete journal entries.
   * ===================================================================== */

  // a real journal entry balances: Cash +100 (debit), Revenue -100 (credit).
  def saleBalances: Boolean = {
    val sale = Tx(FMLong(BigInt(1)),
      Cons(Posting(Cash, FMLong(BigInt(100))),
        Cons(Posting(Revenue, FMLong(BigInt(0) - BigInt(100))), Nil[Posting]())))
    txWellFormed(sale)
  }.holds

  // an unbalanced entry (Cash +100, Revenue -50) is NOT well-formed.
  def unbalancedNotWellFormed: Boolean = {
    val bad = Tx(FMLong(BigInt(2)),
      Cons(Posting(Cash, FMLong(BigInt(100))),
        Cons(Posting(Revenue, FMLong(BigInt(0) - BigInt(50))), Nil[Posting]())))
    !txWellFormed(bad)
  }.holds

  // a single-posting entry is NOT well-formed (fewer than two postings).
  def singlePostingNotWellFormed: Boolean = {
    val one = Tx(FMLong(BigInt(3)), Cons(Posting(Cash, FMLong(BigInt(0))), Nil[Posting]()))
    !txWellFormed(one)
  }.holds

  // posting a balanced sale onto the empty ledger keeps total at zero and the
  // posting count grows by two.
  def postSaleConserves: Boolean = {
    val empty = Ledger(Nil[Tx](), FMLong(BigInt(0)))
    val sale = Tx(FMLong(BigInt(1)),
      Cons(Posting(Cash, FMLong(BigInt(100))),
        Cons(Posting(Revenue, FMLong(BigInt(0) - BigInt(100))), Nil[Posting]())))
    val after = post(empty, sale)
    after.total == FMLong(BigInt(0)) &&
    countPostings(after.txs) == BigInt(2)
  }.holds
}
