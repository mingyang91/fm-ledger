package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import LedgerModel._
import LedgerTables._
import WithdrawalTables._
import LedgerInvariants._

/* =============================================================================
 * WHOLE-DATABASE PROOFS — VERIFY-ONLY capstone. The per-slice proofs show each operation preserves
 * its own slice; these compose them into the statement that matters: the whole-DB invariant
 * `valid(db)` — conservation, every table's referential integrity and key distinctness, AND the
 * cross-slice intent->withdrawal foreign key — is preserved by the top-level operations. An op that
 * touches one slice leaves the other slices' fields (hence their invariants) untouched.
 * ========================================================================== */
object WorldProofs {

  private val ledgerStore = InMemLedgerStore()
  private val wStore      = InMemWithdrawalStore()

  // ---- posting a ledger tx preserves the WHOLE-DB invariant ----
  def postTxPreservesValid(db: DB, tx: LedgerTx): Unit = {
    require(valid(db))
    require(LedgerValidation.admissible(tx))
    require(noHeaderWith(db.txHeaders, tx.id))
    require(noLegWith(db.legs, tx.id))
    LedgerProofs.postPreservesValidLedger(db, tx)
    // the withdrawal/proposal/obligation/payout tables and the intent->withdrawal FK are untouched
    // by post (it copies only txHeaders/legs), so their conjuncts carry from valid(db).
  }.ensuring(_ => valid(ledgerStore.post(db, tx)))

  // ---- requesting a fresh withdrawal preserves the WHOLE-DB invariant ----
  //   the withdrawal-id set grows, so the cross-slice FK needs the weakening lemma.
  def addWithdrawalPreservesValid(db: DB, wd: Withdrawal): Unit = {
    require(valid(db))
    require(noWithdrawalWith(db.withdrawals, wd.id))
    WithdrawalProofs.addWithdrawalPreservesValidW(db, wd)
    PayoutTableProofs.intentsRefWithdrawalConsW(db.intents, db.withdrawals, toWRow(wd))
  }.ensuring(_ => valid(wStore.addWithdrawal(db, wd)))
}
