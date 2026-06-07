package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * VERIFY-ONLY aggregate invariants. These predicates give the proof files one
 * vocabulary for the ledger's durable promises: every posted transaction is
 * admissible, transaction ids and source keys are unique, and state-carrying rows
 * keep positive monetary amounts.
 * ========================================================================== */
object LedgerInvariants {

  def containsTxId(rows: List[LedgerTx], id: FMLong): Boolean =
    rows.exists((t: LedgerTx) => t.id == id)

  def uniqueTxIds(rows: List[LedgerTx]): Boolean =
    rows match
      case Nil() => true
      case Cons(h, tail) => !containsTxId(tail, h.id) && uniqueTxIds(tail)

  def containsSource(rows: List[LedgerTx], kind: String, id: String): Boolean =
    rows.exists((t: LedgerTx) => t.sourceKind == Some[String](kind) && t.sourceId == Some[String](id))

  def sourceAbsent(rows: List[LedgerTx], tx: LedgerTx): Boolean =
    (tx.sourceKind, tx.sourceId) match
      case (Some(k), Some(i)) => !containsSource(rows, k, i)
      case _                  => true

  def uniqueSources(rows: List[LedgerTx]): Boolean =
    rows match
      case Nil() => true
      case Cons(h, tail) =>
        sourceAbsent(tail, h) && uniqueSources(tail)

  def validLedgerRows(rows: List[LedgerTx]): Boolean =
    rows.forall((t: LedgerTx) => LedgerValidation.admissible(t)) &&
      uniqueTxIds(rows) && uniqueSources(rows)

  def validLedger(repo: LedgerRepository): Boolean =
    validLedgerRows(repo.all)

  def validWithdrawal(wd: Withdrawal): Boolean =
    wd.amount > FMLong(BigInt(0))

  def validWithdrawalRows(rows: List[Withdrawal]): Boolean =
    rows.forall((wd: Withdrawal) => validWithdrawal(wd))

  def validWithdrawals(repo: WithdrawalRepository): Boolean =
    validWithdrawalRows(repo.all)

  def validProposal(p: Proposal): Boolean =
    p.amount > FMLong(BigInt(0))

  def validProposalRows(rows: List[Proposal]): Boolean =
    rows.forall((p: Proposal) => validProposal(p))

  def validProposals(repo: ProposalRepository): Boolean =
    validProposalRows(repo.all)

  def validObligation(o: Obligation): Boolean =
    o.status match
      case ObligationStatus.Open      => o.realizedTxId.isEmpty
      case ObligationStatus.Cancelled => o.realizedTxId.isEmpty
      case ObligationStatus.Realized  => !o.realizedTxId.isEmpty

  def validObligationRows(rows: List[Obligation]): Boolean =
    rows.forall((o: Obligation) => validObligation(o))

  def validObligations(repo: ObligationRepository): Boolean =
    validObligationRows(repo.allOpen) // terminal rows are checked by service-specific proofs; allOpen is the exposed read model.

  def validWorld(w: World): Boolean =
    validLedger(w.ledger) && validWithdrawals(w.withdrawals) &&
      validProposals(w.proposals) && validObligations(w.obligations)
}
