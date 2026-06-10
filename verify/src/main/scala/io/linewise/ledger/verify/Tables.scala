package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * THE DATABASE — the whole verified state as ONE integral relational structure: a set of
 * flat row-tables, one List per table, matching the production DDL. This replaces the four
 * `@ghost`-rows aggregate repositories and the aggregate `World`.
 *
 *  - Ids are FMLong (so they transpile to Long for production); amounts are FMLong.
 *  - The database is one object because the tables are not independent: a leg references its
 *    header, a payout intent references a withdrawal. `valid(db)` (in the proofs) is one
 *    predicate over the whole structure, so the cross-table foreign keys can be stated.
 *  - `World(db)` wraps it behind a SINGLE `Has[World, DB]` lens. One lens means the lens laws
 *    are a one-field case-class copy (`w.copy(db = w.db) == w`), which discharges trivially —
 *    the per-table lenses that timed out are gone.
 *  - In production `db` is an always-empty placeholder: every real read/write goes through the
 *    Jdbc realization of the per-slice stores (@extern SQL), exactly as the fieldless Jdbc
 *    repositories did before. The db threaded through the services is just empty-list plumbing.
 * ========================================================================== */

// Payout-lifecycle status enums (promoted from PayoutLifecycleProofs so the payout tables are
// part of the executable model, not proof-only).
enum DispatchStatus       { case Pending, InFlight, Dispatched, Failed }
enum ProviderOutcome      { case Settled, Failed }
enum ReconciliationResult { case Matched, FeeVariance }

// ---- TABLES (rows) ----
// ledger: a journal header plus its balanced legs (the LedgerTx aggregate, split into two tables)
case class TxHeaderRow(id: FMLong, kind: TxKind, sourceKind: Option[String], sourceId: Option[String], userUid: String)
case class LegRow(txId: FMLong, lineNo: BigInt, account: String, dir: EntryDirection, amount: FMLong)
// withdrawal: a row + a status-change event stream (newest-first); current status is a latest-row join
case class WithdrawalRow(id: FMLong, userUid: String, amount: FMLong, clientRequestId: String, reserveTxId: FMLong)
case class WStatusRow(withdrawalId: FMLong, toStatus: WithdrawalStatus)
// proposal: a row + a status-change event stream
case class ProposalRow(
    id: FMLong, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String,
    amount: FMLong, reason: String, proposedBy: String, targetTxId: Option[FMLong])
case class PStatusRow(proposalId: FMLong, toStatus: ProposalStatus, resultTxId: Option[FMLong])
// obligation: a single table keyed by (sourceKind, sourceId)
case class ObligationRow(sourceKind: String, sourceId: String, userUid: String, estimatedPoints: FMLong, status: ObligationStatus, realizedTxId: Option[FMLong])
// payout lifecycle: intent / dispatch outbox / provider event / reconciliation
case class IntentRow(id: FMLong, withdrawalId: FMLong, userUid: String, provider: String, destinationId: String, gross: FMLong, quotedFee: FMLong, expectedNet: FMLong)
case class DispatchRow(intentId: FMLong, withdrawalId: FMLong, amountMinor: FMLong, idempotencyKey: String, status: DispatchStatus, attempts: BigInt, providerTransferRef: Option[String])
case class EventRow(provider: String, eventId: String, withdrawalId: FMLong, outcome: ProviderOutcome, observedFee: FMLong)
case class ReconRow(intentId: FMLong, eventId: String, expectedFee: FMLong, observedFee: FMLong, result: ReconciliationResult)

// ---- BOUNDARY DTOs (assembled views the services accept/return; the HTTP/DTO layer uses these).
// Stored split across tables: a Withdrawal is a WithdrawalRow + its latest WStatusRow; a Proposal is
// a ProposalRow + its latest PStatusRow. Moved here from the retired repository files. ----
case class Withdrawal(id: FMLong, userUid: String, amount: FMLong, status: WithdrawalStatus, clientRequestId: String, reserveTxId: FMLong)
case class Proposal(
    id: FMLong, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String,
    amount: FMLong, reason: String, proposedBy: String, status: ProposalStatus,
    resultTxId: Option[FMLong], targetTxId: Option[FMLong])
// role/projectRef/taskKind are write-time only (not persisted; read back as ""), matching production.
case class Obligation(
    sourceKind: String, sourceId: String, userUid: String, role: String, projectRef: String,
    taskKind: String, estimatedUnit: FMLong, status: ObligationStatus, realizedTxId: Option[FMLong])

// ---- THE DATABASE + THE WORLD ----
case class DB(
    txHeaders:   List[TxHeaderRow],
    legs:        List[LegRow],
    withdrawals: List[WithdrawalRow],
    wstatuses:   List[WStatusRow],
    proposals:   List[ProposalRow],
    pstatuses:   List[PStatusRow],
    obligations: List[ObligationRow],
    intents:     List[IntentRow],
    dispatches:  List[DispatchRow],
    events:      List[EventRow],
    recons:      List[ReconRow])

object DB {
  def empty: DB = DB(
    Nil[TxHeaderRow](), Nil[LegRow](),
    Nil[WithdrawalRow](), Nil[WStatusRow](),
    Nil[ProposalRow](), Nil[PStatusRow](),
    Nil[ObligationRow](),
    Nil[IntentRow](), Nil[DispatchRow](), Nil[EventRow](), Nil[ReconRow]())
}

case class World(db: DB)

// The ONLY lens: World <-> DB. One field, so both laws discharge by the case-class copy axiom.
case class HasDb() extends Has[World, DB] {
  def get(w: World): DB = w.db
  def set(w: World, r: DB): World = w.copy(db = r)
}
