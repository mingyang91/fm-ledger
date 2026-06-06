package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import io.linewise.ledger.Db

/* =============================================================================
 * PROPOSAL — the third verified repository slice, backing the two-person control flow
 * for manual adjustments and rollback reversals. A Proposal already carries the
 * resolved debit/credit accounts of the ledger movement its approval will post (the
 * shell resolves CREDIT vs DEBIT direction, and a reversal's reversed legs, at propose
 * time), so the verified AdjustmentService stays free of account-string arithmetic.
 * Upsert-by-id (`put`) like the withdrawal slice, since approve/reject rewrite status.
 * ========================================================================== */
case class Proposal(
    id:            FMLong,
    kind:          TxKind,          // ManualAdjustment | RollbackReversal
    userUid:       String,
    debitAccount:  String,
    creditAccount: String,
    amount:        FMLong,
    reason:        String,
    proposedBy:    String,
    status:        ProposalStatus,
    resultTxId:    Option[FMLong],  // the posted tx, set on approve
    targetTxId:    Option[FMLong],  // the reversed tx (rollback_reversal only)
)

sealed abstract class ProposalRepository {
  @ghost def rows: List[Proposal]

  def put(p: Proposal): ProposalRepository
  def get(id: FMLong): Option[Proposal]
  def all: List[Proposal]

  @law def putGet(p: Proposal): Boolean =
    put(p).get(p.id) == Some[Proposal](p)
}

case class InMemProposalRepository(items: List[Proposal]) extends ProposalRepository {
  @ghost def rows: List[Proposal] = items
  def put(p: Proposal): ProposalRepository =
    InMemProposalRepository(p :: items.filter((x: Proposal) => x.id != p.id))
  def get(id: FMLong): Option[Proposal] = items.find((x: Proposal) => x.id == id)
  def all: List[Proposal] = items
}

case class JdbcProposalRepository() extends ProposalRepository {
  @ghost def rows: List[Proposal] = Nil[Proposal]()

  @extern @pure
  def put(p: Proposal): ProposalRepository = { Db.proposalPut(p); JdbcProposalRepository() }.ensuring((res: ProposalRepository) =>
    res.get(p.id) == Some[Proposal](p))

  @extern @pure
  def get(id: FMLong): Option[Proposal] = Db.proposalById(id)

  @extern @pure
  def all: List[Proposal] = Db.allProposals
}
