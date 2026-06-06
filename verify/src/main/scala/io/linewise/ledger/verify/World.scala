package io.linewise.verify.fm.ledger

import stainless.lang._

/* =============================================================================
 * THE WORLD — the aggregate ledger state plus the concrete lens into it. One repo
 * for now (the append-only ledger); the lens shape leaves room for sibling repos
 * (rules, withdrawals) without changing the service signatures. Each lens is checked
 * LAWFUL by the Has @laws. Transpile-clean; transpiler input.
 * ========================================================================== */
case class World(
    ledger: LedgerRepository,
    withdrawals: WithdrawalRepository,
    proposals: ProposalRepository,
    obligations: ObligationRepository,
)

case class HasLedger() extends Has[World, LedgerRepository] {
  def get(w: World): LedgerRepository = w.ledger
  def set(w: World, r: LedgerRepository): World = w.copy(ledger = r)
}

case class HasWithdrawals() extends Has[World, WithdrawalRepository] {
  def get(w: World): WithdrawalRepository = w.withdrawals
  def set(w: World, r: WithdrawalRepository): World = w.copy(withdrawals = r)
}

case class HasProposals() extends Has[World, ProposalRepository] {
  def get(w: World): ProposalRepository = w.proposals
  def set(w: World, r: ProposalRepository): World = w.copy(proposals = r)
}


case class HasObligations() extends Has[World, ObligationRepository] {
  def get(w: World): ObligationRepository = w.obligations
  def set(w: World, r: ObligationRepository): World = w.copy(obligations = r)
}
