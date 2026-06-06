package io.linewise.verify.fm.ledger

import stainless.lang._
import io.linewise.verify.effect.FMLong
import LedgerModel._

/* =============================================================================
 * LEDGER — THE VALIDATION LAYER. Pure predicates over a tx and a repository value; no
 * persistence. A tx is admissible when its amount is positive (the DR/CR balance is
 * structural — same amount on both legs by construction) and its source key is not
 * already used (incentive-credit idempotency). Transpile-clean; transpiler input.
 * ========================================================================== */
object LedgerValidation {

  def positiveAmount(tx: LedgerTx): Boolean = tx.amount > FMLong(BigInt(0))

  def isFresh(repo: LedgerRepository, tx: LedgerTx): Boolean =
    (tx.sourceKind, tx.sourceId) match
      case (Some(k), Some(i)) => repo.findBySource(k, i).isEmpty
      case _                  => true
}
