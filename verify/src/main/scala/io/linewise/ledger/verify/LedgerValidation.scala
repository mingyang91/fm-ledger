package io.linewise.verify.fm.ledger

import stainless.lang._
import LedgerModel._

/* =============================================================================
 * Pure predicates over a tx and a repository value; no persistence. A tx is admissible
 * when every entry amount is positive, both sides are present, total DR equals total CR,
 * and any source key is still fresh.
 * ========================================================================== */
object LedgerValidation {

  def positiveAmount(tx: LedgerTx): Boolean = allPositive(tx.entries)

  def balanced(tx: LedgerTx): Boolean =
    hasDirection(tx.entries, EntryDirection.DR) &&
      hasDirection(tx.entries, EntryDirection.CR) &&
      sumDirection(tx.entries, EntryDirection.DR) == sumDirection(tx.entries, EntryDirection.CR)

  def admissible(tx: LedgerTx): Boolean =
    positiveAmount(tx) && balanced(tx)

  def txIdFresh(repo: LedgerRepository, tx: LedgerTx): Boolean =
    repo.get(tx.id).isEmpty

  def isFresh(repo: LedgerRepository, tx: LedgerTx): Boolean =
    (tx.sourceKind, tx.sourceId) match
      case (Some(k), Some(i)) => repo.findBySource(k, i).isEmpty
      case _                  => true

  def admissibleFresh(repo: LedgerRepository, tx: LedgerTx): Boolean =
    admissible(tx) && txIdFresh(repo, tx) && isFresh(repo, tx)
}
