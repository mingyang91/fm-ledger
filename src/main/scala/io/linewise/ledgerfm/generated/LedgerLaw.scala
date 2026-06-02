package io.linewise.ledgerfm.generated

import io.linewise.ledgerfm.generated.LedgerModel.*
import io.linewise.ledgerfm.generated.LedgerCore.Ledger

/* =============================================================================
 * PRODUCTION counterpart of verify/LedgerLaw.scala's AbstractLedger — SIGNATURES
 * ONLY. HAND-WRITTEN, in the `generated` package alongside the transpiled
 * LedgerModel/LedgerCore (mirrors generated/StoreLaw.scala on the job side). The
 * @law contract is verify-only (dropped in transpilation); production keeps just
 * the interface that ListLedger (in-memory oracle) and JdbcLedger (trusted doobie)
 * implement — the ONE ledger the differential test checks for agreement.
 *
 * Beyond the verified surface (snapshot / post / findTx) the production trait also
 * exposes `postSafe` (the precise rejection reason for the driver) and the tracked
 * projections `total` / `txCount` / `entryCount` (记账总数) — all proven-monotonic
 * over the immutable journal in LedgerProofs.
 * ========================================================================== */
object LedgerLaw {
  trait AbstractLedger {
    def snapshot: Ledger
    def post(tx: Tx): AbstractLedger
    def postSafe(tx: Tx): Either[LedgerError, AbstractLedger]
    def findTx(id: Long): Option[Tx]

    // tracked projections of the append-only journal
    def total: Long      // the conservation field — zero under the invariant
    def txCount: Int     // number of transactions tracked
    def entryCount: Int  // number of postings tracked (记账总数)
  }
}
