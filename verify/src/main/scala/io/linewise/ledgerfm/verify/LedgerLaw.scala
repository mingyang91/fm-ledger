package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import LedgerCore._
import LedgerProofs._

/* =============================================================================
 * THE ABSTRACT LEDGER + ITS @law CONTRACT — the interface both the in-memory and
 * the doobie ledger implement, with the conservation contract stated ON it.
 *
 * The store's invariant-preservation requirement lives as a Stainless `@law` on
 * the abstract `AbstractLedger`. Stainless CHECKS the law for the in-memory
 * `ListLedgerModel` below; the doobie ledger is TRUSTED to satisfy the SAME law,
 * and the differential test (LedgerDifferentialSpec) machine-checks that trust.
 *
 * VERIFY-ONLY, NEVER TRANSPILED. `@law` is ghost; production gets a separate,
 * tiny, hand-written signatures-only trait (generated/LedgerLaw.scala) that the
 * executable `ListLedger` and `JdbcLedger` implement. The contract is stated over
 * a GHOST observation `snapshot: Ledger` (the full store value), so it can talk
 * about the journal + the conservation total even though the doobie realization
 * has no materialized list. The law is invariant-PRESERVATION:
 *   ledgerInv(snapshot-before) ==> ledgerInv(snapshot-after-post).
 *
 * DISCHARGE IDIOM (the spike-proven shape, reused from StoreLaw): the in-memory op
 * invokes postPreservesInv ONLY under the `if ledgerInv(...)` hypothesis the law
 * carries — an unconditional lemma call would fail on an already-invalid ledger
 * (the lemma's own precondition is ledgerInv). postPreservesInv is reused verbatim
 * from LedgerProofs — not redesigned.
 * ========================================================================== */
object LedgerLaw {

  abstract class AbstractLedger {
    def snapshot: Ledger
    def post(tx: Tx): AbstractLedger

    // a point read (no @law — its result only reports a transaction; the
    // ledgerInv proof comes from the post law regardless of what findTx returns).
    def findTx(id: FMLong): Option[Tx]

    /* THE CONTRACT — conservation-preservation: posting onto a valid ledger
     * yields a valid ledger (every entry still balanced, ids still distinct,
     * total still zero). Stainless generates a discharge VC per subclass. (The
     * doobie ledger's discharge is the trusted/differential-tested one; it is not
     * a Stainless subclass.) */
    @law def postLaw(tx: Tx): Boolean =
      ledgerInv(snapshot) ==> ledgerInv(post(tx).snapshot)
  }

  /* THE IN-MEMORY MODEL — snapshot = the Ledger value; post delegates to
   * LedgerCore.post. Discharges postLaw via the spike idiom (postPreservesInv
   * under the ledgerInv guard, .ensuring the implication). */
  case class ListLedgerModel(l: Ledger) extends AbstractLedger {
    def snapshot: Ledger = l

    def findTx(id: FMLong): Option[Tx] = LedgerCore.findTx(l, id)

    def post(tx: Tx): AbstractLedger = {
      if ledgerInv(l) then { postPreservesInv(l, tx) }
      ListLedgerModel(LedgerCore.post(l, tx))
    }.ensuring((res: AbstractLedger) => ledgerInv(l) ==> ledgerInv(res.snapshot))
  }
}
