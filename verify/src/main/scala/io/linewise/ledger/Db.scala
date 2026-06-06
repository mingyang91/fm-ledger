package io.linewise.ledger

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import io.linewise.verify.fm.ledger.LedgerModel._
import io.linewise.verify.fm.ledger.Withdrawal
import io.linewise.verify.fm.ledger.Proposal
import io.linewise.verify.fm.ledger.Obligation

/* =============================================================================
 * VERIFY-ONLY STUB for the production Quill persistence facade. The field-less JDBC
 * repository's @extern bodies delegate here. Quill is a third-party macro library NOT
 * on the Stainless verify classpath, so it cannot appear in the verified source; these
 * @extern signatures are havoc'd in verification and never executed (the repo bodies
 * that call them are themselves @extern). This file lives in the production-shaped
 * package `io.linewise.ledger` (NOT `io.linewise.verify.*`), so the generated code
 * references it verbatim and resolves to the HAND-WRITTEN production `Db` (real Quill
 * DAO over a pooled DataSource). Never transpiled.
 * ========================================================================== */
object Db {
  @extern def insertTx(tx: LedgerTx): Unit = ???
  @extern def txById(id: FMLong): Option[LedgerTx] = ???
  @extern def txBySource(kind: String, id: String): Option[LedgerTx] = ???
  @extern def allTxs: List[LedgerTx] = ???

  // withdrawals (upsert-by-id status slice)
  @extern def withdrawalPut(wd: Withdrawal): Unit = ???
  @extern def withdrawalById(id: FMLong): Option[Withdrawal] = ???
  @extern def withdrawalByClientReq(userUid: String, clientRequestId: String): Option[Withdrawal] = ???
  @extern def allWithdrawals: List[Withdrawal] = ???

  // two-person proposals (adjustments + reversals)
  @extern def proposalPut(p: Proposal): Unit = ???
  @extern def proposalById(id: FMLong): Option[Proposal] = ???
  @extern def allProposals: List[Proposal] = ???


  // pending obligations (forecast read-model)
  @extern def obligationPut(o: Obligation): Unit = ???
  @extern def obligationBySource(kind: String, id: String): Option[Obligation] = ???
  @extern def allOpenObligations: List[Obligation] = ???

}
