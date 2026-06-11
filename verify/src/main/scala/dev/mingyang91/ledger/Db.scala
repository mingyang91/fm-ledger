package dev.mingyang91.ledger

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import dev.mingyang91.verify.fm.ledger.LedgerModel._
import dev.mingyang91.verify.fm.ledger.Withdrawal
import dev.mingyang91.verify.fm.ledger.Proposal
import dev.mingyang91.verify.fm.ledger.Obligation

/* =============================================================================
 * VERIFY-ONLY STUB for the production JDBC persistence facade. The field-less
 * repositories' @extern bodies delegate here. The real implementation is a
 * PostgreSQL-backed `Db` in production; this verify-side stub keeps JDBC and SQL out
 * of the Stainless classpath. Never transpiled.
 * ========================================================================== */
object Db {
  @extern def insertTx(tx: LedgerTx): Unit = ???
  @extern def txById(id: Long): Option[LedgerTx] = ???
  @extern def txBySource(kind: String, id: String): Option[LedgerTx] = ???
  @extern def allTxs: List[LedgerTx] = ???

  // withdrawals (upsert-by-id status slice)
  @extern def withdrawalPut(wd: Withdrawal): Unit = ???
  @extern def withdrawalById(id: Long): Option[Withdrawal] = ???
  @extern def withdrawalByClientReq(userUid: String, clientRequestId: String): Option[Withdrawal] = ???
  @extern def allWithdrawals: List[Withdrawal] = ???

  // two-person proposals (adjustments + reversals)
  @extern def proposalPut(p: Proposal): Unit = ???
  @extern def proposalById(id: Long): Option[Proposal] = ???
  @extern def allProposals: List[Proposal] = ???


  // pending obligations (forecast read-model)
  @extern def obligationPut(o: Obligation): Unit = ???
  @extern def obligationBySource(kind: String, id: String): Option[Obligation] = ???
  @extern def allOpenObligations: List[Obligation] = ???

}
