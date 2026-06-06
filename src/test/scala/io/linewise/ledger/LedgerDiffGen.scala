package io.linewise.ledger

import org.scalacheck.Gen

import io.linewise.ledger.generated.{World, HasLedger, HasWithdrawals, HasProposals, HasObligations}
import io.linewise.ledger.generated.{LedgerService, WithdrawalService, AdjustmentService, ObligationService}
import io.linewise.ledger.generated.LedgerModel.{TxKind, WithdrawalStatus, ProposalStatus}

/* =============================================================================
 * Op model + ScalaCheck generators for the DIFFERENTIAL drift gate. A random sequence of
 * verified-service operations is applied in lockstep to the InMem oracle (the proven
 * reference) and the @extern-over-JDBC realization, with IDENTICAL externally-chosen ids,
 * and the two must agree on every verdict and on the final state. This validates the
 * trusted @extern repository axiom that the Stainless proof rests on.
 *
 * Creation ops draw their fresh ledger ids from the runner (so both worlds see the same
 * ids — exactly why the verified core takes ids as inputs). Reference ops carry a small
 * target id drawn from a pool so they frequently hit a previously-created entity; a miss
 * just produces an agreed "not found" on both worlds, which is equally valid to assert.
 * ========================================================================== */
trait LedgerDiffGen:
  enum LedgerOp:
    case Credit(uid: String, amount: Long, sk: String, si: String)
    case OpenObl(sk: String, si: String, uid: String)
    case CancelObl(sk: String, si: String)
    case Reserve(uid: String, amount: Long, creq: String)
    case ApproveW(target: Long, exp: WithdrawalStatus)
    case SettleW(target: Long, exp: WithdrawalStatus)
    case RejectW(target: Long, exp: WithdrawalStatus, legUid: String)
    case CancelW(target: Long, exp: WithdrawalStatus, legUid: String)
    case Propose(uid: String, amount: Long, by: String)
    case Reverse(uid: String, amount: Long, by: String, target: Long)
    case ApproveP(target: Long, exp: ProposalStatus, approver: String)
    case RejectP(target: Long, exp: ProposalStatus)
  import LedgerOp.*

  protected val ledgerSvc = LedgerService[World](HasLedger())
  protected val wSvc      = WithdrawalService[World](HasLedger(), HasWithdrawals())
  protected val aSvc      = AdjustmentService[World](HasLedger(), HasProposals())
  protected val oSvc      = ObligationService[World](HasObligations())

  /** How many fresh ledger ids the op consumes (the runner allocates them and feeds the
    * SAME values to both worlds). Reference ops consume none — they target an existing id. */
  protected def freshIdCount(op: LedgerOp): Int = op match
    case _: Reserve => 2 // withdrawal id + reserve tx id
    case _: Credit | _: SettleW | _: RejectW | _: CancelW | _: Propose | _: Reverse | _: ApproveP => 1
    case _ => 0

  /** Apply one op to a world; the returned verdict is compared across the two worlds. */
  protected def applyOp(op: LedgerOp, w: World, a: Long, b: Long): (World, Any) = op match
    case Credit(uid, amt, sk, si) => ledgerSvc.credit(w, Accounts.IncentiveExpense, Accounts.user(uid), uid, amt, a, sk, si)
    case OpenObl(sk, si, uid)     => oSvc.open(w, sk, si, uid, "", "", "", 600L)
    case CancelObl(sk, si)        => oSvc.cancel(w, sk, si)
    case Reserve(uid, amt, creq)  => wSvc.request(w, uid, amt, creq, a, b, Accounts.user(uid), Accounts.WithdrawalClearing)
    case ApproveW(t, exp)         => wSvc.approve(w, t, exp)
    case SettleW(t, exp)          => wSvc.settle(w, t, exp, a, Accounts.WithdrawalClearing, Accounts.Cash)
    case RejectW(t, exp, u)       => wSvc.reject(w, t, exp, a, Accounts.WithdrawalClearing, Accounts.user(u))
    case CancelW(t, exp, u)       => wSvc.cancel(w, t, exp, a, Accounts.WithdrawalClearing, Accounts.user(u))
    case Propose(uid, amt, by)    => aSvc.propose(w, TxKind.ManualAdjustment, uid, Accounts.Adjustment, Accounts.user(uid), amt, "reason", by, a, None)
    case Reverse(uid, amt, by, t) => aSvc.propose(w, TxKind.RollbackReversal, uid, Accounts.user(uid), Accounts.Adjustment, amt, "reason", by, a, Some(t))
    case ApproveP(t, exp, ap)     => aSvc.approve(w, t, exp, ap, a)
    case RejectP(t, exp)          => aSvc.reject(w, t, exp)

  private val uids    = Gen.oneOf("u1", "u2")
  private val amounts = Gen.oneOf(0L, 100L, 200L, 300L) // 0 exercises NonPositiveAmount on both worlds
  private val sources = Gen.oneOf("s1", "s2")           // small pool -> DuplicateSource collisions
  private val creqs   = Gen.oneOf("c1", "c2")           // small pool -> idempotent-replay collisions
  private val actors  = Gen.oneOf("a1", "a2")           // proposer/approver -> TwoPersonViolation collisions
  private val targets = Gen.choose(1L, 6L)              // reference ids -> hit early-created entities
  private val wStatus = Gen.oneOf(WithdrawalStatus.values.toIndexedSeq)
  private val pStatus = Gen.oneOf(ProposalStatus.values.toIndexedSeq)

  private val genOp: Gen[LedgerOp] = Gen.oneOf(
    for u <- uids; a <- amounts; s <- sources yield Credit(u, a, "job", s),
    for s <- sources; u <- uids yield OpenObl("job", s, u),
    for s <- sources yield CancelObl("job", s),
    for u <- uids; a <- amounts; c <- creqs yield Reserve(u, a, c),
    for t <- targets; s <- wStatus yield ApproveW(t, s),
    for t <- targets; s <- wStatus yield SettleW(t, s),
    for t <- targets; s <- wStatus; u <- uids yield RejectW(t, s, u),
    for t <- targets; s <- wStatus; u <- uids yield CancelW(t, s, u),
    for u <- uids; a <- amounts; by <- actors yield Propose(u, a, by),
    for u <- uids; a <- amounts; by <- actors; t <- targets yield Reverse(u, a, by, t),
    for t <- targets; s <- pStatus; ap <- actors yield ApproveP(t, s, ap),
    for t <- targets; s <- pStatus yield RejectP(t, s),
  )

  protected val genOps: Gen[List[LedgerOp]] = Gen.choose(1, 10).flatMap(n => Gen.listOfN(n, genOp))
