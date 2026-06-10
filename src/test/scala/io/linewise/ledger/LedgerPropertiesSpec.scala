package io.linewise.ledger
import org.scalacheck.Prop.forAll

import sttp.model.StatusCode

import io.linewise.ledger.LedgerEndpoints as E
import io.linewise.ledger.generated.{World, JdbcLedgerRepository, JdbcWithdrawalRepository, JdbcProposalRepository, JdbcObligationRepository}
import io.linewise.ledger.generated.{InMemLedgerRepository, InMemWithdrawalRepository, InMemProposalRepository, InMemObligationRepository}

class LedgerPropertiesSpec extends LedgerHttpSuite with LedgerGen with LedgerDiffGen:
  override val scalaCheckTestParameters: org.scalacheck.Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(50)

  // The differential drift gate, now property-based: a random sequence of verified-service
  // operations is replayed in lockstep against the InMem oracle and the @extern-over-JDBC
  // PostgreSQL realization, fed IDENTICAL externally-chosen ids. They must agree on every
  // verdict and on the full final state — the machine-checked bridge from the Stainless
  // proof (about InMem) to what actually runs (JDBC).
  property("drift gate: the InMem oracle and the JDBC realization agree on every op and final state") {
    forAll(genOps) { (ops: List[LedgerOp]) =>
      val ds = freshDs("ldiff")
      val c0 = ds.getConnection(); try Jdbc.initSchema(c0) finally c0.close()
      Db.init(ds)
      val jdbcW = World(JdbcLedgerRepository(), JdbcWithdrawalRepository(), JdbcProposalRepository(), JdbcObligationRepository())
      var memW: World = World(InMemLedgerRepository(Nil), InMemWithdrawalRepository(Nil), InMemProposalRepository(Nil), InMemObligationRepository(Nil))
      var counter = 0L
      def fresh(): Long = { counter += 1; counter }
      ops.foreach { op =>
        val n = freshIdCount(op)
        val a = if n >= 1 then fresh() else 0L
        val b = if n >= 2 then fresh() else 0L
        val (mw, mr) = applyOp(op, memW, a, b); memW = mw
        val (_, jr) = applyOp(op, jdbcW, a, b)
        assert(mr == jr, s"verdict diverged on $op (ids a=$a b=$b):\n  mem  = $mr\n  jdbc = $jr")
      }
      assertEquals(ledgerSvc.all(memW).sortBy(_.id), ledgerSvc.all(jdbcW).sortBy(_.id), "ledger txs diverged")
      assertEquals(wSvc.all(memW).sortBy(_.id), wSvc.all(jdbcW).sortBy(_.id), "withdrawals diverged")
      assertEquals(aSvc.all(memW).sortBy(_.id), aSvc.all(jdbcW).sortBy(_.id), "proposals diverged")
      assertEquals(
        oSvc.openObligations(memW).sortBy(o => (o.sourceKind, o.sourceId)),
        oSvc.openObligations(jdbcW).sortBy(o => (o.sourceKind, o.sourceId)),
        "open obligations diverged")
      List(("job", "s1"), ("job", "s2")).foreach { (sk, si) =>
        assertEquals(oSvc.bySource(memW, sk, si), oSvc.bySource(jdbcW, sk, si), s"obligation $sk:$si diverged")
      }
    }
  }

  property("incentive credit is source-idempotent and leaves the user balance at the credited amount") {
    forAll { (uid: UserUid, amount: PositivePoints, source: SourceKey, trace: IncentiveTraceId) =>
      val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc")
      val req = CreditRequest(uid.value, amount.value, source.kind, source.id, Some(trace.value), Some("incentives"))
      val first = secure(E.incentiveCredit, be, svc, req)
      assertEquals(first.code, StatusCode.Ok)
      val tx1 = first.body.toOption.get
      val second = secure(E.incentiveCredit, be, svc, req)
      assertEquals(second.code, StatusCode.Ok)
      assertEquals(second.body.toOption.get.id, tx1.id)
      assertEquals(secure(E.userBalance, be, svc, uid.value).body.toOption.get.balancePoints, amount.value)
      assert(secure(E.auditLog, be, svc, (None, None)).body.toOption.get.exists(_.detail.contains(trace.value)))
    }
  }

  property("withdrawal request is client-request idempotent and reserves exactly the requested amount") {
    forAll { (uid: UserUid, funded: FundedWithdrawal, source: SourceKey, clientReq: ClientRequestId) =>
      val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val user = api.userToken(uid.value)
      secure(E.incentiveCredit, be, svc, CreditRequest(uid.value, funded.funded, source.kind, source.id))
      val first = secure(E.requestWithdrawal, be, user, WithdrawalRequestBody(funded.withdrawn, clientReq.value))
      assertEquals(first.code, StatusCode.Ok)
      val wd = first.body.toOption.get
      assertEquals(wd.status, "PendingReview")
      val replay = secure(E.requestWithdrawal, be, user, WithdrawalRequestBody(funded.withdrawn, clientReq.value))
      assertEquals(replay.code, StatusCode.Ok)
      assertEquals(replay.body.toOption.get.id, wd.id)
      assertEquals(secure(E.myBalance, be, user, ()).body.toOption.get.balancePoints, funded.funded - funded.withdrawn)
      assertEquals(secure(E.accountBalance, be, svc, "system:withdrawal_clearing").body.toOption.get.balancePoints, funded.withdrawn)
    }
  }

  property("recipient-pays webhook is event-id idempotent and provider settlement/reconciliation stay consistent") {
    forAll { (uid: UserUid, funded: FundedWithdrawal, source: SourceKey, clientReq: ClientRequestId, trace: IncentiveTraceId) =>
      val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val user = api.userToken(uid.value)
      secure(E.incentiveCredit, be, svc, CreditRequest(uid.value, funded.funded, source.kind, source.id, Some(trace.value), Some("incentives")))
      Db.setSystemConfig("stripe_webhook_secret", "whsec_prop", "test", "test")
      val wd = secure(E.requestWithdrawal, be, user, WithdrawalRequestBody(funded.withdrawn, clientReq.value)).body.toOption.get
      val fee = math.min(20L, funded.withdrawn - 1L)
      secure(E.submitWithdrawal, be, svc, (wd.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-prop", fee, funded.withdrawn - fee, Some(s"q-${trace.value}"), None)))
      val event = StripeEvents.event(s"evt-${trace.value}", "transfer.paid", s"tr-${trace.value}", wd.id, fee)
      val first = public(E.stripeWebhook, be, StripeEvents.signed("whsec_prop", event))
      assertEquals(first.code, StatusCode.Ok)
      val second = public(E.stripeWebhook, be, StripeEvents.signed("whsec_prop", event))
      assertEquals(second.code, StatusCode.Ok)
      assertEquals(secure(E.accountBalance, be, svc, Accounts.providerBalance("stripe")).body.toOption.get.balancePoints, funded.withdrawn)
      val rec = secure(E.getPayoutReconciliation, be, svc, wd.id).body.toOption.get
      assertEquals(rec.result, "matched")
      assertEquals(rec.observedFee, Some(fee))
    }
  }

  property("debit adjustments above the funded balance are refused at approval") {
    forAll { (uid: UserUid, fundedDebit: FundedDebit, source: SourceKey) =>
      val api = freshApi(); val be = stubOf(api); val alice = api.adminToken("alice"); val bob = api.adminToken("bob")
      secure(E.incentiveCredit, be, alice, CreditRequest(uid.value, fundedDebit.funded, source.kind, source.id))
      val prop = secure(E.proposeAdjustment, be, alice, AdjustProposeBody(uid.value, AdjustDirection.DEBIT, fundedDebit.debit, "generated-clawback")).body.toOption.get
      assertEquals(secure(E.approveAdjustment, be, bob, (prop.id, DecisionRequest("PendingReview"))).code, StatusCode.UnprocessableEntity)
      assertEquals(secure(E.userBalance, be, alice, uid.value).body.toOption.get.balancePoints, fundedDebit.funded)
    }
  }

  property("a credit above the single-ledger limit is refused and trips the payout kill switch") {
    forAll { (uid: UserUid, huge: HugePoints, source: SourceKey) =>
      val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val user = api.userToken(uid.value)
      val r = secure(E.incentiveCredit, be, svc, CreditRequest(uid.value, huge.value, source.kind, source.id, Some("t"), Some("incentives")))
      assertEquals(r.code, StatusCode.UnprocessableEntity)
      assert(secure(E.riskEvents, be, svc, (None, None)).body.toOption.get.exists(_.kind == "single_ledger_amount"))
      // kill switch tripped: fund a small balance, then a withdrawal is unavailable
      secure(E.incentiveCredit, be, svc, CreditRequest(uid.value, 1000, source.kind, source.id + "-ok"))
      assertEquals(secure(E.requestWithdrawal, be, user, WithdrawalRequestBody(1, "after")).code, StatusCode.ServiceUnavailable)
    }
  }
