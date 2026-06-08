package io.linewise.ledger

import sttp.model.StatusCode

import io.linewise.ledger.LedgerEndpoints as E

/* =============================================================================
 * Example-style endpoint spec for the ledger microservice. The shared test harness
 * stands up a Testcontainers PostgreSQL instance, gives each test a fresh schema, and
 * drives the tapir endpoints through a sync stub client. This suite keeps the
 * narrative flows readable; property tests (including the differential drift gate) live
 * in LedgerPropertiesSpec.
 * ========================================================================== */
class LedgerEndpointsSpec extends LedgerHttpSuite:

  // Webhook signing secret for tests; set into config after freshApi so Db.stripeWebhookSecret reads it.
  private val whsec = "whsec_test_secret"
  private def withWebhookSecret(): Unit = Db.setSystemConfig("stripe_webhook_secret", whsec, "test", "test")

  test("incentive credit mints (DR expense / CR user), is idempotent, and the balance reflects it") {
    val api = freshApi(); val be = stubOf(api); val tok = api.adminToken("svc")
    val r1 = secure(E.incentiveCredit, be, tok, CreditRequest("u1", 1500, "annotation_job", "job-1"))
    assertEquals(r1.code, StatusCode.Ok)
    val tx = r1.body.toOption.get
    assertEquals(tx.amountPoints, 1500L)
    assertEquals(tx.debitAccount, "system:incentive_expense")
    assertEquals(tx.creditAccount, "user:u1")
    // idempotent replay on the same source returns the SAME tx
    val r2 = secure(E.incentiveCredit, be, tok, CreditRequest("u1", 1500, "annotation_job", "job-1"))
    assertEquals(r2.code, StatusCode.Ok)
    assertEquals(r2.body.toOption.get.id, tx.id)
    // balance (live ledger aggregate)
    assertEquals(secure(E.userBalance, be, tok, "u1").body.toOption.get.balancePoints, 1500L)
  }

  test("the verified positive-amount condition rejects a non-positive credit (400)") {
    val api = freshApi(); val be = stubOf(api); val tok = api.adminToken("svc")
    assertEquals(secure(E.incentiveCredit, be, tok, CreditRequest("u1", 0, "annotation_job", "job-z")).code, StatusCode.BadRequest)
  }

  test("two-person adjustment: propose+approve credits a user, proposer can't self-approve, funds invariant holds, DEBIT-beyond-balance refused") {
    val api = freshApi(); val be = stubOf(api); val alice = api.adminToken("alice"); val bob = api.adminToken("bob")
    secure(E.incentiveCredit, be, alice, CreditRequest("u1", 1000, "annotation_job", "job-2"))
    val prop = secure(E.proposeAdjustment, be, alice, AdjustProposeBody("u1", "CREDIT", 500, "bonus")).body.toOption.get
    assertEquals(prop.status, "PendingReview")
    // the verified proposer != approver invariant: alice cannot approve her own proposal
    assertEquals(secure(E.approveAdjustment, be, alice, (prop.id, DecisionRequest("PendingReview"))).code, StatusCode.Conflict)
    // a second admin approves -> the manual_adjustment posts, user gains 500
    val ap = secure(E.approveAdjustment, be, bob, (prop.id, DecisionRequest("PendingReview")))
    assertEquals(ap.code, StatusCode.Ok)
    assertEquals(ap.body.toOption.get.status, "Approved")
    val s = secure(E.summary, be, alice, ()).body.toOption.get
    assertEquals(s.incentiveExpensePoints, 1000L)
    assertEquals(s.totalUserLiabilityPoints, 1500L)
    assertEquals(s.adjustmentPoints, 500L)
    assert(s.fundsInvariantHolds, s.toString)
    // a DEBIT adjustment beyond the user's balance is refused at approval (production guard)
    val dprop = secure(E.proposeAdjustment, be, alice, AdjustProposeBody("u1", "DEBIT", 99999, "clawback")).body.toOption.get
    assertEquals(secure(E.approveAdjustment, be, bob, (dprop.id, DecisionRequest("PendingReview"))).code, StatusCode.UnprocessableEntity)
  }

  test("unauthenticated requests are rejected (401)") {
    val api = freshApi(); val be = stubOf(api)
    assertEquals(secure(E.incentiveCredit, be, "no-token", CreditRequest("u1", 1, "k", "i")).code, StatusCode.Unauthorized)
    assertEquals(secure(E.myBalance, be, "no-token", ()).code, StatusCode.Unauthorized)
  }

  test("transactions list and get round-trip the posted tx") {
    val api = freshApi(); val be = stubOf(api); val tok = api.adminToken("svc")
    val tx = secure(E.incentiveCredit, be, tok, CreditRequest("u1", 700, "annotation_job", "job-3")).body.toOption.get
    assertEquals(secure(E.getTx, be, tok, tx.id).body.toOption.get.id, tx.id)
    assert(secure(E.listTxs, be, tok, ()).body.toOption.get.nonEmpty)
    assertEquals(secure(E.getTx, be, tok, 999999L).code, StatusCode.NotFound)
  }

  test("withdrawal lifecycle: reserve -> approve -> settle moves clearing to cash; over-balance and stale status are refused") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "jw"))   // fund u1
    // reserve 400 -> pending_review; user balance drops to 600
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(400, "cr-1")).body.toOption.get
    assertEquals(w.status, "PendingReview")
    assertEquals(secure(E.myBalance, be, uTok, ()).body.toOption.get.balancePoints, 600L)
    // idempotent replay on the same clientRequestId returns the same withdrawal
    assertEquals(secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(400, "cr-1")).body.toOption.get.id, w.id)
    // approve -> submitted; settle -> settled; clearing drains to cash
    assertEquals(secure(E.approveWithdrawal, be, svc, (w.id, DecisionRequest("PendingReview"))).body.toOption.get.status, "Submitted")
    assertEquals(secure(E.settleWithdrawal, be, svc, (w.id, DecisionRequest("Submitted"))).body.toOption.get.status, "Settled")
    assertEquals(secure(E.accountBalance, be, svc, "system:withdrawal_clearing").body.toOption.get.balancePoints, 0L)
    assertEquals(secure(E.accountBalance, be, svc, "system:cash").body.toOption.get.balancePoints, 400L)
    // the summary's funds invariant must still hold once points have moved into clearing/cash
    val s2 = secure(E.summary, be, svc, ()).body.toOption.get
    assertEquals(s2.withdrawalClearingPoints, 0L)
    assertEquals(s2.settlementPoints, 400L)
    assert(s2.fundsInvariantHolds, s2.toString)
    // a withdrawal beyond the remaining balance is refused, and a stale expectedStatus 409s
    assertEquals(secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(99999, "cr-2")).code, StatusCode.BadRequest)
    assertEquals(secure(E.approveWithdrawal, be, svc, (w.id, DecisionRequest("PendingReview"))).code, StatusCode.Conflict)
  }


  test("obligation forecast uses an incentive-provided estimate, then realized credit closes it") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc")
    val o = secure(E.openObligation, be, svc, ObligationOpenBody("annotation_job", "job-9", "u1", 1500)).body.toOption.get
    assertEquals(o.status, "Open")
    assertEquals(o.estimatedPoints, 1500L)
    val up1 = secure(E.upcomingExpense, be, svc, ()).body.toOption.get
    assertEquals(up1.openCount, 1L)
    assertEquals(up1.projectedPoints, 1500L)
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1500, "annotation_job", "job-9", Some("inc-evt-9"), Some("incentives")))
    assertEquals(secure(E.upcomingExpense, be, svc, ()).body.toOption.get.openCount, 0L)
    assertEquals(secure(E.openObligation, be, svc, ObligationOpenBody("annotation_job", "job-9", "u1", 1500)).code, StatusCode.Conflict)
  }

  test("ledger records incentive-computed awards with incentive trace metadata") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc")
    val tx = secure(E.incentiveCredit, be, svc,
      CreditRequest("u1", 1500, "incentive_event", "evt-1", Some("price-run-1"), Some("incentives"))).body.toOption.get
    assertEquals(tx.amountPoints, 1500L)
    assertEquals(tx.sourceKind, Some("incentive_event"))
    assertEquals(tx.sourceId, Some("evt-1"))
    assert(secure(E.auditLog, be, svc, ()).body.toOption.get.exists(log =>
      log.action == "ledger.incentive_credit" && log.detail.contains("price-run-1")))
  }

  test("config changes are two-person and audit logged") {
    val api = freshApi(); val be = stubOf(api); val alice = api.adminToken("alice"); val bob = api.adminToken("bob")
    val cp = secure(E.proposeConfig, be, alice, ConfigProposalRequest("single_payout_limit_points", "60000", "raise test limit")).body.toOption.get
    assertEquals(secure(E.approveConfig, be, alice, cp.id).code, StatusCode.Conflict)
    assertEquals(secure(E.approveConfig, be, bob, cp.id).body.toOption.get.status, "Approved")
    val cfg = secure(E.configCurrent, be, bob, ()).body.toOption.get
    assert(cfg.exists(c => c.key == "single_payout_limit_points" && c.value == "60000"))
    assert(secure(E.auditLog, be, bob, ()).body.toOption.get.exists(_.action == "config.approve"))
  }

  test("risk hard limits reject runaway credits and trip the payout kill switch") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    assertEquals(secure(E.incentiveCredit, be, svc,
      CreditRequest("u1", 150000, "incentive_event", "huge", Some("risk-price-run"), Some("incentives"))).code, StatusCode.UnprocessableEntity)
    assert(secure(E.riskEvents, be, svc, ()).body.toOption.get.exists(_.kind == "single_ledger_amount"))
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "post-risk-fund"))
    assertEquals(secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(1, "after-risk")).code, StatusCode.ServiceUnavailable)
  }

  test("recipient-pays submit captures intent + queues dispatch; a real-signed transfer.paid reconciles matched and is auditable") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    withWebhookSecret()
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "stripe-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "stripe-cr")).body.toOption.get
    val submit = secure(E.submitWithdrawal, be, svc,
      (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, Some("quote-1"), None)))
    assertEquals(submit.body.toOption.get.status, "Submitted")
    // submit also queues an outbox dispatch row
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("pending"))
    val intent = secure(E.getPayoutIntent, be, svc, w.id).body.toOption.get
    assertEquals(intent.quotedProviderFee, 20L)
    assertEquals(intent.expectedRecipientNet, 280L)
    // a genuine Stripe-signed transfer.paid event, observed fee == quoted -> matched
    val (sig, body) = StripeEvents.signed(whsec, StripeEvents.event("evt_1", "transfer.paid", "tr_1", w.id, 20))
    val ack = public(E.stripeWebhook, be, (sig, body)).body.toOption.get
    assertEquals(ack.handled, true)
    assertEquals(ack.status, Some("Settled"))
    assertEquals(secure(E.accountBalance, be, svc, Accounts.providerBalance("stripe")).body.toOption.get.balancePoints, 300L)
    assertEquals(secure(E.accountBalance, be, svc, Accounts.FeeRecovery).body.toOption.get.balancePoints, 20L)
    assertEquals(secure(E.accountBalance, be, svc, Accounts.ProviderPayoutFee).body.toOption.get.balancePoints, 20L)
    val s = secure(E.summary, be, svc, ()).body.toOption.get
    assertEquals(s.feeRecoveryPoints, 20L)
    assertEquals(s.providerPayoutFeePoints, 20L)
    assert(s.fundsInvariantHolds, s.toString)
    val rec = secure(E.getPayoutReconciliation, be, svc, w.id).body.toOption.get
    assertEquals(rec.result, "matched")
    assertEquals(rec.observedFee, Some(20L))
    assertEquals(secure(E.listProviderEvents, be, svc, w.id).body.toOption.get.size, 1)
    // idempotent replay of the same event id (fresh signature) is a no-op 200
    assertEquals(public(E.stripeWebhook, be, StripeEvents.signed(whsec, StripeEvents.event("evt_1", "transfer.paid", "tr_1", w.id, 20))).code, StatusCode.Ok)
    assertEquals(secure(E.accountBalance, be, svc, Accounts.providerBalance("stripe")).body.toOption.get.balancePoints, 300L)
    // a tampered/forged signature is rejected (401)
    assertEquals(public(E.stripeWebhook, be, ("t=1,v1=deadbeef", body)).code, StatusCode.Unauthorized)
  }

  test("recipient-pays fee_variance: observed fee != quoted records fee_variance, posts the observed expense, funds invariant still holds") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    withWebhookSecret()
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "var-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "var-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    // Stripe debited 25, not the quoted 20 -> fee_variance; FeeRecovery uses the quote, ProviderPayoutFee the observed
    val (sig, body) = StripeEvents.signed(whsec, StripeEvents.event("evt_var", "transfer.paid", "tr_var", w.id, 25))
    assertEquals(public(E.stripeWebhook, be, (sig, body)).body.toOption.get.status, Some("Settled"))
    val rec = secure(E.getPayoutReconciliation, be, svc, w.id).body.toOption.get
    assertEquals(rec.result, "fee_variance")
    assertEquals(rec.observedFee, Some(25L))
    assert(rec.note.exists(_.contains("observed=25")), rec.note.toString)
    assertEquals(secure(E.accountBalance, be, svc, Accounts.FeeRecovery).body.toOption.get.balancePoints, 20L)
    assertEquals(secure(E.accountBalance, be, svc, Accounts.ProviderPayoutFee).body.toOption.get.balancePoints, 25L)
    assertEquals(secure(E.accountBalance, be, svc, Accounts.providerBalance("stripe")).body.toOption.get.balancePoints, 305L)
    assert(secure(E.summary, be, svc, ()).body.toOption.get.fundsInvariantHolds)
  }

  test("recipient-pays zero-fee fast path: no fee-recovery / provider-fee txs posted, reconciles matched") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    withWebhookSecret()
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "zero-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "zero-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 0, 300, None, None)))
    val (sig, body) = StripeEvents.signed(whsec, StripeEvents.event("evt_zero", "transfer.paid", "tr_zero", w.id, 0))
    assertEquals(public(E.stripeWebhook, be, (sig, body)).body.toOption.get.status, Some("Settled"))
    assertEquals(secure(E.accountBalance, be, svc, Accounts.FeeRecovery).body.toOption.get.balancePoints, 0L)
    assertEquals(secure(E.accountBalance, be, svc, Accounts.ProviderPayoutFee).body.toOption.get.balancePoints, 0L)
    assertEquals(secure(E.accountBalance, be, svc, Accounts.providerBalance("stripe")).body.toOption.get.balancePoints, 300L)
    assertEquals(secure(E.getPayoutReconciliation, be, svc, w.id).body.toOption.get.result, "matched")
  }

  test("submit quote validation: unsupported_provider, bad_quote, quote_mismatch all 400 with their reason") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "qv-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "qv-cr")).body.toOption.get
    val unsup = secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "paypal", "r", "d", 20, 280, None, None)))
    assertEquals(unsup.code, StatusCode.BadRequest); assertEquals(unsup.body.swap.toOption.get._2, "unsupported_provider")
    val bad = secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "r", "d", 20, 0, None, None)))
    assertEquals(bad.code, StatusCode.BadRequest); assertEquals(bad.body.swap.toOption.get._2, "bad_quote")
    val mis = secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "r", "d", 20, 290, None, None)))
    assertEquals(mis.code, StatusCode.BadRequest); assertEquals(mis.body.swap.toOption.get._2, "quote_mismatch")
  }

  test("payout dispatcher: a pending outbox row becomes a transfer with the NET amount + routing metadata, then dispatched") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "disp-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "disp-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    val fake = FakeGateway()
    assertEquals(PayoutDispatcher(fake).tick(), 1)
    val d = Db.dispatchByWithdrawal(w.id).get
    assertEquals(d.status, "dispatched")
    assert(d.providerTransferRef.exists(_.startsWith("tr_")), d.toString)
    assertEquals(fake.calls.head.amountMinor, 280L)            // the recipient NET, not the gross
    assertEquals(fake.calls.head.destination, "acct-1")
    assertEquals(fake.calls.head.metadata.get("withdrawal_id"), Some(w.id.toString))
    assertEquals(PayoutDispatcher(fake).tick(), 0)             // already dispatched -> no-op
  }

  test("payout dispatcher: a permanent gateway error marks the row failed and raises a risk event") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "df-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "df-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    PayoutDispatcher(FailingGateway(retryable = false)).tick()
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("failed"))
    assert(secure(E.riskEvents, be, svc, ()).body.toOption.get.exists(_.kind == "payout_dispatch_failed"))
  }

  test("payout dispatcher: a transient gateway error leaves the row pending for retry") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "dt-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "dt-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    PayoutDispatcher(FailingGateway(retryable = true)).tick()
    val d = Db.dispatchByWithdrawal(w.id).get
    assertEquals(d.status, "pending")
    assertEquals(d.attempts, 1)
  }

  test("stripe-mock: StripeGateway posts a well-formed /v1/transfers and parses the tr_ id") {
    val container =
      try
        val c = StripeMockContainer().withExposedPorts(12111)
        c.start(); Some(c)
      catch case _: Throwable => None
    assume(container.isDefined, "stripe-mock image unavailable")
    val c = container.get
    try
      val base = s"http://${c.getHost}:${c.getMappedPort(12111)}"
      val res = StripeGateway("sk_test_123", base, "2024-06-20")
        .createTransfer(TransferRequest(280, "usd", "acct_123", "idem-mock-1", Map("withdrawal_id" -> "7"), Some("grp-7")))
      assert(res.isRight, res.toString)
      assert(res.toOption.get.providerTransferRef.startsWith("tr_"), res.toString)
    finally c.stop()
  }

  test("F2: a settled webhook before submit is rejected but NOT consumed; the same event id settles after submit") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    withWebhookSecret()
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "f2-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "f2-cr")).body.toOption.get
    // settled arrives while still PendingReview (no intent yet): the transition fails (409) and the
    // event id is rolled back — not consumed — so the same event settles cleanly after submit.
    assertEquals(public(E.stripeWebhook, be, StripeEvents.signed(whsec, StripeEvents.event("evt_f2", "transfer.paid", "tr_f2", w.id, 20))).code, StatusCode.Conflict)
    assertEquals(secure(E.accountBalance, be, svc, Accounts.providerBalance("stripe")).body.toOption.get.balancePoints, 0L)
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, Some("quote-f2"), None)))
    assertEquals(public(E.stripeWebhook, be, StripeEvents.signed(whsec, StripeEvents.event("evt_f2", "transfer.paid", "tr_f2", w.id, 20))).body.toOption.get.status, Some("Settled"))
    assertEquals(secure(E.accountBalance, be, svc, Accounts.providerBalance("stripe")).body.toOption.get.balancePoints, 300L)
  }

  test("a transaction cannot be reversed twice (already_reversed)") {
    val api = freshApi(); val be = stubOf(api); val alice = api.adminToken("alice"); val bob = api.adminToken("bob")
    val tx = secure(E.incentiveCredit, be, alice, CreditRequest("u1", 500, "annotation_job", "jr")).body.toOption.get
    val rev = secure(E.proposeReversal, be, alice, ReversalProposeBody(tx.id, "mistake")).body.toOption.get
    assertEquals(secure(E.approveReversal, be, bob, (rev.id, DecisionRequest("PendingReview"))).code, StatusCode.Ok)
    assertEquals(secure(E.proposeReversal, be, alice, ReversalProposeBody(tx.id, "again")).code, StatusCode.Conflict)
  }

  test("DB-enforced invariants: append-only, credit-has-source, double-entry conservation") {
    val ds = freshDs("ledgerao")
    val c = ds.getConnection()
    try
      Jdbc.initSchema(c)
      val st = c.createStatement()
      // a balanced tx must be inserted in ONE transaction so the deferred conservation trigger
      // checks it at commit with both legs present.
      st.execute("INSERT INTO ACCOUNT (CODE, CATEGORY, NORMAL_SIDE, UNIT, OWNER_TYPE, OWNER_ID, POSTABLE, SYSTEM_MANAGED, STATUS) VALUES ('user:u1', 'user_liability', 'CR', 'PTS', 'User', 'u1', true, true, 'active')")
      c.setAutoCommit(false)
      st.execute("INSERT INTO LEDGER_TX (ID, KIND, USER_UID) VALUES (1, 'ManualAdjustment', 'u1')")
      st.execute("INSERT INTO LEDGER_ENTRY (TX_ID, LINE_NO, ACCOUNT_ID, DIRECTION, AMOUNT) VALUES (1, 1, 'system:adjustment', 'DR', 100)")
      st.execute("INSERT INTO LEDGER_ENTRY (TX_ID, LINE_NO, ACCOUNT_ID, DIRECTION, AMOUNT) VALUES (1, 2, 'user:u1', 'CR', 100)")
      c.commit()
      c.setAutoCommit(true)
      intercept[java.sql.SQLException](st.execute("UPDATE LEDGER_TX SET USER_UID = 'u2' WHERE ID = 1"))
      intercept[java.sql.SQLException](st.execute("DELETE FROM LEDGER_ENTRY WHERE TX_ID = 1"))
      intercept[java.sql.SQLException](st.execute("TRUNCATE LEDGER_ENTRY"))
      intercept[java.sql.SQLException](st.execute("TRUNCATE LEDGER_TX CASCADE"))
      intercept[java.sql.SQLException](st.execute("INSERT INTO LEDGER_TX (ID, KIND, USER_UID) VALUES (2, 'IncentiveCredit', 'u1')"))
      c.setAutoCommit(false)
      st.execute("INSERT INTO LEDGER_TX (ID, KIND, USER_UID) VALUES (3, 'ManualAdjustment', 'u1')")
      st.execute("INSERT INTO LEDGER_ENTRY (TX_ID, LINE_NO, ACCOUNT_ID, DIRECTION, AMOUNT) VALUES (3, 1, 'system:adjustment', 'DR', 50)")
      intercept[java.sql.SQLException](c.commit())
      c.rollback()
    finally c.close()
  }

  test("withdrawal reject and owner-cancel both return the reserve; my/list withdrawals enumerate them") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "wr-fund"))
    val w1 = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "wr-1")).body.toOption.get
    assertEquals(secure(E.rejectWithdrawal, be, svc, (w1.id, DecisionRequest("PendingReview"))).body.toOption.get.status, "Rejected")
    assertEquals(secure(E.myBalance, be, uTok, ()).body.toOption.get.balancePoints, 1000L) // reserve returned
    val w2 = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(200, "wr-2")).body.toOption.get
    assertEquals(secure(E.cancelWithdrawal, be, uTok, (w2.id, DecisionRequest("PendingReview"))).body.toOption.get.status, "Cancelled")
    assertEquals(secure(E.myBalance, be, uTok, ()).body.toOption.get.balancePoints, 1000L)
    assertEquals(secure(E.myWithdrawals, be, uTok, ()).body.toOption.get.size, 2)
    assertEquals(secure(E.listWithdrawals, be, svc, ()).body.toOption.get.size, 2)
  }

  test("webhook failed outcome returns the reserve to the user (the refund leg), cash untouched") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    withWebhookSecret()
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "wf-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(400, "wf-1")).body.toOption.get
    secure(E.approveWithdrawal, be, svc, (w.id, DecisionRequest("PendingReview")))
    assertEquals(public(E.stripeWebhook, be, StripeEvents.signed(whsec, StripeEvents.event("evt_fail", "transfer.failed", "tr_fail", w.id, 0))).body.toOption.get.status, Some("Failed"))
    assertEquals(secure(E.accountBalance, be, svc, "system:withdrawal_clearing").body.toOption.get.balancePoints, 0L)
    assertEquals(secure(E.accountBalance, be, svc, "system:cash").body.toOption.get.balancePoints, 0L)
    assertEquals(secure(E.myBalance, be, uTok, ()).body.toOption.get.balancePoints, 1000L)
  }

  test("cross-user: a user cannot cancel another user's withdrawal (403)") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc")
    val u1 = api.userToken("u1"); val u2 = api.userToken("u2")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "xu-fund"))
    val w = secure(E.requestWithdrawal, be, u1, WithdrawalRequestBody(300, "xu-1")).body.toOption.get
    assertEquals(secure(E.cancelWithdrawal, be, u2, (w.id, DecisionRequest("PendingReview"))).code, StatusCode.Forbidden)
  }

  test("adjustments: list and get round-trip; a rejected proposal cannot be approved") {
    val api = freshApi(); val be = stubOf(api); val alice = api.adminToken("alice"); val bob = api.adminToken("bob")
    secure(E.incentiveCredit, be, alice, CreditRequest("u1", 1000, "annotation_job", "adj-fund"))
    val p = secure(E.proposeAdjustment, be, alice, AdjustProposeBody("u1", "CREDIT", 100, "bonus")).body.toOption.get
    assert(secure(E.listAdjustments, be, alice, ()).body.toOption.get.exists(_.id == p.id))
    assertEquals(secure(E.getAdjustment, be, alice, p.id).body.toOption.get.id, p.id)
    assertEquals(secure(E.rejectAdjustment, be, bob, (p.id, DecisionRequest("PendingReview"))).body.toOption.get.status, "Rejected")
    assertEquals(secure(E.approveAdjustment, be, alice, (p.id, DecisionRequest("PendingReview"))).code, StatusCode.Conflict)
    assertEquals(secure(E.getAdjustment, be, alice, 999999L).code, StatusCode.NotFound)
  }

  test("submit, user-transactions, obligation cancel, invariants run, and config-proposal listing") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "sub-fund"))
    assert(secure(E.userTransactions, be, svc, "u1").body.toOption.get.nonEmpty)
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "sub-1")).body.toOption.get
    assertEquals(secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 15, 285, Some("quote-sub"), Some("tr-sub")))).body.toOption.get.status, "Submitted")
    secure(E.openObligation, be, svc, ObligationOpenBody("annotation_job", "ob-x", "u2", 500))
    assertEquals(secure(E.cancelObligation, be, svc, ObligationCancelBody("annotation_job", "ob-x")).body.toOption.get.status, "Cancelled")
    val run = secure(E.invariantsCheck, be, svc, ()).body.toOption.get
    assert(run.allPassed, run.toString)
    assertEquals(secure(E.invariantsLatest, be, svc, ()).body.toOption.get.runId, run.runId)
    assertEquals(secure(E.invariantById, be, svc, run.runId).body.toOption.get.runId, run.runId)
    val cp = secure(E.proposeConfig, be, svc, ConfigProposalRequest("single_payout_limit_points", "70000", "raise")).body.toOption.get
    assert(secure(E.listConfigProposals, be, svc, ()).body.toOption.get.exists(_.id == cp.id))
  }
