package io.linewise.ledger

import sttp.model.StatusCode

import io.linewise.ledger.LedgerEndpoints as E
import io.linewise.ledger.generated.LedgerModel.{TxKind, WithdrawalStatus}
import scala.jdk.CollectionConverters.*

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
    val prop = secure(E.proposeAdjustment, be, alice, AdjustProposeBody("u1", AdjustDirection.CREDIT, 500, "bonus")).body.toOption.get
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
    val dprop = secure(E.proposeAdjustment, be, alice, AdjustProposeBody("u1", AdjustDirection.DEBIT, 99999, "clawback")).body.toOption.get
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

  test("negative Stripe fee is rejected before consuming the provider event") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    withWebhookSecret()
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "neg-fee-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "neg-fee-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    val rejected = public(E.stripeWebhook, be, StripeEvents.signed(whsec, StripeEvents.event("evt_neg", "transfer.paid", "tr_neg", w.id, -1)))
    assertEquals(rejected.code, StatusCode.BadRequest)
    assertEquals(rejected.body.swap.toOption.get._2, "bad_fee")
    assertEquals(secure(E.listProviderEvents, be, svc, w.id).body.toOption.get.size, 0)
    assertEquals(public(E.stripeWebhook, be, StripeEvents.signed(whsec, StripeEvents.event("evt_neg", "transfer.paid", "tr_neg", w.id, 20))).body.toOption.get.status, Some("Settled"))
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

  test("submit is atomic on status conflict and detects idempotency conflicts") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "idem-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "idem-cr")).body.toOption.get
    val wrong = secure(E.submitWithdrawal, be, svc,
      (w.id, PayoutSubmitRequest("Submitted", "stripe", "stripe_standard", "acct-1", 20, 280, Some("q1"), None)))
    assertEquals(wrong.code, StatusCode.Conflict)
    assertEquals(Db.payoutIntentByWithdrawal(w.id), None)
    assertEquals(Db.dispatchByWithdrawal(w.id), None)

    val req = PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, Some("q1"), None)
    assertEquals(secure(E.submitWithdrawal, be, svc, (w.id, req)).body.toOption.get.status, "Submitted")
    assertEquals(secure(E.submitWithdrawal, be, svc, (w.id, req)).body.toOption.get.status, "Submitted")
    val changed = secure(E.submitWithdrawal, be, svc,
      (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-2", 20, 280, Some("q1"), None)))
    assertEquals(changed.code, StatusCode.Conflict)
    assertEquals(changed.body.swap.toOption.get._2, "idempotency_conflict")
  }

  test("submit rejects a non-divisible payout unit before creating intent or dispatch") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    Db.setSystemConfig("payout_points_per_minor_unit", "100", "test", "test")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "unit-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "unit-cr")).body.toOption.get
    val res = secure(E.submitWithdrawal, be, svc,
      (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    assertEquals(res.code, StatusCode.BadRequest)
    assertEquals(res.body.swap.toOption.get._2, "bad_payout_unit")
    assertEquals(Db.payoutIntentByWithdrawal(w.id), None)
    assertEquals(Db.dispatchByWithdrawal(w.id), None)
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

  test("payout dispatcher claim moves a pending row to inflight and prevents a second claim") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "claim-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "claim-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    val first = Db.claimPendingDispatches(1)
    assertEquals(first.size, 1)
    assertEquals(first.head.status, "inflight")
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("inflight"))
    assertEquals(Db.claimPendingDispatches(1), Nil)
  }

  test("reaper (B1): a dispatch stranded 'inflight' by a crash is reclaimed to 'pending' and a later tick dispatches it") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "reap-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "reap-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    // simulate B1: claim the row to 'inflight', then the worker dies before completing it
    Db.claimPendingDispatches(1)
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("inflight"))
    // the reaper (startup or periodic) reclaims the stranded row back to 'pending' — proven to wake it
    assertEquals(Db.reclaimStaleInflight(0), 1)
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("pending"))
    // a healthy tick now dispatches it under the SAME idempotency key (safe, no double-pay)
    val fake = FakeGateway()
    assertEquals(PayoutDispatcher(fake).tick(), 1)
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("dispatched"))
    assertEquals(fake.calls.head.idempotencyKey, s"wd-${w.id}-intent-${Db.payoutIntentByWithdrawal(w.id).get.id}")
  }

  test("reaper safety: a FRESH inflight row (within the timeout) is NOT reclaimed — never touch a live transfer") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "fresh-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "fresh-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    Db.claimPendingDispatches(1)
    assertEquals(Db.reclaimStaleInflight(3600), 0)
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("inflight"))
  }

  test("#3: a permanently failed dispatch fails the withdrawal AND refunds the reserve (never stuck in Submitted)") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "p3-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "p3-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    assertEquals(secure(E.myBalance, be, uTok, ()).body.toOption.get.balancePoints, 700L) // reserve held in clearing
    // a permanent gateway error: in one tick the dispatch flips to failed AND the withdrawal is failed + refunded
    PayoutDispatcher(FailingGateway(retryable = false), onPermanentFailure = api.failWithdrawalForFailedDispatch).tick()
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("failed"))
    assertEquals(Db.withdrawalById(w.id).map(_.status.toString), Some("Failed"))
    assertEquals(secure(E.myBalance, be, uTok, ()).body.toOption.get.balancePoints, 1000L) // reserve refunded
    assertEquals(secure(E.accountBalance, be, svc, "system:withdrawal_clearing").body.toOption.get.balancePoints, 0L)
    assert(secure(E.riskEvents, be, svc, ()).body.toOption.get.exists(_.kind == "payout_dispatch_failed"))
  }

  test("#5: the webhook observed fee (minor units) is converted to points before posting/reconciling (ratio != 1)") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    withWebhookSecret()
    Db.setSystemConfig("payout_points_per_minor_unit", "2", "test", "test") // 1 minor unit = 2 points
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "p5-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "p5-cr")).body.toOption.get
    // gross 300, quoted fee 20 points, net 280 -> amountMinor = 280/2 = 140 (divisible, accepted)
    assertEquals(secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None))).body.toOption.get.status, "Submitted")
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.amountMinor), Some(140L))
    // Stripe reports the fee as 10 MINOR units; the shell converts 10*2 = 20 points == the quote -> matched
    val (sig, body) = StripeEvents.signed(whsec, StripeEvents.event("evt_p5", "transfer.paid", "tr_p5", w.id, 10))
    assertEquals(public(E.stripeWebhook, be, (sig, body)).body.toOption.get.status, Some("Settled"))
    assertEquals(secure(E.getPayoutReconciliation, be, svc, w.id).body.toOption.get.result, "matched")
    assertEquals(secure(E.accountBalance, be, svc, Accounts.ProviderPayoutFee).body.toOption.get.balancePoints, 20L) // 10 minor -> 20 points
    assert(secure(E.summary, be, svc, ()).body.toOption.get.fundsInvariantHolds)
  }

  /* ===========================================================================
   * REPRODUCTIONS of the external review findings (P1-a/b, P2-a/b). Each is tagged `.fail`:
   * it asserts the CORRECT property, which fails TODAY (documenting the bug) so the suite stays
   * green; when the bug is fixed the property holds, munit reports an unexpected pass, and the fix
   * removes the `.fail`. P1-a/b are reproduced deterministically at the decision/store level (no
   * flaky thread races) — the same root cause concurrency exposes.
   * ======================================================================== */

  test("P1-a (fixed): concurrent over-limit withdrawals serialize on an advisory lock — exactly one passes") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    Db.setSystemConfig("per_user_day_payout_limit_points", "500", "test", "test")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "p1a-fund"))
    val barrier = new java.util.concurrent.CyclicBarrier(2)
    val ok = new java.util.concurrent.atomic.AtomicInteger(0)
    val rejected = new java.util.concurrent.atomic.AtomicInteger(0)
    def fire(cr: String): Unit =
      try
        barrier.await()
        secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, cr)).code match
          case StatusCode.Ok                  => ok.incrementAndGet()
          case StatusCode.UnprocessableEntity => rejected.incrementAndGet()
          case _                              => ()
      catch case _: Throwable => ()
    val t1 = new Thread(() => fire("p1a-A")); val t2 = new Thread(() => fire("p1a-B"))
    t1.start(); t2.start(); t1.join(); t2.join()
    // 300 + 300 = 600 > 500/day: the advisory lock serializes them, the second sees the first's
    // committed reservation in its in-tx check and is rejected. Exactly one passes (never both).
    assertEquals(ok.get, 1, s"expected exactly one to pass; ok=${ok.get} rejected=${rejected.get}")
    assertEquals(rejected.get, 1)
  }

  test("P1-b (fixed): concurrent duplicate-source credits both resolve to the SAME tx idempotently (no 23505 leak)") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc")
    val barrier = new java.util.concurrent.CyclicBarrier(2)
    val ids = new java.util.concurrent.ConcurrentLinkedQueue[Long]()
    val errs = new java.util.concurrent.atomic.AtomicInteger(0)
    def fire(): Unit =
      try
        barrier.await()
        secure(E.incentiveCredit, be, svc, CreditRequest("u1", 100, "annotation_job", "dup-src")).body match
          case Right(tx) => ids.add(tx.id)
          case Left(_)   => errs.incrementAndGet()
      catch case _: Throwable => errs.incrementAndGet()
    val t1 = new Thread(() => fire()); val t2 = new Thread(() => fire())
    t1.start(); t2.start(); t1.join(); t2.join()
    // Whichever order they interleave (collision caught + reselected, or the loser sees the committed
    // row on its pre-check), both end as an idempotent Ok for the SAME tx — never a leaked 23505.
    assertEquals(errs.get, 0, "a concurrent duplicate credit leaked an error instead of an idempotent replay")
    assertEquals(ids.asScala.toSet.size, 1, "both concurrent credits should resolve to the same tx id")
  }

  test("P2-a (fixed): the startup reaper uses the configured timeout, so a fresh live inflight row is left alone") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "p2a-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "p2a-cr")).body.toOption.get
    secure(E.submitWithdrawal, be, svc, (w.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", "acct-1", 20, 280, None, None)))
    Db.claimPendingDispatches(1) // a live transfer just claimed this row
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("inflight"))
    // exactly what LedgerServer startup now runs (the configured window, NOT 0): a just-claimed row
    // is well within the window, so an overlapping/restarted instance does not reclaim a live transfer.
    Db.reclaimStaleInflight(PayoutDispatcher.DefaultReclaimAfterSeconds)
    assertEquals(Db.dispatchByWithdrawal(w.id).map(_.status), Some("inflight"))
  }

  test("P2-b (fixed): an invalid adjustment direction is rejected at the HTTP boundary (enum decode -> 400)") {
    val api = freshApi(); val be = stubOf(api); val alice = api.adminToken("alice")
    secure(E.incentiveCredit, be, alice, CreditRequest("u1", 1000, "annotation_job", "p2b-fund"))
    // a valid DEBIT still debits the user (CREDIT/DEBIT are the only representable directions)
    assertEquals(secure(E.proposeAdjustment, be, alice, AdjustProposeBody("u1", AdjustDirection.DEBIT, 100, "clawback")).body.toOption.get.debitAccount, Accounts.user("u1"))
    // an unknown wire value can't be constructed in the typed client; over the wire it is a decode-time 400.
    assertEquals(rawPost(be, "adjustments", alice, """{"userUid":"u1","direction":"BOGUS","amountPoints":100,"reason":"typo"}""").code, StatusCode.BadRequest)
  }

  test("P2-b (fixed): a non-positive obligation estimate is rejected (400), never entering the forecast") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc")
    assertEquals(secure(E.openObligation, be, svc, ObligationOpenBody("annotation_job", "neg-est", "u1", -100)).code, StatusCode.BadRequest)
    assertEquals(secure(E.openObligation, be, svc, ObligationOpenBody("annotation_job", "zero-est", "u1", 0)).code, StatusCode.BadRequest)
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
    val p = secure(E.proposeAdjustment, be, alice, AdjustProposeBody("u1", AdjustDirection.CREDIT, 100, "bonus")).body.toOption.get
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

  test("specialized Scala read paths match canonical ledger semantics") {
    val api = freshApi(); val be = stubOf(api); val alice = api.adminToken("alice"); val bob = api.adminToken("bob"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, alice, CreditRequest("u1", 1000, "annotation_job", "perf-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(250, "perf-w1")).body.toOption.get
    secure(E.rejectWithdrawal, be, bob, (w.id, DecisionRequest("PendingReview")))
    secure(E.proposeAdjustment, be, alice, AdjustProposeBody("u1", AdjustDirection.CREDIT, 100, "perf-adjust"))

    val acct = Accounts.user("u1")
    val txIdsFromSpecialized = Db.txsForAccount(acct).map(_.id)
    val txIdsFromCanonical = Db.allTxs.filter(_.entries.exists(_.account == acct)).map(_.id)
    assertEquals(txIdsFromSpecialized, txIdsFromCanonical)

    val withdrawalsFromSpecialized = Db.withdrawalsByUser("u1").map(wd => wd.id -> wd.status.toString)
    val withdrawalsFromCanonical = Db.allWithdrawals.filter(_.userUid == "u1").map(wd => wd.id -> wd.status.toString)
    assertEquals(withdrawalsFromSpecialized, withdrawalsFromCanonical)

    val adjustmentIdsFromSpecialized = Db.proposalsByKind(TxKind.ManualAdjustment).map(_.id)
    val adjustmentIdsFromCanonical = Db.allProposals.filter(_.kind == TxKind.ManualAdjustment).map(_.id)
    assertEquals(adjustmentIdsFromSpecialized, adjustmentIdsFromCanonical)

    val summary = Db.summaryBalances
    assertEquals(summary.incentiveExpense, Db.balanceOf(Accounts.IncentiveExpense))
    assertEquals(summary.providerPayoutFee, Db.balanceOf(Accounts.ProviderPayoutFee))
    assertEquals(summary.adjustment, Db.categoryBalance("adjustment"))
    assertEquals(summary.users, Db.categoryBalance("user_liability"))
    assertEquals(summary.clearing, Db.categoryBalance("clearing"))
    assertEquals(summary.settlement, Db.categoryBalance("settlement"))
    assertEquals(summary.revenue, Db.categoryBalance("revenue"))

    val pendingViaQuery = Db.pendingWithdrawalClearing
    val pendingViaFilter = Db.allWithdrawals.filter(wd => wd.status == WithdrawalStatus.PendingReview || wd.status == WithdrawalStatus.Submitted).map(_.amount).sum
    assertEquals(pendingViaQuery, pendingViaFilter)

    assert(Db.ledgerBalanced)
    assert(Db.negativeUserLedgerAccounts.isEmpty)
    assert(Db.approvedRollbackRefsOk)
    assert(Db.noReversalOfReversal)
  }
