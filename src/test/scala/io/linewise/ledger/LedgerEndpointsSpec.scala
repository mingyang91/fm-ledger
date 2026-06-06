package io.linewise.ledger

import java.util.UUID
import scala.sys.process.*
import scala.util.control.NonFatal

import sttp.client4.*
import sttp.model.{StatusCode, Uri}
import sttp.tapir.Endpoint
import sttp.tapir.client.sttp4.SttpClientInterpreter
import sttp.tapir.server.stub4.TapirSyncStubInterpreter

import io.linewise.ledger.LedgerEndpoints as E
import io.linewise.ledger.generated.LedgerModel.TxKind
import io.linewise.ledger.generated.{World, HasLedger, HasWithdrawals, HasProposals, HasObligations}
import io.linewise.ledger.generated.{LedgerService, WithdrawalService, AdjustmentService, ObligationService}
import io.linewise.ledger.generated.{JdbcLedgerRepository, JdbcWithdrawalRepository, JdbcProposalRepository, JdbcObligationRepository}
import io.linewise.ledger.generated.{InMemLedgerRepository, InMemWithdrawalRepository, InMemProposalRepository, InMemObligationRepository}

/* =============================================================================
 * Endpoint spec for the ledger microservice. Each test binds a fresh PostgreSQL
 * schema to the `Db` facade, stands up a LedgerApi, wraps its server endpoints in a
 * tapir sync stub (no port bound), and drives them with a typed sttp client from the
 * same bare endpoints. Every business decision inside runs the Stainless-transpiled
 * LedgerService through the generated JDBC repository. Tests are sequential
 * (munit default), each resetting the global Db to a fresh schema.
 * ========================================================================== */
class LedgerEndpointsSpec extends munit.FunSuite:
  private val baseUri: Uri = uri"http://test.local"
  private val client = SttpClientInterpreter()
  System.setProperty("java.net.useSystemProxies", "false")
  System.clearProperty("socksProxyHost")
  System.clearProperty("socksProxyPort")
  private var containerId: String = ""
  private var jdbcUrl: String = ""

  override def beforeAll(): Unit =
    containerId = Seq(
      "docker", "run", "-d", "--rm",
      "-e", "POSTGRES_PASSWORD=postgres",
      "-e", "POSTGRES_DB=ledger_test",
      "-p", "127.0.0.1::5432",
      "postgres:16-alpine"
    ).!!.trim
    val binding = Seq("docker", "port", containerId, "5432/tcp").!!.linesIterator.next().trim
    val port = binding.substring(binding.lastIndexOf(':') + 1)
    jdbcUrl = s"jdbc:postgresql://127.0.0.1:$port/ledger_test"
    waitForPostgres()

  override def afterAll(): Unit =
    if containerId.nonEmpty then Seq("docker", "rm", "-f", containerId).!

  private def waitForPostgres(): Unit =
    var last: Throwable = RuntimeException("PostgreSQL did not start")
    var i = 0
    while i < 80 do
      try
        val ds = Jdbc.dataSource(jdbcUrl, "postgres", "postgres")
        val c = ds.getConnection()
        try return
        finally c.close()
      catch
        case NonFatal(e) =>
          last = e
          Thread.sleep(500)
          i += 1
    throw last

  private def freshDs(prefix: String) =
    val schema = s"${prefix}_${UUID.randomUUID.toString.replace("-", "")}"
    Jdbc.dataSource(jdbcUrl, "postgres", "postgres", schema)

  private def freshApi(): LedgerApi =
    val ds = freshDs("ledger")
    val c0 = ds.getConnection()
    try Jdbc.initSchema(c0) finally c0.close()
    Db.init(ds)
    LedgerApi()

  private def stubOf(api: LedgerApi): SyncBackend =
    TapirSyncStubInterpreter().whenServerEndpointsRunLogic(api.serverEndpoints).backend()

  private def secure[A, I, EE, O](ep: Endpoint[A, I, EE, O, Any], be: SyncBackend, token: A, in: I): Response[Either[EE, O]] =
    client.toSecureRequestThrowDecodeFailures(ep, Some(baseUri))(token)(in).send(be)

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
    assertEquals(s2.cashPoints, 400L)
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

  test("stripe webhook verifies signatures, deduplicates event ids, and writes one cash leg") {
    val api = freshApi(); val be = stubOf(api); val svc = api.adminToken("svc"); val uTok = api.userToken("u1")
    secure(E.incentiveCredit, be, svc, CreditRequest("u1", 1000, "annotation_job", "stripe-fund"))
    val w = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, "stripe-cr")).body.toOption.get
    secure(E.approveWithdrawal, be, svc, (w.id, DecisionRequest("PendingReview")))
    val sig = Db.webhookSignature("evt-1", w.id, "settled")
    val settled = secure(E.stripeWebhook, be, svc, StripeWebhookBody(w.id, "settled", "evt-1", sig))
    assertEquals(settled.body.toOption.get.status, "Settled")
    assertEquals(secure(E.accountBalance, be, svc, "system:cash").body.toOption.get.balancePoints, 300L)
    assertEquals(secure(E.stripeWebhook, be, svc, StripeWebhookBody(w.id, "settled", "evt-1", sig)).code, StatusCode.Ok)
    assertEquals(secure(E.accountBalance, be, svc, "system:cash").body.toOption.get.balancePoints, 300L)
    assertEquals(secure(E.stripeWebhook, be, svc, StripeWebhookBody(w.id, "settled", "evt-2", "bad")).code, StatusCode.Unauthorized)
  }

  // --- the drift gate: in-memory ORACLE vs the @extern-over-JDBC PostgreSQL realization ---
  // Kept in this suite (not a separate one) so it shares the sequential schedule that
  // guards the global `Db` singleton against the other Db-binding tests above.
  private val ledgerSvc = LedgerService[World](HasLedger())
  private val wSvc = WithdrawalService[World](HasLedger(), HasWithdrawals())
  private val aSvc = AdjustmentService[World](HasLedger(), HasProposals())
  private val oSvc = ObligationService[World](HasObligations())

  test("drift gate: the InMem oracle and the @extern-over-JDBC PostgreSQL realization agree row-for-row") {
    val ds = freshDs("ldiff")
    val c0 = ds.getConnection(); try Jdbc.initSchema(c0) finally c0.close()
    Db.init(ds)
    val jdbcW: World = World(JdbcLedgerRepository(), JdbcWithdrawalRepository(), JdbcProposalRepository(), JdbcObligationRepository())
    var memW: World = World(InMemLedgerRepository(Nil), InMemWithdrawalRepository(Nil), InMemProposalRepository(Nil), InMemObligationRepository(Nil))

    def credit(uid: String, amount: Long, fid: Long, sk: String, si: String): Unit =
      val (mw, mr) = ledgerSvc.credit(memW, Accounts.IncentiveExpense, Accounts.user(uid), uid, amount, fid, sk, si); memW = mw
      assertEquals(mr, ledgerSvc.credit(jdbcW, Accounts.IncentiveExpense, Accounts.user(uid), uid, amount, fid, sk, si)._2, "credit verdict diverged")
    def openObl(sk: String, si: String, uid: String): Unit =
      val (mw, mr) = oSvc.open(memW, sk, si, uid, "", "", "", 600L); memW = mw
      assertEquals(mr, oSvc.open(jdbcW, sk, si, uid, "", "", "", 600L)._2, "obligation open verdict diverged")
    def reserve(uid: String, amount: Long, creq: String, wid: Long, txid: Long): Unit =
      val (mw, mr) = wSvc.request(memW, uid, amount, creq, wid, txid, Accounts.user(uid), Accounts.WithdrawalClearing); memW = mw
      assertEquals(mr, wSvc.request(jdbcW, uid, amount, creq, wid, txid, Accounts.user(uid), Accounts.WithdrawalClearing)._2, "withdrawal request verdict diverged")
    def propose(uid: String, amount: Long, pid: Long): Unit =
      val (mw, mr) = aSvc.propose(memW, TxKind.ManualAdjustment, uid, Accounts.Adjustment, Accounts.user(uid), amount, "bonus", "alice", pid, None); memW = mw
      assertEquals(mr, aSvc.propose(jdbcW, TxKind.ManualAdjustment, uid, Accounts.Adjustment, Accounts.user(uid), amount, "bonus", "alice", pid, None)._2, "proposal verdict diverged")

    credit("u1", 1500L, 1L, "annotation_job", "job-1")
    credit("u1", 1500L, 1L, "annotation_job", "job-1") // duplicate source: both -> Left(DuplicateSource)
    credit("u2", 800L, 2L, "annotation_job", "job-2")
    openObl("annotation_job", "job-9", "u1")
    reserve("u1", 500L, "cr-1", 100L, 101L)
    propose("u1", 200L, 200L)

    assertEquals(ledgerSvc.all(memW).sortBy(_.id), ledgerSvc.all(jdbcW).sortBy(_.id), "ledger txs diverged")
    assertEquals(ledgerSvc.get(memW, 1L), ledgerSvc.get(jdbcW, 1L), "tx get diverged")
    assertEquals(ledgerSvc.get(memW, 999L), ledgerSvc.get(jdbcW, 999L), "missing tx get diverged")
    assertEquals(oSvc.bySource(memW, "annotation_job", "job-9"), oSvc.bySource(jdbcW, "annotation_job", "job-9"), "obligation diverged")
    assertEquals(oSvc.openObligations(memW).sortBy(_.sourceId), oSvc.openObligations(jdbcW).sortBy(_.sourceId), "open obligations diverged")
    assertEquals(wSvc.get(memW, 100L), wSvc.get(jdbcW, 100L), "withdrawal diverged")
    assertEquals(aSvc.get(memW, 200L), aSvc.get(jdbcW, 200L), "proposal diverged")
  }


  test("a transaction cannot be reversed twice (already_reversed)") {
    val api = freshApi(); val be = stubOf(api); val alice = api.adminToken("alice"); val bob = api.adminToken("bob")
    val tx = secure(E.incentiveCredit, be, alice, CreditRequest("u1", 500, "annotation_job", "jr")).body.toOption.get
    val rev = secure(E.proposeReversal, be, alice, ReversalProposeBody(tx.id, "mistake")).body.toOption.get
    assertEquals(secure(E.approveReversal, be, bob, (rev.id, DecisionRequest("PendingReview"))).code, StatusCode.Ok)
    assertEquals(secure(E.proposeReversal, be, alice, ReversalProposeBody(tx.id, "again")).code, StatusCode.Conflict)
  }

  test("LEDGER_TX is append-only and a credit must carry a source (DB-enforced)") {
    val ds = freshDs("ledgerao")
    val c = ds.getConnection()
    try
      Jdbc.initSchema(c)
      val st = c.createStatement()
      st.execute("INSERT INTO LEDGER_TX (ID, KIND, USER_UID) VALUES (1, 'ManualAdjustment', 'u1')")
      st.execute("INSERT INTO LEDGER_ENTRY (TX_ID, ACCOUNT_ID, DIRECTION, AMOUNT) VALUES (1, 'system:adjustment', 'DR', 100)")
      st.execute("INSERT INTO LEDGER_ENTRY (TX_ID, ACCOUNT_ID, DIRECTION, AMOUNT) VALUES (1, 'user:u1', 'CR', 100)")
      intercept[java.sql.SQLException](st.execute("UPDATE LEDGER_TX SET USER_UID = 'u2' WHERE ID = 1")) // append-only: UPDATE rejected
      intercept[java.sql.SQLException](st.execute("DELETE FROM LEDGER_ENTRY WHERE TX_ID = 1"))          // append-only: DELETE rejected
      intercept[java.sql.SQLException](st.execute(                                                    // CHECK: credit needs a source
        "INSERT INTO LEDGER_TX (ID, KIND, USER_UID) VALUES (2, 'IncentiveCredit', 'u1')"))
    finally c.close()
  }
