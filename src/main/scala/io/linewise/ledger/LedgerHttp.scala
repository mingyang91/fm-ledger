package io.linewise.ledger

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import sttp.model.StatusCode
import sttp.monad.{IdentityMonad, MonadError}
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.json.pickler.*
import sttp.tapir.server.ServerEndpoint

import io.linewise.ledger.generated.LedgerModel.*
import io.linewise.ledger.generated.{World, HasDb, DB, LedgerService, WithdrawalService, AdjustmentService, ObligationService, Withdrawal, Proposal, Obligation}
import io.linewise.ledger.generated.LedgerTables.JdbcLedgerStore
import io.linewise.ledger.generated.WithdrawalTables.JdbcWithdrawalStore
import io.linewise.ledger.generated.ProposalTables.JdbcProposalStore
import io.linewise.ledger.generated.ObligationTables.JdbcObligationStore

// Thrown inside a webhook `Db.transaction` to force a rollback when the settle/fail
// transition fails, so the provider-event dedup row is NOT persisted and the event stays
// retryable. Carries the HTTP error to return after the rollback.
private final case class WebhookAbort(err: (StatusCode, String)) extends RuntimeException
// Thrown ONLY when the provider-event PK insert itself hits a 23505 (a genuine duplicate
// event id), so a 23505 from any other write in the same transaction is NOT misread as a
// replay and propagates as a real error.
private final case class DuplicateEvent() extends RuntimeException

/** Walk the cause chain for a Postgres unique-violation (SQLState 23505). Magnum wraps the
  * driver SQLException in its own SqlException, so the 23505 sits on a cause, not the top. */
private def isUniqueViolation(t: Throwable): Boolean =
  var c = t
  var found = false
  while c != null && !found do
    c match
      case s: java.sql.SQLException if s.getSQLState == "23505" => found = true
      case _ => c = c.getCause
  found

/* =============================================================================
 * LEDGER WEB SERVER — direct-style (Ox / Identity) tapir over PostgreSQL-backed JDBC.
 * Every business decision is delegated to the STAINLESS-TRANSPILED LedgerService through
 * the [W] / Has lens (over a JDBC-backed World whose repository delegates to `Db`);
 * only the HTTP shell (routing, JSON, status mapping) and the backed-token auth
 * live here. Balances and the funds summary are production aggregates read from `Db`
 * (the verified core proves append-only + conservation-by-construction; the live SUM is
 * a read concern, guarded by the differential test).
 * ========================================================================== */

// --- wire DTOs (hand-written shell types; Pickler-derived camelCase JSON) ------
final case class CreditRequest(
    userUid: String,
    amountPoints: Long,
    sourceKind: String,
    sourceId: String,
    incentiveTraceId: Option[String] = None,
    incentiveModule: Option[String] = None)
// #P2-b: a closed enum so an unknown direction is rejected at the JSON boundary (decode -> 400),
// never silently defaulted to a DEBIT against the user.
enum AdjustDirection:
  case CREDIT, DEBIT
final case class AdjustProposeBody(userUid: String, direction: AdjustDirection, amountPoints: Long, reason: String)
final case class ReversalProposeBody(targetTxId: Long, reason: String)
final case class ProposalResponse(
    id: Long, kind: String, userUid: String, debitAccount: String, creditAccount: String,
    amountPoints: Long, reason: String, proposedByUid: String, status: String, resultTxId: Option[Long])
final case class TxEntryResponse(account: String, direction: String, amountPoints: Long)
final case class TxResponse(
    id: Long, kind: String, debitAccount: String, creditAccount: String,
    amountPoints: Long, userUid: String, sourceKind: Option[String], sourceId: Option[String], entries: List[TxEntryResponse])
final case class BalanceResponse(account: String, balancePoints: Long, normalSide: String)
final case class SummaryResponse(
    incentiveExpensePoints: Long, totalUserLiabilityPoints: Long, withdrawalClearingPoints: Long,
    settlementPoints: Long, adjustmentPoints: Long, feeRecoveryPoints: Long, providerPayoutFeePoints: Long, fundsInvariantHolds: Boolean)
final case class WithdrawalRequestBody(amountPoints: Long, clientRequestId: String)
final case class WithdrawalResponse(
    id: Long, userUid: String, amountPoints: Long, status: String, clientRequestId: String, reserveTxId: Long)
final case class DecisionRequest(expectedStatus: String)
final case class PayoutSubmitRequest(expectedStatus: String, provider: String, routeCode: String, destinationId: String, quotedProviderFee: Long, expectedRecipientNet: Long, quoteRef: Option[String], providerTransferRef: Option[String])
final case class PayoutIntentResponse(withdrawalId: Long, provider: String, routeCode: String, destinationId: String, grossAmount: Long, quotedProviderFee: Long, expectedRecipientNet: Long, quoteRef: Option[String], providerTransferRef: Option[String])
final case class PayoutReconciliationResponse(withdrawalId: Long, expectedFee: Long, observedFee: Option[Long], result: String, note: Option[String])
final case class ProviderEventResponse(eventId: String, provider: String, providerTransferRef: Option[String], outcome: String, providerFeeDebited: Option[Long])
final case class ObligationOpenBody(sourceKind: String, sourceId: String, userUid: String, estimatedPoints: Long)
final case class ObligationCancelBody(sourceKind: String, sourceId: String)
final case class ObligationResponse(
    sourceKind: String, sourceId: String, userUid: String, estimatedPoints: Long, status: String, realizedTxId: Option[Long])
final case class UpcomingExpenseResponse(openCount: Long, projectedPoints: Long)
// Webhook acknowledgement: Stripe only needs a 2xx, so we report what we did rather than echo a
// withdrawal. handled=false means a signed-but-unrelated event type we acknowledge and ignore.
final case class WebhookAck(eventId: String, handled: Boolean, withdrawalId: Option[Long], status: Option[String])
final case class InvariantCheckDto(name: String, passed: Boolean, detail: Option[String])
final case class InvariantRunResponse(runId: String, allPassed: Boolean, checks: List[InvariantCheckDto])
final case class RiskEventResponse(id: Long, kind: String, subject: String, detail: String)
final case class AuditLogResponse(id: Long, action: String, actor: String, subject: String, detail: String)
final case class ConfigEntryResponse(key: String, value: String)
final case class ConfigProposalRequest(key: String, value: String, reason: String)
final case class ConfigProposalResponse(id: Long, key: String, value: String, reason: String, status: String, proposedBy: String, approvedBy: Option[String], rejectedBy: Option[String])

object LedgerJson:
  given Pickler[CreditRequest]          = Pickler.derived
  given Pickler[AdjustDirection]        = Pickler.derived
  given Pickler[AdjustProposeBody]      = Pickler.derived
  given Pickler[ReversalProposeBody]    = Pickler.derived
  given Pickler[ProposalResponse]       = Pickler.derived
  given Pickler[TxEntryResponse]        = Pickler.derived
  given Pickler[TxResponse]             = Pickler.derived
  given Pickler[BalanceResponse]        = Pickler.derived
  given Pickler[SummaryResponse]        = Pickler.derived
  given Pickler[WithdrawalRequestBody]  = Pickler.derived
  given Pickler[WithdrawalResponse]     = Pickler.derived
  given Pickler[DecisionRequest]        = Pickler.derived
  given Pickler[PayoutSubmitRequest]    = Pickler.derived
  given Pickler[PayoutIntentResponse]   = Pickler.derived
  given Pickler[PayoutReconciliationResponse] = Pickler.derived
  given Pickler[ProviderEventResponse]  = Pickler.derived
  given Pickler[ObligationOpenBody]     = Pickler.derived
  given Pickler[ObligationCancelBody]   = Pickler.derived
  given Pickler[ObligationResponse]     = Pickler.derived
  given Pickler[UpcomingExpenseResponse] = Pickler.derived
  given Pickler[WebhookAck]             = Pickler.derived
  given Pickler[InvariantCheckDto]      = Pickler.derived
  given Pickler[InvariantRunResponse]   = Pickler.derived
  given Pickler[RiskEventResponse]      = Pickler.derived
  given Pickler[AuditLogResponse]       = Pickler.derived
  given Pickler[ConfigEntryResponse]    = Pickler.derived
  given Pickler[ConfigProposalRequest]  = Pickler.derived
  given Pickler[ConfigProposalResponse] = Pickler.derived

/** Pagination bounds for the list endpoints. A request may ask for up to `Max` rows; an absent or
  * non-positive `limit` falls back to `Default`. `before` is a keyset cursor — the id of the last
  * (smallest) row of the previous page — and the store reads strictly-smaller ids, newest-first.
  * Nothing is silently dropped: the client walks the whole set by following the cursor. This bounds
  * the heap a single request can hold, which is the fix for the unbounded-collection finding. */
object Paging:
  val Default = 100
  val Max = 500
  def limitOf(requested: Option[Int]): Int = requested match
    case Some(n) if n > 0 => math.min(n, Max)
    case _                => Default

/** The BARE endpoints (capability `Any`), shared by server and any typed client. */
object LedgerEndpoints:
  import LedgerJson.given
  type Err = (StatusCode, String)
  private val errOut = statusCode.and(stringBody)
  private val bearer = auth.bearer[String]()

  val incentiveCredit = endpoint.securityIn(bearer).post.in("ledger" / "incentive-credit").in(jsonBody[CreditRequest]).out(jsonBody[TxResponse]).errorOut(errOut)
  // two-person adjustments + reversals
  val proposeAdjustment = endpoint.securityIn(bearer).post.in("adjustments").in(jsonBody[AdjustProposeBody]).out(jsonBody[ProposalResponse]).errorOut(errOut)
  val listAdjustments   = endpoint.securityIn(bearer).get.in("adjustments").in(query[Option[Int]]("limit")).in(query[Option[Long]]("before")).out(jsonBody[List[ProposalResponse]]).errorOut(errOut)
  val getAdjustment     = endpoint.securityIn(bearer).get.in("adjustments" / path[Long]("id")).out(jsonBody[ProposalResponse]).errorOut(errOut)
  val approveAdjustment = endpoint.securityIn(bearer).post.in("adjustments" / path[Long]("id") / "approve").in(jsonBody[DecisionRequest]).out(jsonBody[ProposalResponse]).errorOut(errOut)
  val rejectAdjustment  = endpoint.securityIn(bearer).post.in("adjustments" / path[Long]("id") / "reject").in(jsonBody[DecisionRequest]).out(jsonBody[ProposalResponse]).errorOut(errOut)
  val proposeReversal   = endpoint.securityIn(bearer).post.in("rollback-reversals").in(jsonBody[ReversalProposeBody]).out(jsonBody[ProposalResponse]).errorOut(errOut)
  val approveReversal   = endpoint.securityIn(bearer).post.in("rollback-reversals" / path[Long]("id") / "approve").in(jsonBody[DecisionRequest]).out(jsonBody[ProposalResponse]).errorOut(errOut)
  val rejectReversal    = endpoint.securityIn(bearer).post.in("rollback-reversals" / path[Long]("id") / "reject").in(jsonBody[DecisionRequest]).out(jsonBody[ProposalResponse]).errorOut(errOut)
  val getTx           = endpoint.securityIn(bearer).get.in("ledger" / "transactions" / path[Long]("id")).out(jsonBody[TxResponse]).errorOut(errOut)
  val listTxs         = endpoint.securityIn(bearer).get.in("ledger" / "transactions").in(query[Option[Int]]("limit")).in(query[Option[Long]]("before")).out(jsonBody[List[TxResponse]]).errorOut(errOut)
  val accountBalance  = endpoint.securityIn(bearer).get.in("ledger" / "accounts" / path[String]("account") / "balance").out(jsonBody[BalanceResponse]).errorOut(errOut)
  val userBalance     = endpoint.securityIn(bearer).get.in("ledger" / "users" / path[String]("uid") / "balance").out(jsonBody[BalanceResponse]).errorOut(errOut)
  val myBalance       = endpoint.securityIn(bearer).get.in("me" / "balance").out(jsonBody[BalanceResponse]).errorOut(errOut)
  val summary         = endpoint.securityIn(bearer).get.in("ledger" / "summary").out(jsonBody[SummaryResponse]).errorOut(errOut)

  // withdrawals
  val getPayoutIntent  = endpoint.securityIn(bearer).get.in("withdrawals" / path[Long]("id") / "payout-intent").out(jsonBody[PayoutIntentResponse]).errorOut(errOut)
  val getPayoutReconciliation = endpoint.securityIn(bearer).get.in("withdrawals" / path[Long]("id") / "reconciliation").out(jsonBody[PayoutReconciliationResponse]).errorOut(errOut)
  val listProviderEvents = endpoint.securityIn(bearer).get.in("withdrawals" / path[Long]("id") / "provider-events").out(jsonBody[List[ProviderEventResponse]]).errorOut(errOut)
  val requestWithdrawal = endpoint.securityIn(bearer).post.in("me" / "withdrawals").in(jsonBody[WithdrawalRequestBody]).out(jsonBody[WithdrawalResponse]).errorOut(errOut)
  val myWithdrawals     = endpoint.securityIn(bearer).get.in("me" / "withdrawals").in(query[Option[Int]]("limit")).in(query[Option[Long]]("before")).out(jsonBody[List[WithdrawalResponse]]).errorOut(errOut)
  val cancelWithdrawal  = endpoint.securityIn(bearer).post.in("me" / "withdrawals" / path[Long]("id") / "cancel").in(jsonBody[DecisionRequest]).out(jsonBody[WithdrawalResponse]).errorOut(errOut)
  val listWithdrawals   = endpoint.securityIn(bearer).get.in("withdrawals").in(query[Option[Int]]("limit")).in(query[Option[Long]]("before")).out(jsonBody[List[WithdrawalResponse]]).errorOut(errOut)
  val approveWithdrawal = endpoint.securityIn(bearer).post.in("withdrawals" / path[Long]("id") / "approve").in(jsonBody[DecisionRequest]).out(jsonBody[WithdrawalResponse]).errorOut(errOut)
  val rejectWithdrawal  = endpoint.securityIn(bearer).post.in("withdrawals" / path[Long]("id") / "reject").in(jsonBody[DecisionRequest]).out(jsonBody[WithdrawalResponse]).errorOut(errOut)
  val settleWithdrawal  = endpoint.securityIn(bearer).post.in("withdrawals" / path[Long]("id") / "settle").in(jsonBody[DecisionRequest]).out(jsonBody[WithdrawalResponse]).errorOut(errOut)
  val submitWithdrawal  = endpoint.securityIn(bearer).post.in("withdrawals" / path[Long]("id") / "submit").in(jsonBody[PayoutSubmitRequest]).out(jsonBody[WithdrawalResponse]).errorOut(errOut)


  // obligations + upcoming-expense forecast
  val openObligation   = endpoint.securityIn(bearer).post.in("obligations" / "open").in(jsonBody[ObligationOpenBody]).out(jsonBody[ObligationResponse]).errorOut(errOut)
  val cancelObligation = endpoint.securityIn(bearer).post.in("obligations" / "cancel").in(jsonBody[ObligationCancelBody]).out(jsonBody[ObligationResponse]).errorOut(errOut)
  val upcomingExpense  = endpoint.securityIn(bearer).get.in("ledger" / "upcoming-expense").out(jsonBody[UpcomingExpenseResponse]).errorOut(errOut)

  // per-user transactions, Stripe payout webhook, risk/config/audit, and invariant runs
  val userTransactions = endpoint.securityIn(bearer).get.in("ledger" / "users" / path[String]("uid") / "transactions").in(query[Option[Int]]("limit")).in(query[Option[Long]]("before")).out(jsonBody[List[TxResponse]]).errorOut(errOut)
  // Public (no bearer): the Stripe-Signature header IS the auth. Raw stringBody is required so the
  // HMAC is computed over the exact bytes Stripe sent (a JSON re-serialize would break the signature).
  val stripeWebhook    = endpoint.post.in("stripe" / "webhook").in(header[String]("Stripe-Signature")).in(stringBody).out(jsonBody[WebhookAck]).errorOut(errOut)
  val riskEvents       = endpoint.securityIn(bearer).get.in("risk" / "events").in(query[Option[Int]]("limit")).in(query[Option[Long]]("before")).out(jsonBody[List[RiskEventResponse]]).errorOut(errOut)
  val auditLog         = endpoint.securityIn(bearer).get.in("audit-log").in(query[Option[Int]]("limit")).in(query[Option[Long]]("before")).out(jsonBody[List[AuditLogResponse]]).errorOut(errOut)
  val configCurrent    = endpoint.securityIn(bearer).get.in("system" / "config").out(jsonBody[List[ConfigEntryResponse]]).errorOut(errOut)
  val proposeConfig    = endpoint.securityIn(bearer).post.in("system" / "config" / "proposals").in(jsonBody[ConfigProposalRequest]).out(jsonBody[ConfigProposalResponse]).errorOut(errOut)
  val approveConfig    = endpoint.securityIn(bearer).post.in("system" / "config" / "proposals" / path[Long]("id") / "approve").out(jsonBody[ConfigProposalResponse]).errorOut(errOut)
  val rejectConfig     = endpoint.securityIn(bearer).post.in("system" / "config" / "proposals" / path[Long]("id") / "reject").out(jsonBody[ConfigProposalResponse]).errorOut(errOut)
  val listConfigProposals = endpoint.securityIn(bearer).get.in("system" / "config" / "proposals").in(query[Option[Int]]("limit")).in(query[Option[Long]]("before")).out(jsonBody[List[ConfigProposalResponse]]).errorOut(errOut)
  val invariantsCheck  = endpoint.securityIn(bearer).post.in("invariants" / "check").out(jsonBody[InvariantRunResponse]).errorOut(errOut)
  val invariantsLatest = endpoint.securityIn(bearer).get.in("invariants" / "latest").out(jsonBody[InvariantRunResponse]).errorOut(errOut)
  val invariantById    = endpoint.securityIn(bearer).get.in("invariants" / path[String]("runId")).out(jsonBody[InvariantRunResponse]).errorOut(errOut)

class LedgerApi:
  import LedgerJson.given
  import LedgerEndpoints.*
  private given MonadError[Identity] = IdentityMonad
  type SE = ServerEndpoint[Any, Identity]
  type Err = (StatusCode, String)

  // The JDBC-backed world: field-less repositories delegate to the ambient `Db`
  // facade (bind it with Db.init(ds) before serving). The service is generated.
  private val world    = World(DB.empty)
  private val ledgerStore = JdbcLedgerStore()
  private val service  = LedgerService[World](HasDb(), ledgerStore)
  private val wservice = WithdrawalService[World](HasDb(), ledgerStore, JdbcWithdrawalStore())
  private val aservice = AdjustmentService[World](HasDb(), ledgerStore, JdbcProposalStore())
  private val oservice = ObligationService[World](HasDb(), JdbcObligationStore())
  private val invariantRuns = ConcurrentHashMap[String, InvariantRunResponse]()
  @volatile private var latestRun: Option[InvariantRunResponse] = None

  // --- backed-token auth (admin = system/service surface; user = own /me surface) ---
  private val tokens = ConcurrentHashMap[String, String]()
  private def mint(p: String): String = { val t = UUID.randomUUID.toString; tokens.put(t, p); t }
  def adminToken(adminId: String): String = mint(s"admin:$adminId")
  def userToken(uid: String): String      = mint(s"user:$uid")
  private val unauthorized: Err = (StatusCode.Unauthorized, "")
  private def resolveAdmin(token: String): Either[Err, String] =
    Option(tokens.get(token)) match
      case Some(p) if p.startsWith("admin:") => Right(p.stripPrefix("admin:"))
      case _                                 => Left(unauthorized)
  private def resolveUser(token: String): Either[Err, String] =
    Option(tokens.get(token)) match
      case Some(p) if p.startsWith("user:") => Right(p.stripPrefix("user:"))
      case _                                => Left(unauthorized)

  private def txResponse(tx: LedgerTx): TxResponse =
    TxResponse(tx.id, tx.kind.toString, tx.debitAccount, tx.creditAccount, tx.amount, tx.userUid, tx.sourceKind, tx.sourceId,
      tx.entries.map(e => TxEntryResponse(e.account, e.direction.toString, e.amount)))

  private def withdrawalResponse(wd: Withdrawal): WithdrawalResponse =
    WithdrawalResponse(wd.id, wd.userUid, wd.amount, wd.status.toString, wd.clientRequestId, wd.reserveTxId)
  private def userAcctOf(id: Long): String =
    Db.withdrawalById(id).map(w => Accounts.user(w.userUid)).getOrElse(Accounts.user("unknown"))
  private def wErr(e: LedgerError): Err = e match
    case WithdrawalNotFound => (StatusCode.NotFound, "withdrawal not found")
    case StatusConflict     => (StatusCode.Conflict, "status_conflict")
    case NonPositiveAmount  => (StatusCode.BadRequest, "non_positive_amount")
    case UnbalancedTx       => (StatusCode.BadRequest, "unbalanced_tx")
    case DuplicateTxId     => (StatusCode.Conflict, "duplicate_tx_id")
    case _                  => (StatusCode.BadRequest, "bad request")

  private def parseWStatus(s: String): Option[WithdrawalStatus] = WithdrawalStatus.values.find(_.toString == s)
  private def parsePStatus(s: String): Option[ProposalStatus] = ProposalStatus.values.find(_.toString == s)
  private val statusConflict: Err = (StatusCode.Conflict, "status_conflict")
  // Providers we can actually dispatch to (the gateway speaks Stripe). Keep the set here so adding
  // a rail is a one-line change plus a gateway impl, not a scattered string literal.
  private val supportedProviders = Set("stripe")

  private def proposalResponse(p: Proposal): ProposalResponse =
    ProposalResponse(p.id, p.kind.toString, p.userUid, p.debitAccount, p.creditAccount, p.amount, p.reason, p.proposedBy, p.status.toString, p.resultTxId)
  private def configEntryResponse(c: ConfigEntry): ConfigEntryResponse =
    ConfigEntryResponse(c.key, c.value)
  private def configProposalResponse(c: ConfigProposalRecord): ConfigProposalResponse =
    ConfigProposalResponse(c.id, c.key, c.value, c.reason, c.status, c.proposedBy, c.approvedBy, c.rejectedBy)
  private def payoutIntentResponse(p: PayoutIntentRecord): PayoutIntentResponse =
    PayoutIntentResponse(p.withdrawalId, p.provider, p.routeCode, p.destinationId, p.grossAmount, p.quotedProviderFee, p.expectedRecipientNet, p.quoteRef, p.providerTransferRef)
  private def payoutReconciliationResponse(withdrawalId: Long, r: PayoutReconciliationRecord): PayoutReconciliationResponse =
    PayoutReconciliationResponse(withdrawalId, r.expectedFee, r.observedFee, r.result, r.note)
  private def providerEventResponse(e: ProviderEventRecord): ProviderEventResponse =
    ProviderEventResponse(e.eventId, e.provider, e.providerTransferRef, e.outcome, e.providerFeeDebited)
  private def riskResponse(r: RiskEventRecord): RiskEventResponse =
    RiskEventResponse(r.id, r.kind, r.subject, r.detail)
  private def auditResponse(r: AuditLogRecord): AuditLogResponse =
    AuditLogResponse(r.id, r.action, r.actor, r.subject, r.detail)
  private def obligationResponse(o: Obligation): ObligationResponse =
    ObligationResponse(o.sourceKind, o.sourceId, o.userUid, o.estimatedUnit, o.status.toString, o.realizedTxId)

  /** Re-validate the ledger invariants at runtime over the persisted state (a production
    * belt-and-suspenders read — the verified core already guarantees them by construction)
    * and record the run so latest/{runId} can return it. */
  private def runInvariants(): InvariantRunResponse =
    val summary = Db.summaryBalances
    val funds = summary.incentiveExpense + summary.providerPayoutFee == summary.users + summary.clearing + summary.settlement + summary.revenue - summary.adjustment
    val negativeUsers = Db.negativeUserLedgerAccounts
    val pendingClearing = Db.pendingWithdrawalClearing
    val clearingMatches = summary.clearing == pendingClearing
    val drifts = Db.balanceDrifts
    val collisions = Db.sourceCollisions
    val rollbackRefsOk = Db.approvedRollbackRefsOk
    val noReversalOfReversal = Db.noReversalOfReversal
    val checks = List(
      InvariantCheckDto("ledger_balanced", Db.ledgerBalanced, None),
      InvariantCheckDto("funds_invariant", funds,
        if funds then None else Some(s"incentiveExpense=${summary.incentiveExpense} providerPayoutFee=${summary.providerPayoutFee} users=${summary.users} clearing=${summary.clearing} settlement=${summary.settlement} revenue=${summary.revenue} adjustment=${summary.adjustment}")),
      InvariantCheckDto("user_balance_nonneg", negativeUsers.isEmpty,
        if negativeUsers.isEmpty then None else Some(negativeUsers.mkString(","))),
      InvariantCheckDto("withdrawal_clearing_matches_inflight", clearingMatches,
        if clearingMatches then None else Some(s"clearing=${summary.clearing} inflight=$pendingClearing")),
      InvariantCheckDto("account_balance_matches_replay", drifts.isEmpty,
        if drifts.isEmpty then None else Some(drifts.map(d => s"${d.account}:${d.materialized}/${d.replayed}").mkString(","))),
      InvariantCheckDto("rollback_references_prior_tx", rollbackRefsOk, None),
      InvariantCheckDto("no_reversal_of_reversal", noReversalOfReversal, None),
      InvariantCheckDto("at_most_one_state", collisions.isEmpty,
        if collisions.isEmpty then None else Some(collisions.map(c => s"${c.sourceKind}:${c.sourceId}").mkString(","))),
    )
    val resp = InvariantRunResponse(UUID.randomUUID.toString, checks.forall(_.passed), checks)
    invariantRuns.put(resp.runId, resp); latestRun = Some(resp)
    if !resp.allPassed then Db.setPayoutsEnabled(false, "invariant failure", "system")
    resp

  private def pErr(e: LedgerError): Err = e match
    case ProposalNotFound   => (StatusCode.NotFound, "proposal not found")
    case TwoPersonViolation => (StatusCode.Conflict, "two_person_violation")
    case StatusConflict     => (StatusCode.Conflict, "status_conflict")
    case AlreadyReversed    => (StatusCode.Conflict, "already_reversed")
    case NonPositiveAmount  => (StatusCode.BadRequest, "non_positive_amount")
    case UnbalancedTx       => (StatusCode.BadRequest, "unbalanced_tx")
    case DuplicateTxId     => (StatusCode.Conflict, "duplicate_tx_id")
    case _                  => (StatusCode.BadRequest, "bad request")
  /** Approve a proposal (adjustment or reversal), guarding a user-debiting approval
    * against driving the balance negative (a production fold, not a verified condition). */
  private def approveProposal(id: Long, expectedStatus: String, admin: String): Either[Err, ProposalResponse] =
    parsePStatus(expectedStatus) match
      case None => Left(statusConflict)
      case Some(es) =>
        // One transaction: the balance read, the FOR-UPDATE lock on the debited user
        // account, the ledger post, and the proposal flip all commit or roll back together.
        Db.transaction {
          Db.proposalById(id) match
            case Some(p) if Accounts.isUser(p.debitAccount) =>
              Db.lockUserBalance(p.debitAccount)
              // read the LOCKED materialized balance, so the lock actually serializes the check
              if Db.balanceOf(p.debitAccount) < p.amount then
                Left((StatusCode.UnprocessableEntity, "would_violate_balance_invariant"))
              else approveProposalWrite(id, es, admin, p)
            case Some(p) => approveProposalWrite(id, es, admin, p)
            case None    => Left(pErr(ProposalNotFound)) // no such proposal — don't burn an id on a guaranteed miss
        }

  private def approveProposalWrite(id: Long, es: ProposalStatus, admin: String, p: Proposal): Either[Err, ProposalResponse] =
    val txId = Db.nextId()
    Db.asActor(admin) {
      Db.withTxMeta(TxWriteMeta(targetTxId = p.targetTxId, proposalId = Some(id), rawInput = Some(s"proposal=$id;reason=${p.reason}"))) {
        aservice.approve(world, id, es, admin, txId)._2
      }
    } match
      case Right(p2) =>
        Db.audit("proposal.approve", admin, id.toString, p2.kind.toString)
        Right(proposalResponse(p2))
      case Left(e)  => Left(pErr(e))

  private def ledgerErr(e: LedgerError): Err = e match
    case NonPositiveAmount => (StatusCode.BadRequest, "non_positive_amount")
    case UnbalancedTx      => (StatusCode.BadRequest, "unbalanced_tx")
    case DuplicateSource   => (StatusCode.Conflict, "duplicate_source")
    case DuplicateTxId    => (StatusCode.Conflict, "duplicate_tx_id")
    case _                 => (StatusCode.BadRequest, "bad request")

  private def postLedgerTx(kind: TxKind, userUid: String, entries: List[LedgerEntry], meta: TxWriteMeta = TxWriteMeta()): Either[Err, LedgerTx] =
    Db.withTxMeta(meta) {
      service.post(world, LedgerTx(Db.nextId(), kind, entries, None, None, userUid))._2
    } match
      case Right(tx) => Right(tx)
      case Left(e)   => Left(ledgerErr(e))


  private def payoutIntentMatches(wd: Withdrawal, req: PayoutSubmitRequest, intent: PayoutIntentRecord): Boolean =
    intent.withdrawalId == wd.id &&
      intent.userUid == wd.userUid &&
      intent.provider == req.provider &&
      intent.routeCode == req.routeCode &&
      intent.destinationId == req.destinationId &&
      intent.grossAmount == wd.amount &&
      intent.quotedProviderFee == req.quotedProviderFee &&
      intent.expectedRecipientNet == req.expectedRecipientNet &&
      intent.quoteRef == req.quoteRef

  private def payoutAmountMinor(points: Long): Either[Err, Long] =
    val unit = Db.payoutPointsPerMinorUnit
    if points <= 0 || points % unit != 0 then Left((StatusCode.BadRequest, "bad_payout_unit"))
    else
      val minor = points / unit
      if minor <= 0 then Left((StatusCode.BadRequest, "bad_payout_unit"))
      else Right(minor)
  private def settleRecipientPaysFee(withdrawal: Withdrawal, intent: PayoutIntentRecord, providerEventId: String, providerFeeDebited: Long, providerTransferRef: Option[String], actor: String): Either[Err, WithdrawalResponse] =
    val netEntries = List(
      LedgerEntry(Accounts.WithdrawalClearing, EntryDirection.DR, intent.expectedRecipientNet),
      LedgerEntry(Accounts.providerBalance(intent.provider), EntryDirection.CR, intent.expectedRecipientNet),
    )
    val feeRecoveryEntries = List(
      LedgerEntry(Accounts.WithdrawalClearing, EntryDirection.DR, intent.quotedProviderFee),
      LedgerEntry(Accounts.FeeRecovery, EntryDirection.CR, intent.quotedProviderFee),
    )
    val providerFeeEntries = List(
      LedgerEntry(Accounts.ProviderPayoutFee, EntryDirection.DR, providerFeeDebited),
      LedgerEntry(Accounts.providerBalance(intent.provider), EntryDirection.CR, providerFeeDebited),
    )
    val feeRecoveryPost =
      if intent.quotedProviderFee > 0 then
        postLedgerTx(TxKind.WithdrawalFeeRecovery, withdrawal.userUid, feeRecoveryEntries, TxWriteMeta(withdrawalId = Some(withdrawal.id), payoutIntentId = Some(intent.id), rawInput = Some(s"providerEvent=$providerEventId;quotedFee=${intent.quotedProviderFee}")))
      else Right(LedgerTx(-1L, TxKind.WithdrawalFeeRecovery, Nil, None, None, withdrawal.userUid))
    val providerFeePost =
      if providerFeeDebited > 0 then
        postLedgerTx(TxKind.ProviderPayoutFee, withdrawal.userUid, providerFeeEntries, TxWriteMeta(withdrawalId = Some(withdrawal.id), payoutIntentId = Some(intent.id), rawInput = Some(s"providerEvent=$providerEventId;providerFee=$providerFeeDebited")))
      else Right(LedgerTx(-1L, TxKind.ProviderPayoutFee, Nil, None, None, withdrawal.userUid))
    for
      _ <- postLedgerTx(TxKind.WithdrawalSettle, withdrawal.userUid, netEntries, TxWriteMeta(withdrawalId = Some(withdrawal.id), payoutIntentId = Some(intent.id), rawInput = Some(s"providerEvent=$providerEventId;transferRef=${providerTransferRef.getOrElse("")}")))
      _ <- feeRecoveryPost
      _ <- providerFeePost
      wd <- Db.asActor(actor) {
        wservice.transitionOnly(world, withdrawal.id, WithdrawalStatus.Submitted, WithdrawalStatus.Submitted, WithdrawalStatus.Settled)._2
      } match
        case Right(w) => Right(withdrawalResponse(w))
        case Left(e)  => Left(wErr(e))
    yield
      val result =
        if providerFeeDebited == intent.quotedProviderFee then PayoutReconciliationRecord(intent.id, Some(providerEventId), intent.quotedProviderFee, Some(providerFeeDebited), "matched", None)
        else PayoutReconciliationRecord(intent.id, Some(providerEventId), intent.quotedProviderFee, Some(providerFeeDebited), "fee_variance", Some(s"quoted=${intent.quotedProviderFee} observed=$providerFeeDebited"))
      Db.payoutReconciliationPut(result)
      Db.audit("withdrawal.recipient_pays_fee", actor, withdrawal.id.toString, result.result)
      wd

  // --- Stripe webhook: real signature + event parsing. The accounting it funnels into
  //     (settleRecipientPaysFee / wservice.fail) is the proven path, reused unchanged. ---
  private def jstr(v: ujson.Value, k: String): Option[String] = scala.util.Try(v.obj.get(k).flatMap(_.strOpt)).toOption.flatten
  private def jlong(v: ujson.Value, k: String): Option[Long] = scala.util.Try(v.obj.get(k).flatMap(_.numOpt).map(_.toLong)).toOption.flatten
  private def jchild(v: ujson.Value, k: String): Option[ujson.Value] = scala.util.Try(v.obj.get(k)).toOption.flatten

  private def outcomeOf(eventType: String): Option[String] = eventType match
    case "transfer.paid" | "payout.paid"                           => Some("settled")
    case "transfer.failed" | "payout.failed" | "transfer.reversed" => Some("failed")
    case _                                                         => None

  // Observed provider fee: try the object's own fee, then a (possibly expanded) balance_transaction
  // fee; absent either, fall back to the quoted fee so the settlement reconciles as "matched". The
  // exact field is rail-dependent (see the integration plan) — this is the one swappable seam.
  // #5: the provider reports its fee in MINOR currency units (e.g. cents); None when the event
  // carries no fee. The caller converts minor -> ledger points before posting/reconciling.
  private def feeMinorFromObject(obj: ujson.Value): Option[Long] =
    jlong(obj, "fee").orElse(jchild(obj, "balance_transaction").flatMap(bt => jlong(bt, "fee")))

  private def resolveWithdrawalId(obj: ujson.Value, transferRef: Option[String]): Option[Long] =
    jchild(obj, "metadata").flatMap(m => jstr(m, "withdrawal_id")).flatMap(s => scala.util.Try(s.toLong).toOption)
      .orElse(transferRef.flatMap(Db.withdrawalIdByProviderTransferRef))

  private def handleStripeWebhook(sigHeader: String, rawBody: String): Either[Err, WebhookAck] =
    val now = System.currentTimeMillis() / 1000L
    StripeSignature.verify(rawBody, sigHeader, Db.stripeWebhookSecret, now) match
      case Left(_) => Left((StatusCode.Unauthorized, "bad_signature"))
      case Right(_) =>
        scala.util.Try(ujson.read(rawBody)).toOption match
          case None => Left((StatusCode.BadRequest, "bad_payload"))
          case Some(json) =>
            val eventId = jstr(json, "id").getOrElse("")
            val eventType = jstr(json, "type").getOrElse("")
            val dataObj = jchild(json, "data").flatMap(d => jchild(d, "object"))
            (eventId, dataObj) match
              case (e, _) if e.isEmpty => Left((StatusCode.BadRequest, "bad_payload"))
              case (_, None)           => Left((StatusCode.BadRequest, "bad_payload"))
              case (_, Some(obj)) =>
                outcomeOf(eventType) match
                  case None => Right(WebhookAck(eventId, handled = false, None, None)) // ack signed-but-unrelated events
                  case Some(outcome) =>
                    val transferRef = jstr(obj, "id")
                    resolveWithdrawalId(obj, transferRef) match
                      case None => Left((StatusCode.NotFound, "withdrawal not resolvable"))
                      case Some(withdrawalId) =>
                        Db.withdrawalById(withdrawalId) match
                          case None => Left((StatusCode.NotFound, "withdrawal not found"))
                          case Some(withdrawal) =>
                            val intentOpt = Db.payoutIntentByWithdrawal(withdrawalId)
                            // #5: convert the provider fee (minor units) to ledger points
                            // (points = minor * pointsPerMinorUnit) before posting/reconciling, so a
                            // non-default ratio never mis-books the fee. No fee on the event -> fall
                            // back to the quoted fee (already in points).
                            val observedFee = feeMinorFromObject(obj) match
                              case Some(minor) => minor * Db.payoutPointsPerMinorUnit
                              case None        => intentOpt.map(_.quotedProviderFee).getOrElse(0L)
                            if observedFee < 0 then Left((StatusCode.BadRequest, "bad_fee"))
                            else processWebhook(withdrawal, intentOpt, eventId, outcome, observedFee, transferRef)

  private def processWebhook(withdrawal: Withdrawal, intentOpt: Option[PayoutIntentRecord], eventId: String, outcome: String, observedFee: Long, transferRef: Option[String]): Either[Err, WebhookAck] =
    try
      val statusStr = Db.transaction {
        try Db.insertProviderEvent("stripe", eventId, withdrawal.id, transferRef, outcome, Some(observedFee), s"eventId=$eventId;outcome=$outcome;fee=$observedFee")
        catch case e: com.augustnagro.magnum.SqlException if isUniqueViolation(e) => throw DuplicateEvent()
        val res: Either[Err, WithdrawalResponse] =
          if outcome == "settled" then
            intentOpt match
              case Some(intent) => settleRecipientPaysFee(withdrawal, intent, eventId, observedFee, transferRef, "stripe")
              case None =>
                Db.asActor("stripe") {
                  Db.withTxMeta(TxWriteMeta(withdrawalId = Some(withdrawal.id), rawInput = Some(s"stripeEvent=$eventId"))) {
                    wservice.settle(world, withdrawal.id, WithdrawalStatus.Submitted, Db.nextId(), Accounts.WithdrawalClearing, Accounts.Cash)._2
                  }
                } match
                  case Right(w) => Db.audit("withdrawal.webhook", "stripe", withdrawal.id.toString, eventId); Right(withdrawalResponse(w))
                  case Left(e)  => Left(wErr(e))
          else
            Db.asActor("stripe") {
              Db.withTxMeta(TxWriteMeta(withdrawalId = Some(withdrawal.id), rawInput = Some(s"stripeEvent=$eventId"))) {
                wservice.fail(world, withdrawal.id, WithdrawalStatus.Submitted, Db.nextId(), Accounts.WithdrawalClearing, userAcctOf(withdrawal.id))._2
              }
            } match
              case Right(w) => Db.audit("withdrawal.webhook", "stripe", withdrawal.id.toString, eventId); Right(withdrawalResponse(w))
              case Left(e)  => Left(wErr(e))
        res match
          case Right(w) => w.status
          case Left(e)  => throw WebhookAbort(e)
      }
      Right(WebhookAck(eventId, handled = true, Some(withdrawal.id), Some(statusStr)))
    catch
      case WebhookAbort(err) => Left(err)
      case _: DuplicateEvent =>
        Db.withdrawalById(withdrawal.id) match
          case Some(w) => Right(WebhookAck(eventId, handled = true, Some(w.id), Some(w.status.toString)))
          case None    => Left((StatusCode.NotFound, "withdrawal not found"))

  /** #3: drive the verified withdrawal-fail (Submitted -> Failed + WithdrawalReturn refund leg) for
    * a permanently-failed dispatch, so the reserve is returned and the withdrawal is never stranded
    * in Submitted. Best-effort: a withdrawal not in Submitted (e.g. already settled by a race) is
    * left as-is by wservice.fail's status guard. Wired into PayoutDispatcher.onPermanentFailure. */
  def failWithdrawalForFailedDispatch(withdrawalId: Long): Unit =
    Db.transaction {
      Db.asActor("system") {
        Db.withTxMeta(TxWriteMeta(withdrawalId = Some(withdrawalId), rawInput = Some("dispatch_permanent_failure"))) {
          wservice.fail(world, withdrawalId, WithdrawalStatus.Submitted, Db.nextId(), Accounts.WithdrawalClearing, userAcctOf(withdrawalId))._2
        }
      }
      ()
    }

  def serverEndpoints: List[SE] = List(
    incentiveCredit.handleSecurity(resolveAdmin).handle(admin => (req: CreditRequest) =>
      Db.txBySource(req.sourceKind, req.sourceId) match
        case Some(tx) => Right(txResponse(tx))
        case None =>
          Db.guardLedgerAmount(req.userUid, req.amountPoints, s"${req.sourceKind}:${req.sourceId}") match
            case Left(_) => Left((StatusCode.UnprocessableEntity, "risk_limit"))
            case Right(_) =>
              // Credit + obligation realize + audit commit as one unit. A concurrent duplicate-source
              // insert (the pre-check missed the racing row) surfaces as a 23505 -> roll back and
              // return the now-committed existing tx idempotently (#P1-b).
              try Db.transaction {
                Db.advisoryXactLock(req.userUid)                  // #P1-a: serialize same-user credits on the growth bucket
                if !Db.withinDayGrowthLimit(req.userUid, req.amountPoints) then Left((StatusCode.UnprocessableEntity, "risk_limit")) // #P1-a: authoritative, race-free re-check under the lock
                else Db.asActor(admin) {
                  Db.withTxMeta(TxWriteMeta(incentiveTraceId = req.incentiveTraceId, incentiveModule = req.incentiveModule, rawInput = Some(s"incentiveTraceId=${req.incentiveTraceId.getOrElse("")};module=${req.incentiveModule.getOrElse("")}"))) {
                    service.credit(world, Accounts.IncentiveExpense, Accounts.user(req.userUid), req.userUid,
                      req.amountPoints, Db.nextId(), req.sourceKind, req.sourceId)._2
                  }
                } match
                  case Right(tx) =>
                    oservice.realize(world, req.sourceKind, req.sourceId, req.userUid, "", "", "", 0L, tx.id)
                    Db.audit("ledger.incentive_credit", admin, s"${req.sourceKind}:${req.sourceId}", s"amount=${req.amountPoints};trace=${req.incentiveTraceId.getOrElse("")}")
                    Right(txResponse(tx))
                  case Left(NonPositiveAmount) => Left((StatusCode.BadRequest, "non_positive_amount"))
                  case Left(DuplicateSource)   =>
                    Db.txBySource(req.sourceKind, req.sourceId) match
                      case Some(tx) => Right(txResponse(tx))
                      case None     => Left((StatusCode.Conflict, "duplicate_source"))
                  case Left(_)                 => Left((StatusCode.BadRequest, "bad request"))
              }
              catch case e: com.augustnagro.magnum.SqlException if isUniqueViolation(e) =>
                Db.txBySource(req.sourceKind, req.sourceId) match
                  case Some(tx) => Right(txResponse(tx))
                  case None     => Left((StatusCode.Conflict, "duplicate_source"))),


    proposeAdjustment.handleSecurity(resolveAdmin).handle(admin => (req: AdjustProposeBody) =>
      val (dr, cr) =
        if req.direction == AdjustDirection.CREDIT then (Accounts.Adjustment, Accounts.user(req.userUid))
        else (Accounts.user(req.userUid), Accounts.Adjustment)
      Db.transaction {
        Db.asActor(admin) {
          aservice.propose(world, TxKind.ManualAdjustment, req.userUid, dr, cr, req.amountPoints, req.reason, admin, Db.nextId(), None)._2
        } match
          case Right(p) => Db.audit("proposal.adjustment.propose", admin, p.id.toString, req.reason); Right(proposalResponse(p))
          case Left(e)  => Left(pErr(e))
      }),

    listAdjustments.handleSecurity(resolveAdmin).handle(_ => (q: (Option[Int], Option[Long])) =>
      Right(Db.proposalsByKindPage(TxKind.ManualAdjustment, Paging.limitOf(q._1), q._2).map(proposalResponse))),

    getAdjustment.handleSecurity(resolveAdmin).handle(_ => (id: Long) =>
      aservice.get(world, id) match
        case Right(p) => Right(proposalResponse(p))
        case Left(e)  => Left(pErr(e))),

    approveAdjustment.handleSecurity(resolveAdmin).handle(admin => (in: (Long, DecisionRequest)) =>
      approveProposal(in._1, in._2.expectedStatus, admin)),

    rejectAdjustment.handleSecurity(resolveAdmin).handle(admin => (in: (Long, DecisionRequest)) =>
      parsePStatus(in._2.expectedStatus) match
        case None     => Left(statusConflict)
        case Some(es) =>
          Db.transaction {
            Db.asActor(admin) { aservice.reject(world, in._1, es)._2 } match
              case Right(p) => Db.audit("proposal.adjustment.reject", admin, in._1.toString, ""); Right(proposalResponse(p))
              case Left(e)  => Left(pErr(e))
          }),

    proposeReversal.handleSecurity(resolveAdmin).handle(admin => (req: ReversalProposeBody) =>
      Db.txById(req.targetTxId) match
        case None => Left((StatusCode.NotFound, "target transaction not found"))
        case Some(t) if t.kind == TxKind.RollbackReversal => Left((StatusCode.Conflict, "cannot reverse a reversal"))
        case Some(t) if t.entries.size != 2 => Left((StatusCode.Conflict, "multi_entry_reversal_unsupported"))
        case Some(t) =>
          Db.transaction {
            Db.lockTransaction(req.targetTxId)
            Db.asActor(admin) {
              aservice.propose(world, TxKind.RollbackReversal, t.userUid, t.creditAccount, t.debitAccount, t.amount, req.reason, admin, Db.nextId(), Some(req.targetTxId))._2
            } match
              case Right(p) => Db.audit("proposal.reversal.propose", admin, p.id.toString, req.reason); Right(proposalResponse(p))
              case Left(e)  => Left(pErr(e))
          }),

    approveReversal.handleSecurity(resolveAdmin).handle(admin => (in: (Long, DecisionRequest)) =>
      approveProposal(in._1, in._2.expectedStatus, admin)),

    rejectReversal.handleSecurity(resolveAdmin).handle(admin => (in: (Long, DecisionRequest)) =>
      parsePStatus(in._2.expectedStatus) match
        case None     => Left(statusConflict)
        case Some(es) =>
          Db.transaction {
            Db.asActor(admin) { aservice.reject(world, in._1, es)._2 } match
              case Right(p) => Db.audit("proposal.reversal.reject", admin, in._1.toString, ""); Right(proposalResponse(p))
              case Left(e)  => Left(pErr(e))
          }),

    getTx.handleSecurity(resolveAdmin).handle(_ => (id: Long) =>
      service.get(world, id) match
        case Right(tx) => Right(txResponse(tx))
        case Left(_)   => Left((StatusCode.NotFound, "transaction not found"))),

    listTxs.handleSecurity(resolveAdmin).handle(_ => (q: (Option[Int], Option[Long])) =>
      Right(Db.txPage(Paging.limitOf(q._1), q._2).map(txResponse))),

    accountBalance.handleSecurity(resolveAdmin).handle(_ => (account: String) =>
      Right(BalanceResponse(account, Db.balanceOf(account), Db.accountNormalSide(account)))),

    userBalance.handleSecurity(resolveAdmin).handle(_ => (uid: String) =>
      val a = Accounts.user(uid); Right(BalanceResponse(a, Db.balanceOf(a), Db.accountNormalSide(a)))),

    myBalance.handleSecurity(resolveUser).handle(uid => (_: Unit) =>
      val a = Accounts.user(uid); Right(BalanceResponse(a, Db.balanceOf(a), Db.accountNormalSide(a)))),

    summary.handleSecurity(resolveAdmin).handle(_ => (_: Unit) =>
      val s = Db.summaryBalances
      Right(SummaryResponse(s.incentiveExpense, s.users, s.clearing, s.settlement, s.adjustment, s.revenue, s.providerPayoutFee,
        fundsInvariantHolds = s.incentiveExpense + s.providerPayoutFee == s.users + s.clearing + s.settlement + s.revenue - s.adjustment))),

    requestWithdrawal.handleSecurity(resolveUser).handle(uid => (req: WithdrawalRequestBody) =>
      Db.withdrawalByClientReq(uid, req.clientRequestId) match
        case Some(wd) => Right(withdrawalResponse(wd))
        case None =>
          if Db.balanceOf(Accounts.user(uid)) < req.amountPoints then Left((StatusCode.BadRequest, "insufficient_balance"))
          else
            Db.checkWithdrawalRisk(uid, req.amountPoints) match
              case Left("payouts_disabled") => Left((StatusCode.ServiceUnavailable, "payouts_disabled"))
              case Left(_)                  => Left((StatusCode.UnprocessableEntity, "risk_limit"))
              case Right(_) =>
                // A concurrent duplicate clientRequestId surfaces as a 23505 -> roll back and return
                // the now-committed existing withdrawal idempotently (#P1-b).
                try Db.transaction {
                  Db.advisoryXactLock(uid)                                    // #P1-a: serialize same-user reservations on the limit bucket
                  Db.lockUserBalance(Accounts.user(uid))
                  if Db.balanceOf(Accounts.user(uid)) < req.amountPoints then Left((StatusCode.BadRequest, "insufficient_balance"))
                  else if !Db.withinPayoutDayLimits(uid, req.amountPoints) then Left((StatusCode.UnprocessableEntity, "risk_limit")) // #P1-a: authoritative, race-free re-check under the lock
                  else
                    val wid = Db.nextId()
                    val reserveTxId = Db.nextId()
                    Db.asActor(uid) {
                      Db.withTxMeta(TxWriteMeta(withdrawalId = Some(wid), rawInput = Some(s"clientRequestId=${req.clientRequestId}"))) {
                        wservice.request(world, uid, req.amountPoints, req.clientRequestId, wid, reserveTxId,
                          Accounts.user(uid), Accounts.WithdrawalClearing)._2
                      }
                    } match
                      case Right(wd) => Db.audit("withdrawal.request", uid, wid.toString, req.amountPoints.toString); Right(withdrawalResponse(wd))
                      case Left(e)   => Left(wErr(e))
                }
                catch case e: com.augustnagro.magnum.SqlException if isUniqueViolation(e) =>
                  Db.withdrawalByClientReq(uid, req.clientRequestId) match
                    case Some(wd) => Right(withdrawalResponse(wd))
                    case None     => Left((StatusCode.Conflict, "duplicate_client_request"))),

    myWithdrawals.handleSecurity(resolveUser).handle(uid => (q: (Option[Int], Option[Long])) =>
      Right(Db.withdrawalsByUserPage(uid, Paging.limitOf(q._1), q._2).map(withdrawalResponse))),

    cancelWithdrawal.handleSecurity(resolveUser).handle(uid => (in: (Long, DecisionRequest)) =>
      Db.withdrawalById(in._1) match
        case Some(wd) if wd.userUid != uid => Left((StatusCode.Forbidden, "forbidden"))
        case _ =>
          parseWStatus(in._2.expectedStatus) match
            case None     => Left(statusConflict)
            case Some(es) =>
              Db.transaction {
                Db.asActor(uid) {
                  Db.withTxMeta(TxWriteMeta(withdrawalId = Some(in._1), rawInput = Some("cancel"))) {
                    wservice.cancel(world, in._1, es, Db.nextId(), Accounts.WithdrawalClearing, userAcctOf(in._1))._2
                  }
                } match
                  case Right(wd) => Db.audit("withdrawal.cancel", uid, in._1.toString, ""); Right(withdrawalResponse(wd))
                  case Left(e)   => Left(wErr(e))
              }),

    listWithdrawals.handleSecurity(resolveAdmin).handle(_ => (q: (Option[Int], Option[Long])) =>
      Right(Db.withdrawalsPage(Paging.limitOf(q._1), q._2).map(withdrawalResponse))),

    getPayoutIntent.handleSecurity(resolveAdmin).handle(_ => (id: Long) =>
      Db.payoutIntentByWithdrawal(id) match
        case Some(p) => Right(payoutIntentResponse(p))
        case None    => Left((StatusCode.NotFound, "payout intent not found"))),

    getPayoutReconciliation.handleSecurity(resolveAdmin).handle(_ => (id: Long) =>
      Db.payoutReconciliationByWithdrawal(id) match
        case Some(r) => Right(payoutReconciliationResponse(id, r))
        case None    => Left((StatusCode.NotFound, "reconciliation not found"))),

    listProviderEvents.handleSecurity(resolveAdmin).handle(_ => (id: Long) =>
      Right(Db.providerEventsByWithdrawal(id).map(providerEventResponse))),

    approveWithdrawal.handleSecurity(resolveAdmin).handle(admin => (in: (Long, DecisionRequest)) =>
      parseWStatus(in._2.expectedStatus) match
        case None     => Left(statusConflict)
        case Some(es) =>
          Db.transaction {
            Db.asActor(admin) { wservice.approve(world, in._1, es)._2 } match
              case Right(wd) => Db.audit("withdrawal.approve", admin, in._1.toString, ""); Right(withdrawalResponse(wd))
              case Left(e)   => Left(wErr(e))
          }),

    submitWithdrawal.handleSecurity(resolveAdmin).handle(admin => (in: (Long, PayoutSubmitRequest)) =>
      parseWStatus(in._2.expectedStatus) match
        case None => Left(statusConflict)
        case Some(es) if !supportedProviders.contains(in._2.provider) => Left((StatusCode.BadRequest, "unsupported_provider"))
        case Some(es) =>
          Db.transaction {
            Db.withdrawalById(in._1) match
              case None => Left((StatusCode.NotFound, "withdrawal not found"))
              case Some(wd) =>
                Db.payoutIntentByWithdrawal(in._1) match
                  case Some(existing) =>
                    if payoutIntentMatches(wd, in._2, existing) then Right(withdrawalResponse(wd))
                    else Left((StatusCode.Conflict, "idempotency_conflict"))
                  case None if in._2.quotedProviderFee < 0 || in._2.expectedRecipientNet <= 0 =>
                    Left((StatusCode.BadRequest, "bad_quote"))
                  case None if wd.amount != in._2.expectedRecipientNet + in._2.quotedProviderFee =>
                    Left((StatusCode.BadRequest, "quote_mismatch"))
                  case None =>
                    payoutAmountMinor(in._2.expectedRecipientNet) match
                      case Left(err) => Left(err)
                      case Right(amountMinor) =>
                        Db.asActor(admin) { wservice.approve(world, in._1, es)._2 } match
                          case Right(w2) =>
                            val intent = PayoutIntentRecord(Db.nextId(), wd.id, wd.userUid, in._2.provider, in._2.routeCode, in._2.destinationId, wd.amount, in._2.quotedProviderFee, in._2.expectedRecipientNet, in._2.quoteRef, in._2.providerTransferRef)
                            Db.payoutIntentPut(intent)
                            // Outbox: the dispatcher transfers the recipient's NET to the provider account.
                            Db.payoutDispatchPut(PayoutDispatchRecord(intent.id, wd.id, intent.provider, intent.destinationId, amountMinor, Db.payoutCurrency, s"wd-${wd.id}-intent-${intent.id}", "pending", 0, None, None))
                            Db.audit("withdrawal.submit", admin, in._1.toString, s"${in._2.provider}:${in._2.routeCode}:${in._2.quotedProviderFee}")
                            Right(withdrawalResponse(w2))
                          case Left(e) => Left(wErr(e))
          }),

    rejectWithdrawal.handleSecurity(resolveAdmin).handle(admin => (in: (Long, DecisionRequest)) =>
      parseWStatus(in._2.expectedStatus) match
        case None     => Left(statusConflict)
        case Some(es) =>
          Db.transaction {
            Db.asActor(admin) {
              Db.withTxMeta(TxWriteMeta(withdrawalId = Some(in._1), rawInput = Some("reject"))) {
                wservice.reject(world, in._1, es, Db.nextId(), Accounts.WithdrawalClearing, userAcctOf(in._1))._2
              }
            } match
              case Right(wd) => Db.audit("withdrawal.reject", admin, in._1.toString, ""); Right(withdrawalResponse(wd))
              case Left(e)   => Left(wErr(e))
          }),

    settleWithdrawal.handleSecurity(resolveAdmin).handle(admin => (in: (Long, DecisionRequest)) =>
      parseWStatus(in._2.expectedStatus) match
        case None     => Left(statusConflict)
        case Some(es) =>
          Db.transaction {
            Db.asActor(admin) {
              Db.withTxMeta(TxWriteMeta(withdrawalId = Some(in._1), rawInput = Some("manual_settle"))) {
                wservice.settle(world, in._1, es, Db.nextId(), Accounts.WithdrawalClearing, Accounts.Cash)._2
              }
            } match
              case Right(wd) => Db.audit("withdrawal.settle", admin, in._1.toString, "manual"); Right(withdrawalResponse(wd))
              case Left(e)   => Left(wErr(e))
          }),


    openObligation.handleSecurity(resolveAdmin).handle(_ => (b: ObligationOpenBody) =>
      oservice.open(world, b.sourceKind, b.sourceId, b.userUid, "", "", "", b.estimatedPoints)._2 match
        case Right(o)             => Right(obligationResponse(o))
        case Left(SourceTerminal) => Left((StatusCode.Conflict, "source_terminal"))
        case Left(_)              => Left((StatusCode.BadRequest, "bad request"))),

    cancelObligation.handleSecurity(resolveAdmin).handle(_ => (b: ObligationCancelBody) =>
      oservice.cancel(world, b.sourceKind, b.sourceId)._2 match
        case Right(o)             => Right(obligationResponse(o))
        case Left(SourceTerminal) => Left((StatusCode.Conflict, "already_realized"))
        case Left(_)              => Left((StatusCode.BadRequest, "bad request"))),

    upcomingExpense.handleSecurity(resolveAdmin).handle(_ => (_: Unit) =>
      val open = Db.allOpenObligations
      Right(UpcomingExpenseResponse(open.size.toLong, open.map(_.estimatedUnit).sum))),

    userTransactions.handleSecurity(resolveAdmin).handle(_ => (in: (String, Option[Int], Option[Long])) =>
      Right(Db.txsForAccountPage(Accounts.user(in._1), Paging.limitOf(in._2), in._3).map(txResponse))),

    stripeWebhook.handle((in: (String, String)) => handleStripeWebhook(in._1, in._2)),

    riskEvents.handleSecurity(resolveAdmin).handle(_ => (q: (Option[Int], Option[Long])) =>
      Right(Db.riskEventsPage(Paging.limitOf(q._1), q._2).map(riskResponse))),

    auditLog.handleSecurity(resolveAdmin).handle(_ => (q: (Option[Int], Option[Long])) =>
      Right(Db.auditLogsPage(Paging.limitOf(q._1), q._2).map(auditResponse))),

    configCurrent.handleSecurity(resolveAdmin).handle(_ => (_: Unit) =>
      Right(Db.configEntries.map(configEntryResponse))),

    proposeConfig.handleSecurity(resolveAdmin).handle(admin => (req: ConfigProposalRequest) =>
      Right(configProposalResponse(Db.proposeConfig(req.key, req.value, req.reason, admin)))),

    approveConfig.handleSecurity(resolveAdmin).handle(admin => (id: Long) =>
      Db.approveConfig(id, admin) match
        case Right(c) => Right(configProposalResponse(c))
        case Left("two_person_violation") => Left((StatusCode.Conflict, "two_person_violation"))
        case Left("status_conflict")      => Left((StatusCode.Conflict, "status_conflict"))
        case Left(_)                      => Left((StatusCode.NotFound, "config proposal not found"))),

    rejectConfig.handleSecurity(resolveAdmin).handle(admin => (id: Long) =>
      Db.rejectConfig(id, admin) match
        case Right(c) => Right(configProposalResponse(c))
        case Left("status_conflict") => Left((StatusCode.Conflict, "status_conflict"))
        case Left(_)                 => Left((StatusCode.NotFound, "config proposal not found"))),

    listConfigProposals.handleSecurity(resolveAdmin).handle(_ => (q: (Option[Int], Option[Long])) =>
      Right(Db.configProposalsPage(Paging.limitOf(q._1), q._2).map(configProposalResponse))),

    invariantsCheck.handleSecurity(resolveAdmin).handle(_ => (_: Unit) => Right(runInvariants())),

    invariantsLatest.handleSecurity(resolveAdmin).handle(_ => (_: Unit) =>
      latestRun match
        case Some(r) => Right(r)
        case _       => Left((StatusCode.NotFound, "no invariant run yet"))),

    invariantById.handleSecurity(resolveAdmin).handle(_ => (runId: String) =>
      Option(invariantRuns.get(runId)) match
        case Some(r) => Right(r)
        case _       => Left((StatusCode.NotFound, "run not found"))),
  )
