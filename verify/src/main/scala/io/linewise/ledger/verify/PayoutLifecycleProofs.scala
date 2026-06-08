package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import PayoutProofs._

/* =============================================================================
 * PAYOUT LIFECYCLE PROOFS — VERIFY-ONLY. The production shell parses Stripe JSON,
 * verifies HMAC, and calls the gateway. This model covers the durable state machine
 * around payout intent creation, outbox dispatch, provider-event idempotency, and the
 * recipient-pays-fee settlement decision.
 * ========================================================================== */
object PayoutLifecycleProofs {

  enum DispatchStatus { case Pending, InFlight, Dispatched, Failed }
  enum ProviderOutcome { case Settled, Failed }
  enum ReconciliationResult { case Matched, FeeVariance }
  enum PayoutError { case MissingWithdrawal, MissingIntent, StatusConflict, BadQuote, BadFee, IdempotencyConflict, DuplicateEvent }

  case class PayoutSubmit(
      expectedStatus: WithdrawalStatus,
      provider: String,
      destinationId: String,
      quotedFee: FMLong,
      expectedNet: FMLong,
      amountMinor: FMLong,
      idempotencyKey: String)

  case class PayoutIntent(
      id: FMLong,
      withdrawalId: FMLong,
      userUid: String,
      provider: String,
      destinationId: String,
      gross: FMLong,
      quotedFee: FMLong,
      expectedNet: FMLong,
      providerTransferRef: Option[String])

  case class PayoutDispatch(
      intentId: FMLong,
      withdrawalId: FMLong,
      amountMinor: FMLong,
      idempotencyKey: String,
      status: DispatchStatus,
      attempts: BigInt,
      providerTransferRef: Option[String])

  case class ProviderEvent(
      provider: String,
      eventId: String,
      withdrawalId: FMLong,
      outcome: ProviderOutcome,
      observedFee: FMLong,
      providerTransferRef: Option[String])

  case class PayoutReconciliation(
      intentId: FMLong,
      eventId: String,
      expectedFee: FMLong,
      observedFee: FMLong,
      result: ReconciliationResult)

  case class PayoutWorld(
      withdrawals: List[Withdrawal],
      intents: List[PayoutIntent],
      dispatches: List[PayoutDispatch],
      events: List[ProviderEvent],
      reconciliations: List[PayoutReconciliation],
      ledger: List[LedgerTx])

  private def zero: FMLong = FMLong(BigInt(0))

  def findWithdrawal(rows: List[Withdrawal], id: FMLong): Option[Withdrawal] =
    rows.find((wd: Withdrawal) => wd.id == id)

  def putWithdrawal(rows: List[Withdrawal], wd: Withdrawal): List[Withdrawal] =
    wd :: rows.filter((x: Withdrawal) => x.id != wd.id)

  def findIntentByWithdrawal(rows: List[PayoutIntent], withdrawalId: FMLong): Option[PayoutIntent] =
    rows.find((i: PayoutIntent) => i.withdrawalId == withdrawalId)

  def findIntentById(rows: List[PayoutIntent], id: FMLong): Option[PayoutIntent] =
    rows.find((i: PayoutIntent) => i.id == id)

  def findDispatch(rows: List[PayoutDispatch], intentId: FMLong): Option[PayoutDispatch] =
    rows.find((d: PayoutDispatch) => d.intentId == intentId)

  def putDispatch(rows: List[PayoutDispatch], d: PayoutDispatch): List[PayoutDispatch] =
    d :: rows.filter((x: PayoutDispatch) => x.intentId != d.intentId)

  def hasProviderEvent(rows: List[ProviderEvent], provider: String, eventId: String): Boolean =
    rows.exists((e: ProviderEvent) => e.provider == provider && e.eventId == eventId)

  def intentMatches(wd: Withdrawal, req: PayoutSubmit, intent: PayoutIntent): Boolean =
    intent.withdrawalId == wd.id &&
      intent.userUid == wd.userUid &&
      intent.provider == req.provider &&
      intent.destinationId == req.destinationId &&
      intent.gross == wd.amount &&
      intent.quotedFee == req.quotedFee &&
      intent.expectedNet == req.expectedNet

  def quoteValid(wd: Withdrawal, req: PayoutSubmit): Boolean =
    req.quotedFee >= zero && req.expectedNet > zero && req.amountMinor > zero &&
      wd.amount.value == req.expectedNet.value + req.quotedFee.value

  def validIntentForWithdrawal(intent: PayoutIntent, wd: Withdrawal): Boolean =
    intent.withdrawalId == wd.id && intent.userUid == wd.userUid &&
      intent.gross == wd.amount && intent.quotedFee >= zero && intent.expectedNet > zero &&
      intent.gross.value == intent.expectedNet.value + intent.quotedFee.value

  def validDispatchForIntent(d: PayoutDispatch, i: PayoutIntent): Boolean =
    d.intentId == i.id && d.withdrawalId == i.withdrawalId && d.amountMinor > zero &&
      d.idempotencyKey != ""

  def submit(w: PayoutWorld, withdrawalId: FMLong, req: PayoutSubmit, freshIntentId: FMLong): (PayoutWorld, Either[PayoutError, PayoutIntent]) =
    findWithdrawal(w.withdrawals, withdrawalId) match
      case None() => (w, Left[PayoutError, PayoutIntent](PayoutError.MissingWithdrawal))
      case Some(wd) =>
        findIntentByWithdrawal(w.intents, withdrawalId) match
          case Some(existing) =>
            if intentMatches(wd, req, existing) then (w, Right[PayoutError, PayoutIntent](existing))
            else (w, Left[PayoutError, PayoutIntent](PayoutError.IdempotencyConflict))
          case None() =>
            if !quoteValid(wd, req) then (w, Left[PayoutError, PayoutIntent](PayoutError.BadQuote))
            else if wd.status != req.expectedStatus || wd.status != WithdrawalStatus.PendingReview then
              (w, Left[PayoutError, PayoutIntent](PayoutError.StatusConflict))
            else
              val wd2 = wd.copy(status = WithdrawalStatus.Submitted)
              val intent = PayoutIntent(freshIntentId, wd.id, wd.userUid, req.provider, req.destinationId, wd.amount, req.quotedFee, req.expectedNet, None[String]())
              val dispatch = PayoutDispatch(freshIntentId, wd.id, req.amountMinor, req.idempotencyKey, DispatchStatus.Pending, BigInt(0), None[String]())
              (w.copy(withdrawals = putWithdrawal(w.withdrawals, wd2), intents = intent :: w.intents, dispatches = dispatch :: w.dispatches),
                Right[PayoutError, PayoutIntent](intent))

  def claim(w: PayoutWorld, intentId: FMLong): PayoutWorld =
    findDispatch(w.dispatches, intentId) match
      case Some(d) if d.status == DispatchStatus.Pending =>
        w.copy(dispatches = putDispatch(w.dispatches, d.copy(status = DispatchStatus.InFlight)))
      case _ => w

  def dispatchSuccess(w: PayoutWorld, intentId: FMLong, transferRef: String): PayoutWorld =
    findDispatch(w.dispatches, intentId) match
      case Some(d) if d.status == DispatchStatus.InFlight =>
        w.copy(dispatches = putDispatch(w.dispatches, d.copy(status = DispatchStatus.Dispatched, attempts = d.attempts + BigInt(1), providerTransferRef = Some[String](transferRef))))
      case _ => w

  def dispatchFailure(w: PayoutWorld, intentId: FMLong, retryable: Boolean, maxAttempts: BigInt): PayoutWorld =
    findDispatch(w.dispatches, intentId) match
      case Some(d) if d.status == DispatchStatus.InFlight =>
        val nextAttempts = d.attempts + BigInt(1)
        val nextStatus = if !retryable || nextAttempts >= maxAttempts then DispatchStatus.Failed else DispatchStatus.Pending
        w.copy(dispatches = putDispatch(w.dispatches, d.copy(status = nextStatus, attempts = nextAttempts)))
      case _ => w

  def reconciliationResult(expected: FMLong, observed: FMLong): ReconciliationResult =
    if expected == observed then ReconciliationResult.Matched else ReconciliationResult.FeeVariance

  def processSettled(
      w: PayoutWorld,
      provider: String,
      eventId: String,
      withdrawalId: FMLong,
      observedFee: FMLong,
      transferRef: Option[String],
      ids: SettlementIds,
      accounts: SettlementAccounts): (PayoutWorld, Either[PayoutError, Withdrawal]) =
    if observedFee < zero then (w, Left[PayoutError, Withdrawal](PayoutError.BadFee))
    else if hasProviderEvent(w.events, provider, eventId) then (w, Left[PayoutError, Withdrawal](PayoutError.DuplicateEvent))
    else
      (findWithdrawal(w.withdrawals, withdrawalId), findIntentByWithdrawal(w.intents, withdrawalId)) match
        case (None(), _) => (w, Left[PayoutError, Withdrawal](PayoutError.MissingWithdrawal))
        case (_, None()) => (w, Left[PayoutError, Withdrawal](PayoutError.MissingIntent))
        case (Some(wd), Some(intent)) =>
          if wd.status != WithdrawalStatus.Submitted then (w, Left[PayoutError, Withdrawal](PayoutError.StatusConflict))
          else
            val q = RecipientPaysFeeQuote(intent.gross, intent.expectedNet, intent.quotedFee)
            val settlement = recipientPaysFeeSettlement(ids, wd.userUid, q, observedFee, accounts)
            val event = ProviderEvent(provider, eventId, withdrawalId, ProviderOutcome.Settled, observedFee, transferRef)
            val rec = PayoutReconciliation(intent.id, eventId, intent.quotedFee, observedFee, reconciliationResult(intent.quotedFee, observedFee))
            val wd2 = wd.copy(status = WithdrawalStatus.Settled)
            (w.copy(withdrawals = putWithdrawal(w.withdrawals, wd2), events = event :: w.events,
              reconciliations = rec :: w.reconciliations, ledger = settlementRows(settlement) ++ w.ledger),
              Right[PayoutError, Withdrawal](wd2))

  def submitSuccessCreatesIntentAndDispatch(w: PayoutWorld, wd: Withdrawal, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(wd.status == WithdrawalStatus.PendingReview)
    require(quoteValid(wd, req))
    require(req.expectedStatus == WithdrawalStatus.PendingReview)
    require(req.idempotencyKey != "")
    val w0 = w.copy(withdrawals = wd :: w.withdrawals, intents = Nil[PayoutIntent](), dispatches = Nil[PayoutDispatch]())
    val (w1, res) = submit(w0, wd.id, req, freshIntentId)
    res match
      case Right(intent) =>
        findIntentByWithdrawal(w1.intents, wd.id) == Some[PayoutIntent](intent) &&
          (findDispatch(w1.dispatches, intent.id) match
            case Some(d) => d.status == DispatchStatus.Pending && validDispatchForIntent(d, intent)
            case _       => false)
      case _ => false
  }.holds

  def submitExistingSameIntentIsIdempotent(w: PayoutWorld, withdrawalId: FMLong, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(findWithdrawal(w.withdrawals, withdrawalId) match
      case Some(wd) => findIntentByWithdrawal(w.intents, withdrawalId) match
        case Some(intent) => intentMatches(wd, req, intent)
        case _            => false
      case _ => false)
    submit(w, withdrawalId, req, freshIntentId)._1 == w
  }.holds

  def submitExistingDifferentIntentConflicts(w: PayoutWorld, withdrawalId: FMLong, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(findWithdrawal(w.withdrawals, withdrawalId) match
      case Some(wd) => findIntentByWithdrawal(w.intents, withdrawalId) match
        case Some(intent) => !intentMatches(wd, req, intent)
        case _            => false
      case _ => false)
    submit(w, withdrawalId, req, freshIntentId)._2 == Left[PayoutError, PayoutIntent](PayoutError.IdempotencyConflict)
  }.holds

  def submitWrongStatusLeavesWorldUnchanged(w: PayoutWorld, withdrawalId: FMLong, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(findWithdrawal(w.withdrawals, withdrawalId) match
      case Some(wd) => quoteValid(wd, req) && (wd.status != req.expectedStatus || wd.status != WithdrawalStatus.PendingReview)
      case _        => false)
    require(findIntentByWithdrawal(w.intents, withdrawalId).isEmpty)
    submit(w, withdrawalId, req, freshIntentId) == (w, Left[PayoutError, PayoutIntent](PayoutError.StatusConflict))
  }.holds

  def claimPendingMovesInFlight(w: PayoutWorld, d: PayoutDispatch): Boolean = {
    require(d.status == DispatchStatus.Pending)
    val w0 = w.copy(dispatches = d :: w.dispatches)
    findDispatch(claim(w0, d.intentId).dispatches, d.intentId) match
      case Some(claimed) => claimed.status == DispatchStatus.InFlight
      case _             => false
  }.holds

  def dispatchSuccessStoresTransferRef(w: PayoutWorld, d: PayoutDispatch, transferRef: String): Boolean = {
    require(d.status == DispatchStatus.InFlight)
    val w0 = w.copy(dispatches = d :: w.dispatches)
    findDispatch(dispatchSuccess(w0, d.intentId, transferRef).dispatches, d.intentId) match
      case Some(done) => done.status == DispatchStatus.Dispatched && done.providerTransferRef == Some[String](transferRef)
      case _          => false
  }.holds

  def retryableFailureReturnsPending(w: PayoutWorld, d: PayoutDispatch, maxAttempts: BigInt): Boolean = {
    require(d.status == DispatchStatus.InFlight)
    require(d.attempts + BigInt(1) < maxAttempts)
    val w0 = w.copy(dispatches = d :: w.dispatches)
    findDispatch(dispatchFailure(w0, d.intentId, true, maxAttempts).dispatches, d.intentId) match
      case Some(next) => next.status == DispatchStatus.Pending && next.attempts == d.attempts + BigInt(1)
      case _          => false
  }.holds

  def exhaustedFailureMovesFailed(w: PayoutWorld, d: PayoutDispatch, maxAttempts: BigInt): Boolean = {
    require(d.status == DispatchStatus.InFlight)
    require(d.attempts + BigInt(1) >= maxAttempts)
    val w0 = w.copy(dispatches = d :: w.dispatches)
    findDispatch(dispatchFailure(w0, d.intentId, true, maxAttempts).dispatches, d.intentId) match
      case Some(next) => next.status == DispatchStatus.Failed
      case _          => false
  }.holds

  def duplicateProviderEventLeavesWorldUnchanged(w: PayoutWorld, event: ProviderEvent, ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    require(hasProviderEvent(w.events, event.provider, event.eventId))
    processSettled(w, event.provider, event.eventId, event.withdrawalId, event.observedFee, event.providerTransferRef, ids, accounts)._1 == w
  }.holds

  def negativeObservedFeeRejected(w: PayoutWorld, provider: String, eventId: String, withdrawalId: FMLong, observedFee: FMLong, ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    require(observedFee < zero)
    processSettled(w, provider, eventId, withdrawalId, observedFee, None[String](), ids, accounts) ==
      (w, Left[PayoutError, Withdrawal](PayoutError.BadFee))
  }.holds

  def settledWithIntentWritesReconciliation(w: PayoutWorld, wd: Withdrawal, intent: PayoutIntent, eventId: String, observedFee: FMLong, ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    require(wd.status == WithdrawalStatus.Submitted)
    require(validIntentForWithdrawal(intent, wd))
    require(observedFee >= zero)
    val w0 = w.copy(withdrawals = wd :: w.withdrawals, intents = intent :: w.intents, events = Nil[ProviderEvent](), reconciliations = Nil[PayoutReconciliation]())
    processSettled(w0, intent.provider, eventId, wd.id, observedFee, None[String](), ids, accounts)._1.reconciliations match
      case Cons(rec, _) => rec.intentId == intent.id && rec.eventId == eventId && rec.expectedFee == intent.quotedFee && rec.observedFee == observedFee
      case _            => false
  }.holds
}
