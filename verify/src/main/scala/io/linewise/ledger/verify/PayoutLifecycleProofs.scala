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
 * recipient-pays-fee settlement / failure decisions.
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

  def findProviderEvent(rows: List[ProviderEvent], provider: String, eventId: String): Option[ProviderEvent] =
    rows.find((e: ProviderEvent) => e.provider == provider && e.eventId == eventId)

  def hasProviderEvent(rows: List[ProviderEvent], provider: String, eventId: String): Boolean =
    !findProviderEvent(rows, provider, eventId).isEmpty

  def findProviderEventForWithdrawal(rows: List[ProviderEvent], withdrawalId: FMLong, outcome: ProviderOutcome): Option[ProviderEvent] =
    rows.find((e: ProviderEvent) => e.withdrawalId == withdrawalId && e.outcome == outcome)

  def eventCountForWithdrawal(rows: List[ProviderEvent], withdrawalId: FMLong): BigInt =
    rows match
      case Nil() => BigInt(0)
      case Cons(h, tail) =>
        (if h.withdrawalId == withdrawalId then BigInt(1) else BigInt(0)) + eventCountForWithdrawal(tail, withdrawalId)

  def settledEventCountForWithdrawal(rows: List[ProviderEvent], withdrawalId: FMLong): BigInt =
    rows match
      case Nil() => BigInt(0)
      case Cons(h, tail) =>
        (if h.withdrawalId == withdrawalId && h.outcome == ProviderOutcome.Settled then BigInt(1) else BigInt(0)) + settledEventCountForWithdrawal(tail, withdrawalId)

  def failedEventCountForWithdrawal(rows: List[ProviderEvent], withdrawalId: FMLong): BigInt =
    rows match
      case Nil() => BigInt(0)
      case Cons(h, tail) =>
        (if h.withdrawalId == withdrawalId && h.outcome == ProviderOutcome.Failed then BigInt(1) else BigInt(0)) + failedEventCountForWithdrawal(tail, withdrawalId)

  def reconciliationCountForIntent(rows: List[PayoutReconciliation], intentId: FMLong): BigInt =
    rows match
      case Nil() => BigInt(0)
      case Cons(h, tail) =>
        (if h.intentId == intentId then BigInt(1) else BigInt(0)) + reconciliationCountForIntent(tail, intentId)

  def containsIntentByWithdrawal(rows: List[PayoutIntent], withdrawalId: FMLong): Boolean =
    rows.exists((i: PayoutIntent) => i.withdrawalId == withdrawalId)

  def uniqueIntentByWithdrawal(rows: List[PayoutIntent]): Boolean =
    rows match
      case Nil() => true
      case Cons(h, tail) => !containsIntentByWithdrawal(tail, h.withdrawalId) && uniqueIntentByWithdrawal(tail)

  def containsIntentId(rows: List[PayoutIntent], id: FMLong): Boolean =
    rows.exists((i: PayoutIntent) => i.id == id)

  def containsDispatchId(rows: List[PayoutDispatch], intentId: FMLong): Boolean =
    rows.exists((d: PayoutDispatch) => d.intentId == intentId)

  def uniqueDispatchByIntent(rows: List[PayoutDispatch]): Boolean =
    rows match
      case Nil() => true
      case Cons(h, tail) => !containsDispatchId(tail, h.intentId) && uniqueDispatchByIntent(tail)

  def containsProviderEvent(rows: List[ProviderEvent], provider: String, eventId: String): Boolean =
    rows.exists((e: ProviderEvent) => e.provider == provider && e.eventId == eventId)

  def uniqueProviderEvents(rows: List[ProviderEvent]): Boolean =
    rows match
      case Nil() => true
      case Cons(h, tail) => !containsProviderEvent(tail, h.provider, h.eventId) && uniqueProviderEvents(tail)

  def containsReconciliationEvent(rows: List[PayoutReconciliation], eventId: String): Boolean =
    rows.exists((r: PayoutReconciliation) => r.eventId == eventId)

  def uniqueReconciliations(rows: List[PayoutReconciliation]): Boolean =
    rows match
      case Nil() => true
      case Cons(h, tail) => !containsReconciliationEvent(tail, h.eventId) && uniqueReconciliations(tail)

  def containsEventWithdrawal(rows: List[ProviderEvent], withdrawalId: FMLong): Boolean =
    rows.exists((e: ProviderEvent) => e.withdrawalId == withdrawalId)

  def uniqueEventsByWithdrawal(rows: List[ProviderEvent]): Boolean =
    rows match
      case Nil() => true
      case Cons(h, tail) => !containsEventWithdrawal(tail, h.withdrawalId) && uniqueEventsByWithdrawal(tail)

  def containsReconciliationIntent(rows: List[PayoutReconciliation], intentId: FMLong): Boolean =
    rows.exists((r: PayoutReconciliation) => r.intentId == intentId)

  def uniqueReconciliationsByIntent(rows: List[PayoutReconciliation]): Boolean =
    rows match
      case Nil() => true
      case Cons(h, tail) => !containsReconciliationIntent(tail, h.intentId) && uniqueReconciliationsByIntent(tail)

  def intentMatches(wd: Withdrawal, req: PayoutSubmit, intent: PayoutIntent): Boolean =
    intent.withdrawalId == wd.id &&
      intent.userUid == wd.userUid &&
      intent.provider == req.provider &&
      intent.destinationId == req.destinationId &&
      intent.gross == wd.amount &&
      intent.quotedFee == req.quotedFee &&
      intent.expectedNet == req.expectedNet

  def quoteValid(wd: Withdrawal, req: PayoutSubmit): Boolean =
    req.quotedFee >= zero && req.expectedNet > zero && req.amountMinor > zero && req.idempotencyKey != "" &&
      wd.amount.value == req.expectedNet.value + req.quotedFee.value

  def validIntentForWithdrawal(intent: PayoutIntent, wd: Withdrawal): Boolean =
    intent.withdrawalId == wd.id && intent.userUid == wd.userUid &&
      intent.gross == wd.amount && intent.quotedFee >= zero && intent.expectedNet > zero &&
      intent.gross.value == intent.expectedNet.value + intent.quotedFee.value

  def validDispatchForIntent(d: PayoutDispatch, i: PayoutIntent): Boolean =
    d.intentId == i.id && d.withdrawalId == i.withdrawalId && d.amountMinor > zero &&
      d.idempotencyKey != "" && d.attempts >= BigInt(0)

  def validReconciliationFor(intent: PayoutIntent, event: ProviderEvent, rec: PayoutReconciliation): Boolean =
    rec.intentId == intent.id && rec.eventId == event.eventId &&
      rec.expectedFee == intent.quotedFee && rec.observedFee == event.observedFee

  def intentsReferenceWithdrawals(w: PayoutWorld): Boolean =
    w.intents.forall((i: PayoutIntent) => findWithdrawal(w.withdrawals, i.withdrawalId) match
      case Some(wd) => validIntentForWithdrawal(i, wd)
      case _        => false)

  def dispatchesReferenceIntents(w: PayoutWorld): Boolean =
    w.dispatches.forall((d: PayoutDispatch) => findIntentById(w.intents, d.intentId) match
      case Some(i) => validDispatchForIntent(d, i)
      case _       => false)

  def reconciliationsReferenceIntentAndEvent(w: PayoutWorld): Boolean =
    w.reconciliations.forall((r: PayoutReconciliation) => findIntentById(w.intents, r.intentId) match
      case Some(i) => findProviderEvent(w.events, i.provider, r.eventId) match
        case Some(e) => validReconciliationFor(i, e, r)
        case _       => false
      case _ => false)

  def validPayoutWorld(w: PayoutWorld): Boolean =
    LedgerInvariants.validWithdrawalRows(w.withdrawals) &&
      uniqueIntentByWithdrawal(w.intents) &&
      uniqueDispatchByIntent(w.dispatches) &&
      uniqueProviderEvents(w.events) &&
      uniqueEventsByWithdrawal(w.events) &&
      uniqueReconciliations(w.reconciliations) &&
      uniqueReconciliationsByIntent(w.reconciliations) &&
      intentsReferenceWithdrawals(w) &&
      dispatchesReferenceIntents(w) &&
      reconciliationsReferenceIntentAndEvent(w)

  def submittedCouplingHolds(w: PayoutWorld, withdrawalId: FMLong): Boolean =
    findIntentByWithdrawal(w.intents, withdrawalId).nonEmpty &&
      eventCountForWithdrawal(w.events, withdrawalId) == BigInt(0) &&
      (findIntentByWithdrawal(w.intents, withdrawalId) match
        case Some(intent) => reconciliationCountForIntent(w.reconciliations, intent.id) == BigInt(0)
        case _            => false)

  def settledCouplingHolds(w: PayoutWorld, withdrawalId: FMLong): Boolean =
    findIntentByWithdrawal(w.intents, withdrawalId) match
      case Some(intent) =>
        settledEventCountForWithdrawal(w.events, withdrawalId) == BigInt(1) &&
          failedEventCountForWithdrawal(w.events, withdrawalId) == BigInt(0) &&
          reconciliationCountForIntent(w.reconciliations, intent.id) == BigInt(1)
      case _ => false

  def failedCouplingHolds(w: PayoutWorld, withdrawalId: FMLong): Boolean =
    findIntentByWithdrawal(w.intents, withdrawalId) match
      case Some(intent) =>
        settledEventCountForWithdrawal(w.events, withdrawalId) == BigInt(0) &&
          failedEventCountForWithdrawal(w.events, withdrawalId) == BigInt(1) &&
          reconciliationCountForIntent(w.reconciliations, intent.id) == BigInt(0)
      case _ => false
  def exactSubmitDelta(before: PayoutWorld, after: PayoutWorld, wd: Withdrawal, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    val wd2 = wd.copy(status = WithdrawalStatus.Submitted)
    val intent = PayoutIntent(freshIntentId, wd.id, wd.userUid, req.provider, req.destinationId, wd.amount, req.quotedFee, req.expectedNet, None[String]())
    val dispatch = PayoutDispatch(freshIntentId, wd.id, req.amountMinor, req.idempotencyKey, DispatchStatus.Pending, BigInt(0), None[String]())
    after == before.copy(withdrawals = putWithdrawal(before.withdrawals, wd2), intents = intent :: before.intents, dispatches = dispatch :: before.dispatches)
  }

  def exactClaimDelta(before: PayoutWorld, after: PayoutWorld, d: PayoutDispatch): Boolean =
    after == before.copy(dispatches = putDispatch(before.dispatches, d.copy(status = DispatchStatus.InFlight)))

  def exactDispatchSuccessDelta(before: PayoutWorld, after: PayoutWorld, d: PayoutDispatch, transferRef: String): Boolean =
    after == before.copy(dispatches = putDispatch(before.dispatches, d.copy(status = DispatchStatus.Dispatched, attempts = d.attempts + BigInt(1), providerTransferRef = Some[String](transferRef))))

  def exactDispatchFailureDelta(before: PayoutWorld, after: PayoutWorld, d: PayoutDispatch, retryable: Boolean, maxAttempts: BigInt): Boolean = {
    val nextAttempts = d.attempts + BigInt(1)
    val nextStatus = if !retryable || nextAttempts >= maxAttempts then DispatchStatus.Failed else DispatchStatus.Pending
    after == before.copy(dispatches = putDispatch(before.dispatches, d.copy(status = nextStatus, attempts = nextAttempts)))
  }

  def exactSettledDelta(before: PayoutWorld, after: PayoutWorld, wd: Withdrawal, intent: PayoutIntent, eventId: String, observedFee: FMLong, transferRef: Option[String], ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    val q = RecipientPaysFeeQuote(intent.gross, intent.expectedNet, intent.quotedFee)
    val settlement = recipientPaysFeeSettlement(ids, wd.userUid, q, observedFee, accounts)
    val event = ProviderEvent(intent.provider, eventId, wd.id, ProviderOutcome.Settled, observedFee, transferRef)
    val rec = PayoutReconciliation(intent.id, eventId, intent.quotedFee, observedFee, reconciliationResult(intent.quotedFee, observedFee))
    val wd2 = wd.copy(status = WithdrawalStatus.Settled)
    after == before.copy(withdrawals = putWithdrawal(before.withdrawals, wd2), events = event :: before.events,
      reconciliations = rec :: before.reconciliations, ledger = settlementRows(settlement) ++ before.ledger)
  }

  def exactFailedDelta(before: PayoutWorld, after: PayoutWorld, wd: Withdrawal, provider: String, eventId: String, observedFee: FMLong, transferRef: Option[String], freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    val tx = twoLegTx(freshTxId, TxKind.WithdrawalReturn, clearingAccount, userAccount, wd.amount, None[String](), None[String](), wd.userUid)
    val event = ProviderEvent(provider, eventId, wd.id, ProviderOutcome.Failed, observedFee, transferRef)
    val wd2 = wd.copy(status = WithdrawalStatus.Failed)
    after == before.copy(withdrawals = putWithdrawal(before.withdrawals, wd2), events = event :: before.events, ledger = tx :: before.ledger)
  }

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

  def processFailed(
      w: PayoutWorld,
      provider: String,
      eventId: String,
      withdrawalId: FMLong,
      observedFee: FMLong,
      transferRef: Option[String],
      freshTxId: FMLong,
      clearingAccount: String,
      userAccount: String): (PayoutWorld, Either[PayoutError, Withdrawal]) =
    if observedFee < zero then (w, Left[PayoutError, Withdrawal](PayoutError.BadFee))
    else if hasProviderEvent(w.events, provider, eventId) then (w, Left[PayoutError, Withdrawal](PayoutError.DuplicateEvent))
    else
      findWithdrawal(w.withdrawals, withdrawalId) match
        case None() => (w, Left[PayoutError, Withdrawal](PayoutError.MissingWithdrawal))
        case Some(wd) =>
          if wd.status != WithdrawalStatus.Submitted then (w, Left[PayoutError, Withdrawal](PayoutError.StatusConflict))
          else
            val tx = twoLegTx(freshTxId, TxKind.WithdrawalReturn, clearingAccount, userAccount, wd.amount, None[String](), None[String](), wd.userUid)
            val event = ProviderEvent(provider, eventId, withdrawalId, ProviderOutcome.Failed, observedFee, transferRef)
            val wd2 = wd.copy(status = WithdrawalStatus.Failed)
            (w.copy(withdrawals = putWithdrawal(w.withdrawals, wd2), events = event :: w.events, ledger = tx :: w.ledger),
              Right[PayoutError, Withdrawal](wd2))

  def submitMissingWithdrawalLeavesWorldUnchanged(w: PayoutWorld, withdrawalId: FMLong, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(findWithdrawal(w.withdrawals, withdrawalId).isEmpty)
    submit(w, withdrawalId, req, freshIntentId) == (w, Left[PayoutError, PayoutIntent](PayoutError.MissingWithdrawal))
  }.holds

  def submitBadQuoteLeavesWorldUnchanged(w: PayoutWorld, wd: Withdrawal, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(findIntentByWithdrawal(w.intents, wd.id).isEmpty)
    require(!quoteValid(wd, req))
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd))
    submit(w0, wd.id, req, freshIntentId) == (w0, Left[PayoutError, PayoutIntent](PayoutError.BadQuote))
  }.holds

  def submitSuccessCreatesIntentAndDispatch(w: PayoutWorld, wd: Withdrawal, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(wd.status == WithdrawalStatus.PendingReview)
    require(quoteValid(wd, req))
    require(req.expectedStatus == WithdrawalStatus.PendingReview)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd), intents = Nil[PayoutIntent](), dispatches = Nil[PayoutDispatch]())
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

  def submitDifferentPayloadConflictLeavesWorldUnchanged(w: PayoutWorld, withdrawalId: FMLong, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(findWithdrawal(w.withdrawals, withdrawalId) match
      case Some(wd) => findIntentByWithdrawal(w.intents, withdrawalId) match
        case Some(intent) => !intentMatches(wd, req, intent)
        case _            => false
      case _ => false)
    submit(w, withdrawalId, req, freshIntentId)._1 == w
  }.holds

  def submitWrongStatusLeavesWorldUnchanged(w: PayoutWorld, withdrawalId: FMLong, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(findWithdrawal(w.withdrawals, withdrawalId) match
      case Some(wd) => quoteValid(wd, req) && (wd.status != req.expectedStatus || wd.status != WithdrawalStatus.PendingReview)
      case _        => false)
    require(findIntentByWithdrawal(w.intents, withdrawalId).isEmpty)
    submit(w, withdrawalId, req, freshIntentId) == (w, Left[PayoutError, PayoutIntent](PayoutError.StatusConflict))
  }.holds

  def submitSuccessHasExactDelta(w: PayoutWorld, wd: Withdrawal, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(wd.status == WithdrawalStatus.PendingReview)
    require(quoteValid(wd, req))
    require(req.expectedStatus == WithdrawalStatus.PendingReview)
    require(findIntentByWithdrawal(w.intents, wd.id).isEmpty)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd))
    submit(w0, wd.id, req, freshIntentId) match
      case (w1, Right(_)) => exactSubmitDelta(w0, w1, wd, req, freshIntentId)
      case _              => false
  }.holds


  def submitPreservesLedgerRows(w: PayoutWorld, wd: Withdrawal, req: PayoutSubmit, freshIntentId: FMLong): Boolean = {
    require(wd.status == WithdrawalStatus.PendingReview)
    require(quoteValid(wd, req))
    require(req.expectedStatus == WithdrawalStatus.PendingReview)
    require(findIntentByWithdrawal(w.intents, wd.id).isEmpty)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd))
    submit(w0, wd.id, req, freshIntentId)._1.ledger == w0.ledger
  }.holds

  def claimPendingMovesInFlight(w: PayoutWorld, d: PayoutDispatch): Boolean = {
    require(d.status == DispatchStatus.Pending)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    findDispatch(claim(w0, d.intentId).dispatches, d.intentId) match
      case Some(claimed) => claimed.status == DispatchStatus.InFlight
      case _             => false
  }.holds

  def claimNonPendingLeavesWorldUnchanged(w: PayoutWorld, d: PayoutDispatch): Boolean = {
    require(d.status != DispatchStatus.Pending)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    claim(w0, d.intentId) == w0
  }.holds

  def claimPendingHasExactDelta(w: PayoutWorld, d: PayoutDispatch): Boolean = {
    require(d.status == DispatchStatus.Pending)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    exactClaimDelta(w0, claim(w0, d.intentId), d)
  }.holds


  def claimPreservesLedgerRows(w: PayoutWorld, d: PayoutDispatch): Boolean = {
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    claim(w0, d.intentId).ledger == w0.ledger
  }.holds

  def dispatchSuccessStoresTransferRef(w: PayoutWorld, d: PayoutDispatch, transferRef: String): Boolean = {
    require(d.status == DispatchStatus.InFlight)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    findDispatch(dispatchSuccess(w0, d.intentId, transferRef).dispatches, d.intentId) match
      case Some(done) => done.status == DispatchStatus.Dispatched && done.providerTransferRef == Some[String](transferRef)
      case _          => false
  }.holds

  def dispatchSuccessNonInflightLeavesWorldUnchanged(w: PayoutWorld, d: PayoutDispatch, transferRef: String): Boolean = {
    require(d.status != DispatchStatus.InFlight)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    dispatchSuccess(w0, d.intentId, transferRef) == w0
  }.holds

  def dispatchSuccessHasExactDelta(w: PayoutWorld, d: PayoutDispatch, transferRef: String): Boolean = {
    require(d.status == DispatchStatus.InFlight)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    exactDispatchSuccessDelta(w0, dispatchSuccess(w0, d.intentId, transferRef), d, transferRef)
  }.holds


  def retryableFailureReturnsPending(w: PayoutWorld, d: PayoutDispatch, maxAttempts: BigInt): Boolean = {
    require(d.status == DispatchStatus.InFlight)
    require(d.attempts + BigInt(1) < maxAttempts)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    findDispatch(dispatchFailure(w0, d.intentId, true, maxAttempts).dispatches, d.intentId) match
      case Some(next) => next.status == DispatchStatus.Pending && next.attempts == d.attempts + BigInt(1)
      case _          => false
  }.holds

  def exhaustedFailureMovesFailed(w: PayoutWorld, d: PayoutDispatch, maxAttempts: BigInt): Boolean = {
    require(d.status == DispatchStatus.InFlight)
    require(d.attempts + BigInt(1) >= maxAttempts)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    findDispatch(dispatchFailure(w0, d.intentId, true, maxAttempts).dispatches, d.intentId) match
      case Some(next) => next.status == DispatchStatus.Failed
      case _          => false
  }.holds

  def dispatchFailureNonInflightLeavesWorldUnchanged(w: PayoutWorld, d: PayoutDispatch, retryable: Boolean, maxAttempts: BigInt): Boolean = {
    require(d.status != DispatchStatus.InFlight)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    dispatchFailure(w0, d.intentId, retryable, maxAttempts) == w0
  }.holds

  def dispatchRetryableFailureHasExactDelta(w: PayoutWorld, d: PayoutDispatch, maxAttempts: BigInt): Boolean = {
    require(d.status == DispatchStatus.InFlight)
    require(d.attempts + BigInt(1) < maxAttempts)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    exactDispatchFailureDelta(w0, dispatchFailure(w0, d.intentId, true, maxAttempts), d, true, maxAttempts)
  }.holds

  def dispatchPermanentFailureHasExactDelta(w: PayoutWorld, d: PayoutDispatch, maxAttempts: BigInt): Boolean = {
    require(d.status == DispatchStatus.InFlight)
    require(d.attempts + BigInt(1) >= maxAttempts)
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    exactDispatchFailureDelta(w0, dispatchFailure(w0, d.intentId, true, maxAttempts), d, true, maxAttempts)
  }.holds

  def dispatchPreservesLedgerRows(w: PayoutWorld, d: PayoutDispatch, retryable: Boolean, maxAttempts: BigInt): Boolean = {
    val w0 = w.copy(dispatches = putDispatch(w.dispatches, d))
    dispatchFailure(w0, d.intentId, retryable, maxAttempts).ledger == w0.ledger &&
      dispatchSuccess(w0, d.intentId, "tr").ledger == w0.ledger
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

  def settledMissingWithdrawalLeavesWorldUnchanged(w: PayoutWorld, provider: String, eventId: String, withdrawalId: FMLong, observedFee: FMLong, ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    require(observedFee >= zero)
    require(findWithdrawal(w.withdrawals, withdrawalId).isEmpty)
    require(findProviderEvent(w.events, provider, eventId).isEmpty)
    processSettled(w, provider, eventId, withdrawalId, observedFee, None[String](), ids, accounts) ==
      (w, Left[PayoutError, Withdrawal](PayoutError.MissingWithdrawal))
  }.holds

  def settledMissingIntentLeavesWorldUnchanged(w: PayoutWorld, wd: Withdrawal, provider: String, eventId: String, observedFee: FMLong, ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    require(observedFee >= zero)
    require(findIntentByWithdrawal(w.intents, wd.id).isEmpty)
    require(findProviderEvent(w.events, provider, eventId).isEmpty)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd))
    processSettled(w0, provider, eventId, wd.id, observedFee, None[String](), ids, accounts) ==
      (w0, Left[PayoutError, Withdrawal](PayoutError.MissingIntent))
  }.holds

  def settledWrongStatusLeavesWorldUnchanged(w: PayoutWorld, wd: Withdrawal, intent: PayoutIntent, eventId: String, observedFee: FMLong, ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    require(observedFee >= zero)
    require(validIntentForWithdrawal(intent, wd))
    require(wd.status != WithdrawalStatus.Submitted)
    require(findProviderEvent(w.events, intent.provider, eventId).isEmpty)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd), intents = intent :: w.intents)
    processSettled(w0, intent.provider, eventId, wd.id, observedFee, None[String](), ids, accounts) ==
      (w0, Left[PayoutError, Withdrawal](PayoutError.StatusConflict))
  }.holds

  def settledWithIntentWritesReconciliation(w: PayoutWorld, wd: Withdrawal, intent: PayoutIntent, eventId: String, observedFee: FMLong, ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    require(wd.status == WithdrawalStatus.Submitted)
    require(validIntentForWithdrawal(intent, wd))
    require(observedFee >= zero)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd), intents = Cons(intent, Nil[PayoutIntent]()), events = Nil[ProviderEvent](), reconciliations = Nil[PayoutReconciliation]())
    processSettled(w0, intent.provider, eventId, wd.id, observedFee, None[String](), ids, accounts)._1.reconciliations match
      case Cons(rec, _) => rec.intentId == intent.id && rec.eventId == eventId && rec.expectedFee == intent.quotedFee && rec.observedFee == observedFee
      case _            => false
  }.holds

  def settledWithIntentHasExactDelta(w: PayoutWorld, wd: Withdrawal, intent: PayoutIntent, eventId: String, observedFee: FMLong, ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    require(wd.status == WithdrawalStatus.Submitted)
    require(validIntentForWithdrawal(intent, wd))
    require(observedFee >= zero)
    require(findProviderEvent(w.events, intent.provider, eventId).isEmpty)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd), intents = intent :: w.intents)
    processSettled(w0, intent.provider, eventId, wd.id, observedFee, None[String](), ids, accounts) match
      case (w1, Right(_)) => exactSettledDelta(w0, w1, wd, intent, eventId, observedFee, None[String](), ids, accounts)
      case _              => false
  }.holds

  def settledWithIntentCouplesStatus(w: PayoutWorld, wd: Withdrawal, intent: PayoutIntent, eventId: String, observedFee: FMLong, ids: SettlementIds, accounts: SettlementAccounts): Boolean = {
    require(wd.status == WithdrawalStatus.Submitted)
    require(validIntentForWithdrawal(intent, wd))
    require(observedFee >= zero)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd), intents = Cons(intent, Nil[PayoutIntent]()), events = Nil[ProviderEvent](), reconciliations = Nil[PayoutReconciliation]())
    processSettled(w0, intent.provider, eventId, wd.id, observedFee, None[String](), ids, accounts) match
      case (w1, Right(wd2)) => wd2.status == WithdrawalStatus.Settled && settledCouplingHolds(w1, wd.id)
      case _                => false
  }.holds

  def failedMissingWithdrawalLeavesWorldUnchanged(w: PayoutWorld, provider: String, eventId: String, withdrawalId: FMLong, observedFee: FMLong, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    require(observedFee >= zero)
    require(findWithdrawal(w.withdrawals, withdrawalId).isEmpty)
    require(findProviderEvent(w.events, provider, eventId).isEmpty)
    processFailed(w, provider, eventId, withdrawalId, observedFee, None[String](), freshTxId, clearingAccount, userAccount) ==
      (w, Left[PayoutError, Withdrawal](PayoutError.MissingWithdrawal))
  }.holds

  def failedWrongStatusLeavesWorldUnchanged(w: PayoutWorld, wd: Withdrawal, provider: String, eventId: String, observedFee: FMLong, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    require(observedFee >= zero)
    require(wd.status != WithdrawalStatus.Submitted)
    require(findProviderEvent(w.events, provider, eventId).isEmpty)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd))
    processFailed(w0, provider, eventId, wd.id, observedFee, None[String](), freshTxId, clearingAccount, userAccount) ==
      (w0, Left[PayoutError, Withdrawal](PayoutError.StatusConflict))
  }.holds

  def failedWebhookWritesReturnAndNoReconciliation(w: PayoutWorld, wd: Withdrawal, provider: String, eventId: String, observedFee: FMLong, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    require(observedFee >= zero)
    require(wd.status == WithdrawalStatus.Submitted)
    require(findProviderEvent(w.events, provider, eventId).isEmpty)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd), events = Nil[ProviderEvent](), reconciliations = Nil[PayoutReconciliation]())
    processFailed(w0, provider, eventId, wd.id, observedFee, None[String](), freshTxId, clearingAccount, userAccount) match
      case (w1, Right(wd2)) =>
        wd2.status == WithdrawalStatus.Failed &&
          w1.reconciliations == Nil[PayoutReconciliation]() &&
          (w1.ledger match
            case Cons(tx, _) => tx.kind == TxKind.WithdrawalReturn && tx.amount == wd.amount
            case _           => false)
      case _ => false
  }.holds

  def failedWebhookHasExactDelta(w: PayoutWorld, wd: Withdrawal, provider: String, eventId: String, observedFee: FMLong, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    require(observedFee >= zero)
    require(wd.status == WithdrawalStatus.Submitted)
    require(findProviderEvent(w.events, provider, eventId).isEmpty)
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd))
    processFailed(w0, provider, eventId, wd.id, observedFee, None[String](), freshTxId, clearingAccount, userAccount) match
      case (w1, Right(_)) => exactFailedDelta(w0, w1, wd, provider, eventId, observedFee, None[String](), freshTxId, clearingAccount, userAccount)
      case _              => false
  }.holds

  def failedWebhookCouplesStatus(w: PayoutWorld, wd: Withdrawal, provider: String, eventId: String, observedFee: FMLong, freshTxId: FMLong, clearingAccount: String, userAccount: String): Boolean = {
    require(observedFee >= zero)
    require(wd.status == WithdrawalStatus.Submitted)
    val intent = PayoutIntent(FMLong(BigInt(1)), wd.id, wd.userUid, provider, userAccount, wd.amount, zero, wd.amount, None[String]())
    val w0 = w.copy(withdrawals = putWithdrawal(w.withdrawals, wd), intents = Cons(intent, Nil[PayoutIntent]()), events = Nil[ProviderEvent](), reconciliations = Nil[PayoutReconciliation]())
    processFailed(w0, provider, eventId, wd.id, observedFee, None[String](), freshTxId, clearingAccount, userAccount) match
      case (w1, Right(wd2)) => wd2.status == WithdrawalStatus.Failed && failedCouplingHolds(w1, wd.id)
      case _                => false
  }.holds
}
