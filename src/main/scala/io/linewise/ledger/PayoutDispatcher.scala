package io.linewise.ledger

/* =============================================================================
 * PAYOUT DISPATCHER — the outbox worker. submitWithdrawal records a PAYOUT_DISPATCH
 * 'pending' row in the same DB transaction as the intent; this loop atomically claims
 * pending rows as 'inflight' before calling the gateway OUTSIDE any DB transaction (never
 * holding a connection across the network). The gateway Idempotency-Key makes retries safe.
 * tick; permanent ones (or exhausted attempts) flip to 'failed' and raise a risk event.
 * ========================================================================== */
final class PayoutDispatcher(gateway: PayoutGateway, onPermanentFailure: Long => Unit = _ => (), batchSize: Int = 20, maxAttempts: Int = 6, reclaimAfterSeconds: Int = 120):

  /** One pass over the pending outbox. Returns how many rows it successfully dispatched.
    * Idempotent and safe to call repeatedly; tests call this directly instead of run(). */
  def tick(): Int =
    Db.reclaimStaleInflight(reclaimAfterSeconds)  // B1: wake rows stranded 'inflight' by a crashed prior tick
    Db.claimPendingDispatches(batchSize).foldLeft(0) { (n, d) =>
      val req = TransferRequest(
        amountMinor = d.amountMinor,
        currency = d.currency,
        destination = d.destinationId,
        idempotencyKey = d.idempotencyKey,
        metadata = Map("withdrawal_id" -> d.withdrawalId.toString, "payout_intent_id" -> d.payoutIntentId.toString),
        transferGroup = Some(s"withdrawal-${d.withdrawalId}"))
      gateway.createTransfer(req) match
        case Right(res) =>
          Db.markDispatched(d.payoutIntentId, res.providerTransferRef)
          Db.audit("payout.dispatch", "system", d.withdrawalId.toString, s"transfer=${res.providerTransferRef}")
          n + 1
        case Left(err) =>
          val giveUp = !err.retryable || d.attempts + 1 >= maxAttempts
          // #3: on a permanent failure, mark the row failed AND fail+refund the withdrawal in ONE
          // transaction, so a Failed dispatch never leaves the withdrawal stranded in Submitted.
          Db.transaction {
            Db.markDispatchFailed(d.payoutIntentId, if giveUp then "failed" else "pending", s"[${err.httpStatus}/${err.code}] ${err.message}")
            if giveUp then onPermanentFailure(d.withdrawalId)
          }
          if giveUp then Db.risk("payout_dispatch_failed", d.withdrawalId.toString, s"${err.code}:${err.message}")
          n
    }

  @volatile private var running = false

  /** Start a daemon loop that ticks every `intervalMillis`. Returns the thread so the caller
    * can keep a handle; failures in a tick are swallowed so the loop survives a transient DB blip. */
  def run(intervalMillis: Long = 2000L): Thread =
    running = true
    val th = Thread(
      () =>
        while running do
          try tick()
          catch case e: Throwable => System.err.println(s"[payout-dispatcher] tick failed: ${e.getMessage}")
          try Thread.sleep(intervalMillis) catch case _: InterruptedException => running = false,
      "payout-dispatcher")
    th.setDaemon(true)
    th.start()
    th

  def stop(): Unit = running = false
