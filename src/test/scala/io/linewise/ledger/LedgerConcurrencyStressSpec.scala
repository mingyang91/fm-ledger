package io.linewise.ledger

import java.util.concurrent.{ConcurrentLinkedQueue, CyclicBarrier}
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.CollectionConverters.*

import sttp.model.StatusCode

import io.linewise.ledger.LedgerEndpoints as E

/* =============================================================================
 * CONCURRENCY STRESS INTEGRATION TESTS.
 *
 * These are intentionally heavier than the focused race regressions in
 * LedgerEndpointsSpec. They exercise the same persistence and endpoint paths under
 * wider fan-out so the Scala-side batching / specialized query work does not hide a
 * race that only appears under repeated concurrent access.
 * ========================================================================== */
class LedgerConcurrencyStressSpec extends LedgerHttpSuite {

  private def runConcurrent(workers: Int)(body: Int => Unit): Unit = {
    val errors = new ConcurrentLinkedQueue[Throwable]()
    val threads = (0 until workers).map { i =>
      val t = new Thread(new Runnable {
        override def run(): Unit =
          try body(i)
          catch case t: Throwable => errors.add(t); ()
      })
      t.start()
      t
    }
    threads.foreach(_.join())
    assertEquals(errors.asScala.toList, Nil)
  }

  test("stress: many concurrent duplicate-source credits remain idempotent and balance only lands once") {
    val api = freshApi(); val be = stubOf(api); val admin = api.adminToken("svc")
    val ids = new ConcurrentLinkedQueue[Long]()
    val barrier = new CyclicBarrier(12)

    runConcurrent(12) { _ =>
      barrier.await()
      val res = secure(E.incentiveCredit, be, admin, CreditRequest("u1", 250, "annotation_job", "stress-dup-source"))
      assertEquals(res.code, StatusCode.Ok)
      ids.add(res.body.toOption.get.id)
    }

    assertEquals(ids.asScala.size, 12)
    assertEquals(ids.asScala.toSet.size, 1)
    assertEquals(secure(E.myBalance, be, api.userToken("u1"), ()).body.toOption.get.balancePoints, 250L)
    assert(secure(E.invariantsCheck, be, admin, ()).body.toOption.get.allPassed)
  }

  test("stress: concurrent withdrawals never exceed the configured per-user day limit") {
    val api = freshApi(); val be = stubOf(api); val admin = api.adminToken("svc"); val uTok = api.userToken("u1")
    Db.setSystemConfig("per_user_day_payout_limit_points", "1000", "stress", "stress")
    Db.setSystemConfig("per_user_month_payout_limit_points", "1000", "stress", "stress")
    Db.setSystemConfig("system_day_payout_limit_points", "1000000", "stress", "stress")
    Db.setSystemConfig("single_payout_limit_points", "1000000", "stress", "stress")
    secure(E.incentiveCredit, be, admin, CreditRequest("u1", 5000, "annotation_job", "stress-withdraw-fund"))

    val ok = new AtomicInteger(0)
    val rejected = new AtomicInteger(0)
    val barrier = new CyclicBarrier(8)

    runConcurrent(8) { i =>
      barrier.await()
      val res = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, s"stress-withdraw-$i"))
      res.code match {
        case StatusCode.Ok                  => ok.incrementAndGet()
        case StatusCode.UnprocessableEntity => rejected.incrementAndGet(); assertEquals(res.body.swap.toOption.get._2, "risk_limit")
        case other                          => fail(s"unexpected status: $other body=${res.body}")
      }
    }

    assertEquals(ok.get, 3)
    assertEquals(rejected.get, 5)
    assertEquals(Db.pendingWithdrawalClearing, 900L)
    assertEquals(secure(E.myWithdrawals, be, uTok, ()).body.toOption.get.count(w => w.status == "PendingReview"), 3)
    assert(secure(E.invariantsCheck, be, admin, ()).body.toOption.get.allPassed)
  }

  test("stress: concurrent outbox claimers partition pending payout dispatches without duplicates") {
    val api = freshApi(); val be = stubOf(api); val admin = api.adminToken("svc"); val uTok = api.userToken("u1")
    Db.setSystemConfig("per_user_day_payout_limit_points", "1000000", "stress", "stress")
    Db.setSystemConfig("per_user_month_payout_limit_points", "1000000", "stress", "stress")
    Db.setSystemConfig("system_day_payout_limit_points", "1000000", "stress", "stress")
    Db.setSystemConfig("single_payout_limit_points", "1000000", "stress", "stress")
    secure(E.incentiveCredit, be, admin, CreditRequest("u1", 20000, "annotation_job", "stress-dispatch-fund"))

    val withdrawalIds = (0 until 12).map { i =>
      val wd = secure(E.requestWithdrawal, be, uTok, WithdrawalRequestBody(300, s"stress-dispatch-$i")).body.toOption.get
      secure(E.submitWithdrawal, be, admin, (wd.id, PayoutSubmitRequest("PendingReview", "stripe", "stripe_standard", s"acct-$i", 20, 280, None, None)))
      wd.id
    }

    val claimed = new ConcurrentLinkedQueue[Long]()
    runConcurrent(6) { _ =>
      var keepGoing = true
      while keepGoing do
        Db.claimPendingDispatches(1) match {
          case Nil => keepGoing = false
          case rows => rows.foreach(r => claimed.add(r.payoutIntentId))
        }
    }

    assertEquals(claimed.asScala.size, withdrawalIds.size)
    assertEquals(claimed.asScala.toSet.size, withdrawalIds.size)
    withdrawalIds.foreach(id => assertEquals(Db.dispatchByWithdrawal(id).map(_.status), Some("inflight")))
  }
}
