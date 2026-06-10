package io.linewise.ledger

import java.nio.file.{Files, Paths}
import java.util.UUID

import com.zaxxer.hikari.HikariDataSource
import org.testcontainers.containers.PostgreSQLContainer
import org.testcontainers.utility.DockerImageName

import io.linewise.ledger.generated.LedgerModel.{LedgerTx, LedgerEntry, TxKind, WithdrawalStatus, ProposalStatus, EntryDirection}
import io.linewise.ledger.generated.{Proposal, Withdrawal}
/* =============================================================================
 * LEDGER BENCHMARK / PROFILING HARNESS.
 *
 * Run under the test module so it can reuse Testcontainers and the endpoint test
 * dependencies without shipping benchmark code in production artifacts.
 *
 *   ./mill test.runMain io.linewise.ledger.LedgerBench -- --quick
 *   ./mill test.runMain io.linewise.ledger.LedgerBench -- --users 200 --credits-per-user 20
 *
 * It is designed to be stable enough for comparative profiling sessions under JFR
 * or async-profiler: seed one coherent database state, warm up, then loop the hot
 * Scala-side read and write paths that were optimized in Persistence.scala.
 * ========================================================================== */
object LedgerBench {

  final case class BenchConfig(
      users: Int = 80,
      creditsPerUser: Int = 10,
      pendingWithdrawalsPerUser: Int = 1,
      submittedWithdrawalsPerUser: Int = 1,
      settledWithdrawalsPerUser: Int = 1,
      manualAdjustments: Int = 200,
      warmupIters: Int = 2,
      measureIters: Int = 5,
      writesPerIter: Int = 200,
  )

  private final case class BenchCase(name: String, run: () => Any)
  private final case class BenchStats(name: String, timesMs: Vector[Double])
  private final case class BenchEnv(pg: PostgreSQLContainer[?], ds: HikariDataSource) {
    def close(): Unit = {
      try ds.close() catch case _: Throwable => ()
      try pg.stop() catch case _: Throwable => ()
    }
  }

  private object Blackhole {
    @volatile private var value: Any = ()
    def consume(v: Any): Unit = value = v
  }

  def main(args: Array[String]): Unit = {
    val cfg = parseArgs(args.toList)
    val env = freshEnv(cfg)
    try {
      val targetUid = if cfg.users > 0 then s"u${cfg.users / 2}" else "u0"
      val cases = Vector(
        BenchCase("Db.allTxs", () => Blackhole.consume(Db.allTxs.size)),
        BenchCase("Db.txsForAccount", () => Blackhole.consume(Db.txsForAccount(Accounts.user(targetUid)).size)),
        BenchCase("Db.allWithdrawals", () => Blackhole.consume(Db.allWithdrawals.size)),
        BenchCase("Db.withdrawalsByUser", () => Blackhole.consume(Db.withdrawalsByUser(targetUid).size)),
        BenchCase("Db.allProposals", () => Blackhole.consume(Db.allProposals.size)),
        BenchCase("Db.proposalsByKind", () => Blackhole.consume(Db.proposalsByKind(TxKind.ManualAdjustment).size)),
        BenchCase("Db.summaryBalances", () => Blackhole.consume(Db.summaryBalances)),
        BenchCase("Db.pendingWithdrawalClearing", () => Blackhole.consume(Db.pendingWithdrawalClearing)),
        BenchCase("Db.withinPayoutDayLimits", () => Blackhole.consume(Db.withinPayoutDayLimits(targetUid, 50L))),
        BenchCase("Db.ledgerBalanced", () => Blackhole.consume(Db.ledgerBalanced)),
        BenchCase("Db.balanceDrifts", () => Blackhole.consume(Db.balanceDrifts.size)),
      )

      println(s"Dataset: users=${cfg.users}, txs=${Db.allTxs.size}, withdrawals=${Db.allWithdrawals.size}, proposals=${Db.allProposals.size}")
      cases.foreach(runCase(cfg, _))
      runWriteBench(cfg)
    } finally env.close()
  }

  private def runCase(cfg: BenchConfig, bench: BenchCase): Unit = {
    (0 until cfg.warmupIters).foreach(_ => bench.run())
    val samples = Vector.newBuilder[Double]
    (0 until cfg.measureIters).foreach { _ =>
      val t0 = System.nanoTime()
      bench.run()
      val t1 = System.nanoTime()
      samples += (t1 - t0).toDouble / 1_000_000.0
    }
    printStats(BenchStats(bench.name, samples.result()))
  }

  private def runWriteBench(cfg: BenchConfig): Unit = {
    (0 until cfg.warmupIters).foreach(_ => runWriteBatch(cfg.writesPerIter / 4 max 1))
    val samples = Vector.newBuilder[Double]
    (0 until cfg.measureIters).foreach { _ =>
      val t0 = System.nanoTime()
      runWriteBatch(cfg.writesPerIter)
      val t1 = System.nanoTime()
      samples += (t1 - t0).toDouble / 1_000_000.0
    }
    printStats(BenchStats(s"Db.insertTx x${cfg.writesPerIter}", samples.result()))
  }

  private def runWriteBatch(n: Int): Unit = {
    (0 until n).foreach { i =>
      val userUid = s"bench-write-${i % 16}"
      val tx = LedgerTx(
        Db.nextId(),
        TxKind.ManualAdjustment,
        List(
          LedgerEntry(Accounts.Adjustment, EntryDirection.DR, 10L),
          LedgerEntry(Accounts.user(userUid), EntryDirection.CR, 10L),
        ),
        None,
        None,
        userUid,
      )
      Db.insertTx(tx)
    }
  }

  private def printStats(stats: BenchStats): Unit = {
    val sorted = stats.timesMs.sorted
    def pct(p: Double): Double = sorted(math.min(sorted.size - 1, math.floor((sorted.size - 1) * p).toInt))
    val avg = stats.timesMs.sum / stats.timesMs.size
    val p50 = pct(0.50)
    val p95 = pct(0.95)
    val p99 = pct(0.99)
    val best = sorted.head
    val worst = sorted.last
    println(f"${stats.name}%-28s avg=${avg}%.2f ms  p50=${p50}%.2f ms  p95=${p95}%.2f ms  p99=${p99}%.2f ms  best=${best}%.2f ms  worst=${worst}%.2f ms")
  }

  private def parseArgs(args: List[String]): BenchConfig = {
    def nextInt(xs: List[String], key: String): (Int, List[String]) = xs match {
      case value :: tail => (value.toInt, tail)
      case Nil          => sys.error(s"missing value for $key")
    }
    def loop(cfg: BenchConfig, rest: List[String]): BenchConfig = rest match {
      case Nil => cfg
      case "--quick" :: tail =>
        loop(cfg.copy(users = 16, creditsPerUser = 4, pendingWithdrawalsPerUser = 1, submittedWithdrawalsPerUser = 1, settledWithdrawalsPerUser = 1, manualAdjustments = 32, warmupIters = 1, measureIters = 2, writesPerIter = 40), tail)
      case "--users" :: tail =>
        val (v, rem) = nextInt(tail, "--users")
        loop(cfg.copy(users = v), rem)
      case "--credits-per-user" :: tail =>
        val (v, rem) = nextInt(tail, "--credits-per-user")
        loop(cfg.copy(creditsPerUser = v), rem)
      case "--pending-withdrawals-per-user" :: tail =>
        val (v, rem) = nextInt(tail, "--pending-withdrawals-per-user")
        loop(cfg.copy(pendingWithdrawalsPerUser = v), rem)
      case "--submitted-withdrawals-per-user" :: tail =>
        val (v, rem) = nextInt(tail, "--submitted-withdrawals-per-user")
        loop(cfg.copy(submittedWithdrawalsPerUser = v), rem)
      case "--settled-withdrawals-per-user" :: tail =>
        val (v, rem) = nextInt(tail, "--settled-withdrawals-per-user")
        loop(cfg.copy(settledWithdrawalsPerUser = v), rem)
      case "--manual-adjustments" :: tail =>
        val (v, rem) = nextInt(tail, "--manual-adjustments")
        loop(cfg.copy(manualAdjustments = v), rem)
      case "--warmups" :: tail =>
        val (v, rem) = nextInt(tail, "--warmups")
        loop(cfg.copy(warmupIters = v), rem)
      case "--measures" :: tail =>
        val (v, rem) = nextInt(tail, "--measures")
        loop(cfg.copy(measureIters = v), rem)
      case "--writes-per-iter" :: tail =>
        val (v, rem) = nextInt(tail, "--writes-per-iter")
        loop(cfg.copy(writesPerIter = v), rem)
      case bad :: _ => sys.error(s"unknown arg: $bad")
    }
    loop(BenchConfig(), args.dropWhile(_ == "--"))
  }

  private def freshEnv(cfg: BenchConfig): BenchEnv = {
    configureDocker()
    val pg = PostgreSQLContainer(DockerImageName.parse("postgres:16-alpine"))
    pg.start()
    val schema = s"bench_${UUID.randomUUID().toString.replace("-", "")}"
    val ds = Jdbc.dataSource(pg.getJdbcUrl, pg.getUsername, pg.getPassword, schema) match
      case h: HikariDataSource => h
      case other               => sys.error(s"expected HikariDataSource, got ${other.getClass.getName}")
    val c0 = ds.getConnection()
    try Jdbc.initSchema(c0)
    finally c0.close()
    Db.init(ds)
    seed(cfg)
    BenchEnv(pg, ds)
  }

  private def seed(cfg: BenchConfig): Unit = {
    Db.setSystemConfig("per_user_day_payout_limit_points", "100000000", "bench", "bench")
    Db.setSystemConfig("per_user_month_payout_limit_points", "100000000", "bench", "bench")
    Db.setSystemConfig("system_day_payout_limit_points", "100000000", "bench", "bench")
    Db.setSystemConfig("single_payout_limit_points", "100000000", "bench", "bench")

    val users = (0 until cfg.users).map(i => s"u$i")
    users.foreach { uid =>
      (0 until cfg.creditsPerUser).foreach { i =>
        Db.insertTx(LedgerTx(
          Db.nextId(),
          TxKind.IncentiveCredit,
          List(
            LedgerEntry(Accounts.IncentiveExpense, EntryDirection.DR, 100L),
            LedgerEntry(Accounts.user(uid), EntryDirection.CR, 100L),
          ),
          Some("bench-credit"),
          Some(s"$uid-$i"),
          uid,
        ))
      }
    }

    users.zipWithIndex.foreach { case (uid, userIx) =>
      (0 until cfg.pendingWithdrawalsPerUser).foreach { i =>
        val amount = 40L + (i % 4) * 10L
        val reserveTxId = Db.nextId()
        Db.insertTx(LedgerTx(
          reserveTxId,
          TxKind.WithdrawalReserve,
          List(
            LedgerEntry(Accounts.user(uid), EntryDirection.DR, amount),
            LedgerEntry(Accounts.WithdrawalClearing, EntryDirection.CR, amount),
          ),
          None,
          None,
          uid,
        ))
        Db.withdrawalPut(Withdrawal(Db.nextId(), uid, amount, WithdrawalStatus.PendingReview, s"bench-p-$userIx-$i", reserveTxId))
      }

      (0 until cfg.submittedWithdrawalsPerUser).foreach { i =>
        val amount = 60L + (i % 4) * 10L
        val reserveTxId = Db.nextId()
        Db.insertTx(LedgerTx(
          reserveTxId,
          TxKind.WithdrawalReserve,
          List(
            LedgerEntry(Accounts.user(uid), EntryDirection.DR, amount),
            LedgerEntry(Accounts.WithdrawalClearing, EntryDirection.CR, amount),
          ),
          None,
          None,
          uid,
        ))
        Db.withdrawalPut(Withdrawal(Db.nextId(), uid, amount, WithdrawalStatus.Submitted, s"bench-s-$userIx-$i", reserveTxId))
      }

      (0 until cfg.settledWithdrawalsPerUser).foreach { i =>
        val amount = 80L + (i % 4) * 10L
        val reserveTxId = Db.nextId()
        Db.insertTx(LedgerTx(
          reserveTxId,
          TxKind.WithdrawalReserve,
          List(
            LedgerEntry(Accounts.user(uid), EntryDirection.DR, amount),
            LedgerEntry(Accounts.WithdrawalClearing, EntryDirection.CR, amount),
          ),
          None,
          None,
          uid,
        ))
        Db.insertTx(LedgerTx(
          Db.nextId(),
          TxKind.WithdrawalSettle,
          List(
            LedgerEntry(Accounts.WithdrawalClearing, EntryDirection.DR, amount),
            LedgerEntry(Accounts.Cash, EntryDirection.CR, amount),
          ),
          None,
          None,
          uid,
        ))
        Db.withdrawalPut(Withdrawal(Db.nextId(), uid, amount, WithdrawalStatus.Settled, s"bench-t-$userIx-$i", reserveTxId))
      }
    }

    (0 until cfg.manualAdjustments).foreach { i =>
      val uid = users(i % users.size)
      Db.proposalPut(Proposal(
        Db.nextId(),
        TxKind.ManualAdjustment,
        uid,
        Accounts.Adjustment,
        Accounts.user(uid),
        25L,
        s"bench-adjust-$i",
        "bench",
        if i % 3 == 0 then ProposalStatus.Approved else ProposalStatus.PendingReview,
        None,
        None,
      ))
    }
  }

  private def configureDocker(): Unit = {
    System.setProperty("java.net.useSystemProxies", "false")
    System.clearProperty("socksProxyHost")
    System.clearProperty("socksProxyPort")
    if sys.env.get("DOCKER_API_VERSION").isEmpty && System.getProperty("api.version") == null then
      System.setProperty("api.version", "1.41")
    if System.getProperty("docker.client.strategy") == null then
      System.setProperty("docker.client.strategy", "org.testcontainers.dockerclient.UnixSocketClientProviderStrategy")
    if sys.env.get("DOCKER_HOST").isEmpty && System.getProperty("docker.host") == null then
      val defaultSocket = Paths.get("/var/run/docker.sock")
      val orbStackSocket = Paths.get(sys.props("user.home"), ".orbstack", "run", "docker.sock")
      val chosen =
        if Files.exists(defaultSocket) then defaultSocket
        else if Files.exists(orbStackSocket) then orbStackSocket
        else defaultSocket
      System.setProperty("docker.host", s"unix://$chosen")
  }
}
