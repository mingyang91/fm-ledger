package io.linewise.jobfm

import ox.*
import doobie.*
import doobie.implicits.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import java.util.concurrent.atomic.AtomicLong
import io.linewise.jobfm.generated.JobModel.*
import Worker.run

/* =============================================================================
 * DRIVER — seed 5 rows, drive the persisted pipeline through the VERIFIED
 * step(), printing SQL effects + DB row states at each step. Then a claim RACE
 * (two workers, FOR UPDATE SKIP LOCKED, one winner) and a SWEEPER reclaim.
 * ========================================================================== */
object Main:
  def main(args: Array[String]): Unit =
    val xa = Transactor.fromDriverManager[IO](
      driver = "org.h2.Driver",
      url = "jdbc:h2:mem:jobs;DB_CLOSE_DELAY=-1",
      user = "sa",
      password = "",
      logHandler = None
    )

    // a deterministic logical clock (nanos). Advanced explicitly by the demo so
    // lease staleness is reproducible; production uses now() / clock_timestamp().
    val clock = new AtomicLong(1_000_000L)
    def now(): Long = clock.get()
    def tick(by: Long): Unit = { clock.addAndGet(by); () }
    val lease = 1000L // a row whose heartbeat is older than `now-lease` is stale

    def line(s: String): Unit = println(s)
    def rule(): Unit = println("-" * 78)

    def dump(tag: String): Unit =
      val rows = run(Db.dumpAll, xa)
      line(s"  DB[job] state $tag:")
      rows.foreach { (id, kind, st, owner, att, bo) =>
        val o = owner.map(_.toString).getOrElse("-")
        val b = bo.map(Db.event2Str).getOrElse("-")
        line(f"    id=$id%-2d kind=$kind%-9s status=$st%-14s owner=$o%-3s attempts=$att%-2d blocked_on=$b")
      }

    def dumpQueue(tag: String): Unit =
      val rows = run(Db.dumpQueue, xa)
      line(s"  DB[job_queue] shadow $tag (trigger-synced flat mirror):")
      rows.foreach { (id, kind, st, owner) =>
        val o = owner.map(_.toString).getOrElse("-")
        line(f"    id=$id%-2d kind=$kind%-9s status=$st%-14s owner=$o")
      }

    // honest sidecar: every kind succeeds here (Right). Reasons are exercised by
    // the core's own tests; the DB demo focuses on persistence + handoff + race.
    val sidecar: Kind => Either[Reason, Output] = _ => Right(Output("ok"))

    line("=" * 78)
    line("PERSISTED JOB SYSTEM  (real doobie + embedded H2; verified step() unchanged)")
    line("=" * 78)

    // --- DDL ---
    run(Db.ddlJob, xa)
    run(Db.ddlJobQueue, xa)
    line("DDL: created table job (row-as-source-of-truth) + job_queue shadow.")
    line("     SQL: create table job (id BIGINT pk, kind, status, owner, attempts")
    line("          CHECK 0..5, blocked_on, heartbeat) ; create table job_queue(...)")
    rule()

    // --- SEED 5 rows ---
    // id1 Transcode PENDING ; id2 Embedding BLOCKED on VideoMetaDuration ;
    // id3 Masking   BLOCKED on EmbeddingReady ; id4 RagIndex BLOCKED on GcsUri ;
    // id5 Import    PENDING.
    run(Db.seed(1L, JobState(PENDING, None, 0, Transcode, None), now()), xa)
    run(Db.seed(2L, JobState(BLOCKED, None, 0, Embedding, Some(VideoMetaDuration)), now()), xa)
    run(Db.seed(3L, JobState(BLOCKED, None, 0, Masking,   Some(EmbeddingReady)), now()), xa)
    run(Db.seed(4L, JobState(BLOCKED, None, 0, RagIndex,  Some(GcsUri)), now()), xa)
    run(Db.seed(5L, JobState(PENDING, None, 0, Import,    None), now()), xa)
    line("SEED: 5 rows inserted.")
    dump("after seed")
    dumpQueue("after seed")
    rule()

    supervised:
      // === TRANSCODE (id1): claim -> sidecar -> OutDone -> DONE ===
      line("STEP 1  run Transcode (id1)  [worker 11]")
      Worker.runOne(xa, 1L, 11L, lease, now, sidecar, log = line)
      dump("after transcode DONE")

      // === HANDOFF: Transcode DONE produces VideoMetaDuration -> unblock id2 ===
      val ev1 = producedEventOf(fullDag, Transcode).get
      line(s"HANDOFF transcode DONE produces $ev1")
      line(s"  SQL: select id from job where status='BLOCKED' and blocked_on='${Db.event2Str(ev1)}'")
      line( "       update job set status='PENDING', blocked_on=null where id=? and status='BLOCKED'")
      val un1 = run(Db.handoff(ev1), xa)
      un1.foreach((rid, st) => line(f"  UNBLOCK row=$rid%-2d -> ${st.status} (BLOCKED -> PENDING via step UpstreamEvent)"))
      dump("after handoff(VideoMetaDuration)")
      rule()

      // === EMBEDDING (id2): now PENDING, claim -> OutDone -> DONE ===
      line("STEP 2  run Embedding (id2)  [worker 12]")
      Worker.runOne(xa, 2L, 12L, lease, now, sidecar, log = line)
      val ev2 = producedEventOf(fullDag, Embedding).get
      line(s"HANDOFF embedding DONE produces $ev2 -> unblock Masking (id3)")
      val un2 = run(Db.handoff(ev2), xa)
      un2.foreach((rid, st) => line(f"  UNBLOCK row=$rid%-2d -> ${st.status}"))
      dump("after embedding DONE + handoff(EmbeddingReady)")
      rule()

      // === MASKING (id3): now PENDING, claim -> OutDone -> DONE ===
      line("STEP 3  run Masking (id3)  [worker 13]")
      Worker.runOne(xa, 3L, 13L, lease, now, sidecar, log = line)
      dump("after masking DONE")
      rule()

      // === IMPORT (id5) -> RAGINDEX (id4) ===
      line("STEP 4  run Import (id5)  [worker 15]")
      Worker.runOne(xa, 5L, 15L, lease, now, sidecar, log = line)
      val ev3 = producedEventOf(fullDag, Import).get
      line(s"HANDOFF import DONE produces $ev3 -> unblock RagIndex (id4)")
      val un3 = run(Db.handoff(ev3), xa)
      un3.foreach((rid, st) => line(f"  UNBLOCK row=$rid%-2d -> ${st.status}"))
      line("STEP 5  run RagIndex (id4)  [worker 14]")
      Worker.runOne(xa, 4L, 14L, lease, now, sidecar, log = line)
      dump("after import + ragindex DONE")
      dumpQueue("final (shadow mirrors authoritative job)")
      rule()

    // === CLAIM RACE: two workers race the SAME PENDING row ===
    line("=" * 78)
    line("CLAIM RACE  two workers (21, 22) race claim on a fresh PENDING row (id6)")
    line("  via SELECT ... FOR UPDATE SKIP LOCKED + re-validating UPDATE")
    line("=" * 78)
    run(Db.seed(6L, JobState(PENDING, None, 0, Transcode, None), now()), xa)
    dump("before race")

    // run two claims concurrently against the SAME row. Each claim is one tx;
    // FOR UPDATE SKIP LOCKED + the WHERE-revalidating UPDATE guarantee one winner.
    val winners = supervised:
      val f1 = fork { run(Db.claim(6L, 21L, lease, now()), xa).map(_ => 21L) }
      val f2 = fork { run(Db.claim(6L, 22L, lease, now()), xa).map(_ => 22L) }
      List(f1.join(), f2.join()).flatten

    line(s"  claim() returned a winning post-state for workers: ${winners.mkString(", ")}")
    // confirm by READING the DB: exactly one owner, status RUNNING.
    val raceRow = run(Db.loadById(6L), xa).get
    line(f"  DB row id6 after race: status=${raceRow.status} owner=${raceRow.owner.map(_.toString).getOrElse("-")}")
    line(s"  RACE RESULT: ${if winners.size == 1 && raceRow.status == RUNNING then "EXACTLY ONE WINNER (correct)" else "MULTIPLE/zero winners (WRONG)"}")
    rule()

    // === SWEEPER: orphaned BLOCKED -> NOT_APPLICABLE ===
    line("=" * 78)
    line("SWEEPER  reclaim an orphaned BLOCKED row under the Embedding-removed DAG")
    line("=" * 78)
    // seed a row BLOCKED on EmbeddingReady, then sweep under embeddingRemovedDag,
    // where nothing produces EmbeddingReady -> it is orphaned -> NOT_APPLICABLE.
    run(Db.seed(7L, JobState(BLOCKED, None, 0, Masking, Some(EmbeddingReady)), now()), xa)
    line("  seeded id7 Masking BLOCKED on EmbeddingReady")
    line(s"  live DAG = embeddingRemovedDag, produces=${embeddingRemovedDag.produces.map((_, e) => Db.event2Str(e)).mkString(",")}")
    line( "  SQL: select id, blocked_on from job where status='BLOCKED'  (EmbeddingReady has no producer)")
    line( "       update job set status='NOT_APPLICABLE', blocked_on=null where id=? and status='BLOCKED'")
    val swept = run(Db.sweepOrphanedBlocked(embeddingRemovedDag), xa)
    swept.foreach((rid, st) => line(f"  SWEEP row=$rid%-2d -> ${st.status} (BLOCKED -> NOT_APPLICABLE via step SweeperUpstreamFailed)"))
    dump("after sweep")
    rule()

    line("DONE. step() was applied for every transition; the DB layer only did")
    line("      load(point-query) / claim / save / handoff / sweep around it.")
