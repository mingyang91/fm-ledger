package io.linewise.jobfm

import ox.*
import doobie.*
import cats.effect.IO
import java.util.concurrent.atomic.AtomicLong
import io.linewise.jobfm.generated.JobModel.*
import io.linewise.jobfm.generated.WorkerCore
import io.linewise.jobfm.generated.WorkerModel.World

/* =============================================================================
 * DRIVER — seed 5 rows, drive the persisted pipeline through the VERIFIED
 * step(), printing SQL effects + DB row states at each step. Then a claim RACE
 * (two workers, FOR UPDATE SKIP LOCKED, one winner) and a SWEEPER reclaim.
 *
 * The driver talks to the store through its BARE-VALUE API only — no doobie /
 * ConnectionIO / transact appears here. The transactor is owned by JdbcStore,
 * which confines the IO->value eval boundary.
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
    val store = JdbcStore(xa)

    // a deterministic logical clock (nanos). Advanced explicitly by the demo so
    // lease staleness is reproducible; production uses now() / clock_timestamp().
    val clock = new AtomicLong(1_000_000L)
    def now(): Long = clock.get()
    def tick(by: Long): Unit = { clock.addAndGet(by); () }
    val lease = 1000L // a row whose heartbeat is older than `now-lease` is stale

    def line(s: String): Unit = println(s)
    def rule(): Unit = println("-" * 78)

    def dump(tag: String): Unit =
      val rows = store.dumpAll
      line(s"  DB[job] state $tag:")
      rows.foreach { (id, kind, st, owner, att, bo) =>
        val o = owner.map(_.toString).getOrElse("-")
        val b = bo.map(Db.event2Str).getOrElse("-")
        line(f"    id=$id%-2d kind=$kind%-9s status=$st%-14s owner=$o%-3s attempts=$att%-2d blocked_on=$b")
      }

    def dumpQueue(tag: String): Unit =
      val rows = store.dumpQueue
      line(s"  DB[job_queue] shadow $tag (trigger-synced flat mirror):")
      rows.foreach { (id, kind, st, owner) =>
        val o = owner.map(_.toString).getOrElse("-")
        line(f"    id=$id%-2d kind=$kind%-9s status=$st%-14s owner=$o")
      }

    // honest sidecar: every kind succeeds here (Right). Reasons are exercised by
    // the core's own tests; the DB demo focuses on persistence + handoff + race.
    val sidecar: Kind => Either[Reason, Output] = _ => Right(Output("ok"))
    // adapter feeding the GENERATED worker its sidecar capability (Kind => Event).
    def sidecarEvent(worker: Long): Kind => Event =
      (k: Kind) => Worker.outcomeToEvent(worker, k, sidecar(k))

    line("=" * 78)
    line("PERSISTED JOB SYSTEM  (real doobie + embedded H2; verified step() unchanged)")
    line("=" * 78)

    // --- DDL ---
    store.initSchema()
    line("DDL: created table job (row-as-source-of-truth) + job_queue shadow.")
    line("     SQL: create table job (id BIGINT pk, kind, status, owner, attempts")
    line("          CHECK 0..5, blocked_on, heartbeat) ; create table job_queue(...)")
    rule()

    // --- SEED 5 rows ---
    // id1 Transcode PENDING ; id2 Embedding BLOCKED on VideoMetaDuration ;
    // id3 Masking   BLOCKED on EmbeddingReady ; id4 RagIndex BLOCKED on GcsUri ;
    // id5 Import    PENDING.
    store.seed(1L, JobState(PENDING, None, 0, Transcode, None), now())
    store.seed(2L, JobState(BLOCKED, None, 0, Embedding, Some(VideoMetaDuration)), now())
    store.seed(3L, JobState(BLOCKED, None, 0, Masking,   Some(EmbeddingReady)), now())
    store.seed(4L, JobState(BLOCKED, None, 0, RagIndex,  Some(GcsUri)), now())
    store.seed(5L, JobState(PENDING, None, 0, Import,    None), now())
    line("SEED: 5 rows inserted.")
    dump("after seed")
    dumpQueue("after seed")
    rule()

    supervised:
      // the GENERATED worker logic (generated.WorkerCore.runOne) runs against the
      // doobie-backed production World; the heartbeat daemon is forked inside
      // World.claim on this ambient Ox scope.
      val world = World(xa)
      // === TRANSCODE (id1): claim -> sidecar -> OutDone -> DONE ===
      line("STEP 1  run Transcode (id1)  [worker 11]")
      WorkerCore.runOne(world, 1L, 11L, sidecarEvent(11L))
      dump("after transcode DONE")

      // === HANDOFF: Transcode DONE produces VideoMetaDuration -> unblock id2 ===
      val ev1 = producedEventOf(fullDag, Transcode).get
      line(s"HANDOFF transcode DONE produces $ev1")
      line(s"  SQL: select id from job where status='BLOCKED' and blocked_on='${Db.event2Str(ev1)}'")
      line( "       update job set status='PENDING', blocked_on=null where id=? and status='BLOCKED'")
      val un1 = store.handoffEvent(ev1)
      un1.foreach((rid, st) => line(f"  UNBLOCK row=$rid%-2d -> ${st.status} (BLOCKED -> PENDING via step UpstreamEvent)"))
      dump("after handoff(VideoMetaDuration)")
      rule()

      // === EMBEDDING (id2): now PENDING, claim -> OutDone -> DONE ===
      line("STEP 2  run Embedding (id2)  [worker 12]")
      WorkerCore.runOne(world, 2L, 12L, sidecarEvent(12L))
      val ev2 = producedEventOf(fullDag, Embedding).get
      line(s"HANDOFF embedding DONE produces $ev2 -> unblock Masking (id3)")
      val un2 = store.handoffEvent(ev2)
      un2.foreach((rid, st) => line(f"  UNBLOCK row=$rid%-2d -> ${st.status}"))
      dump("after embedding DONE + handoff(EmbeddingReady)")
      rule()

      // === MASKING (id3): now PENDING, claim -> OutDone -> DONE ===
      line("STEP 3  run Masking (id3)  [worker 13]")
      WorkerCore.runOne(world, 3L, 13L, sidecarEvent(13L))
      dump("after masking DONE")
      rule()

      // === IMPORT (id5) -> RAGINDEX (id4) ===
      line("STEP 4  run Import (id5)  [worker 15]")
      WorkerCore.runOne(world, 5L, 15L, sidecarEvent(15L))
      val ev3 = producedEventOf(fullDag, Import).get
      line(s"HANDOFF import DONE produces $ev3 -> unblock RagIndex (id4)")
      val un3 = store.handoffEvent(ev3)
      un3.foreach((rid, st) => line(f"  UNBLOCK row=$rid%-2d -> ${st.status}"))
      line("STEP 5  run RagIndex (id4)  [worker 14]")
      WorkerCore.runOne(world, 4L, 14L, sidecarEvent(14L))
      dump("after import + ragindex DONE")
      dumpQueue("final (shadow mirrors authoritative job)")
      rule()

    // === CLAIM RACE: two workers race the SAME PENDING row ===
    line("=" * 78)
    line("CLAIM RACE  two workers (21, 22) race claim on a fresh PENDING row (id6)")
    line("  via SELECT ... FOR UPDATE SKIP LOCKED + re-validating UPDATE")
    line("=" * 78)
    store.seed(6L, JobState(PENDING, None, 0, Transcode, None), now())
    dump("before race")

    // run two claims concurrently against the SAME row. Each claim is one tx
    // (inside the store); FOR UPDATE SKIP LOCKED + the WHERE-revalidating UPDATE
    // guarantee one winner. The store returns a bare Option — no IO here.
    val winners = supervised:
      val f1 = fork { store.claim(6L, 21L, lease, now()).map(_ => 21L) }
      val f2 = fork { store.claim(6L, 22L, lease, now()).map(_ => 22L) }
      List(f1.join(), f2.join()).flatten

    line(s"  claim() returned a winning post-state for workers: ${winners.mkString(", ")}")
    // confirm by READING the DB: exactly one owner, status RUNNING.
    val raceRow = store.loadById(6L).get
    line(f"  DB row id6 after race: status=${raceRow.status} owner=${raceRow.owner.map(_.toString).getOrElse("-")}")
    line(s"  RACE RESULT: ${if winners.size == 1 && raceRow.status == RUNNING then "EXACTLY ONE WINNER (correct)" else "MULTIPLE/zero winners (WRONG)"}")
    rule()

    // === SWEEPER: orphaned BLOCKED -> NOT_APPLICABLE ===
    line("=" * 78)
    line("SWEEPER  reclaim an orphaned BLOCKED row under the Embedding-removed DAG")
    line("=" * 78)
    // seed a row BLOCKED on EmbeddingReady, then sweep under embeddingRemovedDag,
    // where nothing produces EmbeddingReady -> it is orphaned -> NOT_APPLICABLE.
    store.seed(7L, JobState(BLOCKED, None, 0, Masking, Some(EmbeddingReady)), now())
    line("  seeded id7 Masking BLOCKED on EmbeddingReady")
    line(s"  live DAG = embeddingRemovedDag, produces=${embeddingRemovedDag.produces.map((_, e) => Db.event2Str(e)).mkString(",")}")
    line( "  SQL: select id, blocked_on from job where status='BLOCKED'  (EmbeddingReady has no producer)")
    line( "       update job set status='NOT_APPLICABLE', blocked_on=null where id=? and status='BLOCKED'")
    val swept = store.sweepOrphans(embeddingRemovedDag)
    swept.foreach((rid, st) => line(f"  SWEEP row=$rid%-2d -> ${st.status} (BLOCKED -> NOT_APPLICABLE via step SweeperUpstreamFailed)"))
    dump("after sweep")
    rule()

    line("DONE. step() was applied for every transition; the DB layer only did")
    line("      load(point-query) / claim / save / handoff / sweep around it.")
