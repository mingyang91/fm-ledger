package io.linewise.jobfm

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doobie.*
import doobie.implicits.*
import io.linewise.jobfm.generated.JobModel.*
import io.linewise.jobfm.generated.StoreCore.{JobRow, JobStore}
import io.linewise.jobfm.generated.StoreLaw.AbstractStore
import io.linewise.jobfm.generated.WorkerCore

/* =============================================================================
 * DIFFERENTIAL SPEC — the machine-checked DRIFT GATE, now over the ONE store.
 *
 * Drives the SAME verified worker (generated.WorkerCore.runOne) and the SAME
 * high-level ops (handoff/sweep/claimOne) over BOTH implementations of the one
 * AbstractStore — the verified, transpiler-GENERATED in-memory ListStore (oracle)
 * and the TRUSTED doobie JdbcStore (the SQL the real worker runs) — and asserts
 * they produce identical rows. Because the worker now runs against this same
 * AbstractStore, the test covers the worker's ACTUAL path (claimOne -> findById
 * -> applyOutcome), not just the high-level ops. If the doobie SQL diverges from
 * the verified logic, a deterministic scenario fails and `./mill test` goes red.
 *
 * Each scenario uses a fresh isolated H2 (unique mem URL) for the JDBC side. The
 * concurrent claim race asserts only the VERIFIED invariant (exactly one RUNNING
 * owner), never the trusted SKIP-LOCKED owner identity.
 * ========================================================================== */
class DifferentialSpec extends munit.FunSuite:

  // comparable normal form: sorted by id, native tuple (FM types already erased).
  type Row = (Long, Status, Option[Long], Int, Kind, Option[Event2])
  def norm(s: AbstractStore): List[Row] =
    s.view.map(r => (r.id, r.st.status, r.st.owner, r.st.attempts, r.st.kind, r.st.blockedOn)).sortBy(_._1)

  def freshList(seed: List[JobRow]): ListStore = ListStore.seed(seed)

  def freshJdbc(seed: List[JobRow]): JdbcStore =
    val xa = Transactor.fromDriverManager[IO](
      driver = "org.h2.Driver",
      url = s"jdbc:h2:mem:diff_${java.util.UUID.randomUUID};DB_CLOSE_DELAY=-1",
      user = "sa", password = "", logHandler = None
    )
    val store = JdbcStore(xa)
    store.initSchema()
    seed.foreach(r => store.seed(r.id, r.st, 1_000_000L))
    store

  def runOps(s0: AbstractStore, ops: List[AbstractStore => AbstractStore]): AbstractStore =
    ops.foldLeft(s0)((s, op) => op(s))

  // a sidecar that always succeeds (the worker's outcome event for this worker).
  def succeed(worker: Long): Kind => Event = (_: Kind) => OutDone(worker)

  // the 5-row pipeline seed (mirrors main.scala)
  val pipelineSeed: List[JobRow] = List(
    JobRow(1L, JobState(PENDING, None, 0, Transcode, None)),
    JobRow(2L, JobState(BLOCKED, None, 0, Embedding, Some(VideoMetaDuration))),
    JobRow(3L, JobState(BLOCKED, None, 0, Masking,   Some(EmbeddingReady))),
    JobRow(4L, JobState(BLOCKED, None, 0, RagIndex,  Some(GcsUri))),
    JobRow(5L, JobState(PENDING, None, 0, Import,    None))
  )

  // the pipeline driven by the WORKER (runOne) — binds the worker's actual
  // claimOne -> findById -> applyOutcome path on both stores — plus the handoffs.
  val pipelineOps: List[AbstractStore => AbstractStore] = List(
    (s: AbstractStore) => WorkerCore.runOne(s, 1L, 11L, succeed(11L)),
    (s: AbstractStore) => s.handoff(VideoMetaDuration),
    (s: AbstractStore) => WorkerCore.runOne(s, 2L, 12L, succeed(12L)),
    (s: AbstractStore) => s.handoff(EmbeddingReady),
    (s: AbstractStore) => WorkerCore.runOne(s, 3L, 13L, succeed(13L)),
    (s: AbstractStore) => WorkerCore.runOne(s, 5L, 15L, succeed(15L)),
    (s: AbstractStore) => s.handoff(GcsUri),
    (s: AbstractStore) => WorkerCore.runOne(s, 4L, 14L, succeed(14L))
  )

  test("pipeline (driven by the WORKER): verified ListStore and doobie JdbcStore agree row-for-row") {
    val mem = runOps(freshList(pipelineSeed), pipelineOps)
    val db  = runOps(freshJdbc(pipelineSeed), pipelineOps)
    assertEquals(norm(mem), norm(db))
    assert(norm(mem).forall(_._2 == DONE), "all five rows reach DONE")
  }

  test("sweeper: orphaned BLOCKED -> NOT_APPLICABLE on both") {
    val seed = List(JobRow(7L, JobState(BLOCKED, None, 0, Masking, Some(EmbeddingReady))))
    val ops: List[AbstractStore => AbstractStore] = List((s: AbstractStore) => s.sweep(embeddingRemovedDag))
    val mem = runOps(freshList(seed), ops)
    val db  = runOps(freshJdbc(seed), ops)
    assertEquals(norm(mem), norm(db))
    assertEquals(norm(mem).head._2, NOT_APPLICABLE)
  }

  test("worker on an already-claimed row: second worker is denied, on both") {
    // id6 claimed by worker 21 (runOne -> RUNNING then DONE owner cleared), then
    // worker 22 runs it: claimOne re-claims the now-PENDING... no — after DONE it
    // is terminal, so a second runOne is a no-op. Use a PENDING row claimed once.
    val seed = List(JobRow(6L, JobState(PENDING, None, 0, Transcode, None)))
    val ops: List[AbstractStore => AbstractStore] =
      List((s: AbstractStore) => s.claimOne(6L, 21L, false), (s: AbstractStore) => s.claimOne(6L, 22L, false))
    val mem = runOps(freshList(seed), ops)
    val db  = runOps(freshJdbc(seed), ops)
    assertEquals(norm(mem), norm(db))
    val r = norm(mem).head
    assertEquals(r._2, RUNNING)
    assertEquals(r._3, Some(21L)) // first claimer wins; second denied (live lease)
  }

  test("claim race (concurrent, via Ox SeqMirrorOx.par): exactly one RUNNING owner") {
    val seed = List(JobRow(6L, JobState(PENDING, None, 0, Transcode, None)))
    val db   = freshJdbc(seed)
    // the real fork/join race (SKIP LOCKED) — the production realization of the
    // verified SeqMirrorProofs.raceExactlyOneWinner order-independence theorem.
    SeqMirrorOx.par(() => db.claimOne(6L, 21L, false), () => db.claimOne(6L, 22L, false))
    val r = norm(db).head
    assertEquals(r._2, RUNNING)
    assert(r._3 == Some(21L) || r._3 == Some(22L), s"exactly one winner, got owner ${r._3}")
  }
