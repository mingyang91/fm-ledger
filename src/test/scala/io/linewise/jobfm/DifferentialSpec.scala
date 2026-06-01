package io.linewise.jobfm

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doobie.*
import doobie.implicits.*
import io.linewise.jobfm.generated.JobModel.*
import io.linewise.jobfm.generated.StoreCore.{JobRow, JobStore}

/* =============================================================================
 * DIFFERENTIAL SPEC — the machine-checked DRIFT GATE.
 *
 * Drives the verified, transpiler-GENERATED in-memory store (ListStore, whose
 * ops ARE StoreCore) and the TRUSTED production doobie store (JdbcStore, the same
 * SQL the real worker runs) over the SAME seeded scenarios, and asserts they
 * produce identical rows. If a future edit makes the doobie SQL diverge from the
 * verified StoreCore logic, a deterministic scenario fails and `./mill test`
 * goes red — turning the previously hand-asserted "SQL realizes the predicate"
 * trust into a continuously-checked equivalence.
 *
 * Each scenario uses a fresh isolated H2 (unique mem URL) for the JDBC side. The
 * concurrent claim race asserts only the VERIFIED invariant (exactly one RUNNING
 * owner), never the trusted SKIP-LOCKED owner identity.
 * ========================================================================== */
class DifferentialSpec extends munit.FunSuite:

  // comparable normal form: sorted by id, native tuple (FM types already erased).
  type Row = (Long, Status, Option[Long], Int, Kind, Option[Event2])
  def norm(s: Store): List[Row] =
    s.rows.map(r => (r.id, r.st.status, r.st.owner, r.st.attempts, r.st.kind, r.st.blockedOn)).sortBy(_._1)

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

  def runOps(s0: Store, ops: List[Store => Store]): Store =
    ops.foldLeft(s0)((s, op) => op(s))

  // the 5-row pipeline seed (mirrors main.scala)
  val pipelineSeed: List[JobRow] = List(
    JobRow(1L, JobState(PENDING, None, 0, Transcode, None)),
    JobRow(2L, JobState(BLOCKED, None, 0, Embedding, Some(VideoMetaDuration))),
    JobRow(3L, JobState(BLOCKED, None, 0, Masking,   Some(EmbeddingReady))),
    JobRow(4L, JobState(BLOCKED, None, 0, RagIndex,  Some(GcsUri))),
    JobRow(5L, JobState(PENDING, None, 0, Import,    None))
  )

  val pipelineOps: List[Store => Store] = List(
    (s: Store) => s.claimOne(1L, 11L, false),
    (s: Store) => s.applyOutcome(1L, OutDone(11L)),
    (s: Store) => s.handoff(VideoMetaDuration),
    (s: Store) => s.claimOne(2L, 12L, false),
    (s: Store) => s.applyOutcome(2L, OutDone(12L)),
    (s: Store) => s.handoff(EmbeddingReady),
    (s: Store) => s.claimOne(3L, 13L, false),
    (s: Store) => s.applyOutcome(3L, OutDone(13L)),
    (s: Store) => s.claimOne(5L, 15L, false),
    (s: Store) => s.applyOutcome(5L, OutDone(15L)),
    (s: Store) => s.handoff(GcsUri),
    (s: Store) => s.claimOne(4L, 14L, false),
    (s: Store) => s.applyOutcome(4L, OutDone(14L))
  )

  test("pipeline: verified ListStore and doobie JdbcStore agree row-for-row") {
    val mem = runOps(freshList(pipelineSeed), pipelineOps)
    val db  = runOps(freshJdbc(pipelineSeed), pipelineOps)
    assertEquals(norm(mem), norm(db))
    assert(norm(mem).forall(_._2 == DONE), "all five rows reach DONE")
  }

  test("sweeper: orphaned BLOCKED -> NOT_APPLICABLE on both") {
    val seed = List(JobRow(7L, JobState(BLOCKED, None, 0, Masking, Some(EmbeddingReady))))
    val ops: List[Store => Store] = List((s: Store) => s.sweep(embeddingRemovedDag))
    val mem = runOps(freshList(seed), ops)
    val db  = runOps(freshJdbc(seed), ops)
    assertEquals(norm(mem), norm(db))
    assertEquals(norm(mem).head._2, NOT_APPLICABLE)
  }

  test("claim race (sequential): both stores deny the second claimer") {
    val seed = List(JobRow(6L, JobState(PENDING, None, 0, Transcode, None)))
    val ops: List[Store => Store] =
      List((s: Store) => s.claimOne(6L, 21L, false), (s: Store) => s.claimOne(6L, 22L, false))
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
