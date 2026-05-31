package io.linewise.jobfm

import doobie.*
import doobie.implicits.*
import cats.effect.IO
import io.linewise.jobfm.generated.JobModel.*

/* =============================================================================
 * THE DOOBIE STORE — the REAL persistence layer that replaces the in-memory
 * TrieMap of the original shell. ROW-AS-SOURCE-OF-TRUTH: the row IS the job.
 *
 * The verified core (core.scala) is UNCHANGED. This file only does
 *   load (point-query) / claim / save / heartbeat / handoff / sweep
 * around the verified `step`. It never reimplements a transition — every
 * write of a JobState field is the result of `step(state, event)`.
 *
 * On H2 vs Postgres: the SQL below is the production Postgres flavour,
 * including `FOR UPDATE SKIP LOCKED`. H2 2.3.232 accepts it verbatim. Where
 * runtime behaviour differs is noted inline.
 * ========================================================================== */

object Db:

  // ---------------------------------------------------------------------------
  // CODECS: JobState's algebraic fields <-> varchar columns. These are doobie
  // Meta instances built off the String Meta. A decode failure (an unknown
  // token) THROWS at the doobie boundary — the row is the source of truth, so a
  // corrupt status is a hard error, not a silent default. This mirrors the
  // "LLM wire formats stay strict" discipline: persisted enums decode strictly.
  // ---------------------------------------------------------------------------

  given Meta[Status] = Meta[String].timap {
    case "PENDING"        => PENDING
    case "RUNNING"        => RUNNING
    case "DONE"           => DONE
    case "FAILED"         => FAILED
    case "NOT_APPLICABLE" => NOT_APPLICABLE
    case "PARTIAL"        => PARTIAL
    case "BLOCKED"        => BLOCKED
    case other            => throw new IllegalStateException(s"unknown status in row: $other")
  } {
    case PENDING        => "PENDING"
    case RUNNING        => "RUNNING"
    case DONE           => "DONE"
    case FAILED         => "FAILED"
    case NOT_APPLICABLE => "NOT_APPLICABLE"
    case PARTIAL        => "PARTIAL"
    case BLOCKED        => "BLOCKED"
  }

  given Meta[Kind] = Meta[String].timap {
    case "Transcode" => Transcode
    case "Embedding" => Embedding
    case "Masking"   => Masking
    case "Import"    => Import
    case "RagIndex"  => RagIndex
    case other       => throw new IllegalStateException(s"unknown kind in row: $other")
  } {
    case Transcode => "Transcode"
    case Embedding => "Embedding"
    case Masking   => "Masking"
    case Import    => "Import"
    case RagIndex  => "RagIndex"
  }

  // Event2 (the DAG edge labels) — nullable column => Meta over Option via the
  // String Meta; doobie lifts Meta[Event2] to Read/Write[Option[Event2]].
  given Meta[Event2] = Meta[String].timap {
    case "VideoMetaDuration" => VideoMetaDuration
    case "EmbeddingReady"    => EmbeddingReady
    case "GcsUri"            => GcsUri
    case other               => throw new IllegalStateException(s"unknown event2 in row: $other")
  } {
    case VideoMetaDuration => "VideoMetaDuration"
    case EmbeddingReady    => "EmbeddingReady"
    case GcsUri            => "GcsUri"
  }

  // ---------------------------------------------------------------------------
  // DDL — the job table. ROW-AS-SOURCE-OF-TRUTH: id + the five JobState fields
  // (status, owner, attempts, kind, blocked_on) + the lease column heartbeat.
  // The CHECK on attempts is the runtime echo of the Stainless invariant
  // 0 <= attempts <= maxAttempts that the proof dropped (require was proven).
  // heartbeat is BIGINT nanos-since-some-epoch (we pass an explicit clock),
  // which is portable across H2 and Postgres; production would use timestamptz.
  // ---------------------------------------------------------------------------
  val ddlJob: ConnectionIO[Int] =
    sql"""
      create table job (
        id         BIGINT       primary key,
        kind       VARCHAR(16)  not null,
        status     VARCHAR(16)  not null,
        owner      BIGINT,
        attempts   BIGINT       not null default 0 check (attempts >= 0 and attempts <= 5),
        blocked_on VARCHAR(32),
        heartbeat  BIGINT
      )
    """.update.run

  // OPTIONAL SHADOW (modeled, to show the production trigger-sync): the real
  // system keeps job state in a JSONB column on the anchor entity and mirrors a
  // flat projection into public.job_queue via a trigger. Here we keep an
  // explicit job_queue shadow and sync it from the same tx that writes `job`
  // (sync_job_queue is called wherever the trigger would fire). This is a
  // faithful flat mirror: (id, kind, status, owner). It is NOT a second source
  // of truth — `job` is authoritative; the shadow is a read-optimised copy.
  val ddlJobQueue: ConnectionIO[Int] =
    sql"""
      create table job_queue (
        id     BIGINT      primary key,
        kind   VARCHAR(16) not null,
        status VARCHAR(16) not null,
        owner  BIGINT
      )
    """.update.run

  /** Mirror the authoritative job row into the job_queue shadow (the trigger). */
  def syncJobQueue(id: Long): ConnectionIO[Int] =
    sql"""
      merge into job_queue (id, kind, status, owner)
      key(id)
      select id, kind, status, owner from job where id = $id
    """.update.run

  // ---------------------------------------------------------------------------
  // LOAD — point-query by id. THE STATE-LOADING HONESTY POINT: the shell loads
  // ONLY the relevant row, never the whole table. step() sees exactly this one
  // JobState value, reconstructed from the row's columns.
  // ---------------------------------------------------------------------------
  def loadById(id: Long): ConnectionIO[Option[JobState]] =
    sql"""
      select status, owner, attempts, kind, blocked_on
      from job
      where id = $id
    """
      .query[(Status, Option[Long], Int, Kind, Option[Event2])]
      .option
      .map(_.map((st, ow, att, k, bo) => JobState(st, ow, att, k, bo)))

  /** Seed a row (the demo's insert). Mirrors into the shadow too. */
  def seed(id: Long, s: JobState, nowNanos: Long): ConnectionIO[Int] =
    for
      n <- sql"""
             insert into job (id, kind, status, owner, attempts, blocked_on, heartbeat)
             values ($id, ${s.kind}, ${s.status}, ${s.owner}, ${s.attempts}, ${s.blockedOn}, $nowNanos)
           """.update.run
      _ <- syncJobQueue(id)
    yield n

  // ---------------------------------------------------------------------------
  // CLAIM — the single load-bearing concurrency primitive, TWO STEPS IN ONE TX:
  //   (1) SELECT a claimable candidate FOR UPDATE SKIP LOCKED  (advisory lock)
  //   (2) a RE-VALIDATING UPDATE guarded by WHERE status='PENDING'  (real gate)
  // The lock makes the index-scan advisory; the WHERE-revalidation is what
  // actually prevents two winners. Returns the claimed JobState iff THIS worker
  // won (update affected 1 row). Whole thing is one ConnectionIO, run under one
  // .transact(xa) so the lock is held across the SELECT and the UPDATE.
  //
  // Claimable = PENDING, or RUNNING with a stale heartbeat (lease expired).
  // PARTIAL is also claimable per the core (Claim handles PENDING|PARTIAL); we
  // include it in the candidate predicate.
  // ---------------------------------------------------------------------------
  def claim(id: Long, worker: Long, leaseNanos: Long, nowNanos: Long): ConnectionIO[Option[JobState]] =
    val staleBefore = nowNanos - leaseNanos
    for
      // (1) lock a claimable candidate. FOR UPDATE SKIP LOCKED: a row already
      // locked by another tx is skipped, so racing claimers never block each
      // other; at most one acquires THIS row's lock.
      cand <- sql"""
                select id from job
                where id = $id
                  and ( status = 'PENDING'
                     or status = 'PARTIAL'
                     or (status = 'RUNNING' and (heartbeat is null or heartbeat < $staleBefore)) )
                for update skip locked
              """.query[Long].option
      res <- cand match
        case None => doobie.free.connection.pure(Option.empty[JobState])
        case Some(_) =>
          // load the locked pre-image, fold Claim through the VERIFIED core,
          // then the RE-VALIDATING UPDATE. We reset a stale RUNNING to PENDING
          // first (the staleRunningReclaimable path) so step's Claim arm fires.
          for
            preOpt <- loadById(id)
            out <- preOpt match
              case None => doobie.free.connection.pure(Option.empty[JobState])
              case Some(pre) =>
                val base = if pre.status == RUNNING then pre.copy(status = PENDING, owner = None) else pre
                val post = step(base, Claim(worker)) // <-- VERIFIED step, unchanged
                if post.status == RUNNING then
                  // re-validating UPDATE: only commit if the row is STILL claimable.
                  for
                    n <- sql"""
                           update job
                           set status = ${post.status}, owner = ${post.owner}, heartbeat = $nowNanos
                           where id = $id
                             and ( status = 'PENDING'
                                or status = 'PARTIAL'
                                or (status = 'RUNNING' and (heartbeat is null or heartbeat < $staleBefore)) )
                         """.update.run
                    _ <- if n == 1 then syncJobQueue(id) else doobie.free.connection.pure(0)
                  yield if n == 1 then Some(post) else None
                else doobie.free.connection.pure(Option.empty[JobState])
          yield out
    yield res

  // ---------------------------------------------------------------------------
  // SAVE — write a step() result back to the row, owner-revalidated, in one tx.
  // The WHERE owner=:me clause is the re-validating UPDATE for the write path:
  // if the heartbeat lease was lost and another worker reclaimed the row, the
  // update affects 0 rows and we report the lost write (the core's rule: a
  // worker that lost its claim must NOT write terminal state).
  // ---------------------------------------------------------------------------
  def saveState(id: Long, worker: Long, post: JobState): ConnectionIO[Boolean] =
    for
      n <- sql"""
             update job
             set status = ${post.status}, owner = ${post.owner},
                 attempts = ${post.attempts}, blocked_on = ${post.blockedOn}
             where id = $id and status = 'RUNNING' and owner = $worker
           """.update.run
      _ <- if n == 1 then syncJobQueue(id) else doobie.free.connection.pure(0)
    yield n == 1

  /** Heartbeat renew — its own small tx; renews owner+heartbeat for a row this
    * worker still owns. Returns false if ownership was lost underneath us. */
  def heartbeatRenew(id: Long, worker: Long, nowNanos: Long): ConnectionIO[Boolean] =
    sql"""
      update job set heartbeat = $nowNanos
      where id = $id and status = 'RUNNING' and owner = $worker
    """.update.run.map(_ == 1)

  /** Is this worker still the recorded owner of a RUNNING row? (point-query) */
  def ownsRunning(id: Long, worker: Long): ConnectionIO[Boolean] =
    sql"select 1 from job where id = $id and status = 'RUNNING' and owner = $worker"
      .query[Int].option.map(_.isDefined)

  // ---------------------------------------------------------------------------
  // HANDOFF — an upstream kind's DONE produces a named event. Every downstream
  // BLOCKED row parked on THAT event is reset to PENDING (the reset edge). We
  // fire UpstreamEvent(ev) through the VERIFIED core for each such row, so the
  // BLOCKED->PENDING transition is step's, not the SQL's. The SQL only SELECTs
  // the candidate rows (point-query subset by blocked_on) and writes back the
  // step results.
  // ---------------------------------------------------------------------------
  def handoff(ev: Event2): ConnectionIO[List[(Long, JobState)]] =
    for
      ids <- sql"select id from job where status = 'BLOCKED' and blocked_on = ${ev}"
               .query[Long].to[List]
      results <- ids.foldRight(doobie.free.connection.pure(List.empty[(Long, JobState)])) { (rid, acc) =>
        for
          tail <- acc
          preOpt <- loadById(rid)
          updated <- preOpt match
            case Some(pre) =>
              val post = step(pre, UpstreamEvent(ev)) // <-- VERIFIED step, unchanged
              for
                _ <- sql"""
                       update job set status = ${post.status}, blocked_on = ${post.blockedOn}
                       where id = $rid and status = 'BLOCKED'
                     """.update.run
                _ <- syncJobQueue(rid)
              yield (rid, post) :: tail
            case None => doobie.free.connection.pure(tail)
        yield updated
      }
    yield results

  // ---------------------------------------------------------------------------
  // SWEEPER — every BLOCKED row whose blocked_on has NO producer in the live
  // DAG is reclaimed to NOT_APPLICABLE (SweeperUpstreamFailed through the core).
  // We compute the live producer set from the Dag value, build the orphan
  // predicate from it, SELECT only the orphaned rows, and fold the verified
  // transition over each. (No orphaned BLOCKED can survive a sweep.)
  // ---------------------------------------------------------------------------
  def sweepOrphanedBlocked(dag: Dag): ConnectionIO[List[(Long, JobState)]] =
    val liveProducers: Set[String] = dag.produces.map((_, ev) => Db.event2Str(ev)).toSet
    for
      rows <- sql"select id, blocked_on from job where status = 'BLOCKED' and blocked_on is not null"
                .query[(Long, String)].to[List]
      orphans = rows.filterNot((_, bo) => liveProducers.contains(bo))
      results <- orphans.foldRight(doobie.free.connection.pure(List.empty[(Long, JobState)])) { (row, acc) =>
        val rid = row._1
        for
          tail <- acc
          preOpt <- loadById(rid)
          updated <- preOpt match
            case Some(pre) =>
              val post = step(pre, SweeperUpstreamFailed) // <-- VERIFIED step, unchanged
              for
                _ <- sql"""
                       update job set status = ${post.status}, blocked_on = ${post.blockedOn}
                       where id = $rid and status = 'BLOCKED'
                     """.update.run
                _ <- syncJobQueue(rid)
              yield (rid, post) :: tail
            case None => doobie.free.connection.pure(tail)
        yield updated
      }
    yield results

  // helper to stringify an Event2 for the orphan predicate (matches the Meta)
  def event2Str(e: Event2): String = e match
    case VideoMetaDuration => "VideoMetaDuration"
    case EmbeddingReady    => "EmbeddingReady"
    case GcsUri            => "GcsUri"

  /** Read the whole table for printing demo state (NOT used by the worker). */
  val dumpAll: ConnectionIO[List[(Long, Kind, Status, Option[Long], Int, Option[Event2])]] =
    sql"select id, kind, status, owner, attempts, blocked_on from job order by id"
      .query[(Long, Kind, Status, Option[Long], Int, Option[Event2])].to[List]

  val dumpQueue: ConnectionIO[List[(Long, Kind, Status, Option[Long])]] =
    sql"select id, kind, status, owner from job_queue order by id"
      .query[(Long, Kind, Status, Option[Long])].to[List]
