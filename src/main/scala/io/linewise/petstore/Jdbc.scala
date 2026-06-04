package io.linewise.petstore

import java.sql.Connection

/* =============================================================================
 * SCHEMA + persistence support for the pet store (no cats-effect, no doobie, no
 * http4s). Since the migration to Quill, this object holds ONLY: the isomorphic DDL
 * (the scala-pet-store schema after migrations V1..V3), a pooled DataSource factory,
 * schema bootstrap, and the List[String] <-> VARCHAR codec reused by the Quill DAO
 * (PetStoreDb) for the tags / photo_urls columns. All querying is done by Quill.
 *
 * H2 runs in PostgreSQL compatibility mode so the original Postgres DDL
 * (BIGSERIAL / INT8 / BOOLEAN / TIMESTAMP / REFERENCES ... ON DELETE CASCADE) is
 * accepted verbatim — the table structures are byte-for-byte the originals. Ids are
 * assigned by the application (PetStoreHttp.freshId); the BIGSERIAL identity sequence
 * is intentionally unused (every INSERT supplies an explicit id).
 * ========================================================================== */
object Jdbc:

  private def h2Url(name: String): String =
    s"jdbc:h2:mem:$name;MODE=PostgreSQL;DB_CLOSE_DELAY=-1"

  /** A pooled DataSource over an in-memory H2 — Quill borrows a fresh connection per
    * run (so concurrent handlers never share one non-thread-safe java.sql.Connection).
    * DB_CLOSE_DELAY=-1 keeps the in-mem schema alive for the JVM even when the pool is idle. */
  def dataSource(name: String): javax.sql.DataSource =
    org.h2.jdbcx.JdbcConnectionPool.create(h2Url(name), "sa", "")

  /** The final, post-migration scala-pet-store schema (V1 InitialDatabaseSetup +
    * V2 PASSWORD->HASH + V3 Authentication: JWT table, ORDERS.USER_ID, USERS.ROLE).
    * Structurally isomorphic to src/main/resources/db/migration in the original. */
  val ddl: List[String] = List(
    """CREATE TABLE PET (
         ID BIGSERIAL PRIMARY KEY,
         NAME VARCHAR NOT NULL,
         CATEGORY VARCHAR NOT NULL,
         BIO VARCHAR NOT NULL,
         STATUS VARCHAR NOT NULL,
         PHOTO_URLS VARCHAR NOT NULL,
         TAGS VARCHAR NOT NULL
       )""",
    """CREATE TABLE USERS (
         ID BIGSERIAL PRIMARY KEY,
         USER_NAME VARCHAR NOT NULL UNIQUE,
         FIRST_NAME VARCHAR NOT NULL,
         LAST_NAME VARCHAR NOT NULL,
         EMAIL VARCHAR NOT NULL,
         HASH VARCHAR NOT NULL,
         PHONE VARCHAR NOT NULL,
         ROLE VARCHAR NOT NULL DEFAULT 'Customer'
       )""",
    """CREATE TABLE ORDERS (
         ID BIGSERIAL PRIMARY KEY,
         PET_ID INT8 NOT NULL REFERENCES PET (ID) ON DELETE CASCADE,
         SHIP_DATE TIMESTAMP NULL,
         STATUS VARCHAR NOT NULL,
         COMPLETE BOOLEAN NOT NULL,
         USER_ID BIGINT NOT NULL REFERENCES USERS (ID) ON DELETE CASCADE
       )""",
    """CREATE TABLE JWT (
         ID VARCHAR PRIMARY KEY,
         JWT VARCHAR NOT NULL,
         IDENTITY BIGINT NOT NULL REFERENCES USERS (ID) ON DELETE CASCADE,
         EXPIRY TIMESTAMP NOT NULL,
         LAST_TOUCHED TIMESTAMP
       )""",
  )

  def initSchema(c: Connection): Unit =
    ddl.foreach { sql =>
      val st = c.createStatement()
      try st.execute(sql)
      finally st.close()
    }

  /** A collection field (tags / photo_urls) <-> a single VARCHAR column. Each element
    * is TERMINATED by a control-char delimiter (not merely joined), so the empty list
    * `Nil` encodes to "" while `List("")` encodes to a lone delimiter — the two stay
    * distinguishable on round-trip (a plain join would collapse both to ""). */
  private val Delim = ""
  def encodeList(xs: List[String]): String = if xs.isEmpty then "" else xs.mkString("", Delim, Delim)
  def decodeList(s: String): List[String] = if s.isEmpty then Nil else s.split(Delim, -1).toList.init
