package io.linewise.petstore

import java.sql.{Connection, DriverManager, PreparedStatement, ResultSet, Statement}
import scala.collection.mutable.ListBuffer

/* =============================================================================
 * PLAIN-JDBC SUPPORT — no cats-effect, no doobie, no http4s. Direct java.sql.
 *
 * Holds the isomorphic DDL (the scala-pet-store schema after migrations V1..V3)
 * and small query/update helpers. The repositories (PetRepo/OrderRepo/UserRepo)
 * persist through these and delegate all business logic to the verified,
 * transpiler-generated cores — so the only trusted thing here is persistence.
 *
 * H2 runs in PostgreSQL compatibility mode so the original Postgres DDL
 * (BIGSERIAL / INT8 / BOOLEAN / TIMESTAMP / REFERENCES ... ON DELETE CASCADE) is
 * accepted verbatim — the table structures are byte-for-byte the originals.
 * ========================================================================== */
object Jdbc:

  private def h2Url(name: String): String =
    s"jdbc:h2:mem:$name;MODE=PostgreSQL;DB_CLOSE_DELAY=-1"

  def h2(name: String): Connection =
    DriverManager.getConnection(h2Url(name), "sa", "")

  /** A pooled DataSource over the same in-memory H2 — borrow a fresh connection per
    * request so concurrent handlers never share one (non-thread-safe) java.sql.Connection.
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

  /** A collection field (tags / photo_urls) <-> a single VARCHAR column, joined on
    * a control-char delimiter (the column holds the whole collection, as in the
    * original's VARCHAR encoding). */
  private val Delim = ""
  def encodeList(xs: List[String]): String = xs.mkString(Delim)
  def decodeList(s: String): List[String] = if s.isEmpty then Nil else s.split(Delim, -1).toList

  /** Run a SELECT, mapping each row. */
  def query[A](c: Connection, sql: String)(set: PreparedStatement => Unit)(map: ResultSet => A): List[A] =
    val ps = c.prepareStatement(sql)
    try
      set(ps)
      val rs = ps.executeQuery()
      val out = ListBuffer.empty[A]
      while rs.next() do out += map(rs)
      out.toList
    finally ps.close()

  /** Run an INSERT/UPDATE/DELETE; return affected row count. */
  def update(c: Connection, sql: String)(set: PreparedStatement => Unit): Int =
    val ps = c.prepareStatement(sql)
    try { set(ps); ps.executeUpdate() }
    finally ps.close()

  /** INSERT and return the generated BIGSERIAL key (the id source). */
  def insertReturningId(c: Connection, sql: String)(set: PreparedStatement => Unit): Long =
    val ps = c.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
    try
      set(ps)
      ps.executeUpdate()
      val keys = ps.getGeneratedKeys
      keys.next()
      keys.getLong(1)
    finally ps.close()
