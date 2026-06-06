package io.linewise.ledger

import java.nio.file.{Files, Paths}
import java.util.UUID
import java.util.concurrent.locks.ReentrantLock

import com.zaxxer.hikari.HikariDataSource
import org.testcontainers.containers.PostgreSQLContainer
import org.testcontainers.utility.DockerImageName

import sttp.client4.*
import sttp.model.Uri
import sttp.tapir.Endpoint
import sttp.tapir.client.sttp4.SttpClientInterpreter
import sttp.tapir.server.stub4.TapirSyncStubInterpreter

abstract class LedgerHttpSuite extends munit.ScalaCheckSuite:
  private val baseUri: Uri = uri"http://test.local"
  private val client = SttpClientInterpreter()

  private def configureDocker(): Unit =
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

  configureDocker()

  private val pg = PostgreSQLContainer(DockerImageName.parse("postgres:16-alpine"))

  // Every ledger test rebinds the global `Db` singleton (the generated repos read it), so
  // tests across LedgerEndpointsSpec and LedgerPropertiesSpec must not run concurrently.
  // A process-wide lock held for the whole test body serializes them.
  override def munitTestTransforms: List[TestTransform] =
    super.munitTestTransforms :+ new TestTransform("serialize ledger Db", t =>
      t.withBody { () =>
        LedgerHttpSuite.dbLock.lock()
        try t.body() finally LedgerHttpSuite.dbLock.unlock()
      })

  // Each freshDs builds a new HikariDataSource (one per schema). With tests serialized,
  // only one is live at a time, so close the previous before opening the next and the last
  // in afterAll — otherwise the per-schema pools would accumulate open connections.
  private var lastPool: Option[HikariDataSource] = None

  override def beforeAll(): Unit =
    pg.start()

  override def afterAll(): Unit =
    lastPool.foreach(p => try p.close() catch case _: Throwable => ())
    pg.stop()

  protected def freshDs(prefix: String) =
    lastPool.foreach(p => try p.close() catch case _: Throwable => ())
    val schema = s"${prefix}_${UUID.randomUUID.toString.replace("-", "")}"
    val ds = Jdbc.dataSource(pg.getJdbcUrl, pg.getUsername, pg.getPassword, schema)
    lastPool = ds match { case h: HikariDataSource => Some(h); case _ => None }
    ds

  protected def freshApi(): LedgerApi =
    val ds = freshDs("ledger")
    val c0 = ds.getConnection()
    try Jdbc.initSchema(c0)
    finally c0.close()
    Db.init(ds)
    LedgerApi()

  protected def stubOf(api: LedgerApi): SyncBackend =
    TapirSyncStubInterpreter().whenServerEndpointsRunLogic(api.serverEndpoints).backend()

  protected def secure[A, I, EE, O](ep: Endpoint[A, I, EE, O, Any], be: SyncBackend, token: A, in: I): Response[Either[EE, O]] =
    client.toSecureRequestThrowDecodeFailures(ep, Some(baseUri))(token)(in).send(be)

object LedgerHttpSuite:
  // Shared across all ledger suites so the global `Db` singleton is touched by one test at a time.
  private val dbLock = ReentrantLock()
