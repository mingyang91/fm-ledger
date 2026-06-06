package io.linewise.ledger

import javax.sql.DataSource
import java.sql.{Connection, PreparedStatement, ResultSet, SQLException, Statement, Timestamp}
import java.util.concurrent.atomic.AtomicLong
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import io.linewise.ledger.generated.LedgerModel.{LedgerTx, TxKind, WithdrawalStatus, ProposalStatus, ObligationStatus}
import io.linewise.ledger.generated.Withdrawal
import io.linewise.ledger.generated.Proposal
import io.linewise.ledger.generated.Obligation

/* =============================================================================
 * PRODUCTION persistence — the @extern target the GENERATED Jdbc* repositories
 * delegate to. The generated verified core still sees compact case classes; this
 * file maps them onto the richer audit schema required by the module design.
 * ========================================================================== */

object Accounts:
  val IncentiveExpense   = "system:incentive_expense"
  val WithdrawalClearing = "system:withdrawal_clearing"
  val Cash               = "system:cash"
  val Adjustment         = "system:adjustment"
  def user(uid: String): String = s"user:$uid"
  def isUser(account: String): Boolean = account.startsWith("user:")
  def normalSide(account: String): String =
    if isUser(account) || account == WithdrawalClearing || account == Cash then "CR" else "DR"

object Ids:
  private val n = AtomicLong(0L)
  def next(): Long = n.incrementAndGet()

final case class TxWriteMeta(
    targetTxId: Option[Long] = None,
    withdrawalId: Option[Long] = None,
    proposalId: Option[Long] = None,
    incentiveTraceId: Option[String] = None,
    incentiveModule: Option[String] = None,
    rawInput: Option[String] = None)

final case class RiskEventRecord(id: Long, kind: String, subject: String, detail: String)
final case class AuditLogRecord(id: Long, action: String, actor: String, subject: String, detail: String)
final case class ConfigEntry(key: String, value: String)
final case class ConfigProposalRecord(id: Long, key: String, value: String, reason: String, status: String, proposedBy: String, approvedBy: Option[String], rejectedBy: Option[String])
final case class BalanceDrift(account: String, materialized: Long, replayed: Long)

final class LedgerStore(ds: DataSource):
  private def using[A <: AutoCloseable, B](a: A)(f: A => B): B =
    try f(a) finally a.close()

  private def conn[A](f: Connection => A): A =
    val c = ds.getConnection()
    try f(c) finally c.close()

  private def tx[A](f: Connection => A): A =
    val c = ds.getConnection()
    val old = c.getAutoCommit
    c.setAutoCommit(false)
    try
      val out = f(c)
      c.commit()
      out
    catch
      case e: Throwable =>
        c.rollback()
        throw e
    finally
      c.setAutoCommit(old)
      c.close()

  private def optLong(rs: ResultSet, name: String): Option[Long] =
    val v = rs.getLong(name)
    if rs.wasNull() then None else Some(v)

  private def optString(rs: ResultSet, name: String): Option[String] =
    Option(rs.getString(name))

  private def bind(ps: PreparedStatement, values: Any*): Unit =
    values.zipWithIndex.foreach { case (v, i) =>
      v match
        case None       => ps.setObject(i + 1, null)
        case Some(x)    => ps.setObject(i + 1, x)
        case x: Boolean => ps.setString(i + 1, x.toString)
        case x          => ps.setObject(i + 1, x)
    }

  private def one[A](c: Connection, sql: String, values: Any*)(read: ResultSet => A): Option[A] =
    using(c.prepareStatement(sql)) { ps =>
      bind(ps, values*)
      using(ps.executeQuery()) { rs => if rs.next() then Some(read(rs)) else None }
    }

  private def many[A](c: Connection, sql: String, values: Any*)(read: ResultSet => A): List[A] =
    using(c.prepareStatement(sql)) { ps =>
      bind(ps, values*)
      using(ps.executeQuery()) { rs =>
        val b = List.newBuilder[A]
        while rs.next() do b += read(rs)
        b.result()
      }
    }

  private def execute(c: Connection, sql: String, values: Any*): Int =
    using(c.prepareStatement(sql)) { ps =>
      bind(ps, values*)
      ps.executeUpdate()
    }

  private def insertGenerated(c: Connection, sql: String, values: Any*): Long =
    using(c.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)) { ps =>
      bind(ps, values*)
      ps.executeUpdate()
      using(ps.getGeneratedKeys()) { keys =>
        if keys.next() then keys.getLong(1) else throw SQLException("no generated key")
      }
    }

  private def signedDelta(account: String, direction: String, amount: Long): Long =
    if Accounts.normalSide(account) == direction then amount else -amount

  private def applyBalanceDelta(c: Connection, account: String, direction: String, amount: Long): Unit =
    val delta = signedDelta(account, direction, amount)
    val updated = execute(c,
      "UPDATE ACCOUNT_BALANCE SET BALANCE = BALANCE + ?, UPDATED_AT = CURRENT_TIMESTAMP WHERE ACCOUNT_ID = ?",
      delta, account)
    if updated == 0 then
      execute(c,
        "INSERT INTO ACCOUNT_BALANCE (ACCOUNT_ID, BALANCE, UPDATED_AT) VALUES (?, ?, CURRENT_TIMESTAMP)",
        account, delta)

  private def latestWithdrawalStatus(c: Connection, id: Long): Option[String] =
    one(c,
      "SELECT TO_STATUS FROM WITHDRAWAL_STATUS_CHANGE WHERE WITHDRAWAL_ID = ? ORDER BY ID DESC LIMIT 1",
      id)(_.getString("TO_STATUS"))

  private def latestProposalStatus(c: Connection, id: Long): Option[(String, Option[Long])] =
    one(c,
      "SELECT TO_STATUS, RESULT_TX_ID FROM PROPOSAL_STATUS_CHANGE WHERE PROPOSAL_ID = ? ORDER BY ID DESC LIMIT 1",
      id)(rs => (rs.getString("TO_STATUS"), optLong(rs, "RESULT_TX_ID")))

  private def currentActor: String = Db.currentActor

  private def withdrawalReason(to: String): String = to match
    case "PendingReview" => "user_submitted"
    case "Submitted"     => "admin_approved"
    case "Settled"       => "webhook_settled"
    case "Rejected"      => "admin_rejected"
    case "Cancelled"     => "user_cancelled"
    case "Failed"        => "provider_failed"
    case _               => "status_changed"

  private def proposalReason(to: String): String = to match
    case "PendingReview" => "admin_proposed"
    case "Approved"      => "admin_approved"
    case "Rejected"      => "admin_rejected"
    case _               => "status_changed"

  private def txFromId(c: Connection, id: Long): Option[LedgerTx] =
    one(c,
      "SELECT ID, KIND, SOURCE_KIND, SOURCE_ID, USER_UID FROM LEDGER_TX WHERE ID = ?",
      id) { h =>
        val entries = many(c,
          "SELECT ACCOUNT_ID, DIRECTION, AMOUNT FROM LEDGER_ENTRY WHERE TX_ID = ? ORDER BY ID",
          id)(rs => (rs.getString("ACCOUNT_ID"), rs.getString("DIRECTION"), rs.getLong("AMOUNT")))
        val debit  = entries.find(_._2 == "DR").getOrElse(("", "DR", 0L))
        val credit = entries.find(_._2 == "CR").getOrElse(("", "CR", debit._3))
        LedgerTx(h.getLong("ID"), TxKind.valueOf(h.getString("KIND")), debit._1, credit._1, debit._3,
          optString(h, "SOURCE_KIND"), optString(h, "SOURCE_ID"), h.getString("USER_UID"))
      }

  def insert(t: LedgerTx): Unit =
    val meta = Db.currentTxMeta
    tx { c =>
      execute(c,
        """INSERT INTO LEDGER_TX
           (ID, KIND, SOURCE_KIND, SOURCE_ID, USER_UID, TARGET_TX_ID, WITHDRAWAL_ID, PROPOSAL_ID, INCENTIVE_TRACE_ID, INCENTIVE_MODULE, RAW_INPUT)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""",
        t.id, t.kind.toString, t.sourceKind, t.sourceId, t.userUid, meta.targetTxId, meta.withdrawalId, meta.proposalId,
        meta.incentiveTraceId, meta.incentiveModule, meta.rawInput)
      execute(c, "INSERT INTO LEDGER_ENTRY (TX_ID, ACCOUNT_ID, DIRECTION, AMOUNT) VALUES (?, ?, 'DR', ?)", t.id, t.debitAccount, t.amount)
      execute(c, "INSERT INTO LEDGER_ENTRY (TX_ID, ACCOUNT_ID, DIRECTION, AMOUNT) VALUES (?, ?, 'CR', ?)", t.id, t.creditAccount, t.amount)
      applyBalanceDelta(c, t.debitAccount, "DR", t.amount)
      applyBalanceDelta(c, t.creditAccount, "CR", t.amount)
    }

  def byId(id: Long): Option[LedgerTx] = conn(txFromId(_, id))

  def bySource(kind: String, id: String): Option[LedgerTx] = conn { c =>
    one(c, "SELECT ID FROM LEDGER_TX WHERE SOURCE_KIND = ? AND SOURCE_ID = ?", kind, id)(_.getLong("ID")).flatMap(txFromId(c, _))
  }

  def all: List[LedgerTx] = conn { c =>
    many(c, "SELECT ID FROM LEDGER_TX ORDER BY ID")(_.getLong("ID")).flatMap(txFromId(c, _))
  }

  def balanceOf(account: String): Long = conn { c =>
    one(c, "SELECT BALANCE FROM ACCOUNT_BALANCE WHERE ACCOUNT_ID = ?", account)(_.getLong("BALANCE")).getOrElse(0L)
  }

  def ledgerBalanceOf(account: String): Long = conn { c => ledgerBalanceOf(c, account) }

  private def ledgerBalanceOf(c: Connection, account: String): Long =
    one(c,
      """SELECT COALESCE(SUM(CASE
           WHEN DIRECTION = ? THEN AMOUNT ELSE -AMOUNT END), 0) AS BAL
         FROM LEDGER_ENTRY WHERE ACCOUNT_ID = ?""",
      Accounts.normalSide(account), account)(_.getLong("BAL")).getOrElse(0L)

  def balanceDrifts: List[BalanceDrift] = conn { c =>
    val accounts = many(c,
      "SELECT ACCOUNT_ID FROM ACCOUNT_BALANCE UNION SELECT ACCOUNT_ID FROM LEDGER_ENTRY")(_.getString("ACCOUNT_ID"))
    accounts.flatMap { a =>
      val mat = balanceOf(a)
      val rep = ledgerBalanceOf(c, a)
      if mat == rep then None else Some(BalanceDrift(a, mat, rep))
    }
  }

  def withdrawalPut(wd: Withdrawal): Unit = tx { c =>
    val exists = one(c, "SELECT ID FROM WITHDRAWAL WHERE ID = ?", wd.id)(_.getLong("ID")).isDefined
    if !exists then
      execute(c,
        "INSERT INTO WITHDRAWAL (ID, USER_UID, AMOUNT, CLIENT_REQUEST_ID, RESERVE_TX_ID) VALUES (?, ?, ?, ?, ?)",
        wd.id, wd.userUid, wd.amount, wd.clientRequestId, wd.reserveTxId)
    val from = latestWithdrawalStatus(c, wd.id)
    val to = wd.status.toString
    if from.forall(_ != to) then
      execute(c,
        "INSERT INTO WITHDRAWAL_STATUS_CHANGE (WITHDRAWAL_ID, FROM_STATUS, TO_STATUS, REASON, ACTOR) VALUES (?, ?, ?, ?, ?)",
        wd.id, from, to, withdrawalReason(to), currentActor)
  }

  private def withdrawalFromId(c: Connection, id: Long): Option[Withdrawal] =
    one(c, "SELECT ID, USER_UID, AMOUNT, CLIENT_REQUEST_ID, RESERVE_TX_ID FROM WITHDRAWAL WHERE ID = ?", id) { rs =>
      val st = latestWithdrawalStatus(c, id).getOrElse("PendingReview")
      Withdrawal(rs.getLong("ID"), rs.getString("USER_UID"), rs.getLong("AMOUNT"), WithdrawalStatus.valueOf(st), rs.getString("CLIENT_REQUEST_ID"), rs.getLong("RESERVE_TX_ID"))
    }

  def withdrawalById(id: Long): Option[Withdrawal] = conn(withdrawalFromId(_, id))
  def withdrawalByClientReq(userUid: String, clientRequestId: String): Option[Withdrawal] = conn { c =>
    one(c, "SELECT ID FROM WITHDRAWAL WHERE USER_UID = ? AND CLIENT_REQUEST_ID = ?", userUid, clientRequestId)(_.getLong("ID")).flatMap(withdrawalFromId(c, _))
  }
  def allWithdrawals: List[Withdrawal] = conn { c =>
    many(c, "SELECT ID FROM WITHDRAWAL ORDER BY ID")(_.getLong("ID")).flatMap(withdrawalFromId(c, _))
  }

  def proposalPut(p: Proposal): Unit = tx { c =>
    val exists = one(c, "SELECT ID FROM PROPOSAL WHERE ID = ?", p.id)(_.getLong("ID")).isDefined
    if !exists then
      execute(c,
        """INSERT INTO PROPOSAL
           (ID, KIND, USER_UID, DEBIT_ACCOUNT, CREDIT_ACCOUNT, AMOUNT, REASON, PROPOSED_BY, TARGET_TX_ID)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""",
        p.id, p.kind.toString, p.userUid, p.debitAccount, p.creditAccount, p.amount, p.reason, p.proposedBy, p.targetTxId)
    val from = latestProposalStatus(c, p.id).map(_._1)
    val to = p.status.toString
    if from.forall(_ != to) then
      execute(c,
        "INSERT INTO PROPOSAL_STATUS_CHANGE (PROPOSAL_ID, FROM_STATUS, TO_STATUS, RESULT_TX_ID, ACTOR) VALUES (?, ?, ?, ?, ?)",
        p.id, from, to, p.resultTxId, currentActor)
  }

  private def proposalFromId(c: Connection, id: Long): Option[Proposal] =
    one(c,
      """SELECT ID, KIND, USER_UID, DEBIT_ACCOUNT, CREDIT_ACCOUNT, AMOUNT, REASON, PROPOSED_BY, TARGET_TX_ID
         FROM PROPOSAL WHERE ID = ?""", id) { rs =>
      val latest = latestProposalStatus(c, id).getOrElse(("PendingReview", None))
      Proposal(rs.getLong("ID"), TxKind.valueOf(rs.getString("KIND")), rs.getString("USER_UID"), rs.getString("DEBIT_ACCOUNT"), rs.getString("CREDIT_ACCOUNT"),
        rs.getLong("AMOUNT"), rs.getString("REASON"), rs.getString("PROPOSED_BY"), ProposalStatus.valueOf(latest._1), latest._2, optLong(rs, "TARGET_TX_ID"))
    }

  def proposalById(id: Long): Option[Proposal] = conn(proposalFromId(_, id))
  def allProposals: List[Proposal] = conn { c =>
    many(c, "SELECT ID FROM PROPOSAL ORDER BY ID")(_.getLong("ID")).flatMap(proposalFromId(c, _))
  }


  def obligationPut(o: Obligation): Unit = tx { c =>
    execute(c, "DELETE FROM OBLIGATION WHERE SOURCE_KIND = ? AND SOURCE_ID = ?", o.sourceKind, o.sourceId)
    execute(c,
      """INSERT INTO OBLIGATION
         (SOURCE_KIND, SOURCE_ID, USER_UID, ESTIMATED_POINTS, STATUS, REALIZED_TX_ID)
         VALUES (?, ?, ?, ?, ?, ?)""",
      o.sourceKind, o.sourceId, o.userUid, o.estimatedUnit, o.status.toString, o.realizedTxId)
  }
  def obligationBySource(kind: String, id: String): Option[Obligation] = conn { c =>
    one(c, "SELECT * FROM OBLIGATION WHERE SOURCE_KIND = ? AND SOURCE_ID = ?", kind, id) { r =>
      Obligation(r.getString("SOURCE_KIND"), r.getString("SOURCE_ID"), r.getString("USER_UID"), "", "",
        "", r.getLong("ESTIMATED_POINTS"), ObligationStatus.valueOf(r.getString("STATUS")), optLong(r, "REALIZED_TX_ID"))
    }
  }
  def allOpenObligations: List[Obligation] = conn { c =>
    many(c, "SELECT * FROM OBLIGATION WHERE STATUS = 'Open'") { r =>
      Obligation(r.getString("SOURCE_KIND"), r.getString("SOURCE_ID"), r.getString("USER_UID"), "", "",
        "", r.getLong("ESTIMATED_POINTS"), ObligationStatus.valueOf(r.getString("STATUS")), optLong(r, "REALIZED_TX_ID"))
    }
  }


  private def configLong(c: Connection, key: String, default: Long): Long =
    one(c,
      "SELECT CONFIG_VALUE FROM SYSTEM_CONFIG_CHANGE WHERE CONFIG_KEY = ? AND APPROVED_BY IS NOT NULL ORDER BY EFFECTIVE_AT DESC, ID DESC LIMIT 1",
      key)(_.getString("CONFIG_VALUE")).flatMap(s => scala.util.Try(s.toLong).toOption).getOrElse(default)
  private def configString(c: Connection, key: String, default: String): String =
    one(c,
      "SELECT CONFIG_VALUE FROM SYSTEM_CONFIG_CHANGE WHERE CONFIG_KEY = ? AND APPROVED_BY IS NOT NULL ORDER BY EFFECTIVE_AT DESC, ID DESC LIMIT 1",
      key)(_.getString("CONFIG_VALUE")).getOrElse(default)
  def configEntries: List[ConfigEntry] = conn { c =>
    val keys = many(c, "SELECT DISTINCT CONFIG_KEY FROM SYSTEM_CONFIG_CHANGE ORDER BY CONFIG_KEY")(_.getString("CONFIG_KEY"))
    keys.map(k => ConfigEntry(k, configString(c, k, "")))
  }
  def payoutsEnabled: Boolean = conn(c => configString(c, "payouts_enabled", "true") == "true")
  def setPayoutsEnabled(enabled: Boolean, reason: String, actor: String): Unit = tx { c =>
    execute(c,
      "INSERT INTO SYSTEM_CONFIG_CHANGE (CONFIG_KEY, CONFIG_VALUE, REASON, ACTOR, PROPOSED_BY, APPROVED_BY) VALUES ('payouts_enabled', ?, ?, ?, ?, ?)",
      enabled.toString, reason, actor, actor, actor)
    audit(c, "config.payouts_enabled", actor, "payouts_enabled", enabled.toString)
  }
  def proposeConfig(key: String, value: String, reason: String, proposedBy: String): ConfigProposalRecord = tx { c =>
    val id = insertGenerated(c,
      "INSERT INTO SYSTEM_CONFIG_PROPOSAL (CONFIG_KEY, CONFIG_VALUE, REASON, STATUS, PROPOSED_BY) VALUES (?, ?, ?, 'PendingReview', ?)",
      key, value, reason, proposedBy)
    audit(c, "config.propose", proposedBy, key, reason)
    configProposalFromId(c, id).get
  }
  def approveConfig(id: Long, approver: String): Either[String, ConfigProposalRecord] = tx { c =>
    configProposalFromId(c, id) match
      case None => Left("config_proposal_not_found")
      case Some(p) if p.status != "PendingReview" => Left("status_conflict")
      case Some(p) if p.proposedBy == approver => Left("two_person_violation")
      case Some(p) =>
        execute(c, "UPDATE SYSTEM_CONFIG_PROPOSAL SET STATUS = 'Approved', APPROVED_BY = ?, DECIDED_AT = CURRENT_TIMESTAMP WHERE ID = ?", approver, id)
        execute(c,
          "INSERT INTO SYSTEM_CONFIG_CHANGE (CONFIG_KEY, CONFIG_VALUE, REASON, ACTOR, PROPOSED_BY, APPROVED_BY) VALUES (?, ?, ?, ?, ?, ?)",
          p.key, p.value, p.reason, approver, p.proposedBy, approver)
        audit(c, "config.approve", approver, p.key, p.value)
        Right(configProposalFromId(c, id).get)
  }
  def rejectConfig(id: Long, rejecter: String): Either[String, ConfigProposalRecord] = tx { c =>
    configProposalFromId(c, id) match
      case None => Left("config_proposal_not_found")
      case Some(p) if p.status != "PendingReview" => Left("status_conflict")
      case Some(p) =>
        execute(c, "UPDATE SYSTEM_CONFIG_PROPOSAL SET STATUS = 'Rejected', REJECTED_BY = ?, DECIDED_AT = CURRENT_TIMESTAMP WHERE ID = ?", rejecter, id)
        audit(c, "config.reject", rejecter, p.key, p.reason)
        Right(configProposalFromId(c, id).get)
  }
  private def configProposalFromId(c: Connection, id: Long): Option[ConfigProposalRecord] =
    one(c, "SELECT * FROM SYSTEM_CONFIG_PROPOSAL WHERE ID = ?", id) { r =>
      ConfigProposalRecord(r.getLong("ID"), r.getString("CONFIG_KEY"), r.getString("CONFIG_VALUE"), r.getString("REASON"), r.getString("STATUS"), r.getString("PROPOSED_BY"), optString(r, "APPROVED_BY"), optString(r, "REJECTED_BY"))
    }
  def allConfigProposals: List[ConfigProposalRecord] = conn { c => many(c, "SELECT ID FROM SYSTEM_CONFIG_PROPOSAL ORDER BY ID")(_.getLong("ID")).flatMap(configProposalFromId(c, _)) }

  private def audit(c: Connection, action: String, actor: String, subject: String, detail: String): Unit =
    execute(c, "INSERT INTO AUDIT_LOG (ACTION, ACTOR, SUBJECT, DETAIL) VALUES (?, ?, ?, ?)", action, actor, subject, detail)
  def audit(action: String, actor: String, subject: String, detail: String): Unit = tx(audit(_, action, actor, subject, detail))
  def auditLogs: List[AuditLogRecord] = conn { c =>
    many(c, "SELECT * FROM AUDIT_LOG ORDER BY ID") { r =>
      AuditLogRecord(r.getLong("ID"), r.getString("ACTION"), r.getString("ACTOR"), r.getString("SUBJECT"), r.getString("DETAIL"))
    }
  }

  private def risk(c: Connection, kind: String, subject: String, detail: String): Unit =
    execute(c, "INSERT INTO RISK_EVENT (KIND, SUBJECT, DETAIL) VALUES (?, ?, ?)", kind, subject, detail)
  def risk(kind: String, subject: String, detail: String): Unit = tx(risk(_, kind, subject, detail))
  def riskEvents: List[RiskEventRecord] = conn { c =>
    many(c, "SELECT * FROM RISK_EVENT ORDER BY ID") { r =>
      RiskEventRecord(r.getLong("ID"), r.getString("KIND"), r.getString("SUBJECT"), r.getString("DETAIL"))
    }
  }

  def guardLedgerAmount(userUid: String, amount: Long, subject: String): Either[String, Unit] = conn { c =>
    val single = configLong(c, "single_ledger_amount_limit_points", 100000L)
    val growth = configLong(c, "per_user_day_point_growth_limit_points", 50000L)
    val today = one(c,
      """SELECT COALESCE(SUM(E.AMOUNT), 0) AS N FROM LEDGER_TX T JOIN LEDGER_ENTRY E ON E.TX_ID = T.ID
         WHERE T.USER_UID = ? AND T.KIND = 'IncentiveCredit' AND E.DIRECTION = 'CR' AND E.ACCOUNT_ID = ?
           AND T.CREATED_AT >= DATEADD('DAY', -1, CURRENT_TIMESTAMP)""",
      userUid, Accounts.user(userUid))(_.getLong("N")).getOrElse(0L)
    if amount > single then
      risk(c, "single_ledger_amount", subject, s"amount=$amount limit=$single")
      setPayoutsEnabled(false, "single ledger amount hard limit", "system")
      Left("risk_limit")
    else if today + amount > growth then
      risk(c, "per_user_point_growth", userUid, s"today=${today + amount} limit=$growth")
      setPayoutsEnabled(false, "point growth hard limit", "system")
      Left("risk_limit")
    else Right(())
  }

  def checkWithdrawalRisk(userUid: String, amount: Long): Either[String, Boolean] = conn { c =>
    if configString(c, "payouts_enabled", "true") != "true" then Left("payouts_disabled")
    else
      val single = configLong(c, "single_payout_limit_points", 50000L)
      val userDayLimit = configLong(c, "per_user_day_payout_limit_points", 100000L)
      val userMonthLimit = configLong(c, "per_user_month_payout_limit_points", 500000L)
      val systemDayLimit = configLong(c, "system_day_payout_limit_points", 10000000L)
      val userDay = one(c, "SELECT COALESCE(SUM(AMOUNT), 0) AS N FROM WITHDRAWAL WHERE USER_UID = ? AND CREATED_AT >= DATEADD('DAY', -1, CURRENT_TIMESTAMP)", userUid)(_.getLong("N")).getOrElse(0L)
      val userMonth = one(c, "SELECT COALESCE(SUM(AMOUNT), 0) AS N FROM WITHDRAWAL WHERE USER_UID = ? AND CREATED_AT >= DATEADD('MONTH', -1, CURRENT_TIMESTAMP)", userUid)(_.getLong("N")).getOrElse(0L)
      val systemDay = one(c, "SELECT COALESCE(SUM(AMOUNT), 0) AS N FROM WITHDRAWAL WHERE CREATED_AT >= DATEADD('DAY', -1, CURRENT_TIMESTAMP)")(_.getLong("N")).getOrElse(0L)
      if amount > single then { risk(c, "single_payout", userUid, s"amount=$amount limit=$single"); Left("risk_limit") }
      else if userDay + amount > userDayLimit then { risk(c, "user_day_payout", userUid, s"amount=${userDay + amount} limit=$userDayLimit"); Left("risk_limit") }
      else if userMonth + amount > userMonthLimit then { risk(c, "user_month_payout", userUid, s"amount=${userMonth + amount} limit=$userMonthLimit"); Left("risk_limit") }
      else if systemDay + amount > systemDayLimit then { risk(c, "system_day_payout", "system", s"amount=${systemDay + amount} limit=$systemDayLimit"); setPayoutsEnabled(false, "system payout hard limit", "system"); Left("risk_limit") }
      else
        val prior = one(c, "SELECT COUNT(*) AS N FROM WITHDRAWAL WHERE USER_UID = ?", userUid)(_.getLong("N")).getOrElse(0L)
        val todayCount = one(c, "SELECT COUNT(*) AS N FROM WITHDRAWAL WHERE USER_UID = ? AND CREATED_AT >= DATEADD('DAY', -1, CURRENT_TIMESTAMP)", userUid)(_.getLong("N")).getOrElse(0L)
        val forceReview = prior == 0L || todayCount >= 2L
        if forceReview then risk(c, "payout_anomaly", userUid, s"first=$prior today=$todayCount")
        Right(forceReview)
  }

  def pendingWithdrawalClearing: Long = conn { c =>
    val live = Set("PendingReview", "Submitted")
    allWithdrawals.filter(w => live.contains(w.status.toString)).map(_.amount).sum
  }

  def recordProviderEvent(eventId: String, withdrawalId: Long, outcome: String): Boolean =
    try
      tx { c => execute(c, "INSERT INTO WITHDRAWAL_PROVIDER_EVENT (EVENT_ID, WITHDRAWAL_ID, OUTCOME) VALUES (?, ?, ?)", eventId, withdrawalId, outcome); () }
      true
    catch case _: SQLException => false

  def webhookSignature(eventId: String, withdrawalId: Long, outcome: String): String = conn { c =>
    val secret = configString(c, "stripe_webhook_secret", "test-secret")
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(SecretKeySpec(secret.getBytes("UTF-8"), "HmacSHA256"))
    mac.doFinal(s"$eventId:$withdrawalId:$outcome".getBytes("UTF-8")).map(b => f"$b%02x").mkString
  }

object Db:
  @volatile private var store: LedgerStore = scala.compiletime.uninitialized
  private val actorLocal = ThreadLocal.withInitial[String](() => "system")
  private val metaLocal = ThreadLocal.withInitial[TxWriteMeta](() => TxWriteMeta())

  private[ledger] def currentActor: String = actorLocal.get()
  private[ledger] def currentTxMeta: TxWriteMeta = metaLocal.get()

  def asActor[A](actor: String)(body: => A): A =
    val old = actorLocal.get()
    actorLocal.set(actor)
    try body finally actorLocal.set(old)

  def withTxMeta[A](meta: TxWriteMeta)(body: => A): A =
    val old = metaLocal.get()
    metaLocal.set(meta)
    try body finally metaLocal.set(old)

  def init(ds: DataSource): Unit = { store = LedgerStore(ds) }
  def insertTx(tx: LedgerTx): Unit = store.insert(tx)
  def txById(id: Long): Option[LedgerTx] = store.byId(id)
  def txBySource(kind: String, id: String): Option[LedgerTx] = store.bySource(kind, id)
  def allTxs: List[LedgerTx] = store.all
  def balanceOf(account: String): Long = store.balanceOf(account)
  def ledgerBalanceOf(account: String): Long = store.ledgerBalanceOf(account)
  def balanceDrifts: List[BalanceDrift] = store.balanceDrifts
  def withdrawalPut(wd: Withdrawal): Unit = store.withdrawalPut(wd)
  def withdrawalById(id: Long): Option[Withdrawal] = store.withdrawalById(id)
  def withdrawalByClientReq(userUid: String, clientRequestId: String): Option[Withdrawal] = store.withdrawalByClientReq(userUid, clientRequestId)
  def allWithdrawals: List[Withdrawal] = store.allWithdrawals
  def pendingWithdrawalClearing: Long = store.pendingWithdrawalClearing
  def proposalPut(p: Proposal): Unit = store.proposalPut(p)
  def proposalById(id: Long): Option[Proposal] = store.proposalById(id)
  def allProposals: List[Proposal] = store.allProposals
  def obligationPut(o: Obligation): Unit = store.obligationPut(o)
  def obligationBySource(kind: String, id: String): Option[Obligation] = store.obligationBySource(kind, id)
  def allOpenObligations: List[Obligation] = store.allOpenObligations
  def configEntries: List[ConfigEntry] = store.configEntries
  def proposeConfig(key: String, value: String, reason: String, proposedBy: String): ConfigProposalRecord = store.proposeConfig(key, value, reason, proposedBy)
  def approveConfig(id: Long, approver: String): Either[String, ConfigProposalRecord] = store.approveConfig(id, approver)
  def rejectConfig(id: Long, rejecter: String): Either[String, ConfigProposalRecord] = store.rejectConfig(id, rejecter)
  def allConfigProposals: List[ConfigProposalRecord] = store.allConfigProposals
  def setPayoutsEnabled(enabled: Boolean, reason: String, actor: String): Unit = store.setPayoutsEnabled(enabled, reason, actor)
  def payoutsEnabled: Boolean = store.payoutsEnabled
  def audit(action: String, actor: String, subject: String, detail: String): Unit = store.audit(action, actor, subject, detail)
  def auditLogs: List[AuditLogRecord] = store.auditLogs
  def risk(kind: String, subject: String, detail: String): Unit = store.risk(kind, subject, detail)
  def riskEvents: List[RiskEventRecord] = store.riskEvents
  def guardLedgerAmount(userUid: String, amount: Long, subject: String): Either[String, Unit] = store.guardLedgerAmount(userUid, amount, subject)
  def checkWithdrawalRisk(userUid: String, amount: Long): Either[String, Boolean] = store.checkWithdrawalRisk(userUid, amount)
  def recordProviderEvent(eventId: String, withdrawalId: Long, outcome: String): Boolean = store.recordProviderEvent(eventId, withdrawalId, outcome)
  def webhookSignature(eventId: String, withdrawalId: Long, outcome: String): String = store.webhookSignature(eventId, withdrawalId, outcome)
