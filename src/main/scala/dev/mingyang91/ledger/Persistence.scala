package dev.mingyang91.ledger

import javax.sql.DataSource
import java.sql.{Connection, SQLException}

import scala.collection.mutable

import com.augustnagro.magnum.*

import dev.mingyang91.ledger.generated.LedgerModel.{LedgerTx, LedgerEntry, TxKind, WithdrawalStatus, ProposalStatus, ObligationStatus, EntryDirection}
import dev.mingyang91.ledger.generated.Withdrawal
import dev.mingyang91.ledger.generated.Proposal
import dev.mingyang91.ledger.generated.Obligation
/* =============================================================================
 * PRODUCTION persistence — the @extern target the GENERATED Jdbc* repositories delegate to.
 * The generated verified core still sees compact case classes; this file maps them onto the
 * richer audit schema. Queries go through Magnum (typed sql"" + DbCodec) so column mapping is
 * a tested, reusable layer rather than per-site rs.getX; the transaction scope is bridged
 * across the @extern seam by a ScopedValue (the verified World is pure, so the connection
 * cannot be passed explicitly). Writes run SERIALIZABLE with a bounded retry; reads autocommit.
 * ========================================================================== */

object Accounts:
  val IncentiveExpense   = "system:incentive_expense"
  val WithdrawalClearing = "system:withdrawal_clearing"
  val Cash               = "system:cash"
  val Adjustment         = "system:adjustment"
  val FeeRecovery       = "system:fee_recovery"
  val ProviderPayoutFee = "system:provider_payout_fee"
  def providerBalance(provider: String): String = s"system:provider_balance:$provider"
  def knownSystem(code: String): Boolean =
    code == IncentiveExpense || code == WithdrawalClearing || code == Cash || code == Adjustment ||
      code == FeeRecovery || code == ProviderPayoutFee || code.startsWith("system:provider_balance:")
  def user(uid: String): String = s"user:$uid"
  def isUser(account: String): Boolean = account.startsWith("user:")
  // Compatibility helper; the authoritative value now lives in ACCOUNT.NORMAL_SIDE.
  def normalSide(account: String): String =
    if isUser(account) || account == WithdrawalClearing || account == Cash || account == FeeRecovery || account.startsWith("system:provider_balance:") then "CR" else "DR"

final case class TxWriteMeta(
    targetTxId: Option[Long] = None,
    withdrawalId: Option[Long] = None,
    proposalId: Option[Long] = None,
    payoutIntentId: Option[Long] = None,
    incentiveTraceId: Option[String] = None,
    incentiveModule: Option[String] = None,
    rawInput: Option[String] = None)

// Read-back records (returned across the Db facade). They derive DbCodec, so a SELECT of the
// matching columns in field order reads them directly.
final case class RiskEventRecord(id: Long, kind: String, subject: String, detail: String) derives DbCodec
final case class AuditLogRecord(id: Long, action: String, actor: String, subject: String, detail: String) derives DbCodec
final case class ConfigEntry(key: String, value: String)
final case class ConfigProposalRecord(id: Long, key: String, value: String, reason: String, status: String, proposedBy: String, approvedBy: Option[String], rejectedBy: Option[String]) derives DbCodec
final case class BalanceDrift(account: String, materialized: Long, replayed: Long) derives DbCodec
final case class AccountRecord(code: String, category: String, normalSide: String, ownerType: Option[String], ownerId: Option[String], postable: Boolean, systemManaged: Boolean) derives DbCodec
final case class PayoutIntentRecord(id: Long, withdrawalId: Long, userUid: String, provider: String, routeCode: String, destinationId: String, grossAmount: Long, quotedProviderFee: Long, expectedRecipientNet: Long, quoteRef: Option[String], providerTransferRef: Option[String]) derives DbCodec
final case class ProviderEventRecord(eventId: String, provider: String, providerTransferRef: Option[String], outcome: String, providerFeeDebited: Option[Long]) derives DbCodec
final case class PayoutReconciliationRecord(payoutIntentId: Long, providerEventId: Option[String], expectedFee: Long, observedFee: Option[Long], result: String, note: Option[String]) derives DbCodec
final case class SummaryBalances(incentiveExpense: Long, providerPayoutFee: Long, adjustment: Long, users: Long, clearing: Long, settlement: Long, revenue: Long) derives DbCodec
final case class SourceCollision(sourceKind: String, sourceId: String) derives DbCodec
final case class PayoutDispatchRecord(payoutIntentId: Long, withdrawalId: Long, provider: String, destinationId: String, amountMinor: Long, currency: String, idempotencyKey: String, status: String, attempts: Int, lastError: Option[String], providerTransferRef: Option[String]) derives DbCodec

final class LedgerStore(ds: DataSource):

  // --- enum <-> VARCHAR codecs: one tested mapping, reused everywhere (no per-site valueOf) ---
  private given DbCodec[TxKind]           = DbCodec[String].biMap(TxKind.valueOf, _.toString)
  private given DbCodec[WithdrawalStatus] = DbCodec[String].biMap(WithdrawalStatus.valueOf, _.toString)
  private given DbCodec[ProposalStatus]   = DbCodec[String].biMap(ProposalStatus.valueOf, _.toString)
  private given DbCodec[ObligationStatus] = DbCodec[String].biMap(ObligationStatus.valueOf, _.toString)
  private given DbCodec[EntryDirection]     = DbCodec[String].biMap(EntryDirection.valueOf, _.toString)

  // Row shapes for multi-column reads (the compact verified types are then assembled from these).
  private case class TxFlatRow(id: Long, kind: TxKind, sourceKind: Option[String], sourceId: Option[String], userUid: String, lineNo: Int, accountId: String, direction: EntryDirection, amount: Long) derives DbCodec
  private case class WithdrawalCurrentRow(id: Long, userUid: String, amount: Long, clientRequestId: String, reserveTxId: Long, status: WithdrawalStatus) derives DbCodec
  private case class ProposalCurrentRow(id: Long, kind: TxKind, userUid: String, debitAccount: String, creditAccount: String, amount: Long, reason: String, proposedBy: String, status: ProposalStatus, resultTxId: Option[Long], targetTxId: Option[Long]) derives DbCodec
  private case class ObligationRow(sourceKind: String, sourceId: String, userUid: String, estimatedPoints: Long, status: ObligationStatus, realizedTxId: Option[Long]) derives DbCodec
  private case class ConfigCurrentRow(key: String, value: String) derives DbCodec
  private case class WithdrawalRiskSnapshot(userDay: Long, userMonth: Long, systemDay: Long, priorCount: Long, todayCount: Long) derives DbCodec
  private case class DayGrowthSnapshot(todayCredits: Long) derives DbCodec

  // --- transaction scope, bridged across the @extern repo seam by a ScopedValue. Magnum's
  // transact/connect own the JDBC connection, commit/rollback, and isolation (SERIALIZABLE via
  // the transactor's connectionConfig); the resulting DbTx/DbCon is bound to `current` so the
  // store methods retrieve it. Nested transaction{}/readOnly{} calls join the open scope. ---
  private val current: ScopedValue[DbCon] = ScopedValue.newInstance()
  private val transactor: Transactor =
    Transactor(dataSource = ds, connectionConfig = c => c.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE))

  private def scoped[A](v: DbCon)(body: => A): A =
    ScopedValue.where(current, v).call(() => body)

  /** Run a write transaction at SERIALIZABLE. A serialization failure (40001) or deadlock
    * (40P01) aborts the transaction; the correct response is to re-run it, which is safe here
    * because every body is DB writes (rolled back) plus pure computation — the only
    * non-transactional effect, nextval, just leaves harmless id gaps. Business errors are
    * deterministic and surface as values, so they are never retried. */
  def transaction[A](body: => A): A =
    if current.isBound then body
    else retryOnSerialization { transact(transactor) { (tx0: DbTx) ?=> scoped(tx0)(body) } }

  private def readOnly[A](body: => A): A =
    if current.isBound then body
    else connect(transactor) { (c0: DbCon) ?=> scoped(c0)(body) }

  // Up to 5 retries (6 attempts total) with a tiny linear backoff. Under hot SERIALIZABLE
  // contention, immediately re-running the same statement can recreate the same pivot/deadlock.
  private def retryOnSerialization[A](body: => A): A =
    def go(attempt: Int): A =
      try body
      catch case e: Throwable if attempt < 5 && isSerializationFailure(e) =>
        java.util.concurrent.locks.LockSupport.parkNanos((attempt.toLong + 1L) * 5_000_000L)
        go(attempt + 1)
    go(0)

  private def isSerializationFailure(t: Throwable): Boolean =
    var c = t
    var found = false
    while c != null && !found do
      c match
        case s: SQLException if s.getSQLState == "40001" || s.getSQLState == "40P01" => found = true
        case _ => c = c.getCause
    found

  /** The ambient Magnum context — a DbTx inside transaction{}, a DbCon inside readOnly{}. */
  private def dbc: DbCon = current.get()

  /** Next ledger entity id from the shared sequence (monotone across restarts; gaps are fine). */
  def nextId(): Long = readOnly { sql"SELECT nextval('LEDGER_ID_SEQ')".query[Long].run()(using dbc).head }

  // --- pure helpers ---
  private def signedDelta(account: String, direction: String, amount: Long): Long =
    if accountNormalSide(account) == direction then amount else -amount

  private def currentActor: String = Db.currentActor
  private final case class AccountSpec(category: String, normalSide: String, ownerType: Option[String], ownerId: Option[String], postable: Boolean = true, systemManaged: Boolean = true)
  private val normalSideCache = java.util.concurrent.ConcurrentHashMap[String, String]()

  private def accountSpec(code: String): AccountSpec =
    if Accounts.isUser(code) then
      AccountSpec("user_liability", "CR", Some("User"), Some(code.stripPrefix("user:")))
    else code match
      case Accounts.IncentiveExpense   => AccountSpec("expense", "DR", Some("System"), Some("incentive"))
      case Accounts.WithdrawalClearing => AccountSpec("clearing", "CR", Some("System"), Some("withdrawal_clearing"))
      case Accounts.Cash               => AccountSpec("settlement", "CR", Some("System"), Some("cash"))
      case Accounts.Adjustment         => AccountSpec("adjustment", "DR", Some("System"), Some("adjustment"))
      case Accounts.FeeRecovery        => AccountSpec("revenue", "CR", Some("System"), Some("fee_recovery"))
      case Accounts.ProviderPayoutFee  => AccountSpec("expense", "DR", Some("System"), Some("provider_payout_fee"))
      case c if c.startsWith("system:provider_balance:") =>
        AccountSpec("settlement", "CR", Some("Provider"), Some(c.stripPrefix("system:provider_balance:")))
      case other => throw IllegalArgumentException(s"unknown ledger account code: $other")

  private def ensureAccount(code: String): Unit =
    val spec = accountSpec(code)
    sql"""INSERT INTO ACCOUNT (CODE, CATEGORY, NORMAL_SIDE, UNIT, OWNER_TYPE, OWNER_ID, POSTABLE, SYSTEM_MANAGED, STATUS)
          VALUES ($code, ${spec.category}, ${spec.normalSide}, 'PTS', ${spec.ownerType}, ${spec.ownerId}, ${spec.postable}, ${spec.systemManaged}, 'active')
          ON CONFLICT (CODE) DO NOTHING"""
      .update.run()(using dbc)
    normalSideCache.put(code, spec.normalSide)

  def accountNormalSide(code: String): String =
    Option(normalSideCache.get(code)).getOrElse {
      def load(): String =
        sql"SELECT NORMAL_SIDE FROM ACCOUNT WHERE CODE = $code".query[String].run()(using dbc).headOption
          .getOrElse(accountSpec(code).normalSide)
      val side = if current.isBound then load() else readOnly { load() }
      normalSideCache.put(code, side)
      side
    }

  private def withdrawalReason(to: String): String = to match
    case "PendingReview" => "user_submitted"
    case "Submitted"     => "admin_approved"
    case "Settled"       => "webhook_settled"
    case "Rejected"      => "admin_rejected"
    case "Cancelled"     => "user_cancelled"
    case "Failed"        => "provider_failed"
    case _               => "status_changed"

  // Atomic upsert: an UPDATE-then-INSERT pair races two concurrent first-writes into a 23505;
  // ON CONFLICT folds both into one statement, so a new account's first delta never throws.
  private def applyBalanceDelta(account: String, delta: Long): Unit =
    sql"""INSERT INTO ACCOUNT_BALANCE (ACCOUNT_ID, BALANCE, UPDATED_AT) VALUES ($account, $delta, CURRENT_TIMESTAMP)
          ON CONFLICT (ACCOUNT_ID) DO UPDATE SET BALANCE = ACCOUNT_BALANCE.BALANCE + EXCLUDED.BALANCE, UPDATED_AT = CURRENT_TIMESTAMP"""
      .update.run()(using dbc)

  private def ensureAccounts(entries: List[LedgerEntry]): Unit =
    val seen = mutable.HashSet.empty[String]
    entries.foreach { entry =>
      if seen.add(entry.account) then ensureAccount(entry.account)
    }

  private def aggregateEntryDeltas(entries: List[LedgerEntry]): mutable.LinkedHashMap[String, Long] =
    val deltas = mutable.LinkedHashMap.empty[String, Long]
    entries.foreach { entry =>
      val delta = signedDelta(entry.account, entry.direction.toString, entry.amount)
      deltas.updateWith(entry.account) {
        case Some(cur) => Some(cur + delta)
        case None      => Some(delta)
      }
    }
    deltas

  private def latestWithdrawalStatus(id: Long): Option[String] =
    sql"SELECT TO_STATUS FROM WITHDRAWAL_STATUS_CHANGE WHERE WITHDRAWAL_ID = $id ORDER BY ID DESC LIMIT 1"
      .query[String].run()(using dbc).headOption

  private def latestProposalStatus(id: Long): Option[(String, Option[Long])] =
    sql"SELECT TO_STATUS, RESULT_TX_ID FROM PROPOSAL_STATUS_CHANGE WHERE PROPOSAL_ID = $id ORDER BY ID DESC LIMIT 1"
      .query[(String, Option[Long])].run()(using dbc).headOption

  // --- ledger txs ---
  private def assembleTxs(rows: List[TxFlatRow]): List[LedgerTx] =
    rows match
      case Nil => Nil
      case first :: rest =>
        val txs = mutable.ListBuffer.empty[LedgerTx]
        val entries = mutable.ListBuffer.empty[LedgerEntry]
        var currentId = first.id
        var currentKind = first.kind
        var currentSourceKind = first.sourceKind
        var currentSourceId = first.sourceId
        var currentUserUid = first.userUid
        entries += LedgerEntry(first.accountId, first.direction, first.amount)

        def flush(): Unit =
          txs += LedgerTx(currentId, currentKind, entries.toList, currentSourceKind, currentSourceId, currentUserUid)

        rest.foreach { row =>
          if row.id != currentId then
            flush()
            entries.clear()
            currentId = row.id
            currentKind = row.kind
            currentSourceKind = row.sourceKind
            currentSourceId = row.sourceId
            currentUserUid = row.userUid
          entries += LedgerEntry(row.accountId, row.direction, row.amount)
        }
        flush()
        txs.toList

  def insert(t: LedgerTx): Unit = transaction {
    val meta = Db.currentTxMeta
    ensureAccounts(t.entries)
    sql"""INSERT INTO LEDGER_TX
          (ID, KIND, SOURCE_KIND, SOURCE_ID, USER_UID, TARGET_TX_ID, WITHDRAWAL_ID, PROPOSAL_ID, PAYOUT_INTENT_ID, INCENTIVE_TRACE_ID, INCENTIVE_MODULE, RAW_INPUT)
          VALUES (${t.id}, ${t.kind}, ${t.sourceKind}, ${t.sourceId}, ${t.userUid}, ${meta.targetTxId}, ${meta.withdrawalId}, ${meta.proposalId}, ${meta.payoutIntentId}, ${meta.incentiveTraceId}, ${meta.incentiveModule}, ${meta.rawInput})"""
      .update.run()(using dbc)

    val ps = dbc.connection.prepareStatement("INSERT INTO LEDGER_ENTRY (TX_ID, LINE_NO, ACCOUNT_ID, DIRECTION, AMOUNT) VALUES (?, ?, ?, ?, ?)")
    try
      t.entries.zipWithIndex.foreach { case (entry, idx) =>
        ps.setLong(1, t.id)
        ps.setInt(2, idx + 1)
        ps.setString(3, entry.account)
        ps.setString(4, entry.direction.toString)
        ps.setLong(5, entry.amount)
        ps.addBatch()
      }
      ps.executeBatch()
    finally ps.close()

    aggregateEntryDeltas(t.entries).foreach { case (account, delta) =>
      if delta != 0L then applyBalanceDelta(account, delta)
    }
  }

  def byId(id: Long): Option[LedgerTx] = readOnly {
    assembleTxs(
      sql"""SELECT T.ID, T.KIND, T.SOURCE_KIND, T.SOURCE_ID, T.USER_UID, E.LINE_NO, E.ACCOUNT_ID, E.DIRECTION, E.AMOUNT
            FROM LEDGER_TX T
            JOIN LEDGER_ENTRY E ON E.TX_ID = T.ID
           WHERE T.ID = $id
           ORDER BY T.ID, E.LINE_NO"""
        .query[TxFlatRow].run()(using dbc).toList
    ).headOption
  }

  def bySource(kind: String, id: String): Option[LedgerTx] = readOnly {
    assembleTxs(
      sql"""SELECT T.ID, T.KIND, T.SOURCE_KIND, T.SOURCE_ID, T.USER_UID, E.LINE_NO, E.ACCOUNT_ID, E.DIRECTION, E.AMOUNT
            FROM LEDGER_TX T
            JOIN LEDGER_ENTRY E ON E.TX_ID = T.ID
           WHERE T.SOURCE_KIND = $kind AND T.SOURCE_ID = $id
           ORDER BY T.ID, E.LINE_NO"""
        .query[TxFlatRow].run()(using dbc).toList
    ).headOption
  }

  def all: List[LedgerTx] = readOnly {
    assembleTxs(
      sql"""SELECT T.ID, T.KIND, T.SOURCE_KIND, T.SOURCE_ID, T.USER_UID, E.LINE_NO, E.ACCOUNT_ID, E.DIRECTION, E.AMOUNT
            FROM LEDGER_TX T
            JOIN LEDGER_ENTRY E ON E.TX_ID = T.ID
           ORDER BY T.ID, E.LINE_NO"""
        .query[TxFlatRow].run()(using dbc).toList
    )
  }

  def txsForAccount(account: String): List[LedgerTx] = readOnly {
    assembleTxs(
      sql"""SELECT T.ID, T.KIND, T.SOURCE_KIND, T.SOURCE_ID, T.USER_UID, E.LINE_NO, E.ACCOUNT_ID, E.DIRECTION, E.AMOUNT
            FROM LEDGER_TX T
            JOIN LEDGER_ENTRY E ON E.TX_ID = T.ID
           WHERE EXISTS (
             SELECT 1
               FROM LEDGER_ENTRY U
              WHERE U.TX_ID = T.ID
                AND U.ACCOUNT_ID = $account
           )
           ORDER BY T.ID, E.LINE_NO"""
        .query[TxFlatRow].run()(using dbc).toList
    )
  }

  // ---- PAGED READS (HTTP-facing): keyset pagination by DESCENDING id, bounded to `limit` rows.
  // The unbounded reads above are kept for the in-process drift gate and the verified-service path,
  // which compare full state over small test data. Production HTTP must never materialize a whole
  // table, so every list endpoint reads through one of these instead. `before` is the id of the last
  // (smallest) row of the previous page; None asks for the newest page. Pages are newest-first.
  private def kWhere(before: Option[Long]): SqlLiteral =
    before.map(b => SqlLiteral(s" WHERE ID < $b ")).getOrElse(SqlLiteral(" "))
  private def kAnd(before: Option[Long]): SqlLiteral =
    before.map(b => SqlLiteral(s" AND ID < $b ")).getOrElse(SqlLiteral(" "))

  def txPage(limit: Int, before: Option[Long]): List[LedgerTx] = readOnly {
    assembleTxs(
      sql"""SELECT T.ID, T.KIND, T.SOURCE_KIND, T.SOURCE_ID, T.USER_UID, E.LINE_NO, E.ACCOUNT_ID, E.DIRECTION, E.AMOUNT
            FROM LEDGER_TX T
            JOIN LEDGER_ENTRY E ON E.TX_ID = T.ID
           WHERE T.ID IN (SELECT ID FROM LEDGER_TX ${kWhere(before)} ORDER BY ID DESC LIMIT $limit)
           ORDER BY T.ID DESC, E.LINE_NO"""
        .query[TxFlatRow].run()(using dbc).toList
    )
  }

  def txsForAccountPage(account: String, limit: Int, before: Option[Long]): List[LedgerTx] = readOnly {
    assembleTxs(
      sql"""SELECT T.ID, T.KIND, T.SOURCE_KIND, T.SOURCE_ID, T.USER_UID, E.LINE_NO, E.ACCOUNT_ID, E.DIRECTION, E.AMOUNT
            FROM LEDGER_TX T
            JOIN LEDGER_ENTRY E ON E.TX_ID = T.ID
           WHERE T.ID IN (
             SELECT ID FROM LEDGER_TX
              WHERE EXISTS (SELECT 1 FROM LEDGER_ENTRY U WHERE U.TX_ID = LEDGER_TX.ID AND U.ACCOUNT_ID = $account)
              ${kAnd(before)}
              ORDER BY ID DESC LIMIT $limit)
           ORDER BY T.ID DESC, E.LINE_NO"""
        .query[TxFlatRow].run()(using dbc).toList
    )
  }

  def withdrawalsPage(limit: Int, before: Option[Long]): List[Withdrawal] = readOnly {
    sql"""SELECT W.ID, W.USER_UID, W.AMOUNT, W.CLIENT_REQUEST_ID, W.RESERVE_TX_ID,
                 COALESCE(S.TO_STATUS, 'PendingReview')
            FROM WITHDRAWAL W
            LEFT JOIN LATERAL (
              SELECT TO_STATUS FROM WITHDRAWAL_STATUS_CHANGE WHERE WITHDRAWAL_ID = W.ID ORDER BY ID DESC LIMIT 1
            ) S ON TRUE
           WHERE W.ID IN (SELECT ID FROM WITHDRAWAL ${kWhere(before)} ORDER BY ID DESC LIMIT $limit)
           ORDER BY W.ID DESC"""
      .query[WithdrawalCurrentRow].run()(using dbc).toList.map(toWithdrawal)
  }

  def withdrawalsByUserPage(userUid: String, limit: Int, before: Option[Long]): List[Withdrawal] = readOnly {
    sql"""SELECT W.ID, W.USER_UID, W.AMOUNT, W.CLIENT_REQUEST_ID, W.RESERVE_TX_ID,
                 COALESCE(S.TO_STATUS, 'PendingReview')
            FROM WITHDRAWAL W
            LEFT JOIN LATERAL (
              SELECT TO_STATUS FROM WITHDRAWAL_STATUS_CHANGE WHERE WITHDRAWAL_ID = W.ID ORDER BY ID DESC LIMIT 1
            ) S ON TRUE
           WHERE W.ID IN (SELECT ID FROM WITHDRAWAL WHERE USER_UID = $userUid ${kAnd(before)} ORDER BY ID DESC LIMIT $limit)
           ORDER BY W.ID DESC"""
      .query[WithdrawalCurrentRow].run()(using dbc).toList.map(toWithdrawal)
  }

  def proposalsByKindPage(kind: TxKind, limit: Int, before: Option[Long]): List[Proposal] = readOnly {
    sql"""SELECT P.ID, P.KIND, P.USER_UID, P.DEBIT_ACCOUNT, P.CREDIT_ACCOUNT, P.AMOUNT, P.REASON, P.PROPOSED_BY,
                 COALESCE(S.TO_STATUS, 'PendingReview'), S.RESULT_TX_ID, P.TARGET_TX_ID
            FROM PROPOSAL P
            LEFT JOIN LATERAL (
              SELECT TO_STATUS, RESULT_TX_ID FROM PROPOSAL_STATUS_CHANGE WHERE PROPOSAL_ID = P.ID ORDER BY ID DESC LIMIT 1
            ) S ON TRUE
           WHERE P.ID IN (SELECT ID FROM PROPOSAL WHERE KIND = $kind ${kAnd(before)} ORDER BY ID DESC LIMIT $limit)
           ORDER BY P.ID DESC"""
      .query[ProposalCurrentRow].run()(using dbc).toList.map(toProposal)
  }

  def riskEventsPage(limit: Int, before: Option[Long]): List[RiskEventRecord] = readOnly {
    sql"SELECT ID, KIND, SUBJECT, DETAIL FROM RISK_EVENT ${kWhere(before)} ORDER BY ID DESC LIMIT $limit"
      .query[RiskEventRecord].run()(using dbc).toList
  }

  def auditLogsPage(limit: Int, before: Option[Long]): List[AuditLogRecord] = readOnly {
    sql"SELECT ID, ACTION, ACTOR, SUBJECT, DETAIL FROM AUDIT_LOG ${kWhere(before)} ORDER BY ID DESC LIMIT $limit"
      .query[AuditLogRecord].run()(using dbc).toList
  }

  def configProposalsPage(limit: Int, before: Option[Long]): List[ConfigProposalRecord] = readOnly {
    sql"SELECT ID, CONFIG_KEY, CONFIG_VALUE, REASON, STATUS, PROPOSED_BY, APPROVED_BY, REJECTED_BY FROM SYSTEM_CONFIG_PROPOSAL ${kWhere(before)} ORDER BY ID DESC LIMIT $limit"
      .query[ConfigProposalRecord].run()(using dbc).toList
  }

  def balanceOf(account: String): Long = readOnly {
    sql"SELECT BALANCE FROM ACCOUNT_BALANCE WHERE ACCOUNT_ID = $account".query[Long].run()(using dbc).headOption.getOrElse(0L)
  }

  def categoryBalance(category: String): Long = readOnly {
    sql"""SELECT COALESCE(SUM(B.BALANCE), 0)
          FROM ACCOUNT_BALANCE B
          JOIN ACCOUNT A ON A.CODE = B.ACCOUNT_ID
         WHERE A.CATEGORY = $category"""
      .query[Long].run()(using dbc).head
  }

  def summaryBalances: SummaryBalances = readOnly {
    sql"""SELECT
            COALESCE(SUM(CASE WHEN A.CODE = ${Accounts.IncentiveExpense} THEN B.BALANCE ELSE 0 END), 0),
            COALESCE(SUM(CASE WHEN A.CODE = ${Accounts.ProviderPayoutFee} THEN B.BALANCE ELSE 0 END), 0),
            COALESCE(SUM(CASE WHEN A.CATEGORY = 'adjustment' THEN B.BALANCE ELSE 0 END), 0),
            COALESCE(SUM(CASE WHEN A.CATEGORY = 'user_liability' THEN B.BALANCE ELSE 0 END), 0),
            COALESCE(SUM(CASE WHEN A.CATEGORY = 'clearing' THEN B.BALANCE ELSE 0 END), 0),
            COALESCE(SUM(CASE WHEN A.CATEGORY = 'settlement' THEN B.BALANCE ELSE 0 END), 0),
            COALESCE(SUM(CASE WHEN A.CATEGORY = 'revenue' THEN B.BALANCE ELSE 0 END), 0)
          FROM ACCOUNT A
          LEFT JOIN ACCOUNT_BALANCE B ON B.ACCOUNT_ID = A.CODE"""
      .query[SummaryBalances].run()(using dbc).head
  }

  def ledgerBalanceOf(account: String): Long = readOnly {
    val ns = accountNormalSide(account)
    sql"SELECT COALESCE(SUM(CASE WHEN DIRECTION = $ns THEN AMOUNT ELSE -AMOUNT END), 0) FROM LEDGER_ENTRY WHERE ACCOUNT_ID = $account"
      .query[Long].run()(using dbc).head
  }

  /** Lock a user's materialized ACCOUNT_BALANCE row so concurrent debits serialize; the
    * sufficiency check must then read `balanceOf` (the locked row), not the LEDGER_ENTRY sum.
    * A no-op when the row is absent (zero balance, so any positive debit fails anyway).
    * Requires an open transaction or the lock would release immediately. */
  def lockUserBalance(account: String): Unit =
    require(current.isBound, "lockUserBalance must be called inside transaction { … }")
    sql"SELECT 1 FROM ACCOUNT_BALANCE WHERE ACCOUNT_ID = $account FOR UPDATE".query[Int].run()(using dbc)
    ()

  // --- payout intents / provider settlement / reconciliation ---
  def payoutIntentPut(p: PayoutIntentRecord): Unit = transaction {
    sql"""INSERT INTO PAYOUT_INTENT
          (ID, WITHDRAWAL_ID, USER_UID, PROVIDER, ROUTE_CODE, DESTINATION_ID, GROSS_AMOUNT, QUOTED_PROVIDER_FEE, EXPECTED_RECIPIENT_NET, QUOTE_REF, PROVIDER_TRANSFER_REF)
          VALUES (${p.id}, ${p.withdrawalId}, ${p.userUid}, ${p.provider}, ${p.routeCode}, ${p.destinationId}, ${p.grossAmount}, ${p.quotedProviderFee}, ${p.expectedRecipientNet}, ${p.quoteRef}, ${p.providerTransferRef})"""
      .update.run()(using dbc)
  }

  def payoutIntentByWithdrawal(withdrawalId: Long): Option[PayoutIntentRecord] = readOnly {
    sql"""SELECT ID, WITHDRAWAL_ID, USER_UID, PROVIDER, ROUTE_CODE, DESTINATION_ID, GROSS_AMOUNT, QUOTED_PROVIDER_FEE, EXPECTED_RECIPIENT_NET, QUOTE_REF, PROVIDER_TRANSFER_REF
          FROM PAYOUT_INTENT
         WHERE WITHDRAWAL_ID = $withdrawalId"""
      .query[PayoutIntentRecord].run()(using dbc).headOption
  }

  def payoutReconciliationPut(r: PayoutReconciliationRecord): Unit = transaction {
    sql"""INSERT INTO PAYOUT_RECONCILIATION
          (PAYOUT_INTENT_ID, PROVIDER_EVENT_ID, EXPECTED_FEE, OBSERVED_FEE, RESULT, NOTE)
          VALUES (${r.payoutIntentId}, ${r.providerEventId}, ${r.expectedFee}, ${r.observedFee}, ${r.result}, ${r.note})"""
      .update.run()(using dbc)
  }

  def payoutReconciliationByWithdrawal(withdrawalId: Long): Option[PayoutReconciliationRecord] = readOnly {
    sql"""SELECT R.PAYOUT_INTENT_ID, R.PROVIDER_EVENT_ID, R.EXPECTED_FEE, R.OBSERVED_FEE, R.RESULT, R.NOTE
          FROM PAYOUT_RECONCILIATION R
          JOIN PAYOUT_INTENT P ON P.ID = R.PAYOUT_INTENT_ID
         WHERE P.WITHDRAWAL_ID = $withdrawalId"""
      .query[PayoutReconciliationRecord].run()(using dbc).headOption
  }

  def providerEventsByWithdrawal(withdrawalId: Long): List[ProviderEventRecord] = readOnly {
    sql"""SELECT EVENT_ID, PROVIDER, PROVIDER_TRANSFER_REF, OUTCOME, PROVIDER_FEE_DEBITED
          FROM WITHDRAWAL_PROVIDER_EVENT
         WHERE WITHDRAWAL_ID = $withdrawalId
         ORDER BY ID"""
      .query[ProviderEventRecord].run()(using dbc).toList
  }

  // --- payout dispatch outbox (recipient-pays-fee: created at submit, drained by PayoutDispatcher) ---
  def payoutDispatchPut(d: PayoutDispatchRecord): Unit = transaction {
    sql"""INSERT INTO PAYOUT_DISPATCH
          (PAYOUT_INTENT_ID, WITHDRAWAL_ID, PROVIDER, DESTINATION_ID, AMOUNT_MINOR, CURRENCY, IDEMPOTENCY_KEY, STATUS, ATTEMPTS, LAST_ERROR, PROVIDER_TRANSFER_REF)
          VALUES (${d.payoutIntentId}, ${d.withdrawalId}, ${d.provider}, ${d.destinationId}, ${d.amountMinor}, ${d.currency}, ${d.idempotencyKey}, ${d.status}, ${d.attempts}, ${d.lastError}, ${d.providerTransferRef})"""
      .update.run()(using dbc)
  }

  def pendingDispatches(limit: Int): List[PayoutDispatchRecord] = readOnly {
    sql"""SELECT PAYOUT_INTENT_ID, WITHDRAWAL_ID, PROVIDER, DESTINATION_ID, AMOUNT_MINOR, CURRENCY, IDEMPOTENCY_KEY, STATUS, ATTEMPTS, LAST_ERROR, PROVIDER_TRANSFER_REF
          FROM PAYOUT_DISPATCH
         WHERE STATUS = 'pending'
         ORDER BY CREATED_AT, PAYOUT_INTENT_ID
         LIMIT $limit"""
      .query[PayoutDispatchRecord].run()(using dbc).toList
  }

  def claimPendingDispatches(limit: Int): List[PayoutDispatchRecord] = transaction {
    sql"""WITH picked AS (
            SELECT PAYOUT_INTENT_ID
              FROM PAYOUT_DISPATCH
             WHERE STATUS = 'pending'
             ORDER BY CREATED_AT, PAYOUT_INTENT_ID
             LIMIT $limit
             FOR UPDATE SKIP LOCKED
          )
          UPDATE PAYOUT_DISPATCH D
             SET STATUS = 'inflight',
                 UPDATED_AT = CURRENT_TIMESTAMP
            FROM picked
           WHERE D.PAYOUT_INTENT_ID = picked.PAYOUT_INTENT_ID
       RETURNING D.PAYOUT_INTENT_ID, D.WITHDRAWAL_ID, D.PROVIDER, D.DESTINATION_ID, D.AMOUNT_MINOR, D.CURRENCY,
                 D.IDEMPOTENCY_KEY, D.STATUS, D.ATTEMPTS, D.LAST_ERROR, D.PROVIDER_TRANSFER_REF"""
      .query[PayoutDispatchRecord].run()(using dbc).toList
  }

  def dispatchByWithdrawal(withdrawalId: Long): Option[PayoutDispatchRecord] = readOnly {
    sql"""SELECT PAYOUT_INTENT_ID, WITHDRAWAL_ID, PROVIDER, DESTINATION_ID, AMOUNT_MINOR, CURRENCY, IDEMPOTENCY_KEY, STATUS, ATTEMPTS, LAST_ERROR, PROVIDER_TRANSFER_REF
          FROM PAYOUT_DISPATCH
         WHERE WITHDRAWAL_ID = $withdrawalId"""
      .query[PayoutDispatchRecord].run()(using dbc).headOption
  }

  /** Mark dispatched and record the transfer ref (also onto PAYOUT_INTENT, so the inbound webhook
    * can resolve the withdrawal by transfer ref when event metadata is absent). Guarded by
    * STATUS='inflight' so a duplicate completion is a harmless no-op. */
  def markDispatched(payoutIntentId: Long, providerTransferRef: String): Unit = transaction {
    sql"""UPDATE PAYOUT_DISPATCH
             SET STATUS = 'dispatched',
                 PROVIDER_TRANSFER_REF = $providerTransferRef,
                 ATTEMPTS = ATTEMPTS + 1,
                 UPDATED_AT = CURRENT_TIMESTAMP
           WHERE PAYOUT_INTENT_ID = $payoutIntentId
             AND STATUS = 'inflight'""".update.run()(using dbc)
    sql"""UPDATE PAYOUT_INTENT
             SET PROVIDER_TRANSFER_REF = $providerTransferRef
           WHERE ID = $payoutIntentId
             AND PROVIDER_TRANSFER_REF IS NULL""".update.run()(using dbc)
  }

  def markDispatchFailed(payoutIntentId: Long, newStatus: String, error: String): Unit = transaction {
    sql"""UPDATE PAYOUT_DISPATCH
             SET STATUS = $newStatus,
                 ATTEMPTS = ATTEMPTS + 1,
                 LAST_ERROR = $error,
                 UPDATED_AT = CURRENT_TIMESTAMP
           WHERE PAYOUT_INTENT_ID = $payoutIntentId
             AND STATUS = 'inflight'""".update.run()(using dbc)
  }

  /** Reaper (B1): reclaim dispatch rows wedged in 'inflight' past `timeoutSeconds` — a crash between
    * claim and completion — back to 'pending', so the next tick re-dispatches under the same
    * Idempotency-Key (safe, never double-pays). Mirrors the verified PayoutLifecycleProofs.reclaim
    * decision exactly: the SQL `now - updated_at > timeout` is the proof's opaque `stale` guard, and
    * ATTEMPTS is deliberately left unchanged (a reclaim is not a failure). Returns rows reclaimed. */
  def reclaimStaleInflight(timeoutSeconds: Int): Int = transaction {
    sql"""UPDATE PAYOUT_DISPATCH
             SET STATUS = 'pending',
                 UPDATED_AT = CURRENT_TIMESTAMP
           WHERE STATUS = 'inflight'
             AND UPDATED_AT < CURRENT_TIMESTAMP - make_interval(secs => $timeoutSeconds)""".update.run()(using dbc)
  }

  def withdrawalIdByProviderTransferRef(ref: String): Option[Long] = readOnly {
    sql"""SELECT WITHDRAWAL_ID
            FROM (
              SELECT WITHDRAWAL_ID, 1 AS ORD
                FROM PAYOUT_INTENT
               WHERE PROVIDER_TRANSFER_REF = $ref
              UNION ALL
              SELECT WITHDRAWAL_ID, 2 AS ORD
                FROM PAYOUT_DISPATCH
               WHERE PROVIDER_TRANSFER_REF = $ref
            ) X
           ORDER BY ORD
           LIMIT 1"""
      .query[Long].run()(using dbc).headOption
  }

  /** Lock a LEDGER_TX row (append-only, so FOR UPDATE is allowed) to serialize concurrent
    * rollback-reversal proposals for the same target, making the AlreadyReversed check
    * race-free. Requires an open transaction. */
  def lockTransaction(id: Long): Unit =
    require(current.isBound, "lockTransaction must be called inside transaction { … }")
    sql"SELECT 1 FROM LEDGER_TX WHERE ID = $id FOR UPDATE".query[Int].run()(using dbc)
    ()

  def balanceDrifts: List[BalanceDrift] = readOnly {
    sql"""WITH replayed AS (
            SELECT E.ACCOUNT_ID,
                   COALESCE(SUM(CASE WHEN E.DIRECTION = A.NORMAL_SIDE THEN E.AMOUNT ELSE -E.AMOUNT END), 0) AS REPLAYED
              FROM LEDGER_ENTRY E
              JOIN ACCOUNT A ON A.CODE = E.ACCOUNT_ID
             GROUP BY E.ACCOUNT_ID
          ),
          materialized AS (
            SELECT ACCOUNT_ID, BALANCE AS MATERIALIZED
              FROM ACCOUNT_BALANCE
          )
          SELECT COALESCE(M.ACCOUNT_ID, R.ACCOUNT_ID),
                 COALESCE(M.MATERIALIZED, 0),
                 COALESCE(R.REPLAYED, 0)
            FROM materialized M
            FULL OUTER JOIN replayed R ON R.ACCOUNT_ID = M.ACCOUNT_ID
           WHERE COALESCE(M.MATERIALIZED, 0) <> COALESCE(R.REPLAYED, 0)
           ORDER BY COALESCE(M.ACCOUNT_ID, R.ACCOUNT_ID)"""
      .query[BalanceDrift].run()(using dbc).toList
  }

  private def toWithdrawal(row: WithdrawalCurrentRow): Withdrawal =
    Withdrawal(row.id, row.userUid, row.amount, row.status, row.clientRequestId, row.reserveTxId)

  // --- withdrawals ---
  def withdrawalPut(wd: Withdrawal): Unit = transaction {
    val exists = sql"SELECT ID FROM WITHDRAWAL WHERE ID = ${wd.id}".query[Long].run()(using dbc).nonEmpty
    if !exists then
      sql"INSERT INTO WITHDRAWAL (ID, USER_UID, AMOUNT, CLIENT_REQUEST_ID, RESERVE_TX_ID) VALUES (${wd.id}, ${wd.userUid}, ${wd.amount}, ${wd.clientRequestId}, ${wd.reserveTxId})"
        .update.run()(using dbc)
    val from = latestWithdrawalStatus(wd.id)
    val to = wd.status.toString
    if from.forall(_ != to) then
      sql"INSERT INTO WITHDRAWAL_STATUS_CHANGE (WITHDRAWAL_ID, FROM_STATUS, TO_STATUS, REASON, ACTOR) VALUES (${wd.id}, $from, $to, ${withdrawalReason(to)}, $currentActor)"
        .update.run()(using dbc)
  }

  def withdrawalById(id: Long): Option[Withdrawal] = readOnly {
    sql"""SELECT W.ID, W.USER_UID, W.AMOUNT, W.CLIENT_REQUEST_ID, W.RESERVE_TX_ID,
                 COALESCE(S.TO_STATUS, 'PendingReview')
            FROM WITHDRAWAL W
            LEFT JOIN LATERAL (
              SELECT TO_STATUS
                FROM WITHDRAWAL_STATUS_CHANGE
               WHERE WITHDRAWAL_ID = W.ID
               ORDER BY ID DESC
               LIMIT 1
            ) S ON TRUE
           WHERE W.ID = $id"""
      .query[WithdrawalCurrentRow].run()(using dbc).headOption.map(toWithdrawal)
  }

  def withdrawalByClientReq(userUid: String, clientRequestId: String): Option[Withdrawal] = readOnly {
    sql"""SELECT W.ID, W.USER_UID, W.AMOUNT, W.CLIENT_REQUEST_ID, W.RESERVE_TX_ID,
                 COALESCE(S.TO_STATUS, 'PendingReview')
            FROM WITHDRAWAL W
            LEFT JOIN LATERAL (
              SELECT TO_STATUS
                FROM WITHDRAWAL_STATUS_CHANGE
               WHERE WITHDRAWAL_ID = W.ID
               ORDER BY ID DESC
               LIMIT 1
            ) S ON TRUE
           WHERE W.USER_UID = $userUid
             AND W.CLIENT_REQUEST_ID = $clientRequestId"""
      .query[WithdrawalCurrentRow].run()(using dbc).headOption.map(toWithdrawal)
  }

  def allWithdrawals: List[Withdrawal] = readOnly {
    sql"""SELECT W.ID, W.USER_UID, W.AMOUNT, W.CLIENT_REQUEST_ID, W.RESERVE_TX_ID,
                 COALESCE(S.TO_STATUS, 'PendingReview')
            FROM WITHDRAWAL W
            LEFT JOIN LATERAL (
              SELECT TO_STATUS
                FROM WITHDRAWAL_STATUS_CHANGE
               WHERE WITHDRAWAL_ID = W.ID
               ORDER BY ID DESC
               LIMIT 1
            ) S ON TRUE
           ORDER BY W.ID"""
      .query[WithdrawalCurrentRow].run()(using dbc).toList.map(toWithdrawal)
  }

  def withdrawalsByUser(userUid: String): List[Withdrawal] = readOnly {
    sql"""SELECT W.ID, W.USER_UID, W.AMOUNT, W.CLIENT_REQUEST_ID, W.RESERVE_TX_ID,
                 COALESCE(S.TO_STATUS, 'PendingReview')
            FROM WITHDRAWAL W
            LEFT JOIN LATERAL (
              SELECT TO_STATUS
                FROM WITHDRAWAL_STATUS_CHANGE
               WHERE WITHDRAWAL_ID = W.ID
               ORDER BY ID DESC
               LIMIT 1
            ) S ON TRUE
           WHERE W.USER_UID = $userUid
           ORDER BY W.ID"""
      .query[WithdrawalCurrentRow].run()(using dbc).toList.map(toWithdrawal)
  }

  private def toProposal(row: ProposalCurrentRow): Proposal =
    Proposal(row.id, row.kind, row.userUid, row.debitAccount, row.creditAccount, row.amount, row.reason, row.proposedBy, row.status, row.resultTxId, row.targetTxId)

  // --- proposals ---
  def proposalPut(p: Proposal): Unit = transaction {
    val exists = sql"SELECT ID FROM PROPOSAL WHERE ID = ${p.id}".query[Long].run()(using dbc).nonEmpty
    if !exists then
      sql"""INSERT INTO PROPOSAL (ID, KIND, USER_UID, DEBIT_ACCOUNT, CREDIT_ACCOUNT, AMOUNT, REASON, PROPOSED_BY, TARGET_TX_ID)
            VALUES (${p.id}, ${p.kind}, ${p.userUid}, ${p.debitAccount}, ${p.creditAccount}, ${p.amount}, ${p.reason}, ${p.proposedBy}, ${p.targetTxId})"""
        .update.run()(using dbc)
    val from = latestProposalStatus(p.id).map(_._1)
    val to = p.status.toString
    if from.forall(_ != to) then
      sql"INSERT INTO PROPOSAL_STATUS_CHANGE (PROPOSAL_ID, FROM_STATUS, TO_STATUS, RESULT_TX_ID, ACTOR) VALUES (${p.id}, $from, $to, ${p.resultTxId}, $currentActor)"
        .update.run()(using dbc)
  }

  def proposalById(id: Long): Option[Proposal] = readOnly {
    sql"""SELECT P.ID, P.KIND, P.USER_UID, P.DEBIT_ACCOUNT, P.CREDIT_ACCOUNT, P.AMOUNT, P.REASON, P.PROPOSED_BY,
                 COALESCE(S.TO_STATUS, 'PendingReview'), S.RESULT_TX_ID, P.TARGET_TX_ID
            FROM PROPOSAL P
            LEFT JOIN LATERAL (
              SELECT TO_STATUS, RESULT_TX_ID
                FROM PROPOSAL_STATUS_CHANGE
               WHERE PROPOSAL_ID = P.ID
               ORDER BY ID DESC
               LIMIT 1
            ) S ON TRUE
           WHERE P.ID = $id"""
      .query[ProposalCurrentRow].run()(using dbc).headOption.map(toProposal)
  }

  def allProposals: List[Proposal] = readOnly {
    sql"""SELECT P.ID, P.KIND, P.USER_UID, P.DEBIT_ACCOUNT, P.CREDIT_ACCOUNT, P.AMOUNT, P.REASON, P.PROPOSED_BY,
                 COALESCE(S.TO_STATUS, 'PendingReview'), S.RESULT_TX_ID, P.TARGET_TX_ID
            FROM PROPOSAL P
            LEFT JOIN LATERAL (
              SELECT TO_STATUS, RESULT_TX_ID
                FROM PROPOSAL_STATUS_CHANGE
               WHERE PROPOSAL_ID = P.ID
               ORDER BY ID DESC
               LIMIT 1
            ) S ON TRUE
           ORDER BY P.ID"""
      .query[ProposalCurrentRow].run()(using dbc).toList.map(toProposal)
  }

  def proposalsByKind(kind: TxKind): List[Proposal] = readOnly {
    sql"""SELECT P.ID, P.KIND, P.USER_UID, P.DEBIT_ACCOUNT, P.CREDIT_ACCOUNT, P.AMOUNT, P.REASON, P.PROPOSED_BY,
                 COALESCE(S.TO_STATUS, 'PendingReview'), S.RESULT_TX_ID, P.TARGET_TX_ID
            FROM PROPOSAL P
            LEFT JOIN LATERAL (
              SELECT TO_STATUS, RESULT_TX_ID
                FROM PROPOSAL_STATUS_CHANGE
               WHERE PROPOSAL_ID = P.ID
               ORDER BY ID DESC
               LIMIT 1
            ) S ON TRUE
           WHERE P.KIND = $kind
           ORDER BY P.ID"""
      .query[ProposalCurrentRow].run()(using dbc).toList.map(toProposal)
  }

  // --- obligations ---
  def obligationPut(o: Obligation): Unit = transaction {
    sql"DELETE FROM OBLIGATION WHERE SOURCE_KIND = ${o.sourceKind} AND SOURCE_ID = ${o.sourceId}".update.run()(using dbc)
    sql"""INSERT INTO OBLIGATION (SOURCE_KIND, SOURCE_ID, USER_UID, ESTIMATED_POINTS, STATUS, REALIZED_TX_ID)
          VALUES (${o.sourceKind}, ${o.sourceId}, ${o.userUid}, ${o.estimatedUnit}, ${o.status}, ${o.realizedTxId})"""
      .update.run()(using dbc)
  }

  private def toObligation(r: ObligationRow): Obligation =
    Obligation(r.sourceKind, r.sourceId, r.userUid, "", "", "", r.estimatedPoints, r.status, r.realizedTxId)

  def obligationBySource(kind: String, id: String): Option[Obligation] = readOnly {
    sql"SELECT SOURCE_KIND, SOURCE_ID, USER_UID, ESTIMATED_POINTS, STATUS, REALIZED_TX_ID FROM OBLIGATION WHERE SOURCE_KIND = $kind AND SOURCE_ID = $id"
      .query[ObligationRow].run()(using dbc).headOption.map(toObligation)
  }

  def allOpenObligations: List[Obligation] = readOnly {
    sql"SELECT SOURCE_KIND, SOURCE_ID, USER_UID, ESTIMATED_POINTS, STATUS, REALIZED_TX_ID FROM OBLIGATION WHERE STATUS = 'Open'"
      .query[ObligationRow].run()(using dbc).toList.map(toObligation)
  }

  // --- config / risk / audit ---
  private final case class WithdrawalRiskConfig(payoutsEnabled: Boolean, singleLimit: Long, userDayLimit: Long, userMonthLimit: Long, systemDayLimit: Long)
  private final case class DayGrowthConfig(singleLedgerLimit: Long, dayGrowthLimit: Long)

  private def configString0(key: String, default: String): String =
    sql"SELECT CONFIG_VALUE FROM SYSTEM_CONFIG_CHANGE WHERE CONFIG_KEY = $key AND APPROVED_BY IS NOT NULL ORDER BY EFFECTIVE_AT DESC, ID DESC LIMIT 1"
      .query[String].run()(using dbc).headOption.getOrElse(default)

  private def configLong0(key: String, default: Long): Long =
    sql"SELECT CONFIG_VALUE FROM SYSTEM_CONFIG_CHANGE WHERE CONFIG_KEY = $key AND APPROVED_BY IS NOT NULL ORDER BY EFFECTIVE_AT DESC, ID DESC LIMIT 1"
      .query[String].run()(using dbc).headOption.flatMap(s => scala.util.Try(s.toLong).toOption).getOrElse(default)

  private def latestApprovedConfigRows(): List[ConfigCurrentRow] =
    sql"""SELECT DISTINCT ON (CONFIG_KEY) CONFIG_KEY, CONFIG_VALUE
          FROM SYSTEM_CONFIG_CHANGE
          WHERE APPROVED_BY IS NOT NULL
          ORDER BY CONFIG_KEY, EFFECTIVE_AT DESC, ID DESC"""
      .query[ConfigCurrentRow].run()(using dbc).toList

  private def currentConfigMap(): Map[String, String] =
    latestApprovedConfigRows().iterator.map(row => row.key -> row.value).toMap

  private def configLongFrom(map: Map[String, String], key: String, default: Long): Long =
    map.get(key).flatMap(s => scala.util.Try(s.toLong).toOption).getOrElse(default)

  private def configBoolFrom(map: Map[String, String], key: String, default: Boolean): Boolean =
    map.get(key).map(_.trim.equalsIgnoreCase("true")).getOrElse(default)

  def configEntries: List[ConfigEntry] = readOnly {
    latestApprovedConfigRows().map(row => ConfigEntry(row.key, row.value))
  }

  def payoutsEnabled: Boolean = readOnly { configString0("payouts_enabled", "true") == "true" }

  def setPayoutsEnabled(enabled: Boolean, reason: String, actor: String): Unit = transaction {
    sql"INSERT INTO SYSTEM_CONFIG_CHANGE (CONFIG_KEY, CONFIG_VALUE, REASON, ACTOR, PROPOSED_BY, APPROVED_BY) VALUES ('payouts_enabled', ${enabled.toString}, $reason, $actor, $actor, $actor)"
      .update.run()(using dbc)
    audit0("config.payouts_enabled", actor, "payouts_enabled", enabled.toString)
  }

  /** Bootstrap-set an approved system config value directly (no two-person flow). */
  def setSystemConfig(key: String, value: String, reason: String, actor: String): Unit = transaction {
    sql"INSERT INTO SYSTEM_CONFIG_CHANGE (CONFIG_KEY, CONFIG_VALUE, REASON, ACTOR, PROPOSED_BY, APPROVED_BY) VALUES ($key, $value, $reason, $actor, $actor, $actor)"
      .update.run()(using dbc)
    audit0("config.bootstrap", actor, key, "set")
  }

  def proposeConfig(key: String, value: String, reason: String, proposedBy: String): ConfigProposalRecord = transaction {
    val id = sql"INSERT INTO SYSTEM_CONFIG_PROPOSAL (CONFIG_KEY, CONFIG_VALUE, REASON, STATUS, PROPOSED_BY) VALUES ($key, $value, $reason, 'PendingReview', $proposedBy) RETURNING ID"
      .returning[Long].run()(using dbc).head
    audit0("config.propose", proposedBy, key, reason)
    configProposalFromId(id).get
  }

  def approveConfig(id: Long, approver: String): Either[String, ConfigProposalRecord] = transaction {
    configProposalFromId(id) match
      case None => Left("config_proposal_not_found")
      case Some(p) if p.status != "PendingReview" => Left("status_conflict")
      case Some(p) if p.proposedBy == approver => Left("two_person_violation")
      case Some(p) =>
        sql"UPDATE SYSTEM_CONFIG_PROPOSAL SET STATUS = 'Approved', APPROVED_BY = $approver, DECIDED_AT = CURRENT_TIMESTAMP WHERE ID = $id".update.run()(using dbc)
        sql"INSERT INTO SYSTEM_CONFIG_CHANGE (CONFIG_KEY, CONFIG_VALUE, REASON, ACTOR, PROPOSED_BY, APPROVED_BY) VALUES (${p.key}, ${p.value}, ${p.reason}, $approver, ${p.proposedBy}, $approver)".update.run()(using dbc)
        audit0("config.approve", approver, p.key, p.value)
        Right(configProposalFromId(id).get)
  }

  def rejectConfig(id: Long, rejecter: String): Either[String, ConfigProposalRecord] = transaction {
    configProposalFromId(id) match
      case None => Left("config_proposal_not_found")
      case Some(p) if p.status != "PendingReview" => Left("status_conflict")
      case Some(p) =>
        sql"UPDATE SYSTEM_CONFIG_PROPOSAL SET STATUS = 'Rejected', REJECTED_BY = $rejecter, DECIDED_AT = CURRENT_TIMESTAMP WHERE ID = $id".update.run()(using dbc)
        audit0("config.reject", rejecter, p.key, p.reason)
        Right(configProposalFromId(id).get)
  }

  private def configProposalFromId(id: Long): Option[ConfigProposalRecord] =
    sql"SELECT ID, CONFIG_KEY, CONFIG_VALUE, REASON, STATUS, PROPOSED_BY, APPROVED_BY, REJECTED_BY FROM SYSTEM_CONFIG_PROPOSAL WHERE ID = $id"
      .query[ConfigProposalRecord].run()(using dbc).headOption

  def allConfigProposals: List[ConfigProposalRecord] = readOnly {
    sql"SELECT ID, CONFIG_KEY, CONFIG_VALUE, REASON, STATUS, PROPOSED_BY, APPROVED_BY, REJECTED_BY FROM SYSTEM_CONFIG_PROPOSAL ORDER BY ID"
      .query[ConfigProposalRecord].run()(using dbc).toList
  }

  private def audit0(action: String, actor: String, subject: String, detail: String): Unit =
    sql"INSERT INTO AUDIT_LOG (ACTION, ACTOR, SUBJECT, DETAIL) VALUES ($action, $actor, $subject, $detail)".update.run()(using dbc)

  def audit(action: String, actor: String, subject: String, detail: String): Unit = transaction { audit0(action, actor, subject, detail) }

  def auditLogs: List[AuditLogRecord] = readOnly {
    sql"SELECT ID, ACTION, ACTOR, SUBJECT, DETAIL FROM AUDIT_LOG ORDER BY ID".query[AuditLogRecord].run()(using dbc).toList
  }

  private def risk0(kind: String, subject: String, detail: String): Unit =
    sql"INSERT INTO RISK_EVENT (KIND, SUBJECT, DETAIL) VALUES ($kind, $subject, $detail)".update.run()(using dbc)

  def risk(kind: String, subject: String, detail: String): Unit = transaction { risk0(kind, subject, detail) }

  def riskEvents: List[RiskEventRecord] = readOnly {
    sql"SELECT ID, KIND, SUBJECT, DETAIL FROM RISK_EVENT ORDER BY ID".query[RiskEventRecord].run()(using dbc).toList
  }

  private def liveWithdrawalFilter(alias: String): SqlLiteral =
    SqlLiteral(s" AND (SELECT sc.TO_STATUS FROM WITHDRAWAL_STATUS_CHANGE sc WHERE sc.WITHDRAWAL_ID = $alias.ID ORDER BY sc.ID DESC LIMIT 1) NOT IN ('Rejected','Cancelled','Failed')")

  private def dayGrowthConfigFrom(map: Map[String, String]): DayGrowthConfig =
    DayGrowthConfig(
      singleLedgerLimit = configLongFrom(map, "single_ledger_amount_limit_points", 100000L),
      dayGrowthLimit = configLongFrom(map, "per_user_day_point_growth_limit_points", 50000L),
    )

  private def withdrawalRiskConfigFrom(map: Map[String, String]): WithdrawalRiskConfig =
    WithdrawalRiskConfig(
      payoutsEnabled = configBoolFrom(map, "payouts_enabled", true),
      singleLimit = configLongFrom(map, "single_payout_limit_points", 50000L),
      userDayLimit = configLongFrom(map, "per_user_day_payout_limit_points", 100000L),
      userMonthLimit = configLongFrom(map, "per_user_month_payout_limit_points", 500000L),
      systemDayLimit = configLongFrom(map, "system_day_payout_limit_points", 10000000L),
    )

  private def loadDayGrowthSnapshot(userUid: String): DayGrowthSnapshot =
    val userAcct = Accounts.user(userUid)
    sql"""SELECT COALESCE(SUM(E.AMOUNT), 0)
          FROM LEDGER_TX T
          JOIN LEDGER_ENTRY E ON E.TX_ID = T.ID
         WHERE T.USER_UID = $userUid
           AND T.KIND = 'IncentiveCredit'
           AND E.DIRECTION = 'CR'
           AND E.ACCOUNT_ID = $userAcct
           AND T.CREATED_AT >= CURRENT_TIMESTAMP - INTERVAL '1 day'"""
      .query[DayGrowthSnapshot].run()(using dbc).head

  private def loadWithdrawalRiskSnapshot(userUid: String): WithdrawalRiskSnapshot =
    val live = liveWithdrawalFilter("W")
    sql"""SELECT
            (SELECT COALESCE(SUM(AMOUNT), 0) FROM WITHDRAWAL W WHERE W.USER_UID = $userUid AND W.CREATED_AT >= CURRENT_TIMESTAMP - INTERVAL '1 day'$live),
            (SELECT COALESCE(SUM(AMOUNT), 0) FROM WITHDRAWAL W WHERE W.USER_UID = $userUid AND W.CREATED_AT >= CURRENT_TIMESTAMP - INTERVAL '1 month'$live),
            (SELECT COALESCE(SUM(AMOUNT), 0) FROM WITHDRAWAL W WHERE W.CREATED_AT >= CURRENT_TIMESTAMP - INTERVAL '1 day'$live),
            (SELECT COUNT(*) FROM WITHDRAWAL W WHERE W.USER_UID = $userUid),
            (SELECT COUNT(*) FROM WITHDRAWAL W WHERE W.USER_UID = $userUid AND W.CREATED_AT >= CURRENT_TIMESTAMP - INTERVAL '1 day')"""
      .query[WithdrawalRiskSnapshot].run()(using dbc).head

  private def withinWithdrawalDayLimits(cfg: WithdrawalRiskConfig, snap: WithdrawalRiskSnapshot, amount: Long): Boolean =
    snap.userDay + amount <= cfg.userDayLimit &&
      snap.userMonth + amount <= cfg.userMonthLimit &&
      snap.systemDay + amount <= cfg.systemDayLimit

  private def forceWithdrawalReview(snap: WithdrawalRiskSnapshot): Boolean =
    snap.priorCount == 0L || snap.todayCount >= 2L

  // Wrapped in `transaction` so the risk-event row and the payouts kill-switch commit as one unit.
  def guardLedgerAmount(userUid: String, amount: Long, subject: String): Either[String, Unit] = transaction {
    val cfg = dayGrowthConfigFrom(currentConfigMap())
    val snap = loadDayGrowthSnapshot(userUid)
    if amount > cfg.singleLedgerLimit then
      risk0("single_ledger_amount", subject, s"amount=$amount limit=${cfg.singleLedgerLimit}")
      setPayoutsEnabled(false, "single ledger amount hard limit", "system")
      Left("risk_limit")
    else if snap.todayCredits + amount > cfg.dayGrowthLimit then
      risk0("per_user_point_growth", userUid, s"today=${snap.todayCredits + amount} limit=${cfg.dayGrowthLimit}")
      setPayoutsEnabled(false, "point growth hard limit", "system")
      Left("risk_limit")
    else Right(())
  }

  // Wrapped in `transaction` (and called OUTSIDE the reserve transaction) so its risk events and
  // the system-day kill-switch commit independently of the withdrawal's success.
  def checkWithdrawalRisk(userUid: String, amount: Long): Either[String, Boolean] = transaction {
    val cfg = withdrawalRiskConfigFrom(currentConfigMap())
    if !cfg.payoutsEnabled then Left("payouts_disabled")
    else
      val snap = loadWithdrawalRiskSnapshot(userUid)
      if amount > cfg.singleLimit then
        risk0("single_payout", userUid, s"amount=$amount limit=${cfg.singleLimit}")
        Left("risk_limit")
      else if snap.userDay + amount > cfg.userDayLimit then
        risk0("user_day_payout", userUid, s"amount=${snap.userDay + amount} limit=${cfg.userDayLimit}")
        Left("risk_limit")
      else if snap.userMonth + amount > cfg.userMonthLimit then
        risk0("user_month_payout", userUid, s"amount=${snap.userMonth + amount} limit=${cfg.userMonthLimit}")
        Left("risk_limit")
      else if snap.systemDay + amount > cfg.systemDayLimit then
        risk0("system_day_payout", "system", s"amount=${snap.systemDay + amount} limit=${cfg.systemDayLimit}")
        setPayoutsEnabled(false, "system payout hard limit", "system")
        Left("risk_limit")
      else
        val review = forceWithdrawalReview(snap)
        if review then risk0("payout_anomaly", userUid, s"first=${snap.priorCount} today=${snap.todayCount}")
        Right(review)
  }

  /** #P1-a: per-user advisory lock held to end of transaction. Concurrent reservations/credits for
    * the same user serialize on it, so the second waits and then reads the first's COMMITTED total in
    * its in-tx limit check — closing the check-then-write race. Mirrors the verified LimitDecision's
    * "consistent day total" premise. (Cross-user system-day races are caught by SERIALIZABLE+retry.) */
  def advisoryXactLock(userUid: String): Unit =
    require(current.isBound, "advisoryXactLock must be called inside transaction { … }")
    val key = ("payout:" + userUid).hashCode.toLong
    val ps = dbc.connection.prepareStatement("SELECT pg_advisory_xact_lock(?)")
    try { ps.setLong(1, key); ps.execute() } finally ps.close()

  /** Authoritative payout-limit gate, read INSIDE the reserve transaction (after advisoryXactLock):
    * true iff `amount` fits under the per-user/day, per-user/month, and system/day limits given the
    * currently committed live withdrawals. */
  def withinPayoutDayLimits(userUid: String, amount: Long): Boolean = readOnly {
    withinWithdrawalDayLimits(withdrawalRiskConfigFrom(currentConfigMap()), loadWithdrawalRiskSnapshot(userUid), amount)
  }

  /** Authoritative per-user/day point-growth gate for incentive credits, read INSIDE the credit
    * transaction (after advisoryXactLock): true iff `amount` fits under the day growth limit. */
  def withinDayGrowthLimit(userUid: String, amount: Long): Boolean = readOnly {
    val cfg = dayGrowthConfigFrom(currentConfigMap())
    val snap = loadDayGrowthSnapshot(userUid)
    snap.todayCredits + amount <= cfg.dayGrowthLimit
  }

  def pendingWithdrawalClearing: Long = readOnly {
    sql"""SELECT COALESCE(SUM(W.AMOUNT), 0)
          FROM WITHDRAWAL W
         WHERE COALESCE(
                 (SELECT SC.TO_STATUS
                    FROM WITHDRAWAL_STATUS_CHANGE SC
                   WHERE SC.WITHDRAWAL_ID = W.ID
                   ORDER BY SC.ID DESC
                   LIMIT 1),
                 'PendingReview'
               ) IN ('PendingReview', 'Submitted')"""
      .query[Long].run()(using dbc).head
  }

  def ledgerBalanced: Boolean = readOnly {
    sql"""SELECT COUNT(*)
          FROM (
            SELECT T.ID
              FROM LEDGER_TX T
              LEFT JOIN LEDGER_ENTRY E ON E.TX_ID = T.ID
             GROUP BY T.ID
            HAVING COUNT(E.TX_ID) = 0
                OR BOOL_OR(E.DIRECTION = 'DR') IS NOT TRUE
                OR BOOL_OR(E.DIRECTION = 'CR') IS NOT TRUE
                OR BOOL_AND(E.AMOUNT > 0) IS NOT TRUE
                OR COALESCE(SUM(CASE WHEN E.DIRECTION = 'DR' THEN E.AMOUNT ELSE 0 END), 0)
                 <> COALESCE(SUM(CASE WHEN E.DIRECTION = 'CR' THEN E.AMOUNT ELSE 0 END), 0)
          ) BAD"""
      .query[Long].run()(using dbc).head == 0L
  }

  def negativeUserLedgerAccounts: List[String] = readOnly {
    sql"""SELECT E.ACCOUNT_ID
          FROM LEDGER_ENTRY E
          JOIN ACCOUNT A ON A.CODE = E.ACCOUNT_ID
         WHERE E.ACCOUNT_ID LIKE 'user:%'
         GROUP BY E.ACCOUNT_ID, A.NORMAL_SIDE
        HAVING COALESCE(SUM(CASE WHEN E.DIRECTION = A.NORMAL_SIDE THEN E.AMOUNT ELSE -E.AMOUNT END), 0) < 0
         ORDER BY E.ACCOUNT_ID"""
      .query[String].run()(using dbc).toList
  }

  def sourceCollisions: List[SourceCollision] = readOnly {
    sql"""SELECT O.SOURCE_KIND, O.SOURCE_ID
          FROM OBLIGATION O
          JOIN LEDGER_TX T ON T.SOURCE_KIND = O.SOURCE_KIND AND T.SOURCE_ID = O.SOURCE_ID
         WHERE O.STATUS = 'Open'
         ORDER BY O.SOURCE_KIND, O.SOURCE_ID"""
      .query[SourceCollision].run()(using dbc).toList
  }

  def approvedRollbackRefsOk: Boolean = readOnly {
    sql"""SELECT COUNT(*)
          FROM PROPOSAL P
          LEFT JOIN (
            SELECT DISTINCT ON (PROPOSAL_ID) PROPOSAL_ID, TO_STATUS
              FROM PROPOSAL_STATUS_CHANGE
             ORDER BY PROPOSAL_ID, ID DESC
          ) S ON S.PROPOSAL_ID = P.ID
          LEFT JOIN LEDGER_TX T ON T.ID = P.TARGET_TX_ID
         WHERE P.KIND = 'RollbackReversal'
           AND COALESCE(S.TO_STATUS, 'PendingReview') = 'Approved'
           AND T.ID IS NULL"""
      .query[Long].run()(using dbc).head == 0L
  }

  def noReversalOfReversal: Boolean = readOnly {
    sql"""SELECT COUNT(*)
          FROM PROPOSAL P
          JOIN (
            SELECT DISTINCT ON (PROPOSAL_ID) PROPOSAL_ID, TO_STATUS
              FROM PROPOSAL_STATUS_CHANGE
             ORDER BY PROPOSAL_ID, ID DESC
          ) S ON S.PROPOSAL_ID = P.ID
          JOIN LEDGER_TX T ON T.ID = P.TARGET_TX_ID
         WHERE P.KIND = 'RollbackReversal'
           AND COALESCE(S.TO_STATUS, 'PendingReview') = 'Approved'
           AND T.KIND = 'RollbackReversal'"""
      .query[Long].run()(using dbc).head == 0L
  }

  /** Insert the provider-event dedup row. A duplicate (provider, eventId) raises a unique
    * violation (the caller maps it to an idempotent replay). */
  def insertProviderEvent(provider: String, eventId: String, withdrawalId: Long, providerTransferRef: Option[String], outcome: String, providerFeeDebited: Option[Long], rawInput: String): Unit =
    require(current.isBound, "insertProviderEvent must be called inside transaction { … }")
    sql"""INSERT INTO WITHDRAWAL_PROVIDER_EVENT
          (PROVIDER, EVENT_ID, WITHDRAWAL_ID, PROVIDER_TRANSFER_REF, OUTCOME, PROVIDER_FEE_DEBITED, RAW_INPUT)
          VALUES ($provider, $eventId, $withdrawalId, $providerTransferRef, $outcome, $providerFeeDebited, $rawInput)"""
      .update.run()(using dbc)

  // No shipped fallback secret: when stripe_webhook_secret is unset, sign with a per-process
  // random secret. Production sets the real shared secret from env (LedgerServer); this keeps
  // signing self-consistent within the process without baking in a known constant.
  private val ephemeralWebhookSecret: String = java.util.UUID.randomUUID.toString + java.util.UUID.randomUUID.toString

  // The webhook signing secret + payout currency/conversion, read from approved system config
  // (bootstrapped from STRIPE_WEBHOOK_SECRET / seed). Signature verification itself lives in the
  // pure StripeSignature object; this only surfaces the secret and the points->minor-unit knobs.
  def stripeWebhookSecret: String = readOnly { configString0("stripe_webhook_secret", ephemeralWebhookSecret) }
  def payoutCurrency: String = readOnly { configString0("payout_currency", "usd") }
  def payoutPointsPerMinorUnit: Long = readOnly { math.max(1L, configLong0("payout_points_per_minor_unit", 1L)) }
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
  def transaction[A](body: => A): A = store.transaction(body)
  def nextId(): Long = store.nextId()
  def insertTx(tx: LedgerTx): Unit = store.insert(tx)
  def txById(id: Long): Option[LedgerTx] = store.byId(id)
  def txBySource(kind: String, id: String): Option[LedgerTx] = store.bySource(kind, id)
  def allTxs: List[LedgerTx] = store.all
  def txsForAccount(account: String): List[LedgerTx] = store.txsForAccount(account)
  def balanceOf(account: String): Long = store.balanceOf(account)
  def categoryBalance(category: String): Long = store.categoryBalance(category)
  def summaryBalances: SummaryBalances = store.summaryBalances
  def ledgerBalanceOf(account: String): Long = store.ledgerBalanceOf(account)
  def lockUserBalance(account: String): Unit = store.lockUserBalance(account)
  def lockTransaction(id: Long): Unit = store.lockTransaction(id)
  def balanceDrifts: List[BalanceDrift] = store.balanceDrifts
  def payoutIntentPut(p: PayoutIntentRecord): Unit = store.payoutIntentPut(p)
  def payoutIntentByWithdrawal(withdrawalId: Long): Option[PayoutIntentRecord] = store.payoutIntentByWithdrawal(withdrawalId)
  def payoutReconciliationPut(r: PayoutReconciliationRecord): Unit = store.payoutReconciliationPut(r)
  def payoutReconciliationByWithdrawal(withdrawalId: Long): Option[PayoutReconciliationRecord] = store.payoutReconciliationByWithdrawal(withdrawalId)
  def providerEventsByWithdrawal(withdrawalId: Long): List[ProviderEventRecord] = store.providerEventsByWithdrawal(withdrawalId)
  def withdrawalPut(wd: Withdrawal): Unit = store.withdrawalPut(wd)
  def withdrawalById(id: Long): Option[Withdrawal] = store.withdrawalById(id)
  def withdrawalByClientReq(userUid: String, clientRequestId: String): Option[Withdrawal] = store.withdrawalByClientReq(userUid, clientRequestId)
  def allWithdrawals: List[Withdrawal] = store.allWithdrawals
  def withdrawalsByUser(userUid: String): List[Withdrawal] = store.withdrawalsByUser(userUid)
  def pendingWithdrawalClearing: Long = store.pendingWithdrawalClearing
  def proposalPut(p: Proposal): Unit = store.proposalPut(p)
  def proposalById(id: Long): Option[Proposal] = store.proposalById(id)
  def allProposals: List[Proposal] = store.allProposals
  def proposalsByKind(kind: TxKind): List[Proposal] = store.proposalsByKind(kind)
  def obligationPut(o: Obligation): Unit = store.obligationPut(o)
  def obligationBySource(kind: String, id: String): Option[Obligation] = store.obligationBySource(kind, id)
  def allOpenObligations: List[Obligation] = store.allOpenObligations
  def configEntries: List[ConfigEntry] = store.configEntries
  def proposeConfig(key: String, value: String, reason: String, proposedBy: String): ConfigProposalRecord = store.proposeConfig(key, value, reason, proposedBy)
  def approveConfig(id: Long, approver: String): Either[String, ConfigProposalRecord] = store.approveConfig(id, approver)
  def rejectConfig(id: Long, rejecter: String): Either[String, ConfigProposalRecord] = store.rejectConfig(id, rejecter)
  def allConfigProposals: List[ConfigProposalRecord] = store.allConfigProposals
  // bounded HTTP-facing reads (keyset by descending id) — see LedgerStore for the contract
  def txPage(limit: Int, before: Option[Long]): List[LedgerTx] = store.txPage(limit, before)
  def txsForAccountPage(account: String, limit: Int, before: Option[Long]): List[LedgerTx] = store.txsForAccountPage(account, limit, before)
  def withdrawalsPage(limit: Int, before: Option[Long]): List[Withdrawal] = store.withdrawalsPage(limit, before)
  def withdrawalsByUserPage(userUid: String, limit: Int, before: Option[Long]): List[Withdrawal] = store.withdrawalsByUserPage(userUid, limit, before)
  def proposalsByKindPage(kind: TxKind, limit: Int, before: Option[Long]): List[Proposal] = store.proposalsByKindPage(kind, limit, before)
  def riskEventsPage(limit: Int, before: Option[Long]): List[RiskEventRecord] = store.riskEventsPage(limit, before)
  def auditLogsPage(limit: Int, before: Option[Long]): List[AuditLogRecord] = store.auditLogsPage(limit, before)
  def configProposalsPage(limit: Int, before: Option[Long]): List[ConfigProposalRecord] = store.configProposalsPage(limit, before)
  def setPayoutsEnabled(enabled: Boolean, reason: String, actor: String): Unit = store.setPayoutsEnabled(enabled, reason, actor)
  def setSystemConfig(key: String, value: String, reason: String, actor: String): Unit = store.setSystemConfig(key, value, reason, actor)
  def payoutsEnabled: Boolean = store.payoutsEnabled
  def accountNormalSide(account: String): String = store.accountNormalSide(account)
  def audit(action: String, actor: String, subject: String, detail: String): Unit = store.audit(action, actor, subject, detail)
  def auditLogs: List[AuditLogRecord] = store.auditLogs
  def risk(kind: String, subject: String, detail: String): Unit = store.risk(kind, subject, detail)
  def riskEvents: List[RiskEventRecord] = store.riskEvents
  def guardLedgerAmount(userUid: String, amount: Long, subject: String): Either[String, Unit] = store.guardLedgerAmount(userUid, amount, subject)
  def checkWithdrawalRisk(userUid: String, amount: Long): Either[String, Boolean] = store.checkWithdrawalRisk(userUid, amount)
  def advisoryXactLock(userUid: String): Unit = store.advisoryXactLock(userUid)
  def withinPayoutDayLimits(userUid: String, amount: Long): Boolean = store.withinPayoutDayLimits(userUid, amount)
  def withinDayGrowthLimit(userUid: String, amount: Long): Boolean = store.withinDayGrowthLimit(userUid, amount)
  def ledgerBalanced: Boolean = store.ledgerBalanced
  def negativeUserLedgerAccounts: List[String] = store.negativeUserLedgerAccounts
  def sourceCollisions: List[SourceCollision] = store.sourceCollisions
  def approvedRollbackRefsOk: Boolean = store.approvedRollbackRefsOk
  def noReversalOfReversal: Boolean = store.noReversalOfReversal
  def insertProviderEvent(provider: String, eventId: String, withdrawalId: Long, providerTransferRef: Option[String], outcome: String, providerFeeDebited: Option[Long], rawInput: String): Unit =
    store.insertProviderEvent(provider, eventId, withdrawalId, providerTransferRef, outcome, providerFeeDebited, rawInput)
  def payoutDispatchPut(d: PayoutDispatchRecord): Unit = store.payoutDispatchPut(d)
  def pendingDispatches(limit: Int): List[PayoutDispatchRecord] = store.pendingDispatches(limit)
  def claimPendingDispatches(limit: Int): List[PayoutDispatchRecord] = store.claimPendingDispatches(limit)
  def dispatchByWithdrawal(withdrawalId: Long): Option[PayoutDispatchRecord] = store.dispatchByWithdrawal(withdrawalId)
  def markDispatched(payoutIntentId: Long, providerTransferRef: String): Unit = store.markDispatched(payoutIntentId, providerTransferRef)
  def markDispatchFailed(payoutIntentId: Long, newStatus: String, error: String): Unit = store.markDispatchFailed(payoutIntentId, newStatus, error)
  def reclaimStaleInflight(timeoutSeconds: Int): Int = store.reclaimStaleInflight(timeoutSeconds)
  def withdrawalIdByProviderTransferRef(ref: String): Option[Long] = store.withdrawalIdByProviderTransferRef(ref)
  def stripeWebhookSecret: String = store.stripeWebhookSecret
  def payoutCurrency: String = store.payoutCurrency
  def payoutPointsPerMinorUnit: Long = store.payoutPointsPerMinorUnit