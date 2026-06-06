package io.linewise.ledger

import java.sql.{Connection, SQLException}

/* =============================================================================
 * H2 trigger enforcing the append-only ledger at the DATABASE layer: any UPDATE or
 * DELETE on LEDGER_TX is rejected. The application already only ever INSERTs, so this
 * is defense-in-depth — the H2 analogue of the design's Postgres `DO INSTEAD NOTHING`
 * rules. Registered by Jdbc.ddl as BEFORE UPDATE / BEFORE DELETE … FOR EACH ROW.
 * ========================================================================== */
class LedgerAppendOnlyTrigger extends org.h2.api.Trigger:
  override def init(conn: Connection, schemaName: String, triggerName: String,
      tableName: String, before: Boolean, triggerType: Int): Unit = ()

  override def fire(conn: Connection, oldRow: Array[AnyRef], newRow: Array[AnyRef]): Unit =
    throw new SQLException("LEDGER_TX is append-only: UPDATE/DELETE rejected", "23000")
