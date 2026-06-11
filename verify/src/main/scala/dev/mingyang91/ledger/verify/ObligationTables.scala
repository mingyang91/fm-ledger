package dev.mingyang91.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import dev.mingyang91.ledger.Db
import LedgerModel._

/* =============================================================================
 * OBLIGATION TABLE STORE — read/modify seam over the single composite-key OBLIGATION table.
 * Upsert is drop-same-key-then-prepend. role/projectRef/taskKind are not persisted (read back as "",
 * matching production's toObligation), so the InMem oracle is lossy in the same way as the SQL store.
 * InMem here; Jdbc deferred to integration.
 * ========================================================================== */
object ObligationTables {

  def toORow(o: Obligation): ObligationRow =
    ObligationRow(o.sourceKind, o.sourceId, o.userUid, o.estimatedUnit, o.status, o.realizedTxId)
  def assembleO(r: ObligationRow): Obligation =
    Obligation(r.sourceKind, r.sourceId, r.userUid, "", "", "", r.estimatedPoints, r.status, r.realizedTxId)

  def dropKey(os: List[ObligationRow], sk: String, si: String): List[ObligationRow] =
    os.filter((x: ObligationRow) => !(x.sourceKind == sk && x.sourceId == si))

  sealed abstract class ObligationStore {
    def put(db: DB, o: Obligation): DB
    def bySource(db: DB, kind: String, id: String): Option[Obligation]
    def allOpen(db: DB): List[Obligation]
  }

  case class InMemObligationStore() extends ObligationStore {
    def put(db: DB, o: Obligation): DB =
      db.copy(obligations = toORow(o) :: dropKey(db.obligations, o.sourceKind, o.sourceId))
    def bySource(db: DB, kind: String, id: String): Option[Obligation] =
      db.obligations.find((x: ObligationRow) => x.sourceKind == kind && x.sourceId == id) match
        case Some(r) => Some[Obligation](assembleO(r))
        case _       => None[Obligation]()
    def allOpen(db: DB): List[Obligation] =
      db.obligations.filter((x: ObligationRow) => x.status == ObligationStatus.Open).map(assembleO)
  }

  // Jdbc realization: @extern SQL via the production Db facade; db ignored.
  case class JdbcObligationStore() extends ObligationStore {
    @extern @pure
    def put(db: DB, o: Obligation): DB = { Db.obligationPut(o); db }
    @extern @pure
    def bySource(db: DB, kind: String, id: String): Option[Obligation] = Db.obligationBySource(kind, id)
    @extern @pure
    def allOpen(db: DB): List[Obligation] = Db.allOpenObligations
  }
}
