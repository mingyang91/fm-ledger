package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import io.linewise.ledger.Db

/* =============================================================================
 * PENDING OBLIGATION — the fifth verified slice: a forecast read-model of published-
 * but-unfinished work, keyed by the SAME source.{kind,id} the future incentive credit
 * carries. Lifecycle: open -> realized (the credit minted) or open -> cancelled; the
 * terminal states are sticky, so a late `open` on a realized/cancelled source is
 * refused (SourceTerminal). Upsert-by-source (`put`). The upcoming-expense aggregation
 * over `allOpen` is a PRODUCTION fold (priced in the shell), not verified here.
 * ========================================================================== */
case class Obligation(
    sourceKind:    String,
    sourceId:      String,
    userUid:       String,
    role:          String,
    projectRef:    String,
    taskKind:      String,
    estimatedUnit: FMLong,
    status:        ObligationStatus,
    realizedTxId:  Option[FMLong],
)

sealed abstract class ObligationRepository {
  @ghost def rows: List[Obligation]

  def put(o: Obligation): ObligationRepository
  def bySource(kind: String, id: String): Option[Obligation]
  def allOpen: List[Obligation]

  @law def putGetBySource(o: Obligation): Boolean =
    put(o).bySource(o.sourceKind, o.sourceId) == Some[Obligation](o)
}

case class InMemObligationRepository(items: List[Obligation]) extends ObligationRepository {
  @ghost def rows: List[Obligation] = items
  def put(o: Obligation): ObligationRepository =
    InMemObligationRepository(o :: items.filter((x: Obligation) => x.sourceKind != o.sourceKind || x.sourceId != o.sourceId))
  def bySource(kind: String, id: String): Option[Obligation] =
    items.find((x: Obligation) => x.sourceKind == kind && x.sourceId == id)
  def allOpen: List[Obligation] = items.filter((x: Obligation) => x.status == ObligationStatus.Open)
}

case class JdbcObligationRepository() extends ObligationRepository {
  @ghost def rows: List[Obligation] = Nil[Obligation]()

  @extern @pure
  def put(o: Obligation): ObligationRepository = { Db.obligationPut(o); JdbcObligationRepository() }.ensuring((res: ObligationRepository) =>
    res.bySource(o.sourceKind, o.sourceId) == Some[Obligation](o))

  @extern @pure
  def bySource(kind: String, id: String): Option[Obligation] = Db.obligationBySource(kind, id)

  @extern @pure
  def allOpen: List[Obligation] = Db.allOpenObligations
}
