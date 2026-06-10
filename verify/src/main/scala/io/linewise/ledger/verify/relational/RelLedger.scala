package io.linewise.verify.fm.ledger.relational

import stainless.lang.*
import stainless.collection.*
import stainless.annotation.*
import io.linewise.verify.effect.FMLong
import io.linewise.verify.fm.ledger.LedgerModel.{TxKind, EntryDirection}
import EntryDirection.*

/* =============================================================================
 * RELATIONAL FM CORE — goal: model ALL fm state as TABLES (a row-list per relation, one Has lens
 * each, joins where needed) instead of aggregates. This file is Phase 0 (reusable relational lemma
 * kit + Table/Has shape) + Phase 1 (the LEDGER slice fully migrated and re-proven): header table +
 * leg table joined by txId, with the same guarantees the aggregate gave for free —
 * conservation (balanced), PK distinctness, referential integrity — now proven relationally, and a
 * read-side join (account balance) proven equal to the flat scan.
 *
 * Ids are BigInt (identifiers); amounts are FMLong (overflow-tracked), as in the production schema.
 * Verify (deps): ./mill verify.scala verify/stainless-lib/FMTypes.scala \
 *   verify/src/main/scala/io/linewise/ledger/verify/LedgerModel.scala \
 *   verify/src/main/scala/io/linewise/ledger/verify/Has.scala \
 *   verify/src/main/scala/io/linewise/ledger/verify/relational/RelLedger.scala
 * ========================================================================== */
object RelLedger {

  // ---- TABLES (rows) ----
  case class TxHeaderRow(id: BigInt, kind: TxKind, sourceKind: Option[String], sourceId: Option[String], userUid: String)
  case class LegRow(txId: BigInt, lineNo: BigInt, account: String, dir: EntryDirection, amount: FMLong) {
    require(amount.value > BigInt(0))
  }
  case class LedgerTables(headers: List[TxHeaderRow], legs: List[LegRow])
  // NB: a Has[LedgerTables, List[Row]] lens per table is the intended service-access shape, but the
  // project Has carries @law get/set round-trips whose VCs stall under the FMTypes context; the
  // law-discharged lens layer is deferred to Phase 4 (service wiring). The state + invariants below
  // are the substance of the table migration and stand on their own.

  // ========================= REUSABLE RELATIONAL KIT =========================
  // first-order recursive relational predicates (a .forall(lambda) does not unify closures in proofs)
  def headerIds(hs: List[TxHeaderRow]): List[BigInt] = hs.map(h => h.id)
  def legsOf(legs: List[LegRow], txId: BigInt): List[LegRow] = legs.filter(l => l.txId == txId)

  def allRefOk(legs: List[LegRow], hids: List[BigInt]): Boolean = legs match
    case Nil()      => true
    case Cons(l, t) => hids.contains(l.txId) && allRefOk(t, hids)
  def allSameTx(legs: List[LegRow], id: BigInt): Boolean = legs match
    case Nil()      => true
    case Cons(l, t) => l.txId == id && allSameTx(t, id)
  def noHeaderWith(hs: List[TxHeaderRow], id: BigInt): Boolean = hs match
    case Nil()      => true
    case Cons(h, t) => h.id != id && noHeaderWith(t, id)
  def noLegWith(legs: List[LegRow], id: BigInt): Boolean = legs match
    case Nil()      => true
    case Cons(l, t) => l.txId != id && noLegWith(t, id)
  def distinctIds(hs: List[TxHeaderRow]): Boolean = hs match
    case Nil()      => true
    case Cons(h, t) => noHeaderWith(t, h.id) && distinctIds(t)

  // referential-integrity lemmas
  @induct def allRefOkConsHeader(legs: List[LegRow], hids: List[BigInt], y: BigInt): Unit = {
    require(allRefOk(legs, hids)); ()
  }.ensuring(_ => allRefOk(legs, y :: hids))
  @induct def allRefOkOfFresh(legs: List[LegRow], id: BigInt, hids: List[BigInt]): Unit = {
    require(allSameTx(legs, id)); require(hids.contains(id)); ()
  }.ensuring(_ => allRefOk(legs, hids))
  @induct def allRefOkAppend(a: List[LegRow], b: List[LegRow], hids: List[BigInt]): Unit = {
    require(allRefOk(a, hids)); require(allRefOk(b, hids)); ()
  }.ensuring(_ => allRefOk(a ++ b, hids))

  // join/filter lemmas
  @induct def legsOfAllSame(a: List[LegRow], id: BigInt): Unit = { require(allSameTx(a, id)); () }
    .ensuring(_ => legsOf(a, id) == a)
  @induct def legsOfNoLeg(a: List[LegRow], id: BigInt): Unit = { require(noLegWith(a, id)); () }
    .ensuring(_ => legsOf(a, id) == (Nil[LegRow](): List[LegRow]))
  def noHeaderWithMeansFresh(hs: List[TxHeaderRow], id: BigInt): Unit = {
    require(noHeaderWith(hs, id))
    hs match { case Nil() => (); case Cons(h, t) => noHeaderWithMeansFresh(t, id) }
  }.ensuring(_ => !headerIds(hs).contains(id))

  // ========================= LEDGER INVARIANTS (relational) =========================
  def drSum(legs: List[LegRow]): BigInt = legs.foldLeft(BigInt(0))((a, l) => if l.dir == DR then a + l.amount.value else a)
  def crSum(legs: List[LegRow]): BigInt = legs.foldLeft(BigInt(0))((a, l) => if l.dir == CR then a + l.amount.value else a)
  def hasDir(legs: List[LegRow], d: EntryDirection): Boolean = legs.exists(l => l.dir == d)
  // mirrors LedgerValidation.admissible on the JOINED legs (positivity is the LegRow invariant)
  def admissible(legs: List[LegRow]): Boolean =
    legs.nonEmpty && hasDir(legs, DR) && hasDir(legs, CR) && drSum(legs) == crSum(legs)

  def allHeadersAdmissible(headers: List[TxHeaderRow], legs: List[LegRow]): Boolean = headers match
    case Nil()      => true
    case Cons(h, t) => admissible(legsOf(legs, h.id)) && allHeadersAdmissible(t, legs)

  def refIntegrity(w: LedgerTables): Boolean = allRefOk(w.legs, headerIds(w.headers))
  def conservation(w: LedgerTables): Boolean = allHeadersAdmissible(w.headers, w.legs)
  def distinctHeaders(w: LedgerTables): Boolean = distinctIds(w.headers)

  // POST a tx as a batch: a fresh header + its (balanced) legs. The aggregate's atomic posting,
  // now an explicit unit-of-work over two tables.
  def post(w: LedgerTables, h: TxHeaderRow, newLegs: List[LegRow]): LedgerTables =
    LedgerTables(h :: w.headers, newLegs ++ w.legs)

  // adding legs that all reference a FRESH id leaves the old headers' admissibility untouched
  def admissibleStableUnderFreshLegs(headers: List[TxHeaderRow], oldLegs: List[LegRow], newLegs: List[LegRow], freshId: BigInt): Unit = {
    require(noHeaderWith(headers, freshId))
    require(allSameTx(newLegs, freshId))
    require(allHeadersAdmissible(headers, oldLegs))
    headers match
      case Nil() => ()
      case Cons(hh, t) =>
        legsOfFreshAppend(newLegs, oldLegs, freshId, hh.id)
        admissibleStableUnderFreshLegs(t, oldLegs, newLegs, freshId)
  }.ensuring(_ => allHeadersAdmissible(headers, newLegs ++ oldLegs))

  // legsOf(newLegs ++ oldLegs, hh.id) == legsOf(oldLegs, hh.id) when newLegs all share a fresh id != hh.id
  @induct def legsOfFreshAppend(newLegs: List[LegRow], oldLegs: List[LegRow], freshId: BigInt, hhId: BigInt): Unit = {
    require(allSameTx(newLegs, freshId)); require(hhId != freshId); ()
  }.ensuring(_ => legsOf(newLegs ++ oldLegs, hhId) == legsOf(oldLegs, hhId))

  // ---- PROOF: post preserves referential integrity ----
  def postPreservesRefIntegrity(w: LedgerTables, h: TxHeaderRow, newLegs: List[LegRow]): Boolean = {
    require(refIntegrity(w)); require(allSameTx(newLegs, h.id))
    val hids2 = headerIds(h :: w.headers)
    allRefOkConsHeader(w.legs, headerIds(w.headers), h.id)
    allRefOkOfFresh(newLegs, h.id, hids2)
    allRefOkAppend(newLegs, w.legs, hids2)
    refIntegrity(post(w, h, newLegs))
  }.holds

  // ---- PROOF: post preserves PK distinctness ----
  def postPreservesDistinct(w: LedgerTables, h: TxHeaderRow, newLegs: List[LegRow]): Boolean = {
    require(distinctHeaders(w)); require(noHeaderWith(w.headers, h.id))
    distinctHeaders(post(w, h, newLegs))
  }.holds

  // ---- PROOF: post preserves conservation (every tx balanced) ----
  def postPreservesConservation(w: LedgerTables, h: TxHeaderRow, newLegs: List[LegRow]): Boolean = {
    require(conservation(w))
    require(admissible(newLegs))
    require(allSameTx(newLegs, h.id))
    require(noHeaderWith(w.headers, h.id))
    require(noLegWith(w.legs, h.id))
    legsOfAllSame(newLegs, h.id)
    legsOfNoLeg(w.legs, h.id)
    legsOfFreshAppend2(newLegs, w.legs, h.id) // legsOf(newLegs ++ w.legs, h.id) == newLegs ++ Nil
    admissibleStableUnderFreshLegs(w.headers, w.legs, newLegs, h.id)
    conservation(post(w, h, newLegs))
  }.holds

  // legsOf(newLegs ++ w.legs, h.id) == newLegs, given newLegs all == h.id and w.legs none == h.id
  def legsOfFreshAppend2(newLegs: List[LegRow], oldLegs: List[LegRow], id: BigInt): Unit = {
    require(allSameTx(newLegs, id)); require(noLegWith(oldLegs, id))
    legsOfAppend(newLegs, oldLegs, id)
    legsOfAllSame(newLegs, id)
    legsOfNoLeg(oldLegs, id)
  }.ensuring(_ => legsOf(newLegs ++ oldLegs, id) == newLegs)

  @induct def legsOfAppend(a: List[LegRow], b: List[LegRow], id: BigInt): Unit = { () }
    .ensuring(_ => legsOf(a ++ b, id) == legsOf(a, id) ++ legsOf(b, id))

  // ========================= READ-SIDE JOIN (account balance) =========================
  def contrib(l: LegRow, account: String): BigInt =
    if l.account == account then (if l.dir == DR then l.amount.value else BigInt(0) - l.amount.value) else BigInt(0)
  def acctTotal(legs: List[LegRow], account: String): BigInt = legs match
    case Nil()      => BigInt(0)
    case Cons(l, t) => contrib(l, account) + acctTotal(t, account)
  def sumOverHeaders(headers: List[TxHeaderRow], legs: List[LegRow], account: String): BigInt = headers match
    case Nil()      => BigInt(0)
    case Cons(h, t) => acctTotal(legsOf(legs, h.id), account) + sumOverHeaders(t, legs, account)

  @induct def sumOverHeadersEmpty(headers: List[TxHeaderRow], account: String): Unit = { () }
    .ensuring(_ => sumOverHeaders(headers, Nil[LegRow](), account) == BigInt(0))
  def sumOverHeadersConsMiss(headers: List[TxHeaderRow], l: LegRow, ls: List[LegRow], account: String): Unit = {
    require(!headerIds(headers).contains(l.txId))
    headers match { case Nil() => (); case Cons(h, t) => sumOverHeadersConsMiss(t, l, ls, account) }
  }.ensuring(_ => sumOverHeaders(headers, l :: ls, account) == sumOverHeaders(headers, ls, account))
  def sumOverHeadersConsHit(headers: List[TxHeaderRow], l: LegRow, ls: List[LegRow], account: String): Unit = {
    require(distinctIds(headers)); require(headerIds(headers).contains(l.txId))
    headers match
      case Nil() => ()
      case Cons(h, t) =>
        if h.id == l.txId then
          noHeaderWithMeansFresh(t, l.txId) // bridge: distinctIds gives noHeaderWith(t,h.id); ConsMiss wants !contains
          sumOverHeadersConsMiss(t, l, ls, account)
        else sumOverHeadersConsHit(t, l, ls, account)
  }.ensuring(_ => sumOverHeaders(headers, l :: ls, account) == contrib(l, account) + sumOverHeaders(headers, ls, account))

  // ---- PROOF: the per-tx grouped account balance (join) equals the flat scan ----
  def joinBalanceEqualsFlat(w: LedgerTables, account: String): Boolean = {
    require(distinctHeaders(w)); require(refIntegrity(w))
    joinEqualsFlat(w.headers, w.legs, account)
    sumOverHeaders(w.headers, w.legs, account) == acctTotal(w.legs, account)
  }.holds

  def joinEqualsFlat(headers: List[TxHeaderRow], legs: List[LegRow], account: String): Unit = {
    require(distinctIds(headers)); require(allRefOk(legs, headerIds(headers)))
    legs match
      case Nil() => sumOverHeadersEmpty(headers, account)
      case Cons(l, ls) =>
        sumOverHeadersConsHit(headers, l, ls, account)
        joinEqualsFlat(headers, ls, account)
  }.ensuring(_ => sumOverHeaders(headers, legs, account) == acctTotal(legs, account))
}
