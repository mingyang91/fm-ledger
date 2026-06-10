package io.linewise.verify.fm.ledger.experiment

import stainless.lang.*
import stainless.collection.*
import stainless.annotation.*

/* =============================================================================
 * TABLE-vs-AGGREGATE MODELING EXPERIMENT (verify-only, self-contained).
 * Iteration 4: prove the SAME invariant the aggregate gets for free — `balanced` — on the table
 * model, to measure the full scaffolding tax (recursive predicates + join/filter lemmas + per-row
 * freshness reasoning) and confirm there is no hard wall, only soft cost.
 *   ./mill verify.scala verify/src/main/scala/io/linewise/ledger/verify/experiment/TableModelExperiment.scala
 * ========================================================================== */
object TableModelExperiment {

  enum Dir { case DR, CR }
  import Dir.*

  case class Leg(account: String, dir: Dir, amount: BigInt) { require(amount > 0) }
  def drSum(legs: List[Leg]): BigInt = legs.foldLeft(BigInt(0))((a, l) => if l.dir == DR then a + l.amount else a)
  def crSum(legs: List[Leg]): BigInt = legs.foldLeft(BigInt(0))((a, l) => if l.dir == CR then a + l.amount else a)
  def balanced(legs: List[Leg]): Boolean = legs.nonEmpty && drSum(legs) == crSum(legs)

  // ===== MODEL A — AGGREGATE: zero scaffolding, balanced is structural. =====
  case class AggTx(id: BigInt, legs: List[Leg])
  case class AggWorld(txs: List[AggTx])
  def aggAllBalanced(w: AggWorld): Boolean = w.txs.forall(t => balanced(t.legs))
  def aggPost(w: AggWorld, tx: AggTx): AggWorld = AggWorld(tx :: w.txs)
  def aggPostPreservesBalanced(w: AggWorld, tx: AggTx): Boolean = {
    require(aggAllBalanced(w)); require(balanced(tx.legs))
    aggAllBalanced(aggPost(w, tx))
  }.holds

  // ===== MODEL B — TABLES: header + leg relations joined by txId. =====
  case class HeaderRow(id: BigInt)
  case class LegRow(txId: BigInt, lineNo: BigInt, account: String, dir: Dir, amount: BigInt) { require(amount > 0) }
  case class TWorld(headers: List[HeaderRow], legs: List[LegRow])

  def headerIds(hs: List[HeaderRow]): List[BigInt] = hs.map(h => h.id)
  def legsOf(legs: List[LegRow], txId: BigInt): List[LegRow] = legs.filter(l => l.txId == txId)
  def legsAsLeg(rs: List[LegRow]): List[Leg] = rs.map(r => Leg(r.account, r.dir, r.amount))

  // first-order recursive relational predicates (the aggregate needs none of these)
  def allRefOk(legs: List[LegRow], hids: List[BigInt]): Boolean = legs match
    case Nil()      => true
    case Cons(l, t) => hids.contains(l.txId) && allRefOk(t, hids)
  def allSameTx(legs: List[LegRow], id: BigInt): Boolean = legs match
    case Nil()      => true
    case Cons(l, t) => l.txId == id && allSameTx(t, id)
  def noHeaderWith(hs: List[HeaderRow], id: BigInt): Boolean = hs match
    case Nil()      => true
    case Cons(h, t) => h.id != id && noHeaderWith(t, id)
  def noLegWith(legs: List[LegRow], id: BigInt): Boolean = legs match
    case Nil()      => true
    case Cons(l, t) => l.txId != id && noLegWith(t, id)
  def balancedHeaders(headers: List[HeaderRow], legs: List[LegRow]): Boolean = headers match
    case Nil()      => true
    case Cons(h, t) => balanced(legsAsLeg(legsOf(legs, h.id))) && balancedHeaders(t, legs)

  def refIntegrity(w: TWorld): Boolean = allRefOk(w.legs, headerIds(w.headers))
  def tableAllBalanced(w: TWorld): Boolean = balancedHeaders(w.headers, w.legs)
  def tPost(w: TWorld, h: HeaderRow, newLegs: List[LegRow]): TWorld = TWorld(h :: w.headers, newLegs ++ w.legs)
  def insertLeg(w: TWorld, l: LegRow): TWorld = TWorld(w.headers, l :: w.legs)

  // ---- referential-integrity lemmas (B1) ----
  @induct def allRefOkConsHeader(legs: List[LegRow], hids: List[BigInt], y: BigInt): Unit = {
    require(allRefOk(legs, hids)); ()
  }.ensuring(_ => allRefOk(legs, y :: hids))
  @induct def allRefOkOfFresh(legs: List[LegRow], id: BigInt, hids: List[BigInt]): Unit = {
    require(allSameTx(legs, id)); require(hids.contains(id)); ()
  }.ensuring(_ => allRefOk(legs, hids))
  @induct def allRefOkAppend(a: List[LegRow], b: List[LegRow], hids: List[BigInt]): Unit = {
    require(allRefOk(a, hids)); require(allRefOk(b, hids)); ()
  }.ensuring(_ => allRefOk(a ++ b, hids))

  // ---- join/filter lemmas (B2) ----
  @induct def legsOfAppend(a: List[LegRow], b: List[LegRow], id: BigInt): Unit = { () }
    .ensuring(_ => legsOf(a ++ b, id) == legsOf(a, id) ++ legsOf(b, id))
  @induct def legsOfAllSame(a: List[LegRow], id: BigInt): Unit = { require(allSameTx(a, id)); () }
    .ensuring(_ => legsOf(a, id) == a)
  @induct def legsOfNoneSame(a: List[LegRow], id: BigInt, j: BigInt): Unit = { require(allSameTx(a, id)); require(j != id); () }
    .ensuring(_ => legsOf(a, j) == (Nil[LegRow](): List[LegRow]))
  @induct def legsOfNoLeg(a: List[LegRow], id: BigInt): Unit = { require(noLegWith(a, id)); () }
    .ensuring(_ => legsOf(a, id) == (Nil[LegRow](): List[LegRow]))

  // adding legs that all reference a FRESH id (no old header has it) leaves the old headers'
  // balance untouched — manual structural recursion invoking the filter lemmas in the step.
  def balancedStableUnderFreshLegs(headers: List[HeaderRow], oldLegs: List[LegRow], newLegs: List[LegRow], freshId: BigInt): Unit = {
    require(noHeaderWith(headers, freshId))
    require(allSameTx(newLegs, freshId))
    require(balancedHeaders(headers, oldLegs))
    headers match
      case Nil() => ()
      case Cons(hh, t) =>
        legsOfAppend(newLegs, oldLegs, hh.id)
        legsOfNoneSame(newLegs, freshId, hh.id)
        balancedStableUnderFreshLegs(t, oldLegs, newLegs, freshId)
  }.ensuring(_ => balancedHeaders(headers, newLegs ++ oldLegs))

  // CEILING WITNESS: a single-leg insert leaves an unbalanced tx (balanced can't be a per-row write
  // invariant; writes must batch at the tx boundary — the aggregate re-emerges).
  def singleLegBreaksBalance(): Boolean = {
    val w0 = TWorld(List(HeaderRow(1)), Nil[LegRow]())
    !tableAllBalanced(insertLeg(w0, LegRow(1, 1, "a", DR, 100)))
  }.holds

  // PROOF B1: batched tPost preserves referential integrity.
  def tPostPreservesRefIntegrity(w: TWorld, h: HeaderRow, newLegs: List[LegRow]): Boolean = {
    require(refIntegrity(w)); require(allSameTx(newLegs, h.id))
    val hids2 = headerIds(h :: w.headers)
    allRefOkConsHeader(w.legs, headerIds(w.headers), h.id)
    allRefOkOfFresh(newLegs, h.id, hids2)
    allRefOkAppend(newLegs, w.legs, hids2)
    refIntegrity(tPost(w, h, newLegs))
  }.holds

  // PROOF B2: batched tPost preserves the cross-table balanced invariant (the apples-to-apples
  // counterpart of aggPostPreservesBalanced — same property, all this scaffolding to get there).
  def tPostPreservesBalanced(w: TWorld, h: HeaderRow, newLegs: List[LegRow]): Boolean = {
    require(tableAllBalanced(w))
    require(balanced(legsAsLeg(newLegs)))
    require(allSameTx(newLegs, h.id))
    require(noHeaderWith(w.headers, h.id))
    require(noLegWith(w.legs, h.id))
    // new header's legs after the batched insert == exactly newLegs
    legsOfAppend(newLegs, w.legs, h.id)
    legsOfAllSame(newLegs, h.id)
    legsOfNoLeg(w.legs, h.id)
    // old headers' balance is untouched by the fresh legs
    balancedStableUnderFreshLegs(w.headers, w.legs, newLegs, h.id)
    tableAllBalanced(tPost(w, h, newLegs))
  }.holds
}
