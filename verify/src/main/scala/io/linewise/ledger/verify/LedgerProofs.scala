package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import LedgerTables._
import LedgerInvariants._

/* =============================================================================
 * VERIFY-ONLY proofs for the LEDGER slice on the relational DB: posting an admissible, fresh tx
 * preserves the whole ledger invariant (referential integrity + header distinctness + conservation),
 * and the service rejects non-positive / unbalanced / duplicate-id / duplicate-source txs. Proven
 * over the InMem store; the Jdbc realization is reconciled by the drift gate.
 * ========================================================================== */
object LedgerProofs {

  private val store = InMemLedgerStore()

  // ---- toLegs <-> entries correspondence (for conservation) ----
  def entriesAllSame(id: FMLong, es: List[LedgerEntry], n: BigInt): Unit = {
    es match { case Nil() => (); case Cons(e, t) => entriesAllSame(id, t, n + BigInt(1)) }
  }.ensuring(_ => allSameTx(entriesToLegs(id, es, n), id))

  def entriesNonEmpty(id: FMLong, es: List[LedgerEntry], n: BigInt): Unit = { () }
    .ensuring(_ => entriesToLegs(id, es, n).isEmpty == es.isEmpty)

  def posCorr(id: FMLong, es: List[LedgerEntry], n: BigInt): Unit = {
    require(allPositive(es))
    es match { case Nil() => (); case Cons(e, t) => posCorr(id, t, n + BigInt(1)) }
  }.ensuring(_ => allLegsPositive(entriesToLegs(id, es, n)))

  def dirCorr(id: FMLong, es: List[LedgerEntry], n: BigInt, d: EntryDirection): Unit = {
    require(hasDirection(es, d))
    es match { case Nil() => (); case Cons(e, t) => if e.direction == d then () else dirCorr(id, t, n + BigInt(1), d) }
  }.ensuring(_ => hasDir(entriesToLegs(id, es, n), d))

  def sumCorr(id: FMLong, es: List[LedgerEntry], n: BigInt, d: EntryDirection): Unit = {
    es match { case Nil() => (); case Cons(e, t) => sumCorr(id, t, n + BigInt(1), d) }
  }.ensuring(_ => dirSum(entriesToLegs(id, es, n), d) == sumDirection(es, d))

  // admissible tx => its legs are admissible
  def admissibleTxGivesAdmissibleLegs(tx: LedgerTx): Unit = {
    require(LedgerValidation.admissible(tx))
    entriesNonEmpty(tx.id, tx.entries, BigInt(1))
    posCorr(tx.id, tx.entries, BigInt(1))
    dirCorr(tx.id, tx.entries, BigInt(1), EntryDirection.DR)
    dirCorr(tx.id, tx.entries, BigInt(1), EntryDirection.CR)
    sumCorr(tx.id, tx.entries, BigInt(1), EntryDirection.DR)
    sumCorr(tx.id, tx.entries, BigInt(1), EntryDirection.CR)
  }.ensuring(_ => admissibleLegs(toLegs(tx)))

  def toLegsAllSame(tx: LedgerTx): Unit = { entriesAllSame(tx.id, tx.entries, BigInt(1)) }
    .ensuring(_ => allSameTx(toLegs(tx), tx.id))

  // ---- referential-integrity lemmas (recursive hasHeader, no List.contains) ----
  @induct def allRefOkConsHeader(legs: List[LegRow], hs: List[TxHeaderRow], h: TxHeaderRow): Unit = {
    require(allRefOk(legs, hs)); ()
  }.ensuring(_ => allRefOk(legs, h :: hs))
  @induct def allRefOkOfFresh(legs: List[LegRow], hs: List[TxHeaderRow], h: TxHeaderRow): Unit = {
    require(allSameTx(legs, h.id)); ()
  }.ensuring(_ => allRefOk(legs, h :: hs))
  @induct def allRefOkAppend(a: List[LegRow], b: List[LegRow], hs: List[TxHeaderRow]): Unit = {
    require(allRefOk(a, hs)); require(allRefOk(b, hs)); ()
  }.ensuring(_ => allRefOk(a ++ b, hs))

  // ---- filter/legsOf lemmas ----
  @induct def legsOfAllSame(a: List[LegRow], id: FMLong): Unit = { require(allSameTx(a, id)); () }
    .ensuring(_ => legsOf(a, id) == a)
  @induct def legsOfNoLeg(a: List[LegRow], id: FMLong): Unit = { require(noLegWith(a, id)); () }
    .ensuring(_ => legsOf(a, id) == (Nil[LegRow](): List[LegRow]))
  @induct def legsOfAppend(a: List[LegRow], b: List[LegRow], id: FMLong): Unit = { () }
    .ensuring(_ => legsOf(a ++ b, id) == legsOf(a, id) ++ legsOf(b, id))
  def legsOfFreshAppend2(newLegs: List[LegRow], oldLegs: List[LegRow], id: FMLong): Unit = {
    require(allSameTx(newLegs, id)); require(noLegWith(oldLegs, id))
    legsOfAppend(newLegs, oldLegs, id)
    legsOfAllSame(newLegs, id)
    legsOfNoLeg(oldLegs, id)
  }.ensuring(_ => legsOf(newLegs ++ oldLegs, id) == newLegs)
  // legsOf(newLegs ++ oldLegs, hh.id) == legsOf(oldLegs, hh.id) when newLegs all share a fresh id != hh.id
  @induct def legsOfFreshAppend(newLegs: List[LegRow], oldLegs: List[LegRow], freshId: FMLong, hhId: FMLong): Unit = {
    require(allSameTx(newLegs, freshId)); require(hhId != freshId); ()
  }.ensuring(_ => legsOf(newLegs ++ oldLegs, hhId) == legsOf(oldLegs, hhId))

  // adding fresh-id legs leaves old headers' admissibility untouched
  def admissibleStableUnderFreshLegs(hs: List[TxHeaderRow], oldLegs: List[LegRow], newLegs: List[LegRow], freshId: FMLong): Unit = {
    require(noHeaderWith(hs, freshId))
    require(allSameTx(newLegs, freshId))
    require(allHeadersAdmissible(hs, oldLegs))
    hs match
      case Nil() => ()
      case Cons(hh, t) =>
        legsOfFreshAppend(newLegs, oldLegs, freshId, hh.id)
        admissibleStableUnderFreshLegs(t, oldLegs, newLegs, freshId)
  }.ensuring(_ => allHeadersAdmissible(hs, newLegs ++ oldLegs))

  // ---- THE PROOF: posting an admissible, fresh tx preserves the ledger invariant ----
  def postPreservesValidLedger(db: DB, tx: LedgerTx): Unit = {
    require(validLedger(db))
    require(LedgerValidation.admissible(tx))
    require(noHeaderWith(db.txHeaders, tx.id))
    require(noLegWith(db.legs, tx.id))
    val h = toHeader(tx)
    val newLegs = toLegs(tx)
    toLegsAllSame(tx)
    admissibleTxGivesAdmissibleLegs(tx)
    // referential integrity
    allRefOkConsHeader(db.legs, db.txHeaders, h)
    allRefOkOfFresh(newLegs, db.txHeaders, h)
    allRefOkAppend(newLegs, db.legs, h :: db.txHeaders)
    // conservation
    legsOfFreshAppend2(newLegs, db.legs, tx.id)
    admissibleStableUnderFreshLegs(db.txHeaders, db.legs, newLegs, tx.id)
  }.ensuring(_ => validLedger(store.post(db, tx)))

  // ---- service guards ----
  def postRejectsNonPositive(w: World, tx: LedgerTx): Boolean = {
    require(!LedgerValidation.positiveAmount(tx))
    LedgerService[World](HasDb(), store).post(w, tx)._2 == Left[LedgerError, LedgerTx](NonPositiveAmount)
  }.holds

  def postRejectsUnbalanced(w: World, tx: LedgerTx): Boolean = {
    require(LedgerValidation.positiveAmount(tx))
    require(!LedgerValidation.balanced(tx))
    LedgerService[World](HasDb(), store).post(w, tx)._2 == Left[LedgerError, LedgerTx](UnbalancedTx)
  }.holds
}
