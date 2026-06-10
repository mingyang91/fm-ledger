package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import LedgerTables._

/* =============================================================================
 * VERIFY-ONLY invariants over the relational DB — the durable promises of the whole database as
 * one structure: referential integrity (every leg/status/dispatch points at a live parent), primary-
 * key distinctness, ledger conservation (every tx balances), and the cross-table foreign key (every
 * payout intent references a live withdrawal). `valid(db)` conjoins them all.
 *
 * Every membership test is a FIRST-ORDER RECURSIVE predicate (hasHeader / hasWithdrawal / hasIntent),
 * never List.contains — FMLong's custom == does not unify through List.contains.
 * ========================================================================== */
object LedgerInvariants {

  // ---------- LEDGER (header + leg) ----------
  def hasHeader(hs: List[TxHeaderRow], id: FMLong): Boolean = hs match
    case Nil()      => false
    case Cons(h, t) => h.id == id || hasHeader(t, id)
  def allRefOk(legs: List[LegRow], hs: List[TxHeaderRow]): Boolean = legs match
    case Nil()      => true
    case Cons(l, t) => hasHeader(hs, l.txId) && allRefOk(t, hs)
  def distinctHeaders(hs: List[TxHeaderRow]): Boolean = hs match
    case Nil()      => true
    case Cons(h, t) => noHeaderWith(t, h.id) && distinctHeaders(t)

  def dirSum(legs: List[LegRow], d: EntryDirection): BigInt = legs match
    case Nil()      => BigInt(0)
    case Cons(l, t) => (if l.dir == d then l.amount.value else BigInt(0)) + dirSum(t, d)
  def hasDir(legs: List[LegRow], d: EntryDirection): Boolean = legs match
    case Nil()      => false
    case Cons(l, t) => l.dir == d || hasDir(t, d)
  def allLegsPositive(legs: List[LegRow]): Boolean = legs match
    case Nil()      => true
    case Cons(l, t) => l.amount > FMLong(BigInt(0)) && allLegsPositive(t)
  def admissibleLegs(legs: List[LegRow]): Boolean =
    legs.nonEmpty && allLegsPositive(legs) && hasDir(legs, EntryDirection.DR) && hasDir(legs, EntryDirection.CR) && dirSum(legs, EntryDirection.DR) == dirSum(legs, EntryDirection.CR)
  def allHeadersAdmissible(hs: List[TxHeaderRow], legs: List[LegRow]): Boolean = hs match
    case Nil()      => true
    case Cons(h, t) => admissibleLegs(legsOf(legs, h.id)) && allHeadersAdmissible(t, legs)

  def refIntegrityL(db: DB): Boolean = allRefOk(db.legs, db.txHeaders)
  def distinctHeadersL(db: DB): Boolean = distinctHeaders(db.txHeaders)
  def conservationL(db: DB): Boolean = allHeadersAdmissible(db.txHeaders, db.legs)
  def validLedger(db: DB): Boolean = refIntegrityL(db) && distinctHeadersL(db) && conservationL(db)

  // ---------- WITHDRAWAL (row + status event-stream) ----------
  def hasWithdrawal(ws: List[WithdrawalRow], id: FMLong): Boolean = ws match
    case Nil()      => false
    case Cons(w, t) => w.id == id || hasWithdrawal(t, id)
  def noWithdrawalWith(ws: List[WithdrawalRow], id: FMLong): Boolean = ws match
    case Nil()      => true
    case Cons(w, t) => w.id != id && noWithdrawalWith(t, id)
  def distinctWithdrawals(ws: List[WithdrawalRow]): Boolean = ws match
    case Nil()      => true
    case Cons(w, t) => noWithdrawalWith(t, w.id) && distinctWithdrawals(t)
  def wStatusRefOk(ss: List[WStatusRow], ws: List[WithdrawalRow]): Boolean = ss match
    case Nil()      => true
    case Cons(s, t) => hasWithdrawal(ws, s.withdrawalId) && wStatusRefOk(t, ws)
  def validWithdrawals(db: DB): Boolean = distinctWithdrawals(db.withdrawals) && wStatusRefOk(db.wstatuses, db.withdrawals)
  // compat predicate used by the payout-lifecycle proofs (every withdrawal amount positive)
  def validWithdrawalRows(rows: List[Withdrawal]): Boolean = rows.forall((wd: Withdrawal) => wd.amount > FMLong(BigInt(0)))

  // ---------- PROPOSAL (row + status event-stream) ----------
  def hasProposal(ps: List[ProposalRow], id: FMLong): Boolean = ps match
    case Nil()      => false
    case Cons(p, t) => p.id == id || hasProposal(t, id)
  def noProposalWith(ps: List[ProposalRow], id: FMLong): Boolean = ps match
    case Nil()      => true
    case Cons(p, t) => p.id != id && noProposalWith(t, id)
  def distinctProposals(ps: List[ProposalRow]): Boolean = ps match
    case Nil()      => true
    case Cons(p, t) => noProposalWith(t, p.id) && distinctProposals(t)
  def pStatusRefOk(ss: List[PStatusRow], ps: List[ProposalRow]): Boolean = ss match
    case Nil()      => true
    case Cons(s, t) => hasProposal(ps, s.proposalId) && pStatusRefOk(t, ps)
  def validProposals(db: DB): Boolean = distinctProposals(db.proposals) && pStatusRefOk(db.pstatuses, db.proposals)

  // ---------- OBLIGATION (composite-key single table) ----------
  def noKey(os: List[ObligationRow], sk: String, si: String): Boolean = os match
    case Nil()      => true
    case Cons(o, t) => !(o.sourceKind == sk && o.sourceId == si) && noKey(t, sk, si)
  def distinctObligations(os: List[ObligationRow]): Boolean = os match
    case Nil()      => true
    case Cons(o, t) => noKey(t, o.sourceKind, o.sourceId) && distinctObligations(t)
  def validObligations(db: DB): Boolean = distinctObligations(db.obligations)

  // ---------- PAYOUT (intent / dispatch / event / recon) ----------
  def hasIntent(is: List[IntentRow], id: FMLong): Boolean = is match
    case Nil()      => false
    case Cons(i, t) => i.id == id || hasIntent(t, id)
  def noIntentWith(is: List[IntentRow], id: FMLong): Boolean = is match
    case Nil()      => true
    case Cons(i, t) => i.id != id && noIntentWith(t, id)
  def distinctIntents(is: List[IntentRow]): Boolean = is match
    case Nil()      => true
    case Cons(i, t) => noIntentWith(t, i.id) && distinctIntents(t)
  def dispatchRefOk(ds: List[DispatchRow], is: List[IntentRow]): Boolean = ds match
    case Nil()      => true
    case Cons(d, t) => hasIntent(is, d.intentId) && dispatchRefOk(t, is)
  def reconRefOk(rs: List[ReconRow], is: List[IntentRow]): Boolean = rs match
    case Nil()      => true
    case Cons(r, t) => hasIntent(is, r.intentId) && reconRefOk(t, is)
  def noEventWith(es: List[EventRow], provider: String, eid: String): Boolean = es match
    case Nil()      => true
    case Cons(e, t) => !(e.provider == provider && e.eventId == eid) && noEventWith(t, provider, eid)
  def distinctEvents(es: List[EventRow]): Boolean = es match
    case Nil()      => true
    case Cons(e, t) => noEventWith(t, e.provider, e.eventId) && distinctEvents(t)
  def validPayout(db: DB): Boolean =
    distinctIntents(db.intents) && dispatchRefOk(db.dispatches, db.intents) &&
      reconRefOk(db.recons, db.intents) && distinctEvents(db.events)

  // ---------- CROSS-TABLE FK: payout intent -> live withdrawal ----------
  def allIntentsRefWithdrawal(is: List[IntentRow], ws: List[WithdrawalRow]): Boolean = is match
    case Nil()      => true
    case Cons(i, t) => hasWithdrawal(ws, i.withdrawalId) && allIntentsRefWithdrawal(t, ws)

  // ---------- THE WHOLE-DATABASE INVARIANT ----------
  def valid(db: DB): Boolean =
    validLedger(db) && validWithdrawals(db) && validProposals(db) && validObligations(db) &&
      validPayout(db) && allIntentsRefWithdrawal(db.intents, db.withdrawals)

  def validWorld(w: World): Boolean = valid(w.db)
}
