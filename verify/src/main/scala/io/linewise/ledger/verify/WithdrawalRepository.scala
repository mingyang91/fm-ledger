package io.linewise.verify.fm.ledger

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong
import LedgerModel._
import io.linewise.ledger.Db

/* =============================================================================
 * WITHDRAWAL — a second verified repository slice in the World. A Withdrawal carries
 * its lifecycle status as a string ("pending_review" -> "submitted" -> "settled", or
 * "rejected"/"cancelled"); the WithdrawalService advances it while posting the matching
 * balanced LedgerTx through the ledger lens. Unlike the append-only ledger, this slice
 * is UPSERT-by-id (`put`), because a status transition rewrites the same withdrawal row
 * — the money movements themselves stay append-only in the ledger.
 *
 * Same shape as the ledger repo and the pet store repos: sealed abstract + @ghost rows
 * + a head-match @law (putGet) discharged by the InMem oracle and trusted on the
 * field-less @extern Jdbc realization that delegates to the production Quill `Db`.
 * ========================================================================== */
case class Withdrawal(
    id:              FMLong,
    userUid:         String,
    amount:          FMLong,
    status:          WithdrawalStatus,
    clientRequestId: String,
    reserveTxId:     FMLong,
)

sealed abstract class WithdrawalRepository {
  @ghost def rows: List[Withdrawal]

  def put(wd: Withdrawal): WithdrawalRepository
  def get(id: FMLong): Option[Withdrawal]
  def findByClientReq(userUid: String, clientRequestId: String): Option[Withdrawal]
  def all: List[Withdrawal]

  @law def putGet(wd: Withdrawal): Boolean =
    put(wd).get(wd.id) == Some[Withdrawal](wd)
}

case class InMemWithdrawalRepository(items: List[Withdrawal]) extends WithdrawalRepository {
  @ghost def rows: List[Withdrawal] = items
  // upsert: drop any existing row with the same id, then prepend (so get-after-put
  // head-matches the new row regardless of the prior state).
  def put(wd: Withdrawal): WithdrawalRepository =
    InMemWithdrawalRepository(wd :: items.filter((x: Withdrawal) => x.id != wd.id))
  def get(id: FMLong): Option[Withdrawal] = items.find((x: Withdrawal) => x.id == id)
  def findByClientReq(userUid: String, clientRequestId: String): Option[Withdrawal] =
    items.find((x: Withdrawal) => x.userUid == userUid && x.clientRequestId == clientRequestId)
  def all: List[Withdrawal] = items
}

case class JdbcWithdrawalRepository() extends WithdrawalRepository {
  @ghost def rows: List[Withdrawal] = Nil[Withdrawal]()

  @extern @pure
  def put(wd: Withdrawal): WithdrawalRepository = { Db.withdrawalPut(wd); JdbcWithdrawalRepository() }.ensuring((res: WithdrawalRepository) =>
    res.get(wd.id) == Some[Withdrawal](wd))

  @extern @pure
  def get(id: FMLong): Option[Withdrawal] = Db.withdrawalById(id)

  @extern @pure
  def findByClientReq(userUid: String, clientRequestId: String): Option[Withdrawal] =
    Db.withdrawalByClientReq(userUid, clientRequestId)

  @extern @pure
  def all: List[Withdrawal] = Db.allWithdrawals
}
