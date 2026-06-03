package io.linewise.verify.fm.jdbcpattern

import stainless.lang._
import stainless.annotation._
import stainless.collection._
import io.linewise.verify.effect.FMLong

/* =============================================================================
 * VERIFIED REFERENCE — the @ghost-rows + axiomatized-ops + @extern-REAL-JDBC
 * repository (no whole-table load). This is the validated, complete shape to roll
 * out across the pet store's PetRepository / OrderRepository / UserRepository,
 * replacing the wrong "load every row into a List per call" JdbcWorld.
 *
 * THE SHAPE (verified here):
 *   - `sealed abstract class Repo` with a `@ghost rows: List[Item]` MODEL. `rows`
 *     is ghost: spec-only, ERASED in production (the JDBC realization never
 *     materializes the table). `sealed` (not @mutable) lets the JDBC subclass hold
 *     an external resource without tainting the hierarchy mutable.
 *   - Ops are AXIOMATIZED ALGEBRAICALLY as `@law`s over observable behaviour
 *     (save-then-get returns the item; save preserves other gets), NOT as
 *     `get == rows.find(...)` — the latter ties a non-ghost result to ghost state,
 *     which Stainless rejects.
 *   - The IN-MEMORY realization discharges the axioms (the verified oracle).
 *   - The JDBC realization wraps `java.sql.Connection` behind an `@extern` field,
 *     keeps a `@ghost` model, and implements each op as `@extern @pure` REAL
 *     per-operation SQL — a single SELECT ... WHERE id / INSERT, binding params and
 *     extracting the row. `@extern` havocs the body (the ensuring is trusted);
 *     `@pure` blocks mutation inference so no `@mutable` is needed.
 *   - THE MODELED-LONG SHIM (fmFromLong / longOfFM): Stainless's BigInt has no
 *     apply(Long) and no native Long, so JDBC's getLong/setLong are bridged through
 *     String (BigInt(l.toString)) and BigInt.toLong. These `@extern @pure` shims let
 *     a row-returning READ extract values inside a verified @extern body.
 *
 * ACCEPTED SOUNDNESS RISK (the user's call): `@pure` over effectful DB ops is sound
 * only under LINEAR use of the repo value; the contract is TRUSTED at the @extern
 * boundary and the in-memory-vs-JDBC differential test is the machine-checked guard.
 * ========================================================================== */
object JdbcRepositoryPattern {

  case class Item(id: FMLong, payload: FMLong)

  sealed abstract class Repo {
    @ghost def rows: List[Item]
    def get(id: FMLong): Option[Item]
    def save(item: Item): Repo

    @law def saveGetSame(item: Item): Boolean =
      save(item).get(item.id) == Some[Item](item)
    @law def saveGetOther(item: Item, id: FMLong): Boolean =
      item.id == id || save(item).get(id) == get(id)
  }

  case class InMemRepo(items: List[Item]) extends Repo {
    @ghost def rows: List[Item] = items
    def get(id: FMLong): Option[Item] = items.find((x: Item) => x.id == id)
    def save(item: Item): Repo = InMemRepo(item :: items)
  }

  // wrap the JDK Connection behind an @extern field (Stainless doesn't model it).
  class Conn(@extern val underlying: java.sql.Connection)

  // the modeled-Long shim — bridge JDBC's Long <-> FMLong through String/BigInt.
  @extern @pure def fmFromLong(l: Long): FMLong = FMLong(BigInt(l.toString))
  @extern @pure def longOfFM(f: FMLong): Long = f.value.toLong

  case class JdbcRepo(conn: Conn, @ghost ghostRows: List[Item]) extends Repo {
    @ghost def rows: List[Item] = ghostRows

    // REAL per-op SELECT — binds id, extracts the row via the shim. (havoc'd by
    // @extern; kept by the transpiler as the production read; @ghost rows erased.)
    @extern @pure
    def get(id: FMLong): Option[Item] = {
      val ps = conn.underlying.prepareStatement("SELECT id, payload FROM item WHERE id = ?")
      ps.setLong(1, longOfFM(id))
      val rs = ps.executeQuery()
      if rs.next() then Some[Item](Item(fmFromLong(rs.getLong("id")), fmFromLong(rs.getLong("payload"))))
      else None[Item]()
    }

    // REAL per-op INSERT.
    @extern @pure
    def save(item: Item): Repo = {
      val ps = conn.underlying.prepareStatement("INSERT INTO item (id, payload) VALUES (?, ?)")
      ps.setLong(1, longOfFM(item.id))
      ps.setLong(2, longOfFM(item.payload))
      ps.executeUpdate()
      JdbcRepo(conn, item :: ghostRows)
    }.ensuring((res: Repo) =>
      res.get(item.id) == Some[Item](item) &&
      forall((id: FMLong) => id == item.id || res.get(id) == get(id)))
  }

  // the contract is realization-agnostic: proven over the abstract Repo via its
  // axioms, it holds for BOTH the in-memory oracle and the @extern real-JDBC repo.
  def saveThenGetFindsIt(repo: Repo, item: Item): Boolean = {
    require(repo.saveGetSame(item))
    repo.save(item).get(item.id) == Some[Item](item)
  }.holds
}
