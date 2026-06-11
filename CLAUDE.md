# CLAUDE.md

Guidance for Claude Code and other agents working in **fm-ledger** — a formal-methods-first
incentive-points **ledger**. The verified Stainless core is the single source of truth; production
Scala is *generated* from it by a transpiler, with a thin hand-written shell around the edges.

`AGENTS.md` is a compatibility symlink to this file.

> Hard-won, non-obvious lessons from real migrations live in **[docs/engineering-notes.md](docs/engineering-notes.md)** — read it before touching the verified core, the transpiler, or the proofs.

## The FM-first pipeline (the whole point)

```
 verify/  ──(Stainless, ./mill verify.scala)──▶ proven correct
   │
   │  ./mill transpile  (build-time, scalameta text-rewrite: Stainless → Ox/JDBC)
   ▼
 out/transpile.dest/dev/mingyang91/ledger/generated/   (NOT checked in; regenerated each compile)
   │
   ▼
 src/  (hand-written shell: HTTP/tapir, Persistence, Stripe, dispatcher) ── ./mill compile / test
```

- **`verify/`** is the source of truth. Edit the model and the proofs here.
- The **transpiler** (`transpiler/`, a separate Mill module) rewrites the *executable* verified files
  into `dev.mingyang91.ledger.generated`. It is a build-time tool, never shipped.
- **`src/`** is the production shell that imports the generated core and adds I/O (PostgreSQL via
  Magnum, tapir HTTP, Stripe, the outbox dispatcher). The generated core is wired in via
  `generatedSources`, so `./mill compile` regenerates it automatically.

## Design principles

1. **State is one relational `DB` of flat row-tables.** `case class World(db: DB)`,
   `case class DB(txHeaders: List[TxHeaderRow], legs: List[LegRow], …)` — eleven tables matching the
   production DDL. No aggregates, no per-entity repositories. The ledger is `TxHeader` + `Leg`;
   withdrawals/proposals are a row + a status-change **event stream** (current status = latest-row
   join); obligations are a composite-key table; payouts are intent/dispatch/event/recon.
2. **One lens, `Has[World, DB]`.** Services are generic in `W` and reach state through a single
   `HasDb` lens, then read/modify individual tables by plain `db.copy(...)`. *Never* a lens per
   table — that is what makes the `@law` get/set obligations explode (see engineering-notes §1).
3. **The `DB` is a verification model; production stays lazy.** In production the `World`/`DB` is an
   empty placeholder — the `@extern` Jdbc store realizations ignore it and hit the real `Db` facade
   (one indexed row per point-read). Materializing whole tables into a `World` is forbidden; it
   re-introduces unbounded reads. The verified materialized `DB` is the spec **and** the in-memory
   oracle; production correctness is tied to it by the **drift gate**, not by sharing runtime state.
4. **`valid(db)` is the whole-database invariant.** Referential integrity, primary-key distinctness,
   ledger conservation, and the **cross-slice foreign keys** (e.g. every payout intent references a
   live withdrawal) are one predicate over the whole `DB`. Operations are proven to preserve it
   per-slice and, in `WorldProofs`, across all slices at once. Cross-table FKs are the reason the
   model is a graph (`DB`), not a tree.
5. **Two-tier separation, enforced by a whitelist.** Files transpiled into production are listed in
   `ledgerVerifiedSources` (build.mill) and must be **transpile-clean** (no `.holds`, no `@induct`,
   no `case Nil()/Cons()` — see engineering-notes §3). All lemmas and `.holds` proofs live in
   separate `*Proofs.scala` / `*Invariants.scala` files that are NOT in the whitelist.
6. **Stores are the seam.** Each slice has a `sealed abstract …Store` with an `InMem` realization
   (pure `db.copy`, the oracle) and an `@extern` `Jdbc` realization (the production SQL). Services
   take `Has[World, DB]` + the store(s) they need.
7. **The drift gate is the bridge.** `LedgerPropertiesSpec` replays a random op sequence in lockstep
   against the InMem oracle and the JDBC realization and asserts they agree on every verdict and the
   final state. This is what licenses trusting the `@extern` axioms.
8. **Ids are `FMLong` (→ `Long`), amounts are `FMLong`.** Membership is always a first-order
   recursive predicate, never `List.contains` (engineering-notes §2).

## Layout

| Path | What |
|---|---|
| `verify/src/main/scala/dev/mingyang91/ledger/verify/` | the verified core (executable model + proofs) |
| `verify/src/main/scala/dev/mingyang91/ledger/Db.scala` | `@extern` stub for the production `Db` facade (verify-only; never transpiled) |
| `verify/stainless-lib/` | `FMLong`/`FMInt`, `SafeArith`, collection helpers |
| `transpiler/` | the Stainless→Ox transpiler (scalameta text-rewrite) |
| `src/main/scala/dev/mingyang91/ledger/` | production shell: `LedgerHttp`, `Persistence`, `Jdbc`, `PayoutDispatcher`, `PayoutGateway`, `StripeSignature`, `LedgerServer` |
| `src/test/scala/dev/mingyang91/ledger/` | endpoint specs + the differential drift gate |
| `docs/` | design notes; **engineering-notes.md** = the friction/lessons log |

Executable model files: `Tables.scala` (`DB`/`World`/`HasDb`/rows/DTOs), `LedgerTables`,
`WithdrawalTables`, `ProposalTables`, `ObligationTables` (the stores), the four `*Service` files,
`LedgerModel`, `LedgerValidation`, `Has`. Proof-only files: `LedgerInvariants`, `LedgerProofs`,
`WithdrawalProofs`, `AdjustmentProofs`, `ObligationProofs`, `PayoutTableProofs`, `WorldProofs`,
`PayoutProofs`, `PayoutLifecycleProofs`, `AccountAssertions`, `PayoutUnits`.

## Commands

```bash
./mill verify.scala                 # Stainless on ALL of verify/ (Docker; warm VC cache at verify/.stainless-cache)
./mill verify.scala A.scala B.scala # verify ONLY these files (must pass every dependency, incl. stainless-lib/FMTypes.scala)
./mill compile                      # transpile verified core → generated, then compile generated + shell
./mill transpile                    # regenerate the production core on demand
./mill test                         # endpoint specs + drift gate (Testcontainers PostgreSQL)
```

Verification runs in a Docker image built from `verify/stainless/`. The VC cache is **structural and
persistent** — iterate fast by verifying a single slice's files in isolation; a warm re-run re-solves
only what changed. (Caveat: with explicit file args there is no auto-discovery, so pass every
dependency, e.g. `verify/stainless-lib/FMTypes.scala`.)

## Conventions

- **Leak guard.** `build.mill` fails the build if any non-comment line of generated output contains
  `Nil(`, `Nil[`, `Cons(`, or `.holds`. Keep proof scaffolding out of whitelisted files.
- **Recursion in transpiled files** uses `if l.isEmpty then … else …(l.head, l.tail)`, never
  `case Nil()/Cons()` (the transpiler does not rewrite List patterns). `Nil[T]()` is fine (rewritten
  to `Nil`). Higher-order ops (`map`/`filter`/`find`/`exists`/`forall`) are also fine.
- **Proofs go in proof-only files.** If a predicate is only used by proofs, it does not belong in a
  whitelisted (transpiled) file.
- **Separate git repo.** fm-ledger has its own git, outside any parent supermodule. Commit/push
  only when asked. Commit trailer: `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`.
- **Read [docs/engineering-notes.md](docs/engineering-notes.md)** before non-trivial work in the
  verified core — it records the specific traps (lens-law blow-up, `FMLong` vs `List.contains`,
  transpile-clean recursion, the `Long | BigInt` union, the drift-gate seam, restoring vs rewriting
  self-contained proof files) and how each was resolved.
