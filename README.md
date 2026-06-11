# fm-ledger — a formally verified incentive-points ledger

A double-entry **ledger** for incentive points (earning, two-person adjustments, withdrawals, and
recipient-pays-fee payouts via Stripe) whose business core is **proven correct with
[Stainless](https://github.com/epfl-lara/stainless)** and then **transpiled into the production
service**. The accounting rules are not tested into place; they are machine-checked, and the running
PostgreSQL-backed server is generated from the same verified source.

The interesting claim is the pipeline, not the domain. Most systems verify a model and then
re-implement it by hand, hoping the two stay in sync. Here the verified model *is* the implementation
— a transpiler rewrites the proven Scala into production Scala — and a **differential drift gate**
machine-checks that the live SQL behaves identically to the verified in-memory oracle on random
operation sequences. So the chain from "the proof holds" to "the server is correct" has no
hand-maintained gap.

```
 verify/  ──(Stainless: ./mill verify.scala)──▶  618 VCs proven, 0 unknown
   │
   │  ./mill transpile   (build-time, scalameta source-rewrite: verified Scala → production Scala)
   ▼
 dev.mingyang91.ledger.generated   (regenerated on every compile; never checked in)
   │
   ▼
 src/  hand-written shell: tapir HTTP, Magnum/PostgreSQL, Stripe, the outbox dispatcher
   │
   ▼
 differential drift gate  ──▶  209 tests, incl. proof-oracle ≡ production-SQL on random op sequences
```

## What is proven

The verified core (Stainless, `verify/`) establishes, over the relational database as one structure:

- **Conservation** — every posted transaction balances (total debits equal total credits), so the
  ledger can never mint or destroy value.
- **Referential integrity** — every leg points at a live transaction header, every status-change row
  at a live withdrawal/proposal, every payout dispatch/reconciliation at a live intent.
- **Primary-key distinctness** for every table, and a **cross-table foreign key** — every payout
  intent references a live withdrawal — the graph-shaped invariant a tree-shaped aggregate cannot
  even state.
- **Idempotency** — source-keyed incentive credits, client-request-keyed withdrawals, and
  provider-event-keyed webhooks each apply at most once.
- **Two-person control** — a manual adjustment cannot be approved by its proposer.
- **Recipient-pays-fee payout settlement** — the three-transaction settlement is balanced and
  releases exactly the gross to clearing, with matched-fee neutrality and fee-variance auditability,
  and a duplicate provider event posts nothing twice.

A whole-database theorem (`WorldProofs`) shows the top-level operations preserve all of the above at
once. The drift gate then ties this to production: the in-memory oracle and the JDBC realization are
replayed in lockstep on random operations and asserted equal on every verdict and on the final state.

## The state model

The verified state is **one relational `DB` of flat row-tables** — `case class World(db: DB)`,
`DB(txHeaders, legs, withdrawals, wstatuses, proposals, pstatuses, obligations, intents, dispatches,
events, recons)` — reached through a single `Has[World, DB]` lens. The ledger is a header table plus
a leg table; withdrawals and proposals are a row plus a status-change *event stream* whose current
status is a latest-row join (matching the SQL `ORDER BY id DESC LIMIT 1`); obligations are a
composite-key table; payouts are intent/dispatch/event/reconciliation. In production this `DB` is an
empty placeholder — point reads and writes go straight to PostgreSQL through the store seam — so the
proof reasons over a materialized model while the server stays lazy and bounded.

## Tech stack

Scala 3.8.1 on the JVM, built with [Mill](https://mill-build.org) 1.1.5. The server is direct-style
([Ox](https://ox.softwaremill.com) virtual threads) with [tapir](https://tapir.softwaremill.com) over
a sync Netty backend; persistence is PostgreSQL via [Magnum](https://github.com/AugustNagro/magnum)
at `SERIALIZABLE`; payouts integrate Stripe Connect transfers through a transactional outbox.
Verification runs Stainless in Docker; the test suite uses Testcontainers PostgreSQL.

## Getting started

Prerequisites: **JDK 25+** (the persistence layer uses `java.lang.ScopedValue`), **Docker** (for the
Stainless verifier image and for Testcontainers), and the bundled `./mill` launcher.

```bash
./mill verify.scala      # run Stainless over the whole verified core (Docker; ~20s warm)
./mill compile           # transpile the verified core, then compile generated + shell
./mill test              # endpoint specs + the differential drift gate (Testcontainers PostgreSQL)
./mill run               # start the server on http://localhost:8081
```

`./mill verify.scala A.scala B.scala …` verifies only the named files (pass their dependencies too).
The Stainless VC cache is structural and persistent, so a warm re-run re-solves only what changed.

## HTTP surface

41 tapir endpoints (`dev.mingyang91.ledger.LedgerEndpoints`), bearer-authenticated except the
signature-authenticated Stripe webhook: incentive credits; two-person adjustments and rollback
reversals; the withdrawal lifecycle (request → approve → submit → settle/fail) with its payout
intent, outbox dispatch, provider events, and reconciliation; pending-work obligations; a two-person
system-config workflow; balances and a funds-invariant summary; and an invariants-check endpoint.

## Layout

| Path | What |
|---|---|
| `verify/` | the Stainless source of truth — the executable model + the `*Proofs`/`*Invariants` |
| `transpiler/` | the build-time verified-Scala → production-Scala transpiler |
| `src/main/` | the hand-written shell: HTTP, persistence, Stripe, the dispatcher |
| `src/test/` | endpoint specs + the differential drift gate |
| `docs/` | design notes, the Stripe integration writeup, and the engineering friction log |

## Further reading

- **[CLAUDE.md](CLAUDE.md)** — the design principles, the build graph, and the conventions that keep
  the verified core transpile-clean.
- **[docs/engineering-notes.md](docs/engineering-notes.md)** — a friction log of the non-obvious
  traps hit while building this (the lens-law blow-up that drove the single-`DB` design, why the
  store carries no `@law`, why the `DB` stays a ghost in production, transpile-clean recursion, and
  more), each with its resolution.
- **[docs/stripe-integration.md](docs/stripe-integration.md)** — the outbound transfer + inbound
  webhook design.

## Status

This is a reference implementation and a research vehicle for the FM-first pipeline, not a hosted
product. The verified core (618 VCs) and the differential gate (209 tests) are the contract; treat
everything in `src/` as the replaceable shell around them.

## License

[MIT](LICENSE) © 2026 mingyang91
