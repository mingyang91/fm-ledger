# Engineering notes — FM ledger friction log

Hard-won, non-obvious lessons from working in the verified core, the transpiler, and the proofs.
Each entry is a real trap hit during development, why it happens, and the resolution. Section
numbers are referenced from `CLAUDE.md`.

---

## §1 — One `Has[World, DB]` lens, never a lens per table

**Symptom.** Modelling state as `World(table1: List[Row1], table2: …, …)` with a `Has[World, List[RowN]]`
lens per table makes Stainless time out (120s, `Pfolio`) on the `Has` `@law` obligations
(`lawGetSet`, `lawSetGet`) — not on the table proofs themselves.

**Why.** `lawSetGet(w) = set(w, get(w)) == w` becomes `w.copy(fieldN = w.fieldN) == w` over a record
with many `List` fields; proving that structural equality drags Stainless into inductive list
reasoning, multiplied across every table lens, under the heavy `FMLong` arithmetic context.

**Fix.** Wrap all tables in one `DB` object and expose a single `Has[World, DB]` lens
(`World(db: DB)`). Then `lawSetGet` is `w.copy(db = w.db) == w` — a one-field case-class copy that
discharges trivially, exactly as the old four-field aggregate `World` did. Services take the one
lens and read/modify individual tables by plain `db.copy(tableN = …)` — no per-table lens, no
per-table law. This is also the honest shape: cross-table foreign keys mean the tables are one
integral structure, so `valid(db)` is naturally one predicate.

---

## §2 — `FMLong` ids vs `List.contains`

**Symptom.** Referential-integrity predicates written with `idList.contains(row.fkId)` fail to unify
across proof points / behave unexpectedly when the ids are `FMLong`.

**Why.** `FMLong` defines a custom `==`, which does not unify with the structural equality
`List.contains` expects.

**Fix.** Never use `List.contains` on a list of `FMLong` ids. Use **first-order recursive membership
predicates** instead:

```scala
def hasHeader(hs: List[TxHeaderRow], id: FMLong): Boolean = hs match
  case Nil()      => false
  case Cons(h, t) => h.id == id || hasHeader(t, id)
def allRefOk(legs: List[LegRow], hs: List[TxHeaderRow]): Boolean = legs match
  case Nil()      => true
  case Cons(l, t) => hasHeader(hs, l.txId) && allRefOk(t, hs)
```

Direct `==` on `FMLong` inside a recursive predicate is fine; it is only the higher-order
`List.contains` that breaks. (These predicates live in proof-only files, so `case Nil()/Cons()` is
allowed there — contrast §3.)

---

## §3 — Transpile-clean recursion: `if/isEmpty/head/tail`, not `case Nil()/Cons()`

**Symptom.** The build fails with `[transpile] proof scaffolding leaked into …` after adding a
recursive function (e.g. a direct-recursion sum) to a *whitelisted* file.

**Why.** The transpiler rewrites `Nil[T]()` → `Nil` and `None[T]()` → `None`, but it does **not**
rewrite Stainless `List` patterns: `case Nil()` and `case Cons(h, t)` pass through verbatim, and the
leak guard (`build.mill`) rejects any generated non-comment line containing `Nil(`, `Cons(`, `Nil[`,
or `.holds`.

**Fix.** In files that get transpiled, write recursion with `isEmpty`/`head`/`tail`:

```scala
def sumDirection(es: List[LedgerEntry], d: EntryDirection): BigInt =
  if es.isEmpty then BigInt(0)
  else { val rest = sumDirection(es.tail, d); if es.head.direction == d then rest + es.head.amount.value else rest }
```

`Nil[T]()` (with the type) is fine — the transpiler erases it. Higher-order ops (`map`, `filter`,
`find`, `exists`, `forall`) are fine. Pattern-matching on `Option` (`Some`/`None`) is fine. Only
`List` constructor patterns leak. Proof-only files (not whitelisted) may use `case Nil()/Cons()`
freely.

---

## §4 — The `Long | BigInt` union after `.value` erasure

**Symptom.** Generated code fails to compile: `value + is not a member of (… : Long) | BigInt`.

**Why.** In `verify/`, `e.amount.value` is `BigInt` and `BigInt(0)` is `BigInt`, so an
`if cond then e.amount.value else BigInt(0)` is uniformly `BigInt`. The transpiler erases
`FMLong` → `Long` and `.value` → nothing, so post-transpile the then-branch is `Long` and the
else-branch is `BigInt` — the `if` becomes a union type with no `+`.

**Fix.** Keep the `if` branches uniformly `BigInt` *and* put the `+` on a `BigInt` receiver, so the
implicit `Long → BigInt` only ever applies to an argument:

```scala
val rest = sumDirection(es.tail, d)               // BigInt
if es.head.direction == d then rest + es.head.amount.value else rest
```

The original `foldLeft` form avoided this by accumulating on a `BigInt`; the recursive form must do
the same.

---

## §5 — Don't put a conditional / round-trip `@law` on a store

**Symptom.** A `@law postGet(db, tx) = get(post(db, tx), tx.id) == Some(tx)` with a `require(...)`
precondition produces an INVALID "inlined precondition" VC and times out.

**Why.** `@law`s are universally-quantified contracts; preconditions on them are awkward, and the
header/leg split makes the get∘post round-trip a non-trivial filter-join lemma rather than the
aggregate's trivial find-first.

**Fix.** The stores carry **no `@law`**. Safety is the `valid(db)`-preservation theorems proved over
the concrete `InMem` store; the `@extern` `Jdbc` store is trusted and reconciled to `InMem` by the
drift gate. The aggregate's `postGet` simply does not survive the table split, and it is not needed.

---

## §6 — `@ghost`/`DB` is load-bearing: do not materialize tables in production

**Symptom (avoided).** Tempting to make `World` hold real `List[Row]` fields end to end.

**Why it's wrong.** Building such a `World` in production means loading whole tables into memory on
every request — the exact unbounded-read problem the list endpoints were just paginated to fix.

**Fix.** The materialized `DB` is the **verification model and the in-memory oracle only**. In
production the `World`/`DB` is an empty placeholder; the `@extern` `Jdbc` stores ignore it and do
bounded SQL (point reads by id, paginated lists). Production behaviour is unchanged from the old
fieldless `Jdbc*Repository`; only the *shape of the proof model* changed. The bridge from
"proven about the materialized oracle" to "what production runs" is the drift gate, not shared state.

---

## §7 — Make the InMem oracle as lossy as production (drift-gate fidelity)

**Symptom.** A boundary DTO has fields the DDL does not persist (e.g. `Obligation.role` /
`projectRef` / `taskKind`; the SQL table has only `ESTIMATED_POINTS`). If the InMem oracle keeps
them but JDBC reads them back as `""`, the drift gate diverges.

**Fix.** The InMem store must assemble exactly what production does: read the row back with the
ephemeral fields blanked (`""`), matching `toObligation`. The oracle being "lossy in the same way"
as the SQL store is what keeps the differential test green — and is more faithful to reality.

---

## §8 — Incremental migration is blocked by name collisions; verify slices in isolation

**Symptom.** While the new `Tables.scala` (defining `World`) coexists with the old `World.scala`,
the default `./mill verify.scala` fails on a duplicate `World`. Likewise the old repository files
re-define the boundary DTOs that moved into `Tables.scala`.

**Fix.** During a big-bang migration, verify each new slice **in isolation** by passing only its
files (+ deps) to `./mill verify.scala`, excluding the old core. Do the deletion of the old
aggregate files as one step, then run the full verify. Expect the default full verify to be red
mid-migration; that is normal and resolves the moment the old files come out.

---

## §9 — Conservation through the header/leg split

**Symptom.** Proving "every tx balances" over `TxHeader` + `Leg` tables needs `admissibleLegs(toLegs(tx))`
from `admissible(tx)`, and the directional sums fight `foldLeft`.

**Fix.** Define directional sums by **direct recursion** (not `foldLeft`) so there is no
`foldLeft`-shift lemma, then prove a small `toLegs(tx)` ↔ `tx.entries` correspondence
(`allSameTx`, positivity, has-direction, sum-equality) by induction. With both sides direct-recursive
and `toLegs` a structural map, the correspondence is mechanical. (Note the direct-recursion sum must
also be transpile-clean per §3/§4 if it lives in `LedgerModel`.)

---

## §10 — Restore self-contained proof files; don't rewrite them

**Symptom.** A large proof file (`PayoutLifecycleProofs`, 948 lines) appears to need porting after
the core changes.

**Why it's cheaper than it looks.** Check what it actually references. `PayoutLifecycleProofs` and
the settlement `PayoutProofs` define their *own* `PayoutWorld` + row types and depend only on
`LedgerTx`, the `Withdrawal` DTO, and `AccountAssertions` — none of the retired aggregate
repositories or `World`. So they can be **restored verbatim** from git, not rewritten; the only
fixes were freeing the `PayoutProofs` name (the new structural proofs became `PayoutTableProofs`)
and re-adding one dropped helper (`LedgerInvariants.validWithdrawalRows`).

**Rule.** Before porting a proof file, `grep` it for references to the types you deleted. If there
are none, restore it. `AggregatePreservationProofs`, by contrast, was built entirely on the deleted
`AggregateWorld`/repos and is genuinely superseded by the unified-DB `WorldProofs` capstone — drop
it rather than port it.

---

## §11 — A dropping VC count can be correct

After the migration the verify count went from ~989 to ~635, and that is the *right* number. The
old total included the `relational/` files (which proved the table invariants in **isolation** as a
parallel sketch) and a throwaway `experiment/`. Those same invariants are now proven directly over
the **live** model, which is strictly stronger, so keeping the isolated copies would only verify
them twice. Don't chase a VC number; chase coverage of the code that ships.

---

## §12 — `verify.scala` file args + the structural cache

- `./mill verify.scala` with **no args** verifies all of `stainless-lib` + every auto-discovered
  `verify/` source. With **explicit args** it verifies *only* those files — there is no
  auto-discovery, so you must pass every dependency yourself (commonly
  `verify/stainless-lib/FMTypes.scala`, `LedgerModel.scala`, `Has.scala`, `Tables.scala`).
- The VC cache (`verify/.stainless-cache`, gitignored) is **structural**: a warm re-run reuses every
  previously-solved VC and a one-function change re-solves only what changed. This is why
  per-slice isolated verification is fast — lean on it during development.

---

## §13 — Two `Db.scala` files

There are two files named `Db.scala`, which is confusing: `verify/.../io/linewise/ledger/Db.scala`
is the `@extern` **stub** for the production persistence facade (verify-only, never transpiled,
references the boundary DTOs), while the relational state object is `DB` (capitalised) in
`verify/.../verify/Tables.scala`. Keep them distinct — the migration originally named the new file
`Db.scala` too and had to be renamed to `Tables.scala` to avoid the clash.
