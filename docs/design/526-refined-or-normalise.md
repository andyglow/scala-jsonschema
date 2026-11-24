# refined: numeric `Or`/`And` simplification describes the wrong range  (issue #526, item B27)
Status: draft

## Problem

The refined module simplifies numeric bounds combined with `Or` and `And` into a single range. Several of those
simplifications describe a different set of numbers than the predicate does. The code is the same in both macros
(Scala 3 is a port of Scala 2), at master 4b2aff3: Scala 2 `modules/refined/src/main/scala-2/.../refined/AST.scala`
(`Naked` line 49, `Ge` 127, `Le` 149, `Or` 203, `And` 232, `Inside` 250, `Outside` 259), Scala 3
`modules/refined/src/main/scala-3/.../refined/RefinedMacro.scala` (`Naked` 245, `Ge` 318, `Le` 335, `Or` 390, `And` 422,
`Inside` 440, `Outside` 449).

```scala
// Or.norm (both versions)
case (l: Ge, r: Ge)    => l min r                 // inclusive = l.inclusive || r.inclusive, whatever the values
case (l: Le, r: Le)    => l max r                 // the same
case Outside(min, max) => Not(Bounded(t, min, max))
case Inside(_, _)      => Naked(l.t, prefix)      // every number
case (l, r)            => OneOf(t, ::(l, r :: Nil))
// And.norm: (Ge, Ge) => l max r, (Le, Le) => l min r (the same "inclusive if either"), Inside => Bounded, Outside => error
// Outside: (l: Ge, r: Le) if l.vv > r.vv => Some((l, r));  (l: Le, r: Ge) if l.vv < r.vv => Some((l, r))
```

What goes wrong:

1. `Outside` returns its pair in the order written. For `Greater[7] Or Less[2]` that is `(Ge 7, Le 2)`, so `Bounded` gets
   min = 7, max = 2: the inner schema `{exclusiveMinimum 7, exclusiveMaximum 2}` matches nothing and the `not` matches
   every number.
2. For `Less[2] Or Greater[7]` the pair is `(Le 2, Ge 7)`, so the bounds are in the right place. But the complement of
   `x < 2 or x > 7` is `2 ≤ x ≤ 7`, and `Bounded` keeps the operands' own inclusiveness: `not{exclusiveMinimum 2,
   exclusiveMaximum 7}`, which accepts 2 and 7.
3. A same-direction merge always makes the result inclusive if either side is. That is right only on a tie. For `Or`
   the looser bound wins and keeps its own inclusiveness. For `And` the tighter bound wins. Today
   `Greater[2] Or GreaterEqual[7]` → `minimum 2` and `Greater[5] And GreaterEqual[3]` → `minimum 5`.
4. `Inside`/`Outside` use strict comparisons, so a tie falls through to `OneOf`. `GreaterEqual[2] Or LessEqual[2]`
   (every number) becomes `oneOf[{minimum 2}, {maximum 2}]`, and 2 matches both members, so 2 is rejected.
5. `compile` matches the operands as written, not normalized. `NonNegative`/`NonPositive` are extracted as
   `Not(Neg)`/`Not(Pos)` (Scala 2 `Extractors.scala:113-115`, Scala 3 `RefinedMacro.scala:147-149`), and a `Not` isn't a
   `Ge`/`Le`. So `NonNegative Or Greater[3]` isn't merged and becomes an overlapping `oneOf`.

Repro: `notes/scratch/c-b27/B27ProbeSpec.scala` (not committed; run from `modules/refined/src/test/scala-2.13+/b27probe/`
with networknt added to refined's test deps through an uncommitted `b27-probe.sbt`). For each predicate `P` it renders
`Json.schema[Int Refined P]` as draft-07 and compares networknt 1.5.9's verdict with refined's own
`Validate[Int, P].isValid(x)` for every integer x in -10..10. Logs `notes/scratch/c-b27/probe-2.13.18.log`,
`probe-3.3.9.log`, filtered `p-2.13.18.txt`, `p-3.3.9.txt`. **Same trees and JSON on 2.13.18 and 3.3.9.** 2.12 runs the
same Scala 2 `AST.scala` (literal types aside, `RefinedBooleanSpec`'s 2.12 copy pins the same outputs). The schema is
built at compile time, so Scala.js gets the same trees.

"As if #509 were fixed" means the probe replaced `"type": "not"` (B22, #509, which makes every `not` reject everything)
with `"type": "integer"`.

| predicate | tree today | wrong for (as if #509 were fixed) |
|---|---|---|
| `Greater[7] Or Less[2]` | `not(integer{exclMin 7, exclMax 2})` | 2..7 accepted (so: everything) |
| `Positive Or Less[-5]` | `not(integer{exclMin 0, exclMax -5})` | -5..0 accepted (everything) |
| `GreaterEqual[7] Or LessEqual[2]` | `not(integer{min 7, max 2})` | 3..6 accepted (everything) |
| `Less[2] Or Greater[7]` | `not(integer{exclMin 2, exclMax 7})` | 2, 7 accepted |
| `LessEqual[2] Or GreaterEqual[7]` | `not(integer{min 2, max 7})` | 2, 7 rejected |
| `Greater[2] Or GreaterEqual[7]` | `integer{min 2}` | 2 accepted |
| `Less[7] Or LessEqual[2]` | `integer{max 7}` | 7 accepted |
| `GreaterEqual[2] Or LessEqual[2]` | `oneof(integer{min 2}, integer{max 2})` | 2 rejected |
| `NonNegative Or Greater[3]` | `oneof(not(integer{exclMax 0}), integer{exclMin 3})` | 4..10 rejected |
| `Not[Less[2]] Or Less[7]` | `oneof(not(integer{exclMax 2}), integer{exclMax 7})` | 2..6 rejected |
| `Greater[5] And GreaterEqual[3]` | `integer{min 5}` | 5 accepted |
| `Greater[5] And GreaterEqual[5]` | `integer{min 5}` | 5 accepted |
| `GreaterEqual[2] And Greater[7]` | `integer{min 7}` | 7 accepted |
| `Less[7] And LessEqual[9]` | `integer{max 7}` | 7 accepted |

Correct today (guards): `Greater[2] Or Greater[7]` → `exclMin 2`, `Less[2] Or Less[7]` → `exclMax 7`,
`Greater[2] Or Less[7]` → `integer`, `Greater[2] Or Less[2]` → `oneof(exclMin 2, exclMax 2)` (disjoint, so the `oneOf`
is exact), `Greater[2] And Less[7]` → `{exclMin 2, exclMax 7}`. Correct only once #509 is fixed:
`Not[Less[2]] And Less[7]` → `allof(not(exclMax 2), exclMax 7)`, `NonNegative And Less[10]` →
`allof(not(exclMax 0), exclMax 10)`. Raw operands again: the result is right, it just isn't merged.

The wrong outputs are pinned as the current behaviour: `RefinedBooleanSpec` ("or": `Less[2] Or Greater[7]`, both the
2.12 and the 2.13+ copy) and `RefinedQuirksSpec` ("Or outside, lower bound first: the bounds come out swapped",
"And keeps an inclusive bound when either side is inclusive", "And matches its raw operands: a nested Not isn't
simplified").

## Why it is a problem

- The schema and the Scala type disagree about which numbers are valid. A service that validates its output
  accepts or rejects values that refined would not. Rows 1-3 accept every number, so the constraint is silently lost.
  Rows 4-14 are off at the boundary value or across a range.
- `Positive Or Less[-5]` and `x < lo or x > hi` ("outside a band") are ordinary ways to write a constraint, and
  `NonNegative` is one of refined's most common predicates.
- It has been like this since the normalisation was written (0.x, Scala 2), and the Scala 3 port copied it, so every
  version since 0.7 through 0.9.0 is affected.

## Options

The options differ in what an `Or` of two opposite bounds becomes. They all share the fixes for problems 3-5
(same-direction merges, ties, normalized operands); see "Scope" below.

Notation: `lo` is the `Ge` operand (`x > lo` or `x ≥ lo`), `hi` the `Le` operand (`x < hi` or `x ≤ hi`), in either
order. `lo ∪ hi` covers every number iff `hi > lo`, or `hi == lo` and at least one side is inclusive. Otherwise the two
are **disjoint** and leave a gap (the interval between them).

### A. Correct the `not` form: `not{the gap}`  — LOST

`Or(lo, hi)` with a gap → `Not(Bounded(gapMin, gapMax))`: `gapMin` = `hi`'s value, inclusive iff `hi` is exclusive;
`gapMax` = `lo`'s value, inclusive iff `lo` is exclusive. That is the gap itself.

```
Greater[7] Or Less[2]           → not(integer{minimum 2, maximum 7})
Less[2] Or Greater[7]           → not(integer{minimum 2, maximum 7})
GreaterEqual[7] Or LessEqual[2] → not(integer{exclusiveMinimum 2, exclusiveMaximum 7})
```

- Pro: one node per `Or`, the shape 0.x intended; the smallest change to today's code (fix `Outside`'s pair order and
  flip the inclusiveness).
- Con: every rendered `not` states `"type": "not"` until B22 (#509) lands, so until then these schemas **reject every
  value** (today rows 1-5 do too). Even after B22, its fix must keep `"type": "integer"` beside the `not` (or `2.5`
  passes `not{min 2, max 7}`); that is B22's open concern, and this option depends on it.
- Con: `not` is the keyword tooling handles worst. OpenAPI generators and form builders ignore or reject it, and a
  reader has to invert the range in their head.

Lost: correctness would depend on B22, and the output is harder to use than B's.

### B. A gap stays as the two alternatives the user wrote  — WON

`Or(lo, hi)`: covers everything → the plain type (as today's `Inside`); a gap → the operands go to `Or`'s generic
fallback unchanged (`OneOf` today, B14's `AnyOf` once B14 lands). `Or` never builds `Not(Bounded)` again.

```
Greater[7] Or Less[2]           → oneof(integer{exclusiveMinimum 7}, integer{exclusiveMaximum 2})
Less[2] Or Greater[7]           → oneof(integer{exclusiveMaximum 2}, integer{exclusiveMinimum 7})
GreaterEqual[7] Or LessEqual[2] → oneof(integer{minimum 7}, integer{maximum 2})
GreaterEqual[2] Or LessEqual[2] → integer                       (a tie that covers everything)
Greater[2] Or Less[2]           → oneof(integer{exclusiveMinimum 2}, integer{exclusiveMaximum 2})   (unchanged)
```
Rendered as a field (draft-07): `{"type": "integer", "oneOf": [{"exclusiveMinimum": 7}, {"exclusiveMaximum": 2}]}`.

- Pro: **exact whichever of B14 and B22 lands first.** The two members are disjoint exactly when they leave a gap (an
  `x` in both would have to be `> lo` and `< hi` with `hi ≤ lo`), so "exactly one" (`oneOf`) and "at least one"
  (`anyOf`) accept the same numbers. There is no `not` in the output.
- Pro: it reads as written and needs no new node or render path. It is the shape `Greater[2] Or Less[2]` already has.
- Con: two subschemas instead of one keyword set.
- Con (not new): at the **root**, a `oneOf` renders no `"type"` (B15, #499), so a root `Int Refined (Greater[7] Or
  Less[2])` would accept `"abc"` until B15 or B14 (whose `anyOf` states its type at the root) lands. Today's root output
  rejects everything (#509), and a field states its type. B15 and B14 are separate items; nothing here makes them harder.

Verified: the target trees, built by hand in the probe (test "target"), match refined's verdict for every x in -10..10,
for every row of the table above.

### C. Stop simplifying numeric `Or`  — LOST

Every `Or` goes to the fallback (`OneOf`/`AnyOf`) with its operands as written; no merge, no "everything".

- Pro: the least code: delete the numeric cases.
- Con: until B14 lands, same-direction pairs overlap under `oneOf` (`Greater[2] Or Greater[7]` would reject 8), which
  breaks outputs that are correct today. After B14 they are correct but verbose
  (`anyOf[{exclMin 2}, {exclMin 7}]` for what is `exclMin 2`).
- Con: changes pinned, correct outputs (`RefinedBooleanSpec` "or", normalized).

Lost: worse output than today for the cases that work, and wrong ones until B14.

### Legacy keeps the old output — LOST (not possible)
refined builds its tree at compile time, and the tree has no profile. `JsonSchemaProfile` lowers only `option` and
`family` nodes at render time. Keeping the old output under Legacy would need refined to emit a profile-dependent
node for a bug that has no "style" content. The lead's Legacy policy (fixes that make a schema correct apply under
Legacy too) covers it. No golden file holds refined output (api doesn't depend on refined).

## Scope

Shared by every option, all in the same `norm` code:

- **Same-direction merges (problem 3), for `Or` and `And`.** `Ge.min/max` and `Le.min/max` are one helper used by both
  `Or` and `And`, and the `And` side has the same bug. B27 replaces the helper, so it fixes both:
  - `Or`: the looser bound wins (smaller `Ge`, larger `Le`) with its own inclusiveness; on a tie, inclusive if
    **either** is.
  - `And`: the tighter bound wins (larger `Ge`, smaller `Le`) with its own inclusiveness; on a tie, inclusive only if
    **both** are.
  Alternative: fix `Or` only. It's pointless: `And` would keep a known-wrong merge in a helper it no longer shares.
  (The lead was told; see the report.)
- **Ties (problem 4).** `Or`'s "covers everything" test includes the inclusive tie. `And` keeps today's strict
  `Inside`/`Outside`: `GreaterEqual[5] And LessEqual[5]` stays `allof(min 5, max 5)` (correct, pinned in
  `RefinedQuirksSpec`), and an exclusive tie such as `Greater[5] And Less[5]` stays an `allof` that matches nothing
  (correct; making it a compile error like `Less[2] And Greater[7]` is a separate nicety).
- **Normalized operands (problem 5), for `Or` and `And`.** `compile(l.norm, r.norm)`. For `Or` this fixes wrong output
  (`NonNegative Or Greater[3]`). For `And` the output is already correct once #509 lands; it just becomes the merged
  range (`NonNegative And Less[10]` → `{minimum 0, exclusiveMaximum 10}` instead of `allof(not(exclMax 0), exclMax 10)`),
  and it stops depending on #509. Alternative: normalize `Or`'s operands only, which keeps the `RefinedQuirksSpec` pin
  "And matches its raw operands" and leaves `NonNegative And Less[10]` rejecting everything until #509 lands.
- **"Everything" absorbs.** The "every number" result (today `Naked(t, prefix)`) becomes a named `All(t)` predicate, so a
  nested `Or` can see it: `Or(All, p)` = `All`, `And(All, p)` = `p`. `(Greater[2] Or Less[7]) Or Divisible[3]` is then
  `integer`, not an overlapping `oneof(integer, multipleOf 3)`.

Not in scope (neighbours, reported to the lead):
- `Size[P]` ignores `P`'s inclusiveness: `Size[Greater[5]]` → `minLength 5` (accepts length 5), pinned in
  `RefinedQuirksSpec` "Size[P] ignores P's inclusiveness". Same family, different code (`asSize`).
- B14 (#500): the generic `Or` fallback `oneOf` → `anyOf`. B27 routes gaps to that fallback and works with either node.
- B22 (#509): `"type": "not"`. After B27, `Or` never produces a `not`, so B22's table row
  `Int Refined (Greater[7] Or Less[2])` no longer applies; refined's remaining `not`s are `Not[...]` of non-numeric or
  `Divisible` predicates.
- B31: bounds are emitted as `Double`. B27's comparisons work on the literal values (`math.compare`, below), never on
  the `Double`, so B31 only has to change the emission.
- A `Not` of an `Or`/`And` (`Not[Greater[7] Or Less[2]]`) isn't turned into a range: it stays `not(...)` of the
  normalized operand, which is correct once #509 lands.

## Decision

Option B. An `Or` of two opposite bounds that covers every number is the plain numeric type. One that leaves a gap is
the two bounds as alternatives, via `Or`'s generic fallback, which is exact under `oneOf` and `anyOf` alike. Together
with that: same-direction merges keep the right bound (`Or`: the looser, `And`: the tighter, ties by "either"/"both"),
both `Or` and `And` normalize their operands first, and "every number" absorbs in nested `Or`/`And`.

## Implementation plan

The two macros have the same structure. Make each change in both files, Scala 2 `AST.scala` + `Math.scala`, Scala 3
`RefinedMacro.scala` (its `Pred` and `math` objects), and keep them line-for-line parallel.

1. **`math.compare(l: Any, r: Any): Int`** (Scala 2 `Math.scala`, Scala 3 `RefinedMacro.scala` `object math`): the same
   type cases as `min`/`max` (Byte, Short, Int, Long, Double, Float, BigDecimal, BigInt with `compare`; mixed `Number`s by
   `doubleValue`; else `err`). Re-express `min`/`max` through it, or delete them if nothing else uses them (today only
   `Ge`/`Le` do).
2. **Replace `Ge.min/max`, `Le.min/max`** with `union(o)` (for `Or`) and `intersect(o)` (for `And`), each with a
   `require(t =:= o.t)`:
   - `Ge.union`: `compare(v, o.v)` < 0 → `this`, > 0 → `o`, 0 → `Ge(t, v, inclusive || o.inclusive)`.
   - `Ge.intersect`: > 0 → `this`, < 0 → `o`, 0 → `Ge(t, v, inclusive && o.inclusive)`.
   - `Le.union`: > 0 → `this`, < 0 → `o`, 0 → `||`. `Le.intersect`: < 0 → `this`, > 0 → `o`, 0 → `&&`.
   Returning `this`/`o` keeps the literal value's own type (a `Long` stays a `Long`; B31-friendly).
3. **`All(t)`** replaces `Naked` (Scala 2: `private case class Naked` → `case class All(t) extends NumPred { tree = prefix }`;
   Scala 3 the same, `prefix` is already on `NumPred`). `Naked` has no other use.
4. **`Covers`** extractor (next to `Inside`/`Outside`): `(Ge lo, Le hi)` in either order, `compare(hi.v, lo.v) > 0 ||
   (== 0 && (lo.inclusive || hi.inclusive))`. Move `Inside`/`Outside` to `math.compare` too (their results don't change
   for any literal a `Double` holds exactly).
5. **`Or.norm`** = `compile(l.norm, r.norm)`:
   ```scala
   case (a: Ge, b: Ge)          => a union b
   case (a: Le, b: Le)          => a union b
   case (a: All, _)             => a
   case (_, b: All)             => b
   case Covers()                => All(t)
   case (a, b)                  => OneOf(t, ::(a, b :: Nil))   // AnyOf if B14 has landed; a gap lands here
   ```
   Remove the `Outside` and `Inside` cases from `Or` (`Covers` replaces `Inside` there). `Or` no longer creates `Bounded`.
6. **`And.norm`** = `compile(l.norm, r.norm)`:
   ```scala
   case (a: Ge, b: Ge)    => a intersect b
   case (a: Le, b: Le)    => a intersect b
   case (_: All, b)       => b
   case (a, _: All)       => a
   case Inside(min, max)  => Bounded(t, min, max)      // unchanged
   case Outside(min, max) => err(...)                  // unchanged
   case (a, b)            => AllOf(t, ::(a, b :: Nil))
   ```
   `Size` keeps calling `pp.norm` on its argument and matching `NumericPred`. `Size[Greater[2] Or Greater[7]]` still
   works. `All`, like `Naked` today, is not a `NumericPred`, so `Size[Greater[2] Or Less[7]]` behaves as it does today.
7. **Tests: update the pins** (expected trees, both macros produce them):
   - `RefinedBooleanSpec` "or", **both** copies (`src/test/scala-2.12-/...` with witnesses, `src/test/scala-2.13+/...`):
     `Less[2] Or Greater[7]` → `` `oneof`(Set(lt(2), gt(7))) `` (B14 landed first: its `anyof`). Add
     `Greater[7] Or Less[2]`, `GreaterEqual[2] Or LessEqual[2]` → `` `integer` ``, `Greater[2] Or GreaterEqual[7]` →
     `exclusiveMinimum 2`; in "and", `Greater[5] And GreaterEqual[3]` → `exclusiveMinimum 5`.
   - `RefinedQuirksSpec` (2.13+): "Or outside, lower bound first: the bounds come out swapped" → rename "Or of bounds
     with a gap is the two alternatives", expect `oneof(gt(7), lt(2))`. "And keeps an inclusive bound when either side
     is inclusive" → rename "And of same-direction bounds keeps the tighter one", expect `exclusiveMinimum 5`. "And
     matches its raw operands: a nested Not isn't simplified" → rename "And normalizes its operands", expect
     `integer{minimum 2, exclusiveMaximum 7}`. The other pins stay.
8. **New `RefinedRangesSpec`** (`modules/refined/src/test/scala-2.13+/.../refined/`, so 2.13 and 3, JVM and JS): one
   assertion per row of the Problem table and per guard, with the trees from option B, plus `NonNegative And Less[10]`,
   `NonPositive Or Greater[0]` (→ `integer`), `(Greater[2] Or Less[7]) Or Divisible[3]` (→ `integer`),
   `Greater[5] And GreaterEqual[5]`, `Long Refined (Greater[7L] Or Less[2L])` (the `number`/`integer` prefix is
   unchanged), `Double Refined (Greater[1.5] Or Less[-1.5])` (→ `oneof` of `number[Double]` bounds).
9. **New `RefinedRangesValidationSpec`** (same dir): for each predicate `P` of step 8 over `Int`, render
   `Json.schema[Int Refined P]` and a wrapper `case class W(x: Int Refined P)` (a field, so `"type"` is stated even where
   B15 hasn't landed) as draft-07 and 2020-12, and assert that `petshop.JsonValidator.errors(...)` is empty exactly when
   refined's `Validate[Int, P].isValid(x)`, for x in -10..10. This is the probe, made permanent. Build change:
   refined's `.dependsOn(core % "compile->compile;test->test", api)` → `api % "compile->compile;test->test"` (as the
   bridges do), which brings `JsonValidator` and networknt on the JVM. On JS `JsonValidator` cancels these tests, as
   it does for the bridges. Cost: refined's test compile now compiles api's tests first, as every bridge's does. If the
   reviewer or owner prefers not to, drop step 9 and keep steps 7-8 (trees only).
10. **Docs**: README "Refined" and `docs/src/main/paradox/modules/refined.md` (they say the same thing), one sentence
    after the predicate list: "Numeric bounds under `And`/`Or` are merged into one range where they can be:
    `Greater[2] And Less[7]` is `exclusiveMinimum: 2, exclusiveMaximum: 7`, `Greater[2] Or Greater[7]` is
    `exclusiveMinimum: 2` and `Greater[2] Or Less[7]` any integer. `Less[2] Or Greater[7]`, which leaves a gap, is a
    `oneOf` of the two bounds." (`anyOf` if B14 landed first.) `makeDocs` (no mdoc snippet needed: it is prose).
11. **Gates** as in the protocol: `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on 2.13.18 and 3.3.9;
    `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`; `scalafmtSbtCheck` (build.sbt changed in step 9); `makeDocs`.
    `clean` matters: the refined macro is a macro.

Scala 2 vs 3: the same logic. In Scala 3 `err` is `report.errorAndAbort`, and `TypeRepr =:=` replaces `Type =:=` in
the `require`s. JS: nothing platform-specific (compile-time code). Validation specs cancel on JS.

Risks:
- **Landing order with B14.** B14 renames refined's `OneOf` to `AnyOf` in the same `Or.norm`. Whichever lands second
  rebases a small conflict in `Or.norm` and in the expected trees (`oneof` ↔ `anyof`) of steps 7-8. Semantics are the
  same either way (the gap members are disjoint).
- `RefinedQuirksSpec` and `RefinedBooleanSpec` are also touched by B14/B22: textual conflicts only.
- A user who pattern-matches on refined's trees sees a `oneof` where there was a `not` for rows 1-5. This is a bug fix,
  and refined's trees are not a documented contract.

## Compatibility

- Source and binary: no public API changes (`Pred`, `Naked`, `math` are `private[jsonschema]` / private to the macro).
- Output: changes exactly for the predicate shapes in the Problem table (and the `And`-of-raw-`Not` cases that become a
  merged range), under every profile and draft, since refined's tree has no profile. Every change makes the schema
  agree with refined. No golden or golden-legacy file changes (none holds refined output).
- PVP: a bug fix with no API change, so a patch (0.9.1). Legacy: changes too, as a correctness fix under the lead's
  Legacy policy (2026-11-23). "Legacy keeps the old output" is impossible here (see Options).

## Review log
