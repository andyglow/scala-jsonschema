# refined: numeric `Or`/`And` simplification describes the wrong range  (issue #526, item B27)
Status: approved (r-b27, 2026-11-23)

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
   `Ge`/`Le`. So `NonNegative Or Greater[3]` isn't merged and becomes an overlapping `oneOf`. Likewise a `Bounded` (from
   an inner `And`, e.g. `Interval.Closed`) or a nested `Or` is never merged with another bound.

Repro: `notes/scratch/c-b27/B27ProbeSpec.scala` (not committed; run from `modules/refined/src/test/scala-2.13+/b27probe/`
with networknt added to refined's test deps through an uncommitted `b27-probe.sbt`). For each predicate `P` it renders
`Json.schema[Int Refined P]` as draft-07 and compares networknt 1.5.9's verdict with refined's own
`Validate[Int, P].isValid(x)` for every integer x in -10..10. Logs `notes/scratch/c-b27/probe-2.13.18.log`,
`probe-3.3.9.log`, filtered `p-2.13.18.txt`, `p-3.3.9.txt`. **Same trees and JSON on 2.13.18 and 3.3.9.** 2.12 runs the
same Scala 2 `AST.scala` (literal types aside, `RefinedBooleanSpec`'s 2.12 copy pins the same outputs). The schema is
built at compile time, so Scala.js gets the same trees. r-b27 reproduced it independently
(`notes/scratch/r-b27/`) and added the nested rows marked (r).

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
| (r) `Not[Greater[7] Or Less[2]]` | `not(not(integer{exclMin 7, exclMax 2}))` | 2..7 rejected (everything) |
| (r) `Not[Less[2] Or Greater[7]]` | `not(not(integer{exclMin 2, exclMax 7}))` | 2, 7 rejected |
| (r) `(Greater[7] Or Less[2]) Or Less[0]` | `oneof(not(...), integer{exclMax 0})` | most values |
| (r) `Interval.Closed[2, 7] Or Greater[5]` | `oneof(integer{min 2, max 7}, integer{exclMin 5})` | 6, 7 rejected |

Correct today (guards): `Greater[2] Or Greater[7]` → `exclMin 2`, `Less[2] Or Less[7]` → `exclMax 7`,
`Greater[2] Or Less[7]` → `integer`, `Greater[2] Or Less[2]` → `oneof(exclMin 2, exclMax 2)` (disjoint, so the `oneOf`
is exact), `Greater[2] And Less[7]` → `{exclMin 2, exclMax 7}`. Correct only once #509 is fixed:
`Not[Less[2]] And Less[7]` → `allof(not(exclMax 2), exclMax 7)`, `NonNegative And Less[10]` →
`allof(not(exclMax 0), exclMax 10)`, `Not[Greater[2] And Less[7]]` → `not(integer{exclMin 2, exclMax 7})`. Raw
operands again: the result is right, it just isn't merged, and it carries a `not`.

The wrong outputs are pinned as the current behaviour: `RefinedBooleanSpec` ("or": `Less[2] Or Greater[7]`, both the
2.12 and the 2.13+ copy) and `RefinedQuirksSpec` ("Or outside, lower bound first: the bounds come out swapped",
"And keeps an inclusive bound when either side is inclusive", "And matches its raw operands: a nested Not isn't
simplified").

## Why it is a problem

- The schema and the Scala type disagree about which numbers are valid. A service that validates its output
  accepts or rejects values that refined would not. Rows 1-3 accept every number, so the constraint is silently lost.
  The other rows are off at the boundary value or across a range.
- `Positive Or Less[-5]` and `x < lo or x > hi` ("outside a band") are ordinary ways to write a constraint, and
  `NonNegative` is one of refined's most common predicates.
- It has been like this since the normalisation was written (0.x, Scala 2), and the Scala 3 port copied it, so every
  version since 0.7 through 0.9.0 is affected.

## Options

The options differ in how much of the numeric algebra they get right. Each one fixes the top-level table rows.
Notation: `lo` is a lower bound (`x > lo` or `x ≥ lo`), `hi` an upper bound (`x < hi` or `x ≤ hi`).

### A. Correct the `not` form: `not{the gap}`  — LOST

Keep today's structure and fix each case: `Or(lo, hi)` with a gap → `Not(Bounded(gap))`, where the gap is `hi`'s value
(inclusive iff `hi` is exclusive) to `lo`'s value (inclusive iff `lo` is exclusive). Fix the same-direction merges and
the ties, and normalize operands.

```
Greater[7] Or Less[2]           → not(integer{minimum 2, maximum 7})
GreaterEqual[7] Or LessEqual[2] → not(integer{exclusiveMinimum 2, exclusiveMaximum 7})
```

- Pro: one node per `Or`, the shape 0.x intended; the smallest diff to today's code.
- Con: every rendered `not` states `"type": "not"` until B22 (#509) lands, so until then these schemas **reject every
  value**. Even after B22, its fix must keep `"type": "integer"` beside the `not` (or `2.5` passes `not{min 2, max 7}`).
- Con: `not` is the keyword tooling handles worst. OpenAPI generators and form builders ignore or reject it, and a
  reader has to invert the range in their head.
- Con: still case-by-case. Nested shapes (the (r) rows) need more cases.
- Pro (draft-04 only, B30): the complement of the common strict form (`Greater[7] Or Less[2]`) is inclusive, and
  draft-04 renders inclusive bounds correctly (B30: it renders exclusive ones as numbers, which draft-04 ignores).

### B. Pairwise fix; a gap stays as the two bounds the user wrote  — LOST (was the winner of round 0)

Fix `Or` case by case. An `Or` of two opposite bounds that covers every number is the plain type. One that leaves a gap
sends the two operands to `Or`'s generic fallback (`OneOf` today, B14's `AnyOf` later). Same-direction merges keep the
right bound (`Or`: the looser, `And`: the tighter; on a tie, "either" / "both" inclusive). Operands are normalized
first, and "every number" absorbs.

- Pro: small. A top-level gap is exact under `oneOf` and `anyOf` (the two members are disjoint), with no `not`.
- Con: **only pairs of half-lines are merged.** A gap meets a later operand as an opaque `OneOf`, and a `Bounded`
  (from an `And`, e.g. `Interval.Closed`) is never merged with a bound. So `(Greater[7] Or Less[2]) Or Less[0]`
  → `oneof(oneof(gt 7, lt 2), lt 0)` rejects -5, and `Interval.Closed[2, 7] Or Greater[5]` → `oneof({min 2, max 7},
  {exclMin 5})` rejects 6 and 7. Both stay wrong until B14's `anyOf` lands (correct then, but not merged).
- Con: `Not` of a range or of a gap stays a `not` (B22-dependent).

Lost to D: D is B generalised. It costs a little more code and leaves none of these holes.

### C. Stop simplifying numeric `Or`  — LOST

Every `Or` goes to the fallback with its operands as written: no merge, no "everything".

- Pro: the least code.
- Con: until B14 lands, same-direction pairs overlap under `oneOf` (`Greater[2] Or Greater[7]` would reject 8), which
  breaks outputs that are correct today. After B14: correct but verbose (`anyOf[{exclMin 2}, {exclMin 7}]`).

### D. An interval-set normal form for numeric predicates  — WON

(r-b27's B27-R2.) Every numeric bound predicate normalizes to a **set of disjoint intervals** with open or closed ends.
`Or` is union, `And` is intersection, `Not` is complement. All three are exact and closed over the representation, so
nesting no longer matters.

```scala
case class Bound(v: Any, inclusive: Boolean)
case class Interval(lo: Option[Bound], hi: Option[Bound])  // None: unbounded on that side
case class Ranges(t, is: List[Interval]) extends NumPred   // sorted, pairwise disjoint and not touching
//   List()                    no value                (an error where it's built, below)
//   List(Interval(None,None)) every number            (today's Naked(prefix))
```

- **Lift**: `Greater[v]` → `[(v, ∞)]`, `GreaterEqual[v]` → `[[v, ∞)]`, `Less`/`LessEqual` likewise.
  `Positive`/`Negative`/`NonPositive`/`NonNegative` go through their existing extraction (`Ge`/`Le`/`Not`).
- **`Or(l, r)`**: if both normalize to `Ranges`, then `union`; if either is "every number", then that; else the fallback with the normalized
  operands (non-numeric or `Divisible` operands: B14's domain).
- **`And(l, r)`**: if both are `Ranges`, then `intersect`, and empty → compile error (see Compatibility); "every number" is
  dropped; else `AllOf` of the normalized operands.
- **`Not(p)`**: if `p` normalizes to `Ranges`, then `complement` (empty → compile error); `Not(Not(p))` and
  `Not(Size(Const(0)))` as today; else `not(p)`.
- **Render**: "every number" → the plain type (`integer` / `number[T]`). One interval → today's keywords
  (`minimum`/`exclusiveMinimum`/`maximum`/`exclusiveMaximum`; a point `[5, 5]` → `{minimum 5, maximum 5}`). Several →
  the fallback (`oneof` today, B14's `anyof` later) of single-interval members, in ascending order. The members are
  disjoint, so "exactly one" and "at least one" accept the same numbers: exact whichever lands first, B14 or B22.

```
Greater[7] Or Less[2]                    → oneof(integer{exclusiveMaximum 2}, integer{exclusiveMinimum 7})
Less[2] Or Greater[7]                    → the same
GreaterEqual[2] Or LessEqual[2]          → integer
(Greater[7] Or Less[2]) Or Less[0]       → oneof(integer{exclusiveMaximum 2}, integer{exclusiveMinimum 7})
Interval.Closed[2, 7] Or Greater[5]      → integer{minimum 2}
Not[Greater[2] And Less[7]]              → oneof(integer{maximum 2}, integer{minimum 7})       (no `not`)
Not[Less[2] Or Greater[7]]               → integer{minimum 2, maximum 7}
NonNegative And Less[10]                 → integer{minimum 0, exclusiveMaximum 10}
GreaterEqual[5] And LessEqual[5]         → integer{minimum 5, maximum 5}
GreaterEqual[3] Or LessEqual[2]  (Int)   → oneof(integer{maximum 2}, integer{minimum 3})       (no integer in the gap: kept;
                                                                                                an integer-aware rule would be wrong for Double)
```
Rendered as a field (draft-07): `{"type": "integer", "oneOf": [{"exclusiveMaximum": 2}, {"exclusiveMinimum": 7}]}`.

- Pro: exact for every combination of numeric bounds under `And`/`Or`/`Not`, at any depth. No numeric `not` is left,
  so refined's only `not`s are `Not` of non-numeric or `Divisible` predicates (B22's exposure shrinks).
- Pro: it replaces the case analysis that had the bugs (`Ge/Le.min/max`, `Bounded`, `Inside`, `Outside`, `Naked`)
  with three small set operations. A permanent test checks them against refined's own `Validate` (plan step 9).
- Con: more new code than B, in both macros: about 50 lines each for `Bound`/`Interval`/`union`/`complement`, minus
  the ~60 lines it removes.
- Con: a larger output diff. `Not` of a range changes shape (`not` → bounds or alternatives), an equal-bound `And`
  becomes one keyword set instead of an `allof`, and the alternatives come out in ascending order, not as written.
  Every change makes the schema exact (or the same set, simpler).
- Con (shared with B, not new): at the **root**, a `oneOf` states no `"type"` (B15, #499). So a root
  `Int Refined (Greater[7] Or Less[2])` accepts non-integers outside the gap (`8.5`) until B15 or B14 lands (B14's
  `anyOf` states its type at the root). Non-numbers are still rejected: `"abc"` matches both members, so the `oneOf`
  fails. A field states its type.
- Con (draft-04, B30, shared with B): the alternatives keep the user's strict bounds, which draft-04 renders as numbers
  (B30) and ignores. Then both members match everything and the `oneOf` rejects everything on draft-04. `Greater[7]`
  alone has the same B30 bug; B30 fixes both. A gets the common strict case right on draft-04 by accident.

### Legacy keeps the old output — LOST (not possible)
refined builds its tree at compile time, and the tree has no profile. `JsonSchemaProfile` lowers only `option` and
`family` nodes at render time. Keeping the old output under Legacy would need refined to emit a profile-dependent
node for a bug that has no "style" content. The lead's Legacy policy (fixes that make a schema correct apply under
Legacy too) covers it. No golden file holds refined output (api doesn't depend on refined).

## Decision

Option D. Numeric bound predicates normalize to a sorted set of disjoint intervals. `Or`/`And`/`Not` are union,
intersection and complement. One interval renders as today's keywords, "every number" as the plain type, and several
intervals as the generic `Or` fallback (exact under `oneOf` and `anyOf`, because they're disjoint). An empty set is a
compile error, as `Less[2] And Greater[7]` already is.

To choose B instead (the owner may prefer the smaller change): keep `Ge`/`Le`/`Bounded` and apply B's pairwise rules.
Plan steps 7-11 stay the same, minus the D-only rows of step 8.

`And` is in scope: it shares the broken merge helper with `Or`, and D handles both with the same code (lead informed).

Not in scope (neighbours, reported to the lead):
- `Size[P]` ignores `P`'s inclusiveness (`Size[Greater[5]]` → `minLength 5`), pinned in `RefinedQuirksSpec`. D keeps
  that behaviour exactly (step 6).
- B14 (#500): the generic `Or` fallback `oneOf` → `anyOf`, and `Or`s with non-numeric or `Divisible` operands (still
  overlapping `oneOf` until B14). `Ranges` uses whichever fallback is there.
- B15 (#499): a root `oneOf` without `"type"`. B30: draft-04 exclusive bounds. B31: bounds emitted as `Double`; D
  compares literal values with `math.compare`, never via `Double`, so B31 only changes the emission.
- B22 (#509): after D, refined emits no `not` for numeric bounds. B22's table row `Int Refined (Greater[7] Or Less[2])`
  no longer applies. Today `NonNegative`/`NonPositive` inside `And`/`Or` render a raw `{"type": "not", ...}`
  (`NonNegative And Less[3]`); after D they don't.

## Implementation plan

The two macros have the same structure. Make each change in both files, Scala 2 `AST.scala` + `Math.scala`, Scala 3
`RefinedMacro.scala` (its `Pred` and `math` objects), and keep them line-for-line parallel.

1. **`math.compare(l: Any, r: Any): Int`** (Scala 2 `Math.scala`, Scala 3 `RefinedMacro.scala` `object math`): the same
   type cases as today's `min`/`max` (Byte, Short, Int, Long, Double, Float, BigDecimal, BigInt via `compare`; mixed
   `Number`s via `doubleValue`; else `err`). Delete `min`/`max`: nothing else uses them.
2. **Interval set** (in `Pred`, both versions), on normalized lists:
   - `Bound(v: Any, inclusive: Boolean)`, `Interval(lo: Option[Bound], hi: Option[Bound])`.
   - `normalize(is)`: drop empty intervals (`lo > hi`, or `lo == hi` unless both are inclusive), sort by `lo` (`None`
     first; at equal values, inclusive first), then fold. Merge the next interval into the current one when it overlaps
     or touches (`next.lo < cur.hi`, or equal values with either side inclusive). The merged `hi` is the larger,
     inclusive if the larger value's bound is (either, on a tie).
   - `union(a, b) = normalize(a ++ b)`.
   - `complement(is)`: the gaps of the sorted list, each bound's inclusiveness flipped, from -∞ to the first `lo` and
     from the last `hi` to +∞, skipping a side that is already unbounded. `complement(Nil)` = every number.
   - `intersect(a, b) = complement(union(complement(a), complement(b)))`.
3. **`Ranges(t, is)`** replaces `Bounded` and `Naked` as the numeric normal form (`extends NumPred`; `prefix` and
   `isInt` as today). `Ge`/`Le` stay as extraction results, and their `norm` lifts them to a one-interval `Ranges`.
   `tree`: every number → `prefix`. One interval → `prefixWith(...)` with `minimum`/`exclusiveMinimum` for `lo`,
   `maximum`/`exclusiveMaximum` for `hi`, in that order (as `Bounded` today). Several → the fallback node (step 4) of
   one-interval `Ranges`, ascending.
4. **The fallback node** stays `OneOf` (B14 renames it `AnyOf`; see Risks). `Ranges` with several intervals builds it.
5. **`norm`**:
   ```scala
   Or.norm  = (l.norm, r.norm) match {
     case (a: Ranges, b: Ranges)            => Ranges(t, union(a.is, b.is))
     case (a: Ranges, _) if a.isAll         => a
     case (_, b: Ranges) if b.isAll         => b
     case (a, b)                            => OneOf(t, ::(a, b :: Nil))
   }
   And.norm = (l.norm, r.norm) match {
     case (a: Ranges, b: Ranges)            => nonEmpty(Ranges(t, intersect(a.is, b.is)), a, b)
     case (a: Ranges, b) if a.isAll         => b
     case (a, b: Ranges) if b.isAll         => a
     case (a, b)                            => AllOf(t, ::(a, b :: Nil))
   }
   Not.norm: Not(Not(p)) => p.norm; Not(Size(_, Const(0))) => Size(Min(1)) (as today);
             p.norm is Ranges r => nonEmpty(Ranges(t, complement(r.is)), ...); else Not(p.norm)
   ```
   `nonEmpty` reports an empty result with `err`. For `And` of two half-lines, keep today's message word for word,
   values in the order written (`"There are no values inside of specified range [2.0 .. 7.0]"` for
   `Less[2] And Greater[7]`: `Scala3RefinedErrorsSpec` pins it). Otherwise:
   `"There are no values that satisfy <the predicate, shown>"`.
   `Or` extending `NumPred` only served `prefix` for `Naked`; it can become a plain `Pred`.
6. **`Size[P]`** (Scala 2 `Extractors.scala`, Scala 3 `unmatch` `sSize` case): match `pp.norm` against a one-interval
   `Ranges` with one unbounded side: `[lo, ∞)` → `SizeDef.Min(lo.v.toInt)`, `(-∞, hi]` → `SizeDef.Max(hi.v.toInt)`,
   ignoring inclusiveness exactly as `asSize` does today (`Size[Greater[5]]` → `minLength 5`, the pinned quirk). Anything
   else → `None` as today. Remove `asSize`/`NumericPred` if nothing else uses them.
7. **Tests: update the pins** (both macros produce the same trees):
   - `RefinedBooleanSpec` "or", **both** copies (`src/test/scala-2.12-/...` with witnesses, `src/test/scala-2.13+/...`):
     `Less[2] Or Greater[7]` → `` `oneof`(Set(lt(2), gt(7))) `` (B14 landed first: its `anyof`). Add
     `Greater[7] Or Less[2]` (same tree), `GreaterEqual[2] Or LessEqual[2]` → `` `integer` ``,
     `Greater[2] Or GreaterEqual[7]` → `exclusiveMinimum 2`. In "and", add `Greater[5] And GreaterEqual[3]` →
     `exclusiveMinimum 5`.
   - `RefinedQuirksSpec` (2.13+): "Or outside, lower bound first: the bounds come out swapped" → rename "Or of bounds
     with a gap is the alternatives, ascending", expect `oneof(lt(2), gt(7))`. "And keeps an inclusive bound when
     either side is inclusive" → rename "And of same-direction bounds keeps the tighter one", expect
     `exclusiveMinimum 5`. "And matches its raw operands: a nested Not isn't simplified" → rename "And normalizes its
     operands", expect `integer{minimum 2, exclusiveMaximum 7}`. "And of equal bounds is an allof" → rename "And of
     equal inclusive bounds is a point", expect `integer{minimum 5, maximum 5}`. The rest stay (`NonDivisible`,
     intervals, `A And B And C` of `Divisible`, string `Or`, `Not[IPv4]`, ...).
   - Scala 3 scaladoc of `RefinedMacro` (lines 9-14): replace "`And`/`Or` are matched on the raw (unnormalized)
     operands, `Not` nested in `allof`/`oneof` is not simplified" with one line on the interval normal form. Keep the
     `Size[P]` and `A And B And C` notes.
8. **New `RefinedRangesSpec`** (`modules/refined/src/test/scala-2.13+/.../refined/`, so 2.13 and 3, JVM and JS): tree
   assertions for every row of the Problem table, the guards, and D's examples above, plus:
   - `NonPositive Or Greater[0]` → `integer`; `(Greater[2] Or Less[7]) Or Divisible[3]` → `integer`;
     `Greater[1] And Less[9] And Greater[3]` → `{exclusiveMinimum 3, exclusiveMaximum 9}`;
   - `GreaterEqual[3] Or LessEqual[2]` on `Int` → two intervals (pinned: no integer-aware merging);
   - `Long Refined (Greater[7L] Or Less[2L])` (the `integer` prefix), `Double Refined (Greater[1.5] Or Less[-1.5])`
     (`number[Double]` members);
   - mixed: `Divisible[2] Or Greater[3]` → `oneof(multipleOf 2, exclMin 3)` (unchanged, B14's), `Divisible[2] And
     NonNegative` → `allof(multipleOf 2, minimum 0)`;
   - compile errors, Scala 3 in `Scala3RefinedErrorsSpec` (`typeCheckErrors`): `NonNegative And Less[-3]`,
     `Greater[5] And Less[5]`, `Not[Greater[2] Or Less[7]]`, with their messages; `Less[2] And Greater[7]` keeps its
     message. Scala 2: try `assertTypeError` in a 2.13+ spec. If scalatest doesn't catch the macro abort (the comment
     in `RefinedBooleanSpec` "and" says it didn't), leave a comment saying so: Scala 2 shares the error path with the
     existing `Outside` error.
9. **New `RefinedRangesValiditySpec`** (same dir): for each `Int` predicate of step 8, render `Json.schema[Int Refined P]`
   and a wrapper `case class W(x: Int Refined P)` (a field: `"type"` stated even before B15; one top-level or
   object-member case class per predicate, since a local case class in a test body can trip the Scala 2 macro) as draft-07 and 2020-12. Assert
   that `petshop.JsonValidator.errors(...)` is empty exactly when refined's `Validate[Int, P].isValid(x)`, for x in
   -12..12. For the `Double` rows, check the wrapper for x in -12.0..12.0 step 0.5. This is the probe made permanent;
   it is D's correctness check. Build change: refined's `.dependsOn(core % "compile->compile;test->test", api)` →
   `api % "compile->compile;test->test"`, as the bridges do. That brings `JsonValidator` and networknt (JVM). On JS,
   `JsonValidator` cancels these tests, as it does for the bridges.
   Cost: refined's test compile now compiles api's tests first. **B14's approved plan (#500, step 1) makes the same
   build change and adds `RefinedOrValiditySpec`**: whoever lands second drops the duplicate build change and moves
   the shared "refined verdict vs validator" helper into one place (a small `RefinedValidity` object in refined's tests).
10. **Docs**: README "Refined" and `docs/src/main/paradox/modules/refined.md` (they say the same thing), after the
    predicate list: "Numeric bounds under `And`, `Or` and `Not` are merged into ranges: `Greater[2] And Less[7]` is
    `exclusiveMinimum: 2, exclusiveMaximum: 7`, `Greater[2] Or Less[7]` any integer, and `Less[2] Or Greater[7]`, which
    leaves a gap, is a `oneOf` of the two ranges. A combination with no values, like `Less[2] And Greater[7]`, is a
    compile error." (`anyOf` if B14 landed first.) This replaces the existing sentence "An unsupported predicate, or a
    range with no values (`Less[2] And Greater[7]`), is a compile error" in paradox, keeping its "unsupported
    predicate" half. `makeDocs` (prose only, no mdoc).
11. **Gates** as in the protocol: `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on 2.13.18 and 3.3.9;
    `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`; `scalafmtSbtCheck` (build.sbt changed in step 9); `makeDocs`.
    `clean` matters: the refined macro is a macro.

Scala 2 vs 3: the same logic. In Scala 3 `err` is `report.errorAndAbort`, and `TypeRepr =:=` replaces `Type =:=` in
the `require`s. The predicate in the generic error message is `show`n (`p.show` in Scala 3, `showCode`/`toString` of
the type in Scala 2). JS: nothing platform-specific (compile-time code). Validity specs cancel on JS.

Risks:
- **Landing order with B14.** B14 renames refined's `OneOf` to `AnyOf` in the same `Or.norm`, and D's `Ranges.tree`
  builds that node. Whoever lands second rebases a small conflict there, the `oneof` ↔ `anyof` expectations of steps
  7-8, and the build change of step 9. The semantics are the same either way, because the intervals are disjoint.
- `RefinedQuirksSpec` and `RefinedBooleanSpec` are also touched by B14/B22: textual conflicts only.
- The set operations are where a bug would hide. Step 9 checks them against refined itself, including ties,
  equal-bound points and nested shapes. Keep that spec even if the build change is unwelcome. Fallback: run the same
  comparison with a tiny in-test evaluator of the rendered keywords (no networknt).
- A user who pattern-matches on refined's trees sees `oneof`/bounds where there was a `not`/`allof`. refined's trees
  are not a documented contract.

## Compatibility

- Binary: no public API changes (`Pred`, `Ranges`, `math` are private to the macro).
- **Source: some programs that compile on 0.9.0 become compile errors.** These are predicates with no values that
  today slip past the strict `Outside` check because of raw operands, ties or nesting: `NonNegative And Less[-3]`,
  `Greater[5] And Less[5]`, `Not[Less[2]] And Less[0]`, `Not[Greater[2] Or Less[7]]`. This is the same rule as
  `Less[2] And Greater[7]`, which is an error today. Such a type is uninhabited (refined can't construct a value), so
  no working program depends on it beyond the schema line. The alternative is to emit a compile **warning** and today's
  `allof`/`not` for the newly detected cases, keeping the error only where it is today. That costs one flag on
  `nonEmpty`. Pick it if the owner wants strict source compatibility in a patch.
- Output: changes for every predicate shape in the Problem table, for `Not` of numeric ranges, for equal-bound `And`s,
  and for `And`/`Or` with `NonNegative`/`NonPositive`/`Not` operands. These are under every profile and draft, since
  refined's tree has no profile. Each change makes the schema agree with refined, or keeps the same set in a simpler form.
  No golden or golden-legacy file changes (none holds refined output).
- PVP: patch (0.9.1). No API change, the output changes are fixes, and the new compile errors affect only uninhabited
  types (owner's call; the warning alternative above makes it strictly compatible). Legacy: changes too, as a
  correctness fix under the lead's Legacy policy (2026-11-23).

## Review log

Round 1 (r-b27, on 062d6b1):

- B27-R1 (must) nested gap / `Bounded` overlaps break B's "exact either way" → accepted: the winner is now D (R2),
  which merges nested gaps, `Bounded`s and `Not`s exactly. B is kept as an option with this hole stated as its main con;
  the (r) rows are in the Problem table and step 8.
- B27-R2 (must) interval-set option → accepted, and it **won**: option D. It removes the case analysis where all five
  bug classes lived, and step 9 checks it against refined's `Validate`. B and A lost to it (verdicts in Options).
- B27-R3 (must) normalized `And` operands turn uninhabited types into compile errors → accepted. Compatibility states
  the source break, the PVP verdict (patch, owner's call) and a warning-instead-of-error alternative. Step 8 adds the
  compile-error tests (Scala 3 `typeCheckErrors`; Scala 2 `assertTypeError` or a stated reason). D adds more such cases
  (ties, `Not` of "every number"), and they're listed.
- B27-R4 (nit) root con wording → accepted: a root `oneOf` without type rejects `"abc"` (both members match it) and
  accepts `8.5` outside the gap. Fixed in D's cons.
- B27-R5 (nit) draft-04 (B30) trade-off → accepted: listed as a con of B/D and a pro of A.
- B27-R6 (nit) `Not` of a gap → accepted, through D: `Not` is complement, so `Not[Less[2] Or Greater[7]]` →
  `{minimum 2, maximum 7}`.
- B27-R7 (nit) plan details → accepted: the scaladoc quirk list (step 7), B14's identical build change and
  `RefinedOrValiditySpec` (step 9 + Risks), the R1 shapes, compile errors and the integer-only gap in step 8, and the B22
  note on raw `NonNegative` operands (Decision, "Not in scope").

Round 2 (r-b27, on bf0b4c6): approved, no further comments. Implementer hint taken into step 9 (wrapper case classes top-level
or in an object, not local).
