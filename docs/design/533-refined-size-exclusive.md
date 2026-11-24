# refined: `Size[P]` ignores whether `P`'s bound is exclusive  (issue #533, item B32)
Status: approved (r-b32, 2026-11-23)

## Problem

The refined module turns `Size[P]` (refined's "the size of the value satisfies `P`") into `minLength`/`maxLength` (strings) or
`minItems`/`maxItems` (collections). It takes the bound out of `P` and drops `P`'s inclusiveness:

- Scala 2: `modules/refined/src/main/scala-2/.../refined/AST.scala`, `Ge.asSize = Size.Def.Min(vv.toInt)` and
  `Le.asSize = Size.Def.Max(vv.toInt)`; called from `Extractors.scala` (`case TypeRef(_, sSize, List(Ex(pp)))`, `pp.norm` must be a
  `NumericPred`).
- Scala 3: `modules/refined/src/main/scala-3/.../refined/RefinedMacro.scala`, the same `asSize` on `Ge`/`Le` (lines ~319 and ~336),
  called from the `sSize` case of `unmatch` (~122). The file's header documents it as a quirk: "`Size[P]` takes `P`'s bound and
  ignores its inclusiveness (`Size[Greater[5]]` is `minLength 5`)".

JSON Schema's length/items keywords are always inclusive, so every exclusive bound comes out off by one. Negative bounds are passed through,
so the schema gets a negative `minLength`/`maxLength`, which the meta-schema forbids (`nonNegativeInteger`). A non-integral literal is
truncated with `toInt` instead of rounded in the right direction.

Repro: `notes/scratch/c-b32/B32ProbeSpec.scala` (2.13+, literal types; networknt via the uncommitted `b32-probe.sbt` in the same dir) and
`B32Probe212Spec.scala` (2.12, ``W.`5`.T``). For each type it compares refined's own `Validate.isValid` with networknt (draft-07) on the
rendered schema, sizes 0..10. Logs: `probe-2.13.18.log`, `probe-3.3.9.log`, `probe-2.12.21.log`; filtered `p-2.13.18.txt`,
`p-3.3.9.txt`. Output is identical on 2.12 (same macro), 2.13 and 3.3; JS is the same (the schema is built at compile time).

| refined type | today | refined rejects, schema accepts | correct |
|---|---|---|---|
| `String Refined Size[Greater[5]]` | `minLength: 5` | length 5 | `minLength: 6` |
| `String Refined Size[Less[3]]` | `maxLength: 3` | length 3 | `maxLength: 2` |
| `String Refined Size[Positive]` | `minLength: 0` | `""` | `minLength: 1` |
| `String Refined Size[Not[LessEqual[5]]]` (normalises to `Ge(5, exclusive)`) | `minLength: 5` | length 5 | `minLength: 6` |
| `String Refined Size[Not[GreaterEqual[3]]]` | `maxLength: 3` | length 3 | `maxLength: 2` |
| `List[Int] Refined Size[Greater[2]]` | `minItems: 2` | 2 items | `minItems: 3` |
| `List[Int] Refined Size[Less[2]]` | `maxItems: 2` | 2 items | `maxItems: 1` |
| `List[Int] Refined Size[Positive]` | `minItems: 0` | `[]` | `minItems: 1` |
| `String Refined Size[Greater[-2]]`, `Size[GreaterEqual[-2]]` | `minLength: -2` | (networknt is lenient; the schema is invalid) | `minLength: 0` |
| `String Refined Size[Less[0]]`, `Size[Negative]` | `maxLength: 0` | `""` | nothing satisfies it |
| `String Refined Size[LessEqual[-1]]` | `maxLength: -1` | (invalid schema) | nothing satisfies it |
| `String Refined Size[Greater[2147483647]]` | `minLength: 2147483647` | (a size is an `Int`) | nothing satisfies it |
| `String Refined Size[Greater[2.5]]` | `minLength: 2` | (no refined `Validate`, tree only) | `minLength: 3` |
| `String Refined Size[GreaterEqual[2.5]]` | `minLength: 2` | (tree only) | `minLength: 3` |

Correct today (guards): `Size[GreaterEqual[5]]`, `Size[LessEqual[3]]`, `Size[NonNegative]`, `Size[NonPositive]`, `Size[Not[Less[3]]]`,
`MinSize[5]`, `MaxSize[3]`, `Size[N]` (a literal), `Empty`, `NonEmpty`. `MinSize`/`MaxSize`/`Empty`/`NonEmpty` are matched by their own
symbols before dealiasing, so they never reach `asSize`.

`RefinedQuirksSpec` (2.13+) pins today's behavior in the test "Size[P] ignores P's inclusiveness".

## Why it is a problem

The schema accepts values the Scala side rejects: a client validating against the schema sends a 5-character string for
`Size[Greater[5]]`, or an empty string for `Size[Positive]` (a natural way to spell "non-empty"), and decoding fails on the server.
Negative bounds produce a schema that is invalid against the draft meta-schema, so strict tooling refuses the whole document.
Nothing hints at it: the predicate compiles and the output looks plausible.

## Options

A size is an `Int` in `0 .. Int.MaxValue` (refined checks `Size[P]` with `Validate[Int, P]`), so every bound on it maps exactly onto an
inclusive integer bound:

| bound of `P` | size `n` must satisfy | inclusive bound |
|---|---|---|
| lower `v`, inclusive | `n >= v` | `a = max(0, ceil(v))` |
| lower `v`, exclusive | `n > v` | `a = max(0, floor(v) + 1)` |
| upper `v`, inclusive | `n <= v` | `b = min(Int.MaxValue, floor(v))` |
| upper `v`, exclusive | `n < v` | `b = min(Int.MaxValue, ceil(v) - 1)` |
| no lower bound | | `a = 0` |

There is no size when `a > b` or `a > Int.MaxValue` (one check: `a >= 0`, so `b < 0` implies `a > b`). For integer `v` the table is just
`v`, `v + 1`, `v`, `v - 1`. The options differ in what they do with this mapping.

### A. Compute the exact inclusive integer bound where `Size[P]` is extracted  — WON

One helper per macro, `sizeRange(lo, hi)`, where `lo`/`hi` are `Option[(BigDecimal, Boolean)]` (value, inclusive), applies the table and
returns `Option[Size.Def]` (`None`: no size). The `Size[P]` case of the extractor takes `P`'s bounds out of `pp.norm` (which normal form
that is depends on whether B27 has landed, see "Coordination" below), calls `sizeRange`, and on `None` reports the compile error
`There are no sizes that satisfy <P>` (`P` shown as written: Scala 3 `arg.show`, Scala 2 the type's `toString`, the same convention as
B27's new generic message "There are no values that satisfy <the predicate, shown>").

Output:

```
String Refined Size[Greater[5]]          {"type":"string","minLength":6}
String Refined Size[Less[3]]             {"type":"string","maxLength":2}
String Refined Size[Positive]            {"type":"string","minLength":1}
List[Int] Refined Size[Less[2]]          {"type":"array","maxItems":1,"items":{"type":"integer"}}
String Refined Size[Greater[-2]]         {"type":"string","minLength":0}
String Refined Size[Less[0]]             compile error: There are no sizes that satisfy eu.timepit.refined.numeric.Less[0]
String Refined Size[Greater[2147483647]] compile error: There are no sizes that satisfy eu.timepit.refined.numeric.Greater[2147483647]
```

The unsatisfiable case is a compile error because the refined module already treats "a range with no values" that way
(`Less[2] And Greater[7]`: "There are no values inside of specified range [2.0 .. 7.0]"), and the docs say so: "An unsupported
predicate, or a range with no values (`Less[2] And Greater[7]`), is a compile error." `Size[Less[0]]` is such a range (sizes are `>= 0`).
The negative minimum is clamped instead of rejected, because `Size[Greater[-2]]` is satisfiable (every size is `> -2`); `minLength: 0` is
valid and means "no constraint", the same output `Size[NonNegative]` already has.

Pros: a small change at the one place the bug is; every satisfiable `Size[P]` becomes exact; no new schema shapes; both versions stay in
lockstep; one empty check and one message for every empty `Size`. Cons: `Size[Less[0]]`-style types, which compile today (with a wrong
schema), stop compiling. No refined value can inhabit them (`refineV` always returns `Left`), so this is a user mistake surfacing, not a
working program broken. Also, the check runs at extraction, so an empty `Size` is an error even where the enclosing predicate is
satisfiable: `Not[Size[Less[0]]]` (every string) and `Size[Less[0]] Or NonEmpty` (= `NonEmpty`) stop compiling too. That is the same as
today's `Not[Less[2] And Greater[7]]` (the `And` errors before `Not` sees it), so A is consistent, but B doesn't have this con.

### B. Keep compiling the unsatisfiable case, render an always-false schema  — LOST

Same as A for satisfiable bounds; when `sizeRange` finds no size, render a schema that accepts no value of the type, for example
`{"type":"string","minLength":1,"maxLength":0}` (`Size.Def` gets a `Never` case: `minLength 1` + `maxLength 0`, or `minItems 1` +
`maxItems 0`).

Pros: no type that compiles today stops compiling; an empty `Size` inside `Not`/`Or` renders correctly (`Not[Size[Less[0]]]` is
`not{minLength 1, maxLength 0}`, i.e. every string). Cons: an odd-looking schema for what is almost always a mistake in the user's type;
inconsistent with `And`'s empty range, which is a compile error; one more `Size.Def` case in both macros. Lost on consistency, but it is
the easy switch if the owner prefers "never break compilation": only the `None` branch of the `Size` case changes.

### C. Reject exclusive bounds inside `Size` at compile time  — LOST

Error out on `Size[Greater[_]]`, `Size[Less[_]]`, `Size[Positive]`, ... and tell the user to write `MinSize`/`MaxSize`/`GreaterEqual`.

Pros: no arithmetic. Cons: breaks types that are perfectly valid refined and have an exact schema; the conversion is trivial and sound
because sizes are integers. Lost.

### Extension: `Size` of a range  — INCLUDED (lead, 2026-11-23); alternative: its own item

`Size[Interval.Closed[2, 7]]` or `Size[Greater[2] And Less[7]]` normalise to `Bounded(min: Ge, max: Le)`, not a `NumericPred`, so today
they are a compile error ("Can't infer Predicate out of ...") on both versions (checked in the probe, `assertDoesNotCompile`). A closed
interval with equal ends (`Interval.Closed[5, 5]`) normalises to `AllOf(Ge(5, incl), Le(5, incl))` instead (`Inside` needs `min < max`;
pinned as "And of equal bounds is an allof"), so it is unsupported too.

This is new functionality rather than the bug, but `sizeRange` already handles two sides, so the lead asked for it as a separate plan
step (step 4): pass both bounds, and render `Size.Def.Range(a, b)` as min + max. Output:

```
String Refined Size[Interval.Closed[2, 7]]      {"type":"string","minLength":2,"maxLength":7}
String Refined Size[Interval.Open[2, 7]]        {"type":"string","minLength":3,"maxLength":6}
String Refined Size[Greater[2] And Less[7]]     {"type":"string","minLength":3,"maxLength":6}
String Refined Size[Interval.Closed[5, 5]]      {"type":"string","minLength":5,"maxLength":5}
String Refined Size[Interval.Closed[-3, 4]]     {"type":"string","minLength":0,"maxLength":4}
List[Int] Refined Size[Interval.Open[1, 4]]     {"type":"array","minItems":2,"maxItems":3,"items":{"type":"integer"}}
String Refined Size[Interval.Open[2, 3]]        compile error: There are no sizes that satisfy eu.timepit.refined.numeric.Interval.Open[2, 3]
String Refined Size[Interval.Closed[-5, -1]]    compile error: There are no sizes that satisfy eu.timepit.refined.numeric.Interval.Closed[-5, -1]
```

Not covered (stay compile errors, as today): `Size[A Or B]` with a gap (today `Not(Bounded)`, under B27 a several-interval `Ranges`), and
`Size` of non-numeric predicates. If the owner would rather keep the extension out, drop step 4 and its tests; the other steps don't
depend on it (`sizeRange` then only ever gets one side).

## Decision

Option A: the `Size[P]` case computes the exact inclusive integer bound (`+1`/`-1` for exclusive integer bounds, `ceil`/`floor` for
non-integral ones, a minimum clamped to 0, no size at all a compile error), through one helper, identically in the Scala 2 and Scala 3
macros. As a separate step (lead's call), the same helper gives `Size` of a two-sided range (`Interval.*`, `A And B`) a min and a max.

## Coordination with B27 and B31

B27 (#526, design approved, `docs/design/526-refined-or-normalise.md` on `gap/b27-refined-or-normalise`) replaces `Bounded`/`Naked` with
`Ranges(t, is: List[Interval])` (`Interval(lo: Option[Bound], hi: Option[Bound])`, `Bound(v, inclusive)`), lifts `Ge`/`Le` to
one-interval `Ranges` in `norm`, rewrites the `Size[P]` case (its step 6) to match a one-interval `Ranges` with one unbounded side, keeps
"ignoring inclusiveness exactly as `asSize` does today", and removes `asSize`. B31 (#532, `gap/b31-refined-bounds-precision`) replaces
the `Double` `vv` with `Lit(value: BigDecimal, kind)` and expects B32 to read `lit.value`. So the only thing that differs by landing
order is **how the `Size` case gets `lo`/`hi` out of `pp.norm`**, and how a bound's value becomes a `BigDecimal`. `sizeRange` and
everything after it are the same.

| B32 lands... | `Size` case: `pp.norm` → `(lo, hi)` | bound value |
|---|---|---|
| before B27 (today's code) | `Ge(_, v, i)` → `(Some(v, i), None)`; `Le(_, v, i)` → `(None, Some(v, i))`; step 4 adds `Bounded(_, lo, hi)` and `AllOf(_, Ge :: Le :: Nil)` (either order) → both sides; anything else → unsupported, as today | before B31: `BigDecimal(vv)` (exact for the `Double` it already is); after B31: `lit.value` |
| after B27 | `Ranges(_, Interval(lo, hi) :: Nil)` → `(lo, hi)` mapped with `b => (value(b.v), b.inclusive)`; without step 4 only intervals with an unbounded side; with it, any one interval (a point `[5, 5]` included: B27 makes `GreaterEqual[5] And LessEqual[5]` a point). Several intervals → unsupported. "Every number" (`Interval(None, None)`) → `Min(0)` | the same rule on `Bound.v` |

The second row is simpler: under B27 the whole `Size` case is one rule, and step 4 is a one-line widening of the match.

Whichever of B27/B32 lands second:
- **B27 second:** B27's step 6 must call `sizeRange(lo, hi)` on the interval's bounds instead of `toInt`, and must not re-pin the quirk
  (delete, don't port, "Size[P] ignores P's inclusiveness"). B32's tests (step 6) fail if this is missed, so the rebase can't silently
  regress. I mail c-b27 and the lead to amend B27's step 6 accordingly (it is approved).
- **B32 second:** implement the second row.
- Either way, the one that lands second adds `String Refined Size[Greater[5] And GreaterEqual[3]]` → `minLength 6` (today
  `Ge.max` keeps `inclusive || o.inclusive`, so it normalises to `Ge(5, inclusive)` and renders `minLength 5`; only both fixes together
  make it 6).
- B31 is orthogonal: `sizeRange` takes `BigDecimal` already, so B31 only changes the `value(...)` conversion at the call site.

## Implementation plan

The two macros have the same structure; make each change in both and keep them line-for-line parallel: Scala 2 `AST.scala` (+
`Extractors.scala`), Scala 3 `RefinedMacro.scala`. Steps are written for "B32 before B27"; the "after B27" variant of step 3/4 is in the
table above.

1. **`Size.Def` / `SizeDef`**: add `Range(min: Int, max: Int)` (step 4 produces it; with step 4 dropped, leave it out). Drop the `v`
   member of `Def` (Scala 2) / the `SizeDef(val v: Int)` constructor parameter (Scala 3): it is never read (only the case classes'
   extractors are), and `Range` has no single `v`. `Size.tree`: `Range(a, b)` renders `minLength a, maxLength b` (strings) /
   `minItems a, maxItems b` (collections), like `Const`.
2. **`sizeRange`** (Scala 2: in `object Pred`, or in `Math`; Scala 3: a private method of `RefinedMacro` next to `asNumber`):
   ```scala
   // a size is an Int in 0 .. Int.MaxValue; None: no size satisfies the bounds
   def sizeRange(lo: Option[(BigDecimal, Boolean)], hi: Option[(BigDecimal, Boolean)]): Option[Size.Def] = {
     import scala.math.BigDecimal.RoundingMode.{CEILING, FLOOR}
     val maxInt = BigDecimal(Int.MaxValue)
     val a = lo.fold(BigDecimal(0)) { case (v, incl) => (if (incl) v.setScale(0, CEILING) else v.setScale(0, FLOOR) + 1) max 0 }
     val b = hi.map { case (v, incl) => (if (incl) v.setScale(0, FLOOR) else v.setScale(0, CEILING) - 1) min maxInt }
     if (a > maxInt || b.exists(_ < a)) None
     else (lo, b) match {
       case (_, None)            => Some(Size.Def.Min(a.toInt))
       case (None, Some(hiN))    => Some(Size.Def.Max(hiN.toInt))
       case (Some(_), Some(hiN)) => Some(Size.Def.Range(a.toInt, hiN.toInt))
     }
   }
   ```
   No `err` inside: the caller owns the message (it knows `P`).
3. **The `Size[P]` case** (Scala 2 `Extractors.scala` `case TypeRef(_, sSize, List(Ex(pp)))`, Scala 3 `unmatch` `(sSize, List(arg))`):
   replace `case pp: NumericPred => Some(Size(t, pp.asSize))` with
   `bounds(pp.norm).map(b => Size(t, sizeRange(b._1, b._2).getOrElse(err(s"There are no sizes that satisfy ${shown(arg)}"))))`, where
   `bounds` is the first row of the Coordination table (`Ge`/`Le` only) and returns `None` for anything else (unsupported, as today).
   `shown` is `arg.show` (Scala 3) / the argument type's `toString` (Scala 2, where the pattern binds it: `List(arg @ Ex(pp))`); check both print `eu.timepit.refined.numeric.Less[0]` and
   pin only the stable part (step 6). `err` is `c.abort` (Scala 2 `HasLog`) / `report.errorAndAbort` (Scala 3).
   Delete `asSize` from `NumericPred`, `Ge` and `Le` (nothing else uses it; `NumericPred` stays for `Bounded`). The literal `Size[N]` case
   (`Const`) is unchanged.
4. **Extension: `Size` of a range**: `bounds` also maps `Bounded(_, lo, hi)` → `(Some(lo), Some(hi))` and `AllOf(_, (lo: Ge) :: (hi: Le) :: Nil)`
   / `AllOf(_, (hi: Le) :: (lo: Ge) :: Nil)` → the same. Scala 2's `Bounded.min`/`max` are typed `NumericPred`, but `Inside` only builds them
   from `Ge`/`Le`, so `vv`/`inclusive` are all that's needed (no cast).
5. **Scala 3 header scaladoc** (`RefinedMacro.scala`): replace the bullet "`Size[P]` takes `P`'s bound and ignores its inclusiveness ..."
   with "`Size[P]` turns `P`'s bounds into inclusive sizes (`Size[Greater[5]]` is `minLength 6`, `Size[Interval.Open[2, 7]]` is
   `minLength 3, maxLength 6`); no size at all is a compile error".
6. **Tests, `modules/refined/src/test`**
   - `scala-2.13+/.../RefinedQuirksSpec.scala`: delete the test "Size[P] ignores P's inclusiveness" (it is no longer a quirk).
   - `scala-2.13+/.../RefinedStringsSpec.scala`: new test "size with an exclusive or non-integral bound":
     `Size[Greater[5]]` → `minLength 6`, `Size[Less[3]]` → `maxLength 2`, `Size[Positive]` → `minLength 1`,
     `Size[Not[LessEqual[5]]]` → `minLength 6`, `Size[Not[GreaterEqual[3]]]` → `maxLength 2`, `Size[Greater[-2]]` → `minLength 0`,
     `Size[GreaterEqual[2.5]]` → `minLength 3`, `Size[Less[2.5]]` → `maxLength 2`; guards `Size[GreaterEqual[5]]` → 5,
     `Size[LessEqual[3]]` → 3, `Size[NonPositive]` → `maxLength 0`.
   - `scala-2.13+/.../RefinedCollectionsSpec.scala`: `List[Int] Refined Size[Greater[2]]` → `minItems 3`, `Size[Less[2]]` → `maxItems 1`,
     `Size[Positive]` → `minItems 1`.
   - `scala-2.12-/.../RefinedStringsSpec.scala` and `RefinedCollectionsSpec.scala`: the same core cases with ``W.`5`.T`` literals
     (`Greater`, `Less`, `Positive`), so 2.12 is covered.
   - Empty: `scala-2.13+/.../RefinedStringsSpec.scala` `assertDoesNotCompile` for `Size[Less[0]]`, `Size[Negative]`, `Size[LessEqual[-1]]`,
     `Size[Greater[2147483647]]` (both versions); `scala-3/.../Scala3RefinedErrorsSpec.scala` pins the message:
     `typeCheckErrors("refined[String Refined Size[Less[0]]]")` includes `"There are no sizes that satisfy"` and `"Less[0]"`.
   - Ranges (step 4), `scala-2.13+/.../RefinedStringsSpec.scala` "size of a range": `Interval.Closed[2, 7]` → 2/7, `Interval.Open[2, 7]` →
     3/6, `Interval.OpenClosed[2, 7]` → 3/7, `Interval.ClosedOpen[2, 7]` → 2/6, `Greater[2] And Less[7]` → 3/6, `Interval.Closed[5, 5]` → 5/5,
     `Interval.Closed[-3, 4]` → 0/4; `RefinedCollectionsSpec.scala`: `List[Int] Refined Size[Interval.Open[1, 4]]` → `minItems 2, maxItems 3`;
     `assertDoesNotCompile` for `Size[Interval.Open[2, 3]]` and `Size[Interval.Closed[-5, -1]]`, the first one's message also in
     `Scala3RefinedErrorsSpec`. 2.12: one ``Size[Interval.Closed[W.`2`.T, W.`7`.T]]`` case.
   - Only if B27 has landed: `Size[Greater[5] And GreaterEqual[3]]` → `minLength 6` (see Coordination).
   - The refined specs compare trees, so no networknt is needed; the probe in `notes/scratch/c-b32/` shows the new numbers match refined's
     `Validate` (each "correct" column in the Problem table is the smallest/largest size refined accepts). If B27's
     `RefinedRangesValiditySpec` (networknt vs `Validate`) exists by then, add the `Size` rows of the Problem table to it as well.
7. **Golden files:** none. The refined module isn't in `api`'s golden suite (`grep -rli refined api/src/test/resources/` is empty),
   and `modules/refined/src/test/resources/refined/fs-example.draft07.json` only uses `MinSize[1]` (unchanged path; re-run its spec to
   confirm). Refined output doesn't depend on `JsonSchemaProfile`, so `golden-legacy` is unaffected.
8. **Docs**: README "Refined" section (~line 772) and `docs/src/main/paradox/modules/refined.md`, which must say the same thing. Today the
   paradox page ends with "An unsupported predicate, or a range with no values (`Less[2] And Greater[7]`), is a compile error." and the
   README doesn't have that sentence; add it to the README too. After the predicate list, in both, add: "`Size[P]` becomes an inclusive
   length or item count: `Size[Greater[5]]` is `minLength: 6`, and a range (`Size[Interval.Open[2, 7]]`) gives both ends, `minLength: 3`
   and `maxLength: 6`." (Drop the second half if step 4 is dropped.) B27's step 10 rewrites the same empty-range sentence: whoever lands
   second merges the two texts. `makeDocs` to check.
9. **JS:** nothing specific; the change is compile-time only and the refined module's tests run on `refinedJS` as they are.
10. **Gates** (when implemented): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on 2.13.18 and 3.3.9; `scalafmtCheckAll` +
    `rootJS/scalafmtCheckAll`; `makeDocs`. Run `clean` after every macro edit (CLAUDE.md).

Risks:
- The landing order with B27 (above). The tests of step 6 are what keep either order honest.
- `shown(arg)` differs in detail between Scala 2 and 3 (prefixes, spacing in `Interval.Open[2, 3]`); the tests pin only
  `"There are no sizes that satisfy"` plus the simple name and arguments.

## Compatibility

- Source/binary: no public API change (all of it is inside the macros). A `Size[P]` whose `P` admits no size (`Size[Less[0]]`,
  `Size[Negative]`, `Size[LessEqual[-1]]`, `Size[Greater[2147483647]]`) stops compiling, also inside `Not`/`Or`; no value can inhabit
  such a `Size`. Option B avoids even that. Step 4 only adds: `Size` of a range doesn't compile today, so nothing that compiles changes
  meaning.
- Output: changes for every `Size[P]` with an exclusive or non-integral bound, and for negative minimums (listed in the Problem table, all
  from a wrong schema to the exact one). Inclusive bounds, `MinSize`, `MaxSize`, `Size[N]`, `Empty`, `NonEmpty` are unchanged.
- Legacy profile: refined output is the macro's tree and doesn't depend on the profile, so `JsonSchemaProfile.Legacy` changes too. Under the
  lead's policy (Legacy freezes style, not bugs) this is a correctness fix; no golden-legacy file contains a refined `Size`. Alternative:
  none practical (the refined macro has no profile to consult).
- PVP: a bug fix, patch level (0.9.x), with the empty-`Size` compile error called out in the release notes.

## Review log

Round 1 (r-b32, on 8ab312b):
- B32-R1 (must) the plan edits code B27's approved design deletes; spell out both landing orders, amend B27's step 6 → accepted: new
  section "Coordination with B27 and B31" (a table of what the `Size` case reads out of `pp.norm` before/after B27, B31's `lit.value`);
  the arithmetic moved into one `sizeRange(lo, hi)` on `(BigDecimal, inclusive)` bounds that both variants call (plan step 2), `asSize`
  deleted in both; who adds `Size[Greater[5] And GreaterEqual[3]]` → 6 (the second to land); Risks line about "no further change"
  removed. Mailed c-b27 and the lead to amend B27's step 6.
- B32-R2 (nit) eager error inside `Not`/`Or` → accepted: added to A's cons and B's pros, and to Compatibility.
- B32-R3 (nit) message mixes `0` and `0.0` → accepted: aligned with B27's generic text, "There are no sizes that satisfy <P shown>";
  the Scala 3 test pins `"There are no sizes that satisfy"` and `"Less[0]"` (step 6).
- B32-R4 (nit) README lacks the empty-range sentence → accepted: step 8 adds it to the README too, and notes B27's step 10 edits it.
- B32-R5 (nit) upper saturation → accepted: `a > Int.MaxValue` is "no size" (`Size[Greater[2147483647]]` is a compile error), in the
  table, the helper, the tests and Compatibility; an upper bound above `Int.MaxValue` still clamps (harmless, every size satisfies it).
- B32-R6 (nit) one empty check, the range's own message → accepted: `sizeRange` has the single check (`a > max` or `b < a`) and returns
  `None`; the caller reports it with `P` as written, so `Size[Interval.Closed[-5, -1]]` names its own range.

Round 2 (r-b32, on f29aa56): approved. Leftover hint taken: the `sizeRange` sketch no longer shadows `b` in its match (`hiN`).
