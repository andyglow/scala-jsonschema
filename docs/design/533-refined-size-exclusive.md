# refined: `Size[P]` ignores whether `P`'s bound is exclusive  (issue #533, item B32)
Status: draft

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

Sizes are non-negative integers, so every `Size[P]` with a one-sided numeric `P` maps exactly onto an inclusive integer bound:

| `P` normalises to | size `n` must satisfy | inclusive bound |
|---|---|---|
| `Ge(v, inclusive = true)` | `n >= v` | `min = max(0, ceil(v))` |
| `Ge(v, inclusive = false)` | `n > v` | `min = max(0, floor(v) + 1)` |
| `Le(v, inclusive = true)` | `n <= v` | `max = floor(v)`; `< 0` → no size |
| `Le(v, inclusive = false)` | `n < v` | `max = ceil(v) - 1`; `< 0` → no size |

For integer `v` this is just `v`, `v + 1`, `v`, `v - 1`. The options differ in what they do with this mapping.

### A. Convert to the exact inclusive integer bound in `asSize`  — WON

Make `asSize` use the table above, in both macros:

- `Ge.asSize = Size.Def.Min(clampToInt(if (inclusive) ceil(vv) else floor(vv) + 1) max 0)`.
- `Le.asSize`: `m = if (inclusive) floor(vv) else ceil(vv) - 1`; if `m < 0`, a compile error (no size satisfies `P`); else
  `Size.Def.Max(clampToInt(m))`.
- `clampToInt` saturates at `Int.MaxValue` (a `Long` literal bound above it; `Double.toInt` already saturates, but say it explicitly).

Output:

```
String Refined Size[Greater[5]]     {"type":"string","minLength":6}
String Refined Size[Less[3]]        {"type":"string","maxLength":2}
String Refined Size[Positive]       {"type":"string","minLength":1}
List[Int] Refined Size[Less[2]]     {"type":"array","maxItems":1,"items":{"type":"integer"}}
String Refined Size[Greater[-2]]    {"type":"string","minLength":0}
String Refined Size[Less[0]]        compile error: There are no sizes in the specified range [0 .. 0.0)
```

The unsatisfiable case is a compile error because the refined module already treats "a range with no values" that way
(`Less[2] And Greater[7]`: "There are no values inside of specified range [2.0 .. 7.0]"), and the docs say so: "An unsupported
predicate, or a range with no values (`Less[2] And Greater[7]`), is a compile error." `Size[Less[0]]` is such a range (sizes are `>= 0`).
Message, same text on both versions: `There are no sizes in the specified range [0 .. <v>]` for `LessEqual`, `[0 .. <v>)` for `Less`,
with `<v>` printed as `vv` is today (a Double, matching the existing message). The negative minimum is clamped instead of rejected,
because `Size[Greater[-2]]` is satisfiable (every size is `> -2`); `minLength: 0` is valid and means "no constraint", the same output
`Size[NonNegative]` already has.

Pros: a two-line change per macro, at the one place the bug is; every satisfiable `Size[P]` becomes exact; no new schema shapes; both
versions stay in lockstep. Cons: `Size[Less[0]]`-style types, which compile today (with a wrong schema), stop compiling. No refined value
can inhabit them (`refineV` always returns `Left`), so this is a user mistake surfacing, not a working program broken.

### B. Keep compiling the unsatisfiable case, render an always-false schema  — LOST

Same as A for satisfiable bounds; for `max < 0` render a schema that accepts no value of the type, for example
`{"type":"string","minLength":1,"maxLength":0}` (`Size.Def` would get a `Never` case: `minLength 1` + `maxLength 0`, or `minItems 1` +
`maxItems 0`).

Pros: no type that compiles today stops compiling. Cons: an odd-looking schema for what is certainly a mistake in the user's type;
inconsistent with `And`'s empty range, which is a compile error; one more `Size.Def` case in both macros. Lost on consistency, but it is
the easy switch if the owner prefers "never break compilation": only the `m < 0` branch of `Le.asSize` changes.

### C. Reject exclusive bounds inside `Size` at compile time  — LOST

Error out on `Size[Greater[_]]`, `Size[Less[_]]`, `Size[Positive]`, ... and tell the user to write `MinSize`/`MaxSize`/`GreaterEqual`.

Pros: no arithmetic. Cons: breaks types that are perfectly valid refined and have an exact schema; the conversion is trivial and sound
because sizes are integers. Lost.

### Not taken: `Size` of a range

`Size[Interval.Closed[2, 7]]` or `Size[Greater[2] And Less[7]]` normalise to `Bounded`, not a `NumericPred`, so today they are a compile
error ("Can't infer Predicate out of ...") on both versions (checked in the probe). Supporting them (a `Size.Def.Range(min, max)` built
from the same table) would be a small addition on top of A, but it is new functionality, not this bug; noted for the lead as a separate
item. A's helper is written so that a `Range` case can reuse it (one function per side: `minSize(v, inclusive)`, `maxSize(v, inclusive)`).

## Decision

Option A: `Ge.asSize`/`Le.asSize` compute the exact inclusive integer bound (`+1`/`-1` for exclusive integer bounds, `ceil`/`floor`
for non-integral ones, a negative minimum clamped to 0, a negative maximum a compile error), identically in the Scala 2 and Scala 3 macros.

## Implementation plan

1. **Scala 2, `modules/refined/src/main/scala-2/com/github/andyglow/jsonschema/refined/AST.scala`**
   - Add two private helpers in `object Pred` (or in `Math`):
     `minSize(v: Double, inclusive: Boolean): Int = clampToInt(math.max(0d, if (inclusive) math.ceil(v) else math.floor(v) + 1))` and
     `maxSize(v: Double, inclusive: Boolean): Int` = `m = if (inclusive) math.floor(v) else math.ceil(v) - 1`;
     `if (m < 0) err(s"There are no sizes in the specified range [0 .. $v${if (inclusive) "]" else ")"}")` else `clampToInt(m)`;
     `clampToInt(d: Double): Int = if (d >= Int.MaxValue) Int.MaxValue else d.toInt` (`d >= 0` already holds).
   - `Ge.asSize = Size.Def.Min(minSize(vv, inclusive))`, `Le.asSize = Size.Def.Max(maxSize(vv, inclusive))`.
   - `err` is `c.abort` (`HasLog`); `AST` already self-types `HasLog` and uses `err` in `And.norm`.
2. **Scala 3, `modules/refined/src/main/scala-3/com/github/andyglow/jsonschema/refined/RefinedMacro.scala`**
   - The same two helpers (private methods of `RefinedMacro`, next to `asNumber`), using the class's `err` (`report.errorAndAbort`).
   - `Ge.asSize`/`Le.asSize` as in step 1.
   - Update the header scaladoc: drop the bullet "`Size[P]` takes `P`'s bound and ignores its inclusiveness ...", replace it with
     "`Size[P]` turns `P`'s bound into an inclusive size (`Size[Greater[5]]` is `minLength 6`); a bound below 0 is an empty range".
3. **Tests, `modules/refined/src/test`**
   - `scala-2.13+/.../RefinedQuirksSpec.scala`: delete the test "Size[P] ignores P's inclusiveness" (it is no longer a quirk).
   - `scala-2.13+/.../RefinedStringsSpec.scala`: new test "size with an exclusive or non-integral bound":
     `Size[Greater[5]]` → `minLength 6`, `Size[Less[3]]` → `maxLength 2`, `Size[Positive]` → `minLength 1`,
     `Size[Not[LessEqual[5]]]` → `minLength 6`, `Size[Not[GreaterEqual[3]]]` → `maxLength 2`, `Size[Greater[-2]]` → `minLength 0`,
     `Size[GreaterEqual[2.5]]` → `minLength 3`, `Size[Less[2.5]]` → `maxLength 2`; and guards `Size[GreaterEqual[5]]` → 5,
     `Size[LessEqual[3]]` → 3, `Size[NonPositive]` → `maxLength 0`.
   - `scala-2.13+/.../RefinedCollectionsSpec.scala`: `List[Int] Refined Size[Greater[2]]` → `minItems 3`, `Size[Less[2]]` → `maxItems 1`,
     `Size[Positive]` → `minItems 1`.
   - `scala-2.12-/.../RefinedStringsSpec.scala` and `RefinedCollectionsSpec.scala`: the same core cases with ``W.`5`.T`` literals
     (`Greater`, `Less`, `Positive`), so 2.12 is covered.
   - Empty range: `scala-2.13+/.../RefinedStringsSpec.scala` `assertDoesNotCompile("schema[String Refined Size[Less[0]]]")` and
     `Size[Negative]`, `Size[LessEqual[-1]]` (both versions); `scala-3/.../Scala3RefinedErrorsSpec.scala` checks the message
     (`typeCheckErrors("refined[String Refined Size[Less[0]]]")` includes "There are no sizes in the specified range [0 .. 0.0)").
   - The refined specs compare trees, so no networknt is needed; the probe in `notes/scratch/c-b32/` already shows the new numbers match
     refined's `Validate` (each "correct" column above is the smallest/largest size refined accepts).
4. **Golden files:** none. The refined module isn't in `api`'s golden suite (`grep -rl refined api/src/test/resources/golden*` is empty),
   and `modules/refined/src/test/resources/refined/fs-example.draft07.json` only uses `MinSize[1]` (unchanged path; re-run its spec to
   confirm). Refined output doesn't depend on `JsonSchemaProfile`, so `golden-legacy` is unaffected.
5. **Docs** (README "Refined" section and `docs/src/main/paradox/modules/refined.md`, which say the same thing): after the list of supported
   predicates add one sentence: "`Size[P]` becomes an inclusive length or item count: `Size[Greater[5]]` is `minLength: 6`." The existing
   sentence about empty ranges being compile errors already covers `Size[Less[0]]`. `makeDocs` to check.
6. **JS:** nothing specific; the change is compile-time only and the refined module's tests run on `refinedJS` as they are.
7. **Gates** (when implemented): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on 2.13.18 and 3.3.9; `scalafmtCheckAll` +
   `rootJS/scalafmtCheckAll`; `makeDocs`.

Risks and neighbours:
- The fix is only as good as the inclusiveness that normalisation hands to `asSize`. `And`/`Or` merges of two lower (or two upper) bounds
  keep `inclusive || o.inclusive` (pinned as "And keeps an inclusive bound when either side is inclusive"), so
  `Size[Greater[5] And GreaterEqual[3]]` normalises to `Ge(5, inclusive)` and still renders `minLength 5` (should be 6). That is B27's area
  (its "And merges" extension); once the merge is right, this fix gives the right size without further change. Not fixed here.
- B31 (refined bounds precision) may change how `v`/`vv` are represented. The helpers take the bound and the flag; if B31 lands first,
  they take its exact value instead of `Double` (`setScale(0, CEILING/FLOOR)` on a `BigDecimal`). Sizes are `Int`, so `Double` is exact
  for every bound that matters here.

## Compatibility

- Source/binary: no public API change (all of it is inside the macros). A `Size[P]` whose `P` admits no size (`Size[Less[0]]`,
  `Size[Negative]`, `Size[LessEqual[-1]]`) stops compiling; no value can inhabit such a type. Option B avoids even that.
- Output: changes for every `Size[P]` with an exclusive or non-integral bound, and for negative minimums (listed in the Problem table, all
  from a wrong schema to the exact one). Inclusive bounds, `MinSize`, `MaxSize`, `Size[N]`, `Empty`, `NonEmpty` are unchanged.
- Legacy profile: refined output is the macro's tree and doesn't depend on the profile, so `JsonSchemaProfile.Legacy` changes too. Under the
  lead's policy (Legacy freezes style, not bugs) this is a correctness fix; no golden-legacy file contains a refined `Size`. Alternative:
  none practical (the refined macro has no profile to consult).
- PVP: a bug fix, patch level (0.9.x), with the empty-range compile error called out in the release notes.

## Review log
