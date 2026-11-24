# Enum values in declaration order  (issue #458, item B7)
Status: approved (r-b7, 2026-11-23)

## Problem

`json.Schema.enum` holds its values in an immutable `Set[Value]`:

```scala
final case class `enum`[T](tpe: Schema[_], values: Set[Value]) extends Schema[T]   // core/src/main/scala/json/Schema.scala:441
```

Every builder fills it with `.toSet`. Scala's `Set1`..`Set4` keep insertion order, so up to four values come out as
declared; from five on, `.toSet` makes a `HashSet` and the renderer (`AsDraftSupport.mkEnum`: `arr(x.values.toSeq)`)
writes them in hash order. The derivations already have the values in declaration order (Scala 2 `UEnums`, Scala 3
`Enums`/`Sums`, the profile's family lowering); only the `Set` loses it.

Repro (`api/src/test/scala/.../EnumOrderSpec.scala`, `api/src/test/scala-3/.../sums/Scala3EnumOrderSpec.scala`,
`EnumeratumSupportSpec`, `ParseJsonSchemaSpec`, all failing on master at e050daa):

```scala
sealed trait Weekday
object Weekday { case object Mon extends Weekday; /* Tue .. Sun */ }
Json.stringify(Json.schema[Weekday], Draft07("x"))
```

| source | 2.13 / 3.3 | 2.12 |
|---|---|---|
| sealed case objects (also Legacy profile) | `["Wed","Sat","Sun","Thu","Tue","Mon","Fri"]` | `["Thu","Mon","Wed","Fri","Sun","Tue","Sat"]` |
| case objects + `ToValue` (`10..60`) | `[20,50,30,10,60,40]` | `[10,60,20,40,50,30]` |
| singletons of a mixed family (`Run(arg)`, `Start..Kill`) | `["Pause","Stop","Resume","Reset","Start","Kill"]` | `["Pause","Reset","Kill","Start","Resume","Stop"]` |
| `scala.Enumeration` (Mercury..Neptune) | `["Mars","Venus","Uranus","Earth",...]` | `["Venus","Jupiter","Mercury",...]` |
| Java enum `DayOfWeek` | `["TUESDAY","FRIDAY","MONDAY",...]` | `["SUNDAY","FRIDAY","SATURDAY",...]` |
| Scala 3 `enum Weekday { case Mon, ..., Sun }` (and its singletons next to a case with params) | hash order, as the sealed family | n/a |
| `Schema.enum.of(...)` | hash order | hash order |
| enumeratum `Enum` / `IntEnum` / ... | hash order (`namesToValuesMap.keySet`, `valuesToEntriesMap.keySet`) | same |
| parser: `{"enum": ["Mon",...,"Sun"]}` | parsed into a hash `Set` | same |

The hashes are deterministic, so the order is fixed per Scala version (the table is JVM; JS uses the same hashes and
the JS test run covers it); 2.12 differs from 2.13/3 because 2.13 seeds case-class hashes with the product prefix. The
same enum as a `Map` key already gets declaration order (`"^(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)$"`, derived key patterns
don't go through a `Set`).

What "declaration order" means on Scala 2 for a sealed family **compiled in another module** (loaded from class
files): there are no source positions, so `UCommons.inDeclarationOrder` lists those members by full name (the 0.9.0
rule, README "Sum types and enums"). The derived `family` then is in name order on Scala 2 and in declaration order on
Scala 3, and this fix keeps whatever order the derivation gives (B7-R1).

## Why it is a problem

Order doesn't change what validates. But the schema is how the type is documented: doc generators, form/UI pickers
and code generators (OpenAPI → client enums) list the values in schema order, so `DayOfWeek` comes out as
Tuesday, Friday, Monday... The output also differs between Scala 2.12 and 2.13/3, and between an enum of 4 and of 5
values, which surprises anyone diffing generated schemas. README/paradox document the limitation ("with more, the
order is fixed, but it is hash order"), which is not a contract anyone wants.

## Options

### A. Keep `Set[Value]`, build it as an insertion-ordered `immutable.ListSet` — WON

`ListSet` is an immutable `Set` that iterates in insertion order on 2.12 (since the 2.12.0 rewrite), 2.13 and 3.
Every library site that builds an `enum` passes a `ListSet` built from the ordered values instead of `.toSet`:

- `Schema.enum.of` (both overloads) builds a `ListSet`. It becomes the one builder the derivations use (B7-R4): the
  Scala 2 macro already emits `enum.of`; Scala 3 `Enums.emitEnum` (Java enums, `Enumeration`, `Sums`' merged enum)
  switches from `Schema.enum($s, $vs.toSet)` to `Schema.enum.of($s, $head, $tail*)`; the `JsonSchemaProfile` family
  lowering (`Merged(vs, values) => enum(vs, values.toSet)`, which every sealed case-object family and Scala 3 enum goes
  through on both Scala versions) to `enum.of(vs, values.head, values.tail: _*)`;
- enumeratum: `co.values` (declaration order) instead of the `keySet`s of the maps (`namesToValuesMap` also holds
  `extraNamesToValuesMap` aliases: kept after the declared names, sorted, see plan step 4);
- parser: `ListSet(arr.value: _*)` instead of `arr.toSet`.

The modules build their `ListSet` themselves rather than call `enum.of`, because their value lists can be empty
(`"enum": []`, an enumeratum `Enum` with no entries) and `enum.of` takes at least one value.

Rendering needs nothing (`values.toSeq` of a `ListSet` keeps its order); `AsTree` likewise.

Sample: `{"type":"string","enum":["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]}` on all versions (for a family compiled
in another module, Scala 2 gives name order: see Problem).

Pros: no signature change, source and binary compatible (a patch under PVP). Equality is unchanged (`ListSet == HashSet`
with the same elements is `true`, same `hashCode`), so every existing `shouldBe enum.of(...)` test and user equality
check keeps working, and duplicates are still dropped.
Cons: order is a property of the value the user passes, not of the type: a user who builds `enum(tpe, Set(a,b,c,d,e))`
by hand still gets hash order (their `Set` has none), and a later `.map`/`++` on `values` typed as `Set` can silently
rebuild a hash set. `ListSet` is O(n) per insert: building is O(n²), irrelevant at enum sizes (a 1000-constant Java
enum costs about 10⁶ cheap steps, once, when the schema value is built).

### B. Change the field to an ordered `Seq[Value]` (distinct) — LOST

`final case class enum[T](tpe: Schema[_], values: Seq[Value])`, `of` and every builder pass lists; equality stays
order-insensitive (`values.toSet == v.toSet`) or becomes ordered.

Pros: the model says what it means; no way to lose the order later.
Cons: breaking under PVP (source: `enum(t, Set(...))`, pattern matches binding `values: Set`; binary: the case class's
`apply`/`copy`/accessor signatures), so it needs 0.10.0 for a cosmetic fix. Could be paired with a deprecated
`Set`-taking `apply`, but `unapply`/the accessor type still change. Worth doing only together with B3 (`oneof.subTypes`
is the same kind of `Set`) in a future breaking release; A doesn't block that.

### C. Sort at render time — LOST

Render `values` sorted (canonical text, or numbers numerically). Deterministic across Scala versions, one place to
change. But it's not declaration order (`Mon, Tue, ...` becomes `Fri, Mon, Sat, ...`; `DayOfWeek` alphabetical), and it
would reorder small enums that are correct today. Fixes the instability, not the bug.

### Sub-decision: the Legacy profile

`JsonSchemaProfile.Legacy` reproduces 0.7, and 0.7 rendered ≥5-value enums in hash order. With A, Legacy gets
declaration order too (the change is in the tree, which Legacy lowers like Neutral).

- **A1 (recommended): Legacy gets declaration order.** 0.7's order isn't a stable contract: it differs between 2.12 and
  2.13, and between 4 and 5 values. No golden changes (the golden harness sorts arrays, `golden-legacy/` included).
- A2: Legacy keeps hash order: in the renderer, when the profile is Legacy, write `HashSet(values.toSeq: _*).toSeq`.
  This needs the profile at `mkEnum` (or a Legacy-only re-wrap in the profile's tree pass, which doesn't see
  `Enumeration`/Java-enum `enum`s built outside families, so it'd have to walk the whole tree). Extra code to keep an
  accident. Choose this if byte-exact 0.7 output for ≥5-value enums matters to someone.

The lead agreed to A1 (2026-11-23), on condition that the PR's output-change section calls it out for the owner.

## Decision

A with A1: keep `Set[Value]` and make every library builder pass an insertion-ordered `ListSet`; all profiles, Legacy
included, render enum values in the order the derivation lists them: declaration order, except a Scala 2 family loaded
from another module's class files (name order, unchanged by this fix). Patch-level, no API change.

## Implementation plan

1. `core/src/main/scala/json/Schema.scala`: `enum.of(tpe, x, xs*)` → `ListSet((x +: xs): _*)`; `enum.of(x, xs*)` →
   `ListSet((x +: xs).map(va.adapt): _*)` (map the `Seq` first, then build: `.toSet.map` would rebuild a hash set).
   Scaladoc on `enum`: values iterate (and render) in the order of the `Set` passed; the library's builders pass
   `ListSet`s in declaration order; pass a `ListSet` to keep yours.
2. `core/src/main/scala-3/.../derivation/Enums.scala` `emitEnum`: emit `Schema.enum.of[t]($s, $h, ${Varargs(t)}*)`
   for a non-empty value list (always the case: an enum without items is rejected earlier, "No items defined"); keep a
   defensive `Nil` branch that emits `Schema.enum[t]($s, ListSet.empty)`.
3. `core/src/main/scala/json/schema/JsonSchemaProfile.scala` line 342: `enum.of(vs, values.head, values.tail: _*)`
   (`Merged` always holds at least one value); update the comment at line 221 ("`oneof` and `enum` hold Sets ...") to
   say `enum` keeps the members' order. Don't touch the `oneof` lines (B3).
4. `modules/enumeratum/.../EnumeratumSupport.scala`: `mkEnumeratumEnum` → `ListSet(co.values.map(_.entryName) ++
   co.extraNamesToValuesMap.keys.toList.sorted: _*)` mapped to `Value.str`: declared names first, then the aliases in
   name order (enumeratum documents no order for them; sorted keeps them deterministic from 5 on, B7-R2), deduped.
   Value enums → `ListSet(co.values.map(v => Value.num(v.value)): _*)` (`Value.str` for Char/String), which also
   dedupes `AllowAlias` duplicates (`StringVWithAlias` stays `["q","z"]`). Key patterns (`KeyPattern.forEnum`, which
   sorts) unchanged: out of scope (B6's area), deterministic already.
5. `modules/parser/.../ParseJsonSchema.scala` lines 60–63: `ListSet(arr.value: _*)`.
6. Tests (already committed, failing): `EnumOrderSpec` (shared: sealed objects under Neutral and Legacy, `ToValue`,
   mixed family, `Enumeration`, Java enums `DayOfWeek`/`TimeUnit`, both `enum.of`s, the Map-key pattern), Scala 3
   `Scala3EnumOrderSpec` (enum of singletons, Legacy, enum with a parameterized case), enumeratum (`E6`, `IntV6` in
   non-sorted order), parser. They compare the rendered `enum` array as an ordered list, which the golden harness
   (it sorts arrays) can't. New (B7-R1): a 6-object family `LibraryColor` (Red, Green, Blue, Cyan, Yellow, Black) in
   `modules/test-models`, derived in `EnumOrderSpec`; the expected order comes from a per-version test object
   (`api/src/test/scala-2/.../EnumOrderExpected.scala`: name order Black..Yellow; `scala-3/...`: declaration order),
   so a later Scala 2 fix shows up as a deliberate test change. Run on 2.12, 2.13, 3 and JS (`apiJS`,
   `enumeratumJS`, `parserJS`).
7. Goldens: no change expected (canonicalized). Regenerate once with `GOLDEN_UPDATE=1 sbt clean api/test` on each
   version to confirm an empty diff.
8. Docs: README "Sum types and enums" (l. 336–337) and `docs/src/main/paradox/types.md` (l. 75–76): an `enum` keeps
   the members' order (declaration order; on Scala 2, members of a family compiled in another module are in name
   order) whatever its size; the `oneOf` half of the sentence stays as is (B3's), and the sentence says `enum`, so it
   doesn't read as covering `Flag.EnumsAsOneOf` (B7-R3). One line for people who build their own (B7-R5): use
   `enum.of(...)`, or pass a `ListSet`, to keep your order (a plain `Set` of 5 or more values iterates in hash
   order). The same line in the `enum` scaladoc. Leave the 0.9 migration notes alone (they describe 0.9.0). No
   CHANGELOG edit.

Scala 2 vs 3: the only per-version code is step 2 (and the test expectation in step 6). JS: nothing platform-specific.

## Compatibility

- Source and binary compatible: no signature changes. `values` is still `Set[Value]`; equality and `hashCode` unchanged.
- Output: for enums with 5+ values, the `enum` array order changes from hash order to declaration order, in every
  profile (Neutral, Legacy, the bridge profiles) and every draft. Enums of ≤4 values: unchanged. Validation result: unchanged.
- PVP: patch (0.9.1).
- Legacy: an order-only change vs 0.7 for 5+ value enums (sub-decision A1; A2 is the alternative). `golden-legacy/` is
  canonicalized, so it doesn't change.
- Not changed (B7-R3): anything rendered as a `oneOf` keeps the `oneof` Set's order, i.e. hash order from 5 members
  on: `Flag.EnumsAsOneOf` (a `oneOf` of `const`s) and families with 5 or more non-singleton members. That's B3's
  `oneof.subTypes`.
- Not changed (B7-R1): Scala 2 lists the members of a family compiled in another module by name (no source
  positions); the enum renders them in that order. Recovering declaration order there (e.g. by the members' index in
  the owner's `decls`, as `fromJavaEnum` does) would also reorder Scala 2 `oneOf`s of library families (cats
  `Validated`/`Ior`): a separate item if wanted, noted to the lead.
- Neighbours: B3 (sealed-collapse) changes `oneof.subTypes`, which is built next to the `enum` line in
  `JsonSchemaProfile.familyAt`; both edits are one-liners in different lines. If B3 picks `ListSet` too, the README
  sentence can drop the "up to 4" caveat entirely.

## Review log
B7-R1 (must) Scala 2 library families are in name order; document and pin with a test → accepted: (a) Problem,
Decision, sample, Compatibility and the planned README/paradox sentence now say so; (b) plan step 6 adds a 6-object
family in `test-models` with per-version expectations; (c) not in scope: listed under Compatibility and mailed to the
lead as a possible separate item.

B7-R2 (nit) enumeratum aliases from a Map → accepted: `extraNamesToValuesMap.keys.toList.sorted`, after the declared
names (plan step 4).

B7-R3 (nit) say what stays unordered → accepted: `Flag.EnumsAsOneOf` and 5+-member `oneOf`s named under
Compatibility; the README sentence speaks of `enum` only (plan step 8).

B7-R4 (nit) one core builder → accepted for the derivations: Scala 3 `emitEnum` and the profile lowering call
`enum.of` (plan steps 2–3), so the ordering lives in `enum.of`. The parser and enumeratum keep building a `ListSet`
themselves, because their lists can be empty and `enum.of` takes at least one value.

B7-R5 (nit) a user-doc line about hand-built enums → accepted: plan step 8 (README, paradox, scaladoc).
