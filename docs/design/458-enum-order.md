# Enum values in declaration order  (issue #458, item B7)
Status: draft

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

JVM and JS give the same order (the hashes are deterministic); 2.12 differs from 2.13/3 because 2.13 seeds case-class
hashes with the product prefix. The same enum as a `Map` key already gets declaration order
(`"^(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)$"`, derived key patterns don't go through a `Set`).

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

- `Schema.enum.of` (both overloads; the Scala 2 macro emits `enum.of`, so it needs no change);
- Scala 3 `Enums.emitEnum`: `$vs.toSet` → `ListSet.from($vs)` (covers Java enums, `Enumeration`, and `Sums`' merged enum);
- `JsonSchemaProfile` family lowering: `Merged(vs, values) => enum(vs, values.toSet)` → `ListSet(values: _*)` (sealed
  families and Scala 3 enums on both Scala versions go through here);
- enumeratum: `co.values.map(_.entryName)` / `co.values.map(_.value).distinct` instead of the `keySet`s of the maps
  (`namesToValuesMap` also holds `extraNamesToValuesMap` aliases: kept after the declared names, see plan step 4);
- parser: `ListSet(arr.value: _*)` instead of `arr.toSet`.

Rendering needs nothing (`values.toSeq` of a `ListSet` keeps its order); `AsTree` likewise.

Sample: `{"type":"string","enum":["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]}` on all versions.

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
included, render enum values in declaration order. Patch-level, no API change.

## Implementation plan

1. `core/src/main/scala/json/Schema.scala`: `enum.of(tpe, x, xs*)` → `ListSet((x +: xs): _*)`; `enum.of(x, xs*)` →
   `ListSet((x +: xs).map(va.adapt): _*)` (map the `Seq` first, then build: `.toSet.map` would rebuild a hash set).
   Scaladoc on `enum`: values iterate (and render) in the order of the `Set` passed; the library's builders pass
   `ListSet`s in declaration order; pass a `ListSet` to keep yours.
2. `core/src/main/scala-3/.../derivation/Enums.scala` `emitEnum`: `ListSet.from($vs)` (Scala 3 only, 2.13 stdlib).
3. `core/src/main/scala/json/schema/JsonSchemaProfile.scala` line 342: `ListSet(values: _*)`; update the comment at
   line 221 ("`oneof` and `enum` hold Sets ...") to say `enum` keeps declaration order. Don't touch the `oneof` lines (B3).
4. `modules/enumeratum/.../EnumeratumSupport.scala`: `mkEnumeratumEnum` → `ListSet((co.values.map(_.entryName) ++
   co.extraNamesToValuesMap.keys): _*)` mapped to `Value.str` (declared names first, then any aliases, deduped);
   value enums → `ListSet(co.values.map(v => Value.num(v.value)): _*)` (and `str` for Char/String), which also dedupes
   `AllowAlias` duplicates (`StringVWithAlias` stays `["q","z"]`). Key patterns (`KeyPattern.forEnum`, which sorts)
   unchanged: out of scope, deterministic already.
5. `modules/parser/.../ParseJsonSchema.scala` lines 60–63: `ListSet(arr.value: _*)`.
6. Tests (already committed, failing): `EnumOrderSpec` (shared: sealed objects under Neutral and Legacy, `ToValue`,
   mixed family, `Enumeration`, Java enums `DayOfWeek`/`TimeUnit`, both `enum.of`s, the Map-key pattern), Scala 3
   `Scala3EnumOrderSpec` (enum of singletons, Legacy, enum with a parameterized case), enumeratum (`E6`, `IntV6` in
   non-sorted order), parser. They compare the rendered `enum` array as an ordered list, which the golden harness
   (it sorts arrays) can't. Run on 2.12, 2.13, 3 and JS (`apiJS`, `enumeratumJS`, `parserJS`).
7. Goldens: no change expected (canonicalized). Regenerate once with `GOLDEN_UPDATE=1 sbt clean api/test` on each
   version to confirm an empty diff.
8. Docs: README "Sum types and enums" (l. 336–337) and `docs/src/main/paradox/types.md` (l. 75–76): an `enum` keeps
   declaration order whatever its size; the `oneOf` half of the sentence stays as is (B3's). Leave the 0.9 migration
   notes alone (they describe 0.9.0). No CHANGELOG edit.

Scala 2 vs 3: the only per-version code is step 2. JS: nothing platform-specific.

## Compatibility

- Source and binary compatible: no signature changes. `values` is still `Set[Value]`; equality and `hashCode` unchanged.
- Output: for enums with 5+ values, the `enum` array order changes from hash order to declaration order, in every
  profile (Neutral, Legacy, the bridge profiles) and every draft. Enums of ≤4 values: unchanged. Validation result: unchanged.
- PVP: patch (0.9.1).
- Legacy: an order-only change vs 0.7 for 5+ value enums (sub-decision A1; A2 is the alternative). `golden-legacy/` is
  canonicalized, so it doesn't change.
- Neighbours: B3 (sealed-collapse) changes `oneof.subTypes`, which is built next to the `enum` line in
  `JsonSchemaProfile.familyAt`; both edits are one-liners in different lines. If B3 picks `ListSet` too, the README
  sentence can drop the "up to 4" caveat entirely.

## Review log
