# Misc types: java.util collections and Optional, Array[Byte], @deprecated, cats NonEmptySeq, joda Period/DateTimeZone/Interval, a null schema  (issues #481, #288, item E8)
Status: approved (r-e8, 2026-11-23); implemented (#485)

## Problem
All on 0.9.0, the same on 2.12.21, 2.13.18 and 3.3.9 (JVM; JS derives the same trees) unless noted. Repro specs on the branch:
`api/.../MiscTypesSpec.scala`, `modules/cats/.../NonEmptySeqReproSpec.scala`, `modules/joda-time/.../JodaMiscReproSpec.scala`,
`modules/play-json/.../JsNullReproSpec.scala` (they become the real specs in the implementation).

1. **java.util collections and Optional.** `Json.schema[java.util.List[Int]]` (also `Set`, `Map`, `Optional`, and as fields of a case class)
   is a compile error: `schema for java.util.List[Int] is not supported, ` (Scala 3: `java.util.List[scala.Int]`). The macros' collection
   families test `<: Iterable` / `<: Array` / `<: collection.Map` and the option family tests `scala.Option` only.
2. **`Array[Byte]`** renders `{"type":"array","items":{"type":"number"}}` (via `Predef.arrayS` + `Predef.byteS`). There is no way to say
   "a base64 string", which is what Jackson writes.
3. **`@deprecated`** is ignored: `case class Old(@deprecated("use b", "1.0") a: Int, b: Int)` renders `"a": {"type": "integer"}` on 2020-12.
4. **cats `NonEmptySeq[Int]`** (with `import CatsSupport._`): `schema for cats.data.NonEmptySeq[Int] is not supported` (the other non-empty
   collections work). **joda `Period`, `DateTimeZone`, `Interval`** (with `import JodaTimeSupport._`): `... is not supported`.
5. **#288, a null schema.** `json.Schema.`null`` (`{"type":"null"}`, 0.8.0) exists but isn't documented. The obvious way to give a JSON
   library's null a schema,
   `implicit val jsNull: Predef[JsNull.type] = Predef(Schema.`null`)`, works on Scala 2 (`"name": {"type": "null"}`), and **fails on Scala 3**:
   `Found: json.Schema.null.type, Required: json.Schema[play.api.libs.json.JsNull.type]` (on Scala 3 `Null` doesn't conform to a singleton type).
   Without it: `schema for play.api.libs.json.JsNull.type is not supported`.

What the libraries write (probe, 2.13, JDK 21: circe 0.14.17, play-json 3.0.6, upickle 4.4.3, json4s 4.0.7, spray 1.3.6, jackson-module-scala
2.23.0 with jdk8/joda modules, jsoniter-scala 2.43.0; notes/scratch/c-e8/probe/RESULTS.md):

| type | circe, play, upickle, spray, jsoniter | jackson | json4s (decompose) |
|---|---|---|---|
| `Array[Byte](1,-2,127)` | `[1,-2,127]` (all six, json4s too) | `"Af5/"` (base64) | `[1,-2,127]` |
| `java.util.List/Set/Map` | no codec | `[1,2]`, `["a","b"]`, `{"a":1}`, `Map[Int,_]` keys as strings `{"1":2}` | List/Set as arrays; Map fails on JDK 17+ |
| `Optional[Int]` field | no codec | `of(1)` → `1`, `empty()` → `null` (written, not omitted) | fails on JDK 17+ |
| `NonEmptySeq[Int]` | circe `[1,2]`; others no codec | bean `{"toSeq":[1,2]}` (root) | same as jackson |
| joda `Period` | no codec | `toString`: `P1Y2M3W4DT5H6M7.008S`, `P2W`, `P-1DT-2H` (per-field signs) | json4s-ext: `toString` |
| joda `DateTimeZone` | no codec | the id: `Europe/Paris`, `UTC`, `+01:00` | `{}` |
| joda `Interval` | no codec | `1577836800000-1580558400000` (millis) | json4s-ext: `{"start":..,"end":..}`; `toString` is `2020-01-01T00:00:00.000Z/2020-02-01T12:00:00.000Z` |

## Why it is a problem
- java.util types appear in models shared with Java code (Jackson, jsoniter users); today the only way is a hand-written `Predef` per
  element type, because a `Predef[java.util.List[T]]` combinator needs a `Predef[T]` (the cats problem, E1).
- `Array[Byte]`: the default matches six of the seven libraries, but Jackson users get a schema that rejects every value.
- `@deprecated`: the 2019-09 meta-data keyword `deprecated` exists exactly for this; generated API docs lose it.
- NonEmptySeq / joda: gaps in modules that otherwise cover the family (cats 2.4+ has NonEmptySeq, circe ships a codec).
- #288 (owner): "there is no way to expose type: null". There is, on Scala 2, undocumented; on Scala 3 users need a cast.

## Options

### 1. java.util collections and Optional

#### 1A. Teach both macros' families the java.util shapes — WON
- Arrays (Scala 2 `UArrays.ArrExtractor`, Scala 3 `Collections.deriveArray`): also accept `<: java.util.Collection[E]`; the element is the
  `java.util.Collection` base type's argument; `unique` iff `<: java.util.Set`. The tree is built with a **canonical container**, not the
  type's own constructor: `array[E, java.util.Set](element, true)` for sets, `array[E, java.util.Collection](element, false)` for the rest,
  cast to `Schema[<the type>]` (R1: `array[E, C[_]]`'s `C` is unbounded, so `EnumSet[E <: Enum[E]]` as `C` is a kind error on 2.13 and 3; the
  container is phantom, and `Lowering` rebuilds every array as `array[Any, List]` anyway). So `ArrayList`, `LinkedList`, `HashSet`, `TreeSet`,
  `ArrayDeque`, `EnumSet[TimeUnit]`, ... all work, and the Scala `C[E]` rule isn't needed for java types: `class Names extends
  java.util.ArrayList[String]` is an array of strings (the element comes from the `Collection` base type).
- Dictionaries (`UDictionaries`, `Collections.deriveDictionary`): also accept `<: java.util.Map[K, V]` (`HashMap`, `TreeMap`, `LinkedHashMap`,
  `ConcurrentHashMap`, `EnumMap[TimeUnit, V]`, ...), built as `dictionary[K, V, java.util.Map]` and cast the same way (K, V from the
  `java.util.Map` base type), with exactly the Scala key rules (String/value class of String: no pattern; `KeyPattern[K]`; string enums; else the
  key error). Unlike `scala.collection.mutable.Map` (rejected, TRIAGE-B), java maps are accepted: there is no immutable java map type to prefer,
  and the one library that writes them (Jackson) writes every `java.util.Map` the same way.
- Optional: `java.util.Optional[X]` is an `option` node wherever `scala.Option[X]` is (root, element, map value, field), recognised before the sum
  and product families. As a field: not required, like `Option` (Jackson writes `empty()` as `null`, which the Neutral `["x","null"]` accepts).
  The node is `Schema.option[X]`, a `Schema[Option[X]]`; at the root (and as an element) the macro casts it to `Schema[java.util.Optional[X]]`
  (the type parameter is phantom; profiles match `option` structurally). Defaults of an `Optional` field get no special case: like any default,
  they need a `ToValue[java.util.Optional[X]]` (none in core or the bridges), so `= Optional.empty()` is the existing "Can't infer json value for
  default" error (B18 decides defaults without a `ToValue` for everyone). `OptionalInt`/`OptionalLong`/`OptionalDouble`: not in scope (no type
  argument, so not an `option` of anything the field logic knows; rare in Scala).
  At render time the `option` node can't tell `Optional` from `Option`, so **`Optional` renders like `Option` under every profile** (R7):
  Neutral `["x","null"]` / not required; Legacy 0.7's Option shapes (a root or element `Optional` is
  `oneOf[object{value}, enum["None"]]`, a field drops the layer); each bridge profile like its library's `Option` (`optionKeyRequired`
  included), though none of those libraries has an `Optional` codec. The user docs say so.
- Magnets (R5): `Magnet[java.util.Collection[X], Iterable[_]]`, `List`, `Set` → `Iterable[_]`, `Magnet[java.util.Map[K, V], Map[_, _]]`, so
  `.withValidation(minItems := 1)` works on a `Schema[java.util.List[X]]` like on `Schema[List[X]]`.

Pros: works for elements of every kind (derived case classes, implicit `Schema` → `$ref`, Predefs), exactly like the Scala counterparts; no new
API. Cons: touches both macros (four families).

#### 1B. Core Predef combinators `Predef[java.util.List[T]]` from `Predef[T]` (like `Predef.listS`) — LOST
Small and macro-free, but only for elements that have a `Predef` (a `java.util.List[User]` of a derived case class stays an error) and not for
`Optional` fields (a Predef'd type is required: the field logic only knows `Option`). It's the cats problem E1 solves; doing it here would give
two half-solutions.

#### 1C. Do nothing, document "write a Predef" — LOST
The status quo; a Predef per element type is unreasonable for `java.util.List[User]`.

### 2. Array[Byte]

#### 2A. Keep the array default; add an opt-in `Predef.bytesAsBase64` — WON
`json.schema.Predef.bytesAsBase64: Predef[Array[Byte]]` (a plain `val`, not implicit) =
`string[Array[Byte]].withValidation(contentEncoding := "base64", pattern := "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$")`,
rendered `{"type":"string","contentEncoding":"base64","pattern":"..."}`. The pattern (R4: standard base64 with padding, no line feeds,
what Jackson writes) makes it validate on every draft: `contentEncoding` is an annotation in 2019-09+ and not asserted by networknt on
draft-07. Users opt in
with `implicit val bytes: Predef[Array[Byte]] = Predef.bytesAsBase64`; a lexically visible implicit wins over `Predef.arrayS` in the companion
(Scala 2 and 3). `contentEncoding` is an existing validation (`json.schema.validation.Instance`); like every validation it is rendered on all
drafts (keyword gating per draft is a separate known issue), and it is a draft-07+ keyword that older validators ignore. The items' `number`
stays: `Byte` → `integer` is D2's decision.

#### 2B. base64 by default — LOST
Wrong for six of the seven libraries probed (all the ones the bridges target): their output would be rejected.

#### 2C. A profile knob (`bytesStyle`) — LOST
`Array[Byte]` reaches the renderer as `array(number)`; a profile can't tell it from `Array[Short]` (the `number` node only keeps a `Numeric`).
It would need a new node or a marker, for one library we have no bridge for. 2A gives Jackson users the same with one line.

### 3. @deprecated

#### 3A. A `deprecated` flag on `Schema` (like `title`), set from `@deprecated` on fields and types, rendered from 2019-09 — WON
- Model: `Schema` gets `def deprecated: Boolean` and `def withDeprecated(x: Boolean): Self` (a new private var, copied by `duplicate`,
  `withExtraFrom`; part of `equals` and of the tree's `toString`, ` deprecated`). Additive; `duplicate`'s signature is unchanged.
- Macros: `@scala.deprecated` or `@java.lang.Deprecated` on a case class parameter (or its field/getter: the macros already merge them) makes
  the field's schema `.withDeprecated(true)`; on a type, the same where the type's `@title` is applied today (the root and its definitions;
  nested derived types follow B12). The message and `since` aren't rendered (the keyword is a boolean).
- Rendering: `"deprecated": true` next to the schema's other keywords on Draft 2019-09 and 2020-12 (and Raw); not on Draft 04/06/07, where
  the keyword doesn't exist. A field whose schema is a `$ref` renders `{"$ref": ..., "deprecated": true}` (siblings of `$ref` are allowed from
  2019-09); the definition itself isn't marked.
- Where a schema is rebuilt, the flag must survive (R2): `duplicate` and `withExtraFrom` (which `Lowering` uses for every node it rebuilds) copy
  it; `Schema.def.adapt` (an implicit `Schema` found by the macros, master drops the def node's extras, B1 #469 keeps its title/description)
  keeps it too; `Lowering.field`'s three branches are pinned by tests: the Legacy-Option unwrap moves the dropped `option` layer's flag to what
  stays (B1's `unwrapped`, extended; on master a small equivalent), because it is reachable with the flag kept, through a non-Legacy profile
  with `withOption(OptionStyle.Legacy)`; `optionKeyRequired` and plain go through `apply(t)`, which keeps it. The macros put a field's `.withDeprecated(true)` outermost, after `adapt`, so a `$ref` field is marked at the `$ref`
  and the shared definition is not. B1 (#469) touches the same spots (`adapt`, B1's `unwrapped`, `AsDraftSupport.apply`'s `texts(x)`, where
  `"deprecated"` goes): I implement on master and rebase onto B1 once it merges, extending its `unwrapped`/`adapt`/`texts` with the flag.
- Profiles: every profile keeps it except **Legacy, which strips it** (lead, 2026-11-23: a new keyword is a style addition, and Legacy keeps
  0.7's output for existing models). Mechanism: a profile knob `def deprecatedKeyword: Boolean = true` (+ `withDeprecatedKeyword(x)`, a
  `Knobs` field), `false` for `Legacy` only, read by `Knobs.apply(p)` so every `withX` builder keeps it (R9a). When it is false, `Lowering` clears
  the flag on every node it visits, leaves, `enum`, `const`, `ref` included (today they pass through `case other => other`), and through field
  schemas, def bodies and family members (R9b), touching only flagged nodes (`if (s.deprecated) s.withDeprecated(false) else s`), so unflagged
  schemas lower to the same nodes as today (R9c). A knob rather than a test on the profile's name, so `Legacy.withName(..)` and other Legacy-derived profiles keep it. The lost
  alternative "Legacy keeps it too" is `Legacy`'s knob set to `true`.

#### 3B. A `deprecated` fact on `object.Field` (like `rwMode`) — LOST
Closer to `readOnly`/`writeOnly`, but `Field` is a case class (a new field breaks its `apply`/`copy`/`unapply` binary signatures: PVP minor),
and it can't express a deprecated type.

#### 3C. A new `@json.schema.deprecated` annotation — LOST
Users already write Scala's `@deprecated`; a second annotation would only add noise. (3A reads Scala's and Java's.)

### 4. cats NonEmptySeq, joda Period / DateTimeZone / Interval

#### 4A. Follow each module's existing pattern — WON
- cats: `NonEmptySeq` exactly like `NonEmptyList`: `nonEmptySeqSchema[T](implicit Schema[T])`, `...FromPredef[T](implicit Predef[T])`, a
  `Magnet[NonEmptySeq[X], Iterable[_]]`; `array` + `minItems: 1` (circe writes `[1,2]`). In `CatsSupport.scala` (all Scala versions:
  cats 2.13 has it on 2.12). A `NonEmptySeq` of a derived element stays E1's.
- joda: `implicit val` `Schema`s (so definitions + `$ref`, like the other joda types) and `predef.*` `Predef`s:
  - `Period`: `string` + `pattern` `^P(?=[-0-9T])(?:-?[0-9]+Y)?(?:-?[0-9]+M)?(?:-?[0-9]+W)?(?:-?[0-9]+D)?(?:T(?=[-0-9])(?:-?[0-9]+H)?(?:-?[0-9]+M)?(?:-?[0-9]+(?:\.[0-9]+)?S)?)?$`
    (joda's `toString`, what Jackson and json4s-ext write; the lookaheads reject a bare `P` / trailing `T`, which joda never writes, R6). Not `format: duration`: RFC 3339 durations have no signs, no fractions, and don't
    mix `W` with other units, and joda writes all three (same reasoning as E3's java `Period`).
  - `DateTimeZone`: plain `string` (the id; like E3's `ZoneId`).
  - `Interval`: plain `string`: it covers `toString` (`start/end` ISO) and Jackson's `millis-millis`; a pattern would pick one of them.

#### 4B. `format: duration` for Period, a `start/end` pattern for Interval — LOST
Rejects values joda writes (`P-1D`, `PT1.5S`, `P1Y2W`) where formats are asserted; the Interval pattern rejects Jackson's form.

### 5. A null schema (#288)

#### 5A. Document `Schema.`null`` / `Predef[Null]`, and ship `Predef`s for the bridges' null singletons — WON
- README + paradox: `Schema.`null`` renders `{"type":"null"}`; a `Null` field needs `implicit val n: Predef[Null] = Predef(Schema.`null`)`
  (if B11 #476 has merged by then, its `Null` error text points at it too).
- Bridges: an implicit `Predef` for the library's null singleton in an opt-in `predef` object of the bridge (`import AsPlay.predef._`):
  play-json `JsNull.type` (`AsPlay.predef`), spray-json `JsNull.type` (`AsSpray.predef`), json4s `JNull.type` (`AsJson4s.predef`), upickle
  `ujson.Null.type` (`AsU.predef`). Not in `AsPlay` itself: c-e2 measured that an implicit `Predef` in the object users import for `ToValue`
  makes a user's own `Predef` of the same type ambiguous (2.13 and 3.3.9); `JodaTimeSupport.predef` is the precedent, and E2 puts its
  `JsValue`/`JsObject` Predefs in the same `predef` objects (agreed with c-e2: whoever merges second merges the two object bodies). Built once with a cast (`Schema.`null`.asInstanceOf[Schema[JsNull.type]]`), so it works on Scala 3. circe has no
  null type (`Json.Null` is a `Json`). These are leaves next to E2's "any value" Predefs (`JsValue`, `Json`, ...); E2 adds those, not the null ones.

#### 5B. Documentation only (with a cast for Scala 3) — LOST
Answers the issue on paper; every Scala 3 user writes the cast.

#### 5C. A core helper (`Schema.nullOf[T]`) — LOST
A public API for one line in each bridge; users would still have to write the implicit.

## Decision
java.util collections and `Optional` derive structurally in both macros like their Scala counterparts (1A). `Array[Byte]` keeps the array
default with an opt-in base64 `Predef` (2A). `@deprecated` becomes a `deprecated` flag on `Schema`, rendered from 2019-09 (3A). cats `NonEmptySeq`
and joda `Period`/`DateTimeZone`/`Interval` follow their modules' patterns (4A). #288: document the null schema and ship null-singleton Predefs
in four bridges (5A).

## Implementation plan
1. **Scala 2 macro** (`macros/`): `UCommons.T`: `jCollection`, `jSet`, `jMap`, `jOptional`, `annotation.deprecated` (scala + java).
   `UArrays`: the java.util.Collection branch (canonical container + cast, no `C[E]` rule). `UDictionaries`: the java.util.Map branch
   (shares the key logic; canonical `java.util.Map` + cast). `Macroses.resolve`: `Optional`
   next to `Option` (`U.Opt`, with the cast for a non-field position: `U.Opt` gets the outer type and emits `.asInstanceOf` only when it isn't
   `scala.Option`). `UProductTypes`: `isOption` also for `Optional` (required, effective type; the `None` default check stays `Option`-only, an
   `Optional` default goes the `ToValue` way). `SchemaTypes.Extra`: `deprecated: Boolean`, emitted as `.withDeprecated(true)`; fields from
   `f.hasAnnotation.deprecated`, types from the type decorations.
2. **Scala 3 derivation** (`core/src/main/scala-3/.../derivation`): `Options.deriveOption` + `optionElement` for `Optional` (cast on emit);
   `Products`: `optional` for `Optional` fields; `Collections.deriveArray`/`deriveDictionary`: the java branches (canonical containers, emitted with a cast; key errors shared);
   `Nodes.Extra.deprecated`, `Annotations` (type) and `Products` (field) read `scala.deprecated` / `java.lang.Deprecated`. Families order
   unchanged (option, dictionary, array before the sum family; B11 checks `Null` before them).
3. **core model/renderers**: `Schema` `deprecated` / `withDeprecated` (+ `duplicate`, `withExtraFrom`, `equals`, `ToString.writeExtra`);
   `Schema.def.adapt` keeps the def node's flag; `AsDraftSupport.apply`: `"deprecated": true` when `isDraft09 || isDraft12` (in B1's
   `texts(x)` after the rebase); `AsRaw` too. `SchemaEquality` if it compares extras.
   `Predef.bytesAsBase64` (+ a private `Magnet[Array[Byte], String]`). `Magnet`: the four java collection magnets (R5). `JsonSchemaProfile`: the `deprecatedKeyword` knob (Legacy `false`),
   applied in `Lowering`.
4. **cats**: `NonEmptySeq` instances in `CatsSupport.scala`. **joda-time**: three `Schema`s + `predef` entries in `JodaTimeSupport.scala`.
   **bridges**: the four null-singleton Predefs.
5. **Tests** (repro specs become these): `MiscTypesSpec` (shared, all versions, JVM + JS): trees and rendered JSON of java `List`/`Set`/
   `ArrayList`/`TreeSet`/`Map`/`HashMap`/`Map[Int, _]`/`Optional` (root, element, field: not required, nested `Optional[Optional[X]]`), a java
   collection of a derived case class and of a type with an implicit `Schema` (`$ref`), `EnumSet[TimeUnit]` and `EnumMap[TimeUnit, Int]`
   (R1; `TimeUnit` is in the Scala.js javalib), `class Names extends ArrayList[String]` (an array of strings), the key error for
   `java.util.Map[UUID, _]`, `Optional` field under a profile with `optionKeyRequired` (required + nullable), `.withValidation(minItems := 1)`
   on a `Schema[java.util.List[X]]`; `Array[Byte]` default unchanged; `bytesAsBase64` on 04/07/2020-12, and opted in through derivation (a local
   `implicit val`, a case class field `data: Array[Byte]`) on 2.12/2.13/3; `@deprecated` (scala and java) on a field, an `Option` field
   (Neutral, Legacy, a bridge profile with `optionKeyRequired`), a `$ref` field (the same implicit-`Schema` type used by a deprecated and a
   plain field: one definition, unmarked; only the deprecated field's `$ref` is marked) and a type, rendered on 2019-09/2020-12, absent on
   04/06/07, kept by a bridge profile, stripped by Legacy (and `Legacy.withName/withOption/withSum(..)`), including on a
   leaf (`@deprecated s: String`), a `$ref` field and an `Option` field; kept through a Neutral profile `.withOption(OptionStyle.Legacy)`'s
   field unwrap. Test models with `@deprecated` get `@nowarn("cat=deprecation")` (2.13 warns at the case class itself). `CatsSupportSpec`,
   `JodaTimeSupportSpec` (+ the Period pattern against the probe's strings), the bridges' specs (`JsNull` field → `{"type":"null"}` on 2 and 3).
   One golden model (`MiscTypes`) for the full rendered output per draft.
6. **Docs** (README + paradox, same words): java.util collections and Optional under Collections/Option (`Optional` renders like `Option`
   under every profile); `Array[Byte]` and
   `Predef.bytesAsBase64`; `@deprecated` under annotations (2019-09+); NonEmptySeq in the cats list; Period/DateTimeZone/Interval in the joda
   list; "A null schema" (`Schema.`null``, `Predef[Null]`, the bridges' null Predefs). No CHANGELOG.
7. JS: all java.util types used are in the Scala.js javalib and appear as types only; the shared spec runs on JS. joda, spray, json4s are JVM-only.

## Compatibility
- Source/binary: additive only (new methods on the sealed `Schema`, new implicits in cats/joda/bridges, a new `Predef` val). `duplicate`'s
  signature is unchanged. PVP: minor-compatible, fits a patch/minor release.
- Output: unchanged for every model that compiled before, except models with `@deprecated` rendered as 2019-09/2020-12 (and Raw), which gain
  `"deprecated": true`, under every profile except **Legacy**, which strips it and stays byte-exact with 0.7 (lead decision 2026-11-23).
  Lost alternative: Legacy renders it too (0.7 ignored the annotation; the keyword changes no validation): Legacy's `deprecatedKeyword = true`.
- `JsonSchemaProfile` gets a knob with a default (`deprecatedKeyword`), like `honorsSchemaAnnotations`: additive for user-implemented profiles.
- New compile successes: java.util collections, Optional, NonEmptySeq, the joda types, null singletons with a bridge import (previously errors).
- An implicit `Predef[JsNull.type]` in a bridge: Predef is invariant, so it can't stand in for `JsValue` (or anything else).

## Review log
Lead (2026-11-23, on 6840419's Compatibility question): Legacy strips `deprecated` → accepted: `deprecatedKeyword` knob, "Legacy keeps it" is
the lost alternative (ea7be17).

Round 1 (r-e8, on 6840419):
- E8-R1 (must) `EnumSet`/`EnumMap` don't fit the unbounded `C[_]` → accepted (a): java branches build with a canonical container
  (`java.util.Set` / `java.util.Collection` / `java.util.Map`) and cast to the type; the `C[E]` rule is dropped for java types (no longer
  needed), so `class Names extends ArrayList[String]` works. Tests for `EnumSet[TimeUnit]`, `EnumMap[TimeUnit, Int]`. Wording fixed.
- E8-R2 (must) `deprecated` through `adapt` / `Lowering.field` / B1 → accepted: named in 3A and plan step 3 (`def.adapt` keeps it; field flag
  outermost, after `adapt`; `Lowering.field` branches pinned by tests). Implemented on master, rebased onto B1 (#469) once it merges, extending
  B1's `unwrapped`/`adapt`/`texts` with the flag. Under Legacy the unwrap branch is moot (Legacy strips).
- E8-R3 (nit) tests → accepted, all four in step 5.
- E8-R4 (nit) base64 `pattern` → accepted (validates on every draft).
- E8-R5 (nit) java collection Magnets → accepted (four magnets, plan step 3, a test).
- E8-R6 (nit) Period pattern lookaheads → accepted.
- E8-R7 (nit) `Optional` under every profile → accepted: stated in 1A; user docs say "rendered like `Option` under every profile".
- E8-R8 (nit) process → the lead already decided (Legacy strips); E2 overlap: mailed the lead with round 1's answer (the bridge null Predefs sit in
  the same bridge objects as E2's). B11 mention made conditional.

Round 1b (r-e8, on ea7be17):
- E8-R2 (must, addendum) the Legacy unwrap is reachable via `withOption(OptionStyle.Legacy)` with the flag kept → accepted: the unwrap carries
  the flag (3A, test in step 5).
- E8-R9 (nit) knob mechanics → accepted (a) `Knobs.apply` reads it, tests for several builders; (b) cleared on leaves/enum/const/ref, field
  schemas, def bodies, family members; (c) only flagged nodes are copied.

During implementation:
- (from c-e2, E2) the bridge null Predefs move from `AsX` into opt-in `AsX.predef` objects: an implicit `Predef` in the object imported for
  `ToValue` makes a user's own `Predef` of that type ambiguous. Shared `predef` objects with E2. (Mailed r-e8; r-e8 OK, "docs must show
  the extra import": they do.)
- R9 as implemented: `Knobs` has the 8th field and `Knobs.apply` reads it (`FamilyLoweringSpec` "builders" covers every builder, incl.
  `withDeprecatedKeyword`); `Lowering.apply` = `stripped(node(s))`, so leaves/`enum`/`const`/`ref` are cleared too, and only flagged nodes are
  copied; the Legacy-Option unwrap moves the flag (`unwrapped`), tested through `Neutral.withOption(OptionStyle.Legacy)`.
- The golden harness's lossless `schema` dump records `deprecated`; a golden model each for `JavaColls`, `Old`, `OldOpt`.
- Found (pre-existing, mailed the lead): on Scala 3, `Json.schema[X]` of a `@deprecated` class warns at a position in the derivation's own
  source, which a `@nowarn` at the call site can't silence (a deprecated enclosing owner does). The spec uses a deprecated wrapper def.
