# Library profiles: built-in types the library writes differently  (issue #519, item B24)
Status: draft

## Problem

A library profile (`PlayProfile`, `Json4sProfile`, `UPickleProfile`, ...) re-shapes two kinds of tree nodes when a schema is
rendered: `option` and `family`. Every other node passes through as derived, so every built-in type keeps its core `Predef`
schema under every profile. Some libraries write some built-in types in another JSON shape, and the profile's schema then
rejects what the library writes.

```scala
import json.Json
import json.schema.Version.Draft07
import com.github.andyglow.jsonschema.{AsValue, PlayProfile, UPickleProfile}

AsValue.schema(Json.schema[java.util.Date], Draft07("x"))(implicitly, PlayProfile.default)  // {"type":"string","format":"date-time"}
play.api.libs.json.Json.toJson(new java.util.Date(1704103200000L))                         // 1704103200000

AsValue.schema(Json.schema[BigDecimal], Draft07("x"))(implicitly, UPickleProfile.default)   // {"type":"number"}
upickle.default.write(BigDecimal("1.5"))                                                   // "1.5"
```

Measured on 0.9.0 with play-json 3.0.6, json4s 4.0.7 (+ json4s-ext `JavaTimeSerializers.all`, `JodaTimeSerializers.all`),
upickle 4.4.3 and circe 0.14.17. Each value was written by the library and validated with networknt 1.5.9 against the schema
rendered under that library's profile (draft-07, and 2020-12 with format assertions on). Every row is rejected on both drafts.
Output is identical on 2.13.18 and 3.3.9: the Predefs and the profiles are shared code. Probes:
`notes/scratch/c-b24/probe/` (schemas + verdicts, `out.txt`), `probe-s3/` (the same on 3.3.9), `probe2/` (more written forms).

| library | type | written | schema under the library's profile |
|---|---|---|---|
| play-json | `java.util.Date`, `java.sql.Date`, `java.sql.Timestamp` (and `java.sql.Time`: same `Writes[java.util.Date]`) | epoch millis `1704103200000` | `string` (`date-time` / `date` / `time`) |
| play-json | `Map[K, V]` with `K` not `String` and not an `AnyVal`: `UUID`, a case-object enum, `LocalDate`, ... | an array of pairs `[["123e4567-…",1]]` | `object` + `patternProperties` |
| json4s-ext | `java.time.LocalTime`, joda `LocalTime` | `{"hour":10,"minute":15,"second":30,"millis":500000000}` (`millis` holds nanos for java.time) | `string` (`time`) |
| json4s-ext | `java.time.LocalDate`, joda `LocalDate` | `{"year":2024,"month":1,"day":1}` | `string` (`date`) |
| json4s-ext | `java.time.Instant`, `java.time.Duration`, joda `Instant`, joda `Duration` | millis `1704103200000`, `1500` | `string` |
| json4s | `java.sql.Date` (no serializer: reflection on a non-case class) | `{}` | `string` (`date`) |
| upickle | `Long` outside ±2^53 | a string: `"9007199254740993"` (±2^53 itself is still a number) | `number` |
| upickle | `BigInt`, `BigDecimal`, always | a string: `"5"`, `"1.5"`, `"1E+10"` (`read[BigInt]("5")` fails) | `number` |
| upickle | `Double`, `Float`: `NaN`, `±Infinity` | `"NaN"`, `"Infinity"`, `"-Infinity"` | `number` |

Controls that are fine today: play's `Map[Int|Long|Double|Char|Boolean, V]` (objects), play's java.time types (ISO strings),
json4s `java.util.Date` / `LocalDateTime` / `ZonedDateTime` (strings), circe and upickle `Map[UUID, V]` (objects), circe and play
`Long`/`BigInt`/`BigDecimal` (numbers). json4s-ext also writes `Year` as a number and `Period`, `YearMonth`, `MonthDay` as objects,
for types that get a Predef in E3 (#474).

**Why a profile can't fix it today:** the tree doesn't say which Scala type a leaf came from. `java.util.Date` and
`java.time.Instant` are both `` `string`(date-time) ``, and `Long`, `Double` and `BigDecimal` are all `` `number` ``. A
dictionary records nothing about its key, except a `patternProperties` validation.

## Why it is a problem

A profile promises "the shapes this library writes" (P6A in the 0.9 profile design: what the library emits, widened by what its
decoder accepts). For these types it describes another library's output. A play service that validates its responses rejects
every `java.util.Date` field. Under upickle every `BigDecimal` field is rejected, and so is every `Long` id above 2^53
(Snowflake-style ids, hashes). A json4s user who registers the usual `JavaTimeSerializers.all` gets a schema that rejects every
`LocalDate`, `LocalTime`, `Instant` and `Duration`. Only someone who knows the internals can work around it today: a local
`implicit Predef[T]` where the schema is *derived*, which then applies to every library the schema is rendered for.

There's also a user-facing gap next to it: a user whose own codec writes `Instant` as epoch millis has no way to tell a profile
so. The same mechanism closes both.

## Options

### A. Built-in Predefs record their Scala type; a profile knob maps a type to its library's schema  — WON

Two parts, both render-time, like `option` and `family`:

1. **The fact.** `Schema` gets one more attribute next to `title` / `description` / `discriminationKey`:
   `scalaType: Option[String]`, the full name of the Scala type the schema stands for (`"java.util.Date"`, `"scala.Long"`,
   `"scala.math.BigDecimal"`, `"org.joda.time.LocalTime"`). The core Predefs set it (`Predef(`string`[java.util.Date](...).withScalaType("java.util.Date"))`),
   and so do the joda-time Schemas. The macros don't change: they already inline `p.schema` (Scala 2 `FromPredef(q"$x.schema")`,
   Scala 3 the same), so the attribute reaches every position (fields, collection elements, map values, `Option`, value classes,
   definition bodies, family members) on both versions. `duplicate`, `withValidation`, `withTitle`, ... keep it. It is **not**
   part of `equals`, `hashCode` or `toString`, so trees and tree specs don't change.
2. **The knob.** `JsonSchemaProfile.typeSchemas: Map[String, Schema[_] => Schema[_]]`, empty for Neutral and Legacy. When
   lowering meets a node with `scalaType = Some(t)` and `typeSchemas` has `t`, the node is replaced by `f(node)`. Then the
   lowering strips `scalaType` from every node it passes, replaced or not (so `withProfile(p)` stays final and lowering stays
   idempotent, as for `option`/`family`).

```scala
// the bridges
PlayProfile:     "java.util.Date" | "java.sql.Date" | "java.sql.Timestamp" | "java.sql.Time" -> _ => `integer`
UPickleProfile:  "scala.Long"            -> n => oneOf(n, string{pattern ^-?[0-9]+$})
                 "scala.math.BigInt"     -> _ => string{pattern ^-?[0-9]+$}
                 "scala.math.BigDecimal" -> _ => string{pattern ^-?[0-9]+(?:\.[0-9]+)?(?:E[+-]?[0-9]+)?$}
Json4sProfile:   what the Formats writes, by encoding a sample of each type (below)

// a user: "my codec writes Instant as epoch millis"
implicit val profile: JsonSchemaProfile = CirceProfile.default.withTypeSchema[java.time.Instant](_ => Schema.`integer`)
```

Sample output, `case class Event(at: java.util.Date, n: Long, price: BigDecimal)`, draft-07:

| profile | `at` | `n` | `price` |
|---|---|---|---|
| Neutral, Legacy, circe, spray | `{"type":"string","format":"date-time"}` (as today) | `number` (`integer` after D2) | `number` |
| play | `{"type":"integer"}` | as Neutral | as Neutral |
| upickle | as Neutral | `{"oneOf":[{"type":"number"},{"type":"string","pattern":"^-?[0-9]+$"}]}` (`integer` inside, after D2) | `{"type":"string","pattern":"^-?[0-9]+(?:\\.[0-9]+)?(?:E[+-]?[0-9]+)?$"}` |

Pros: it reuses the 0.9 model (the derivation records a fact, the profile decides at render), so one derived schema renders
right for every library, `withProfile` and the implicit profile work as they do, and it doesn't matter where the schema was
derived (`derives json.Schema` on Scala 3 included). No macro change, no new node, no tree change, so Neutral and Legacy are
byte-exact by construction. It is open: modules (joda-time) and users' own Predefs can record a type, and users can re-shape any
recorded type for their own codecs. Json4sProfile can follow the runtime `Formats`, which no compile-time option can.

Cons: a hidden attribute that equality ignores is a little magic (two trees that differ only by it compare equal). Only the
schemas of a Predef that records its type can be re-shaped. A hand-written `` `string`[java.util.Date](...) `` or a refined
type's schema (the refined macro builds fresh `integer`/`number`/`string` nodes) isn't, until it records it. String keys can be
mistyped. Mitigation: users use the typed builder `withTypeSchema[T]`, which reads the key from `Predef[T]` and fails if that
Predef records none, and a test pins every core Predef's key.

### B. A new tree node `typed(scalaType, schema)` instead of an attribute  — LOST

The same mechanism, but the fact is a visible node wrapping the Predef's schema (like `option(inner)`), lowered away by every
profile. Pros: explicit in `toString`/`toDebugString`, can't be lost by a copy that forgets an attribute. Cons: every tree
with a `Long`, `String`, `Int`, `UUID`, `Instant`, ... changes, so hundreds of tree specs and every user who matches on derived
trees (`case `number`() =>`) break. A new case in the sealed `Schema` means non-exhaustive matches for users and for each
internal match (`AsDraftSupport`, `AsTree`, `Families.as07`, the parser, `jsonType` of `enum(tpe)` / `value-class(tpe)` that
would hit the wrapper). That's where the `def.jsonType = ???` class of bugs comes from. It's breaking, for no gain over A
in what it can express. Lost on cost and risk.

### C. Bridge Predefs that come with `profiles.<lib>._`  — LOST

`profiles.play._` adds `implicit val playJuDate: Predef[java.util.Date] = Predef(`integer`)` etc. An imported implicit is in
lexical scope, so it beats the core Predef in the implicit scope of `Predef` (checked by c-d1, 2.13 and 3.3). No core change.
Cons: it acts where the schema is **derived**, not where it's rendered, against the 0.9 rule. A schema derived elsewhere (a
`derives json.Schema` class, another module, an `implicit val` in a companion) keeps the core shape, and a schema derived next
to the play import says `integer` even when rendered for circe. `AsPlay._` users get nothing. json4s' shape depends on the
runtime `Formats`, which a Predef can only see if a `Formats` is in implicit scope at the derivation site. upickle's
`Long` → `oneOf` would also change the *tree* (tree specs, `Json.sig`). Lost.

### D. Keep the output; document the mismatch and the local-Predef workaround  — LOST

README/paradox gain a "types a library writes differently" table and the `implicit Predef[T]` override. Zero risk. Cons: the
profiles stay wrong for real values of common types, which is the bug. The workaround has C's problems, and the user has to
know the shapes. Kept as the fallback if the owner rejects A.

(A variant of A that matches tree shapes instead of recording the type was rejected outright: it can't tell
`java.util.Date` from `Instant`, or `Long` from `Double`.)

### Sub-decisions inside A (each easy to flip)

| # | question | pick | alternatives |
|---|---|---|---|
| A1 | which Predefs record their type | **every leaf Predef** in core and joda-time (`String`, numbers, `Boolean`, `Char`, `UUID`, `URI`, java.time, java.util/sql dates, durations; E3/E4/E5's new ones too). Collections don't, since their shape is structural | only the types some bridge re-shapes today (smaller, but users can't re-shape `Instant` for their own codec) |
| A2 | key format | the type's full Scala name as a `String` (`scala.Long`, `java.lang.String`) | `Class[_]` (no `classOf[java.sql.Date]` on Scala.js); a closed ADT (not open to modules/users) |
| A3 | upickle `Long` | `oneOf[number→integer, string ^-?[0-9]+$]` | string only (rejects small ones, which upickle writes as numbers); a tighter `^-?[0-9]{16,19}$` (upickle's reader accepts any numeric string anyway); keep |
| A4 | upickle (and circe) `NaN`/`±Infinity` | **not modelled**: every `Double` field becoming `oneOf[number, enum]` would make schema-driven codegen emit `Double | String` unions, for values that are usually bugs. Users opt in with `withTypeSchema[Double]` | model it in `UPickleProfile` (circe writes `null` for `NaN`, a `oneOf[number, null]`) |
| A5 | json4s: how the profile learns the shapes | **encode one sample per recorded type with the `Formats`** at `fromFormats` and map the result: `JString` → keep; `JInt`/`JLong` → `integer`; `JDouble`/`JDecimal` → `number`; a non-empty `JObject` of integers → an object with those fields, `integer`, all required. Anything else (`{}`, an exception such as JDK 17+ reflection) → keep | a fixed list keyed by serializer class: doesn't work, json4s-ext registers `LocalDate`, `LocalTime`, `Period`, ... as anonymous `ClassSerializer`s |
| A6 | json4s `java.sql.Date` → `{}` | keep the `string` schema: `{}` is reflection debris that json4s can't read back, not an encoding | describe it (`{"type":"object"}`) |
| A7 | a replacement's texts and validations | the replacement gets the original's `title`/`description` where it has none. `f` receives the original (validations included) and decides: upickle `Long` keeps them on its number member, a string replacement drops numeric ones | copy validations blindly (a `minimum` on a string) |
| A8 | a replacement is lowered | yes (its `option`/`family`/D2 `number`s) but **not re-shaped** again: no loops, one pass | re-shape to a fixed point |
| A9 | enum and singleton value schemas (`@typeHint[Long]`) | **not re-shaped** (values are fixed; re-shaping would give `enum(oneOf(...))`) | re-shape |

### K. Map keys (play's array of pairs)  — phase 2 of A

play writes a `Map[K, V]` as an object only when `K` has a `KeyWrites`, which play has for `String` and every `AnyVal`. Any other
key gives an array of `[key, value]` pairs, each key written by `Writes[K]`. To render that, the dictionary node needs the key's
schema, and the macros have to provide it.

- **K1 (WON, phase 2):** the `dictionary` node gets a `keySchema: Option[Schema[_]]` attribute (same rules as `scalaType`: kept by
  copies, outside `equals`). It's set for `String` keys (the `String` Predef's schema), for keys with a built-in `KeyPattern`
  (a new `KeyPattern#keySchema`, the type's Predef schema) and for enum keys (the enum's derived schema). Value-class keys and
  user `KeyPattern`s get none. A new knob `mapStyle: MapStyle`: `MapStyle.Object` (default, today) or
  `MapStyle.Pairs(asObject: Schema[_] => Boolean)`. Under `Pairs`, a dictionary with a key schema that isn't `asObject` lowers to
  `array(array(anyOf[K', V']) {minItems 2, maxItems 2})`, with `K'`/`V'` lowered by the profile (a play enum key becomes its
  `{"_type": ...}` object). Play: `Pairs(k => k.scalaType exists PlayKeyWrites)`, where `PlayKeyWrites` is `java.lang.String`
  plus the nine `AnyVal` primitives. The exact `items: [K, V]` form needs a positional-array node, which is BACKLOG 8 (tuples as
  arrays): `anyOf` accepts every pair play writes and also `[v, k]`, and switches to the positional form when that node exists.
- **K2:** a profile flag "non-String keys are pairs" with no key schema: the pair's key element would be `{}`, and enum keys
  couldn't be told from `Int` keys. Lost.
- **K3:** leave maps as they are and document play's behaviour (BACKLOG 16 today). The fallback if the owner wants phase 1 only.

Phase 2 is separable: it touches both macros and `KeyPattern`, depends on B6 (#479: the UUID/Boolean `KeyPattern`s) and B3
(#470: the `anyof` node), and fixes play only.

## Decision

Option A. Core leaf Predefs (and joda-time's Schemas) record their Scala type as a schema attribute that equality ignores. A new
profile knob `typeSchemas` maps a type to the schema its library writes, applied at lowering. Play re-shapes `java.util.Date`
and the `java.sql` dates, upickle re-shapes `Long`/`BigInt`/`BigDecimal`, and json4s re-shapes whatever its `Formats` writes as
a number or an object. Neutral, Legacy, circe and spray output doesn't change. Map keys (K1) follow as phase 2, after B6 and B3.

## Implementation plan

### Phase 1: the type fact and the `typeSchemas` knob (core + bridges, no macro change)

1. `core/src/main/scala/json/Schema.scala`, trait `Schema`:
   - `private var _scalaType: Option[String] = None`; `def scalaType: Option[String] = _scalaType`, with scaladoc: "the
     Scala type this schema stands for, recorded by its `Predef` (full name, e.g. `java.util.Date`, `scala.Long`) so that a
     `JsonSchemaProfile` can give it the shape its library writes (`typeSchemas`). Not part of equality."
   - `def withScalaType(x: String): Self` (copy with it set) and `private[json] def withoutScalaType: Self`.
   - `duplicate(...)` and `withExtraFrom(x)` copy `_scalaType` (both mean "this node, rebuilt"). `withValidation` goes through
     `duplicate`, so it keeps it. `withValidationsAddedFrom` starts from `mkCopy()` and drops everything (B13, #498): if B13 has
     merged, follow its fix; otherwise leave it.
   - `equals`, `hashCode`, `ToString` unchanged (the doc comment on `equals` says why).
   - `` `value-class` ``/`` `def` `` `toDefinition` overrides: check that they copy through `withExtraFrom`. A definition body
     keeps its own tag (the tag is on the leaf, not on the `def`).
2. `core/src/main/scala/json/schema/Predef.scala`: every leaf Predef records its type, using a private helper
   `private def leaf[T](name: String, s: Schema[T]): Predef[T] = Predef(s.withScalaType(name))`. One name per `implicit val`
   (0.9.0 has 26): `strS` `java.lang.String`, `chrS` `scala.Char`, `jchrS` `java.lang.Character`, `boolS` `scala.Boolean`,
   `byteS` `scala.Byte`, `shortS` `scala.Short`, `intS` `scala.Int`, `doubleS` `scala.Double`, `floatS` `scala.Float`,
   `longS` `scala.Long`, `bigIntS` `scala.math.BigInt`, `bigDecimalS` `scala.math.BigDecimal`, `uuidS` `java.util.UUID`,
   `uriS` `java.net.URI`, `urlS` `java.net.URL`, `juDateS` `java.util.Date`, `jsqlTimestampS` `java.sql.Timestamp`,
   `instantS` `java.time.Instant`, `localDateTimeS` `java.time.LocalDateTime`, `zonedDateTimeS` `java.time.ZonedDateTime`,
   `jsqlDateS` `java.sql.Date`, `localDateS` `java.time.LocalDate`, `jsqlTimeS` `java.sql.Time`, `localTimeS`
   `java.time.LocalTime`, `jDurationS` `java.time.Duration`, `durationS` `scala.concurrent.duration.Duration`.
   `boolS` and `intS` wrap the singleton objects `` `boolean` `` / `` `integer` ``: `withScalaType` copies (through `mkCopy`),
   it never mutates a shared instance, and `case `integer` =>` still matches the copy (equality ignores the tag).
   Collections (`arrayS`, `setS`, `listS`, `vectorS`, `dictS`) record nothing. E3 (#474), E4 (#473),
   E5 (#475) and D1 (#490) touch the same vals: whichever lands second adds/keeps the `leaf(...)` call (textual conflict only).
   The boxed `java.lang.*` Predefs of E4 record their own names (`java.lang.Long`), not the primitive's.
3. `modules/joda-time/.../JodaTimeSupport.scala`: the six Schemas record `org.joda.time.{DateTime, Instant, LocalDateTime,
   LocalDate, LocalTime, Duration}`. They are implicit Schemas, so the tag ends up inside the definition body, and lowering
   re-shapes the body.
4. `core/src/main/scala/json/schema/JsonSchemaProfile.scala`:
   - trait: `def typeSchemas: Map[String, Schema[_] => Schema[_]] = Map.empty`, with scaladoc (what it is, that keys are
     `Schema.scalaType` names, that `f` gets the node with its validations and texts, A7/A8/A9, an example).
   - builders: `final def withTypeSchemas(x: Map[String, Schema[_] => Schema[_]]): JsonSchemaProfile` (replace all; `Map.empty`
     drops a bridge's) and `final def withTypeSchema[T](f: Schema[_] => Schema[_])(implicit p: Predef[T]): JsonSchemaProfile`
     (adds/replaces one, keyed by `p.schema.scalaType`; `IllegalArgumentException` naming the type if that Predef records none).
   - `Knobs`: field `typeSchemas: Map[...] = Map.empty`, `override val` in `profile`, copied in `Knobs.apply(p)`. The trait
     scaladoc's "Everything else ... passes through unchanged" and `lower`'s "comes back equal" get the type-schema case.
     (D2 #491 and E8 #485 add knobs on the same lines: textual conflicts.)
   - `Lowering`: a `reshape: Boolean = true` constructor flag. `apply(s)` first does
     ```scala
     s.scalaType match {
       case Some(t) =>
         val plain = s.withoutScalaType
         p.typeSchemas.get(t) match {
           case Some(f) if reshape => new Lowering(p, reshape = false)(texts(f(plain), plain))   // A7, A8
           case _                  => node(plain)
         }
       case None => node(s)
     }
     ```
     where `node` is today's match, `texts(r, o) = r.duplicate(title = r.title orElse o.title, description = r.description orElse o.description)`
     and `reshape = false` is passed on recursively (the replacement isn't re-shaped, but its nested nodes are lowered
     and stripped). `case other => other` strips the tag of untouched leaves. `enum` keeps its value schema as it is, only
     stripped (A9). In `familyAt`, `singletonSchema` uses `apply` with `reshape = false` (a singleton's value schema is a merge
     key, A9). Keep D2's `integralStyle` working on the replacement: it is lowered by the same `Lowering`, and D2's
     `number` case comes after the tag step.
   - The renderers' fallback `Neutral.lower` (`AsDraftSupport.lowered`) finds nothing left (tags stripped, Neutral has no
     type schemas).
5. Bridges (each `fromX` adds its map with `withTypeSchemas`, keyed by the names of step 2):
   - `PlayProfile.fromConfig`: `java.util.Date`, `java.sql.Date`, `java.sql.Timestamp`, `java.sql.Time` → `` _ => `integer` ``
     (scaladoc: "epoch millis, as play's `DefaultDateWrites`"). Shared JVM/JS source: string keys only, no `classOf[java.sql.*]`.
   - `UPickleProfile.fromApi`: `scala.Long` → `n => oneof(Set(n, string+pattern ^-?[0-9]+$))`; `scala.math.BigInt` →
     `string`+`^-?[0-9]+$`; `scala.math.BigDecimal` → `string`+`^-?[0-9]+(?:\.[0-9]+)?(?:E[+-]?[0-9]+)?$` (matches `toString`:
     `1.5`, `-0.5`, `1E+10`, `1E-7`, `1.23E+5`). Patterns use `[0-9]` (ECMA 262 and Java, as in D1/E3). Scaladoc: "ujson writes a
     `Long` outside ±2^53 as a string (`BaseCharRenderer.visitInt64`), and `BigInt`/`BigDecimal` always". Same for every
     `AttributeTagged` API (it's ujson's renderer, not configuration).
   - `Json4sProfile.fromFormats(f)`: a private table of name → sample (java.time `Instant`, `LocalDate`, `LocalTime`,
     `LocalDateTime`, `ZonedDateTime`, `Duration`; `java.util.Date`, `java.sql.*`; joda `Instant`, `LocalDate`, `LocalTime`,
     `LocalDateTime`, `DateTime`, `Duration` built reflectively by class name, since json4s-json doesn't depend on joda-time
     (JVM-only module: reflection is allowed); E3's `Year`/`Period`/`YearMonth`/`MonthDay` once they have Predefs).
     `Extraction.decompose(sample)(f)` in a `Try`, mapped by A5. The fields of an object replacement come in the sample's
     order, `integer`, required, the object strict like a derived one. Computed once per `fromFormats`, so `default` costs
     one pass at class init.
   - `CirceProfile`, `SprayProfile`: no change (scaladoc: circe writes `NaN` as `null`, not modelled, A4).
6. Tests (new files; all shared unless noted):
   - `core/src/test/scala/json/schema/TypeSchemaLoweringSpec.scala`: the tag survives `duplicate`/`withValidation`/`withTitle`/
     `toDefinition`; `equals`/`hashCode` ignore it; a profile with a type schema re-shapes it at the root, in a field, an
     `Option` (Nullable and Legacy), an array, a map value, a value class, a definition body, a family member; texts kept
     (A7); the replacement is lowered (an `option` inside it) but not re-shaped (A8, a self-referencing `f` terminates);
     `lower` is idempotent; `withProfile(p)` then a render under another profile keeps `p`'s shape; an `@typeHint[Long]`
     enum isn't re-shaped (A9); Neutral and Legacy lower every tagged leaf to an equal, untagged one.
   - `core/src/test/scala/json/schema/PredefScalaTypeSpec.scala`: every leaf Predef records the expected name (a table of the
     step-2 list), and collection Predefs record none. JVM + JS. `withTypeSchema[T]` throws for a Predef without a tag.
   - api: `Json.schema[case class with Long, Date, BigDecimal fields]` tree specs unchanged (equality ignores the tag); the
     golden sets (`golden/`, `golden-legacy/`) are unchanged. Run `GOLDEN_UPDATE=1 sbt clean api/test` and expect no diff.
   - Acceptance (real codec output validated under the profile, all drafts, the `Acceptance` helpers):
     play (`modules/play-json/src/test/scala/petshop/`): `java.util.Date`, `java.sql.Date`, `java.sql.Timestamp` fields;
     upickle: `Long` (42, 2^53, 2^53+1, `Long.MinValue`), `BigInt`, `BigDecimal` (`1.5`, `1E+10`), `List[Long]`, `Option[Long]`;
     json4s (Scala 2 dir, like `Json4sAcceptanceSpec`): a model with java.time and joda fields under
     `DefaultFormats ++ JavaTimeSerializers.all ++ JodaTimeSerializers.all`, plus `Json4sProfile.default` unchanged for them.
     Each case also asserts the rendered JSON (old → new) so the output change is pinned. JS: the validating tests are
     canceled there (as today), and the rendering asserts still run.
   - `build.sbt`: `"org.json4s" %% "json4s-ext" % "4.0.7" % Test` and `joda-time` `% Test` for json4s-json.
7. Docs: README "Library profiles" and `docs/src/main/paradox/rendering.md` (same words): a "types a library writes differently"
   table (the rows of this doc's Problem table with the new schemas), a paragraph on `withTypeSchema` (the `Instant` as
   millis example, plain `scala` or `mdoc:compile-only`), and the scaladoc of `Schema.scalaType`. `types.md`: one line that
   built-in types record their type for profiles. No CHANGELOG (the lead's).

### Phase 2: map keys under play (K1; after B6 #479 and B3 #470)

8. `Schema.scala`: `` `dictionary` `` gets `private var _keySchema: Option[Schema[_]]`, `keySchema`, `withKeySchema(x: Option[Schema[_]])`.
   It is copied in `mkCopy` and is not part of equality. `KeyPattern` gets `def keySchema: Option[Schema[_]] = None`. Built-ins
   override it with their Predef's schema (`def`, not `val`, to avoid an init cycle between `Schema` and `Predef`). `mk`/`forEnum`
   keep `None`.
9. `Predef.dictS`: `.withKeySchema(keyP.keySchema)` (String keys: the String Predef's schema).
10. Scala 2 (`macros/.../UDictionaries.scala`, `SchemaTypes.Dict`): `Dict` gets `keySchema: Option[Tree]`. String key →
    `Predef.strS.schema`. A `KeyPattern` found → `$kp.keySchema` (an `Option`). Enum key → the enum's IR tree (`Enum(...)` already
    has it). Value class → `None`. `prefix` appends `.withKeySchema(...)`. Scala 3 (`derivation/Collections.scala`): `Dict.emitRaw`
    does the same from `DictKey`. `KeyKind.Str`/`JavaStr` → the String Predef, a `KeyPattern` → `kp.keySchema`, `StringEnum` →
    derive the key type's node (the enum derivation already exists), `ValueClassOf`/`Custom` → `None`. **`clean`** after the
    macro edits.
11. `JsonSchemaProfile`: `sealed trait MapStyle`, `MapStyle.Object` (default), `final case class MapStyle.Pairs(asObject: Schema[_] => Boolean)`,
    knob `mapStyle`, builder `withMapStyle`, `Knobs` field. In `Lowering`, the dictionary case keeps `keySchema` (lowered) on
    the rebuilt node. Under `Pairs`, when `keySchema` is defined and isn't `asObject`, it becomes
    `` `array`(`array`(`anyof`(Set(apply(k), apply(v)))).withValidation(minItems := 2, maxItems := 2)) `` with the
    dictionary's texts. Then strip `keySchema` (final, idempotent).
12. `PlayProfile`: `withMapStyle(MapStyle.Pairs(k => k.scalaType exists PlayKeyWrites))` (decide on the tag *before*
    lowering strips it: `asObject` sees the key schema as derived).
13. Tests: a lowering spec for `MapStyle` (both styles, nested maps, a map in an `Option`, idempotency, `keySchema` kept by
    copies); macro tests on 2.12/2.13/3: `Map[UUID, V]`, `Map[Color, V]` (case objects and a Scala 3 `enum`), `Map[String, V]`,
    `Map[Id, V]` record the expected key schema, and trees still compare equal to today's. Play acceptance:
    `Map[UUID, Int]`, `Map[LocalDate, Int]` (with B6/E3's key patterns), `Map[Color, Int]` with play's derived `Writes`, and
    `Map[Int, Int]` / `Map[String, Int]` staying objects.
14. Docs: the play row of the table gains "maps with non-`String`, non-`AnyVal` keys: an array of pairs". Remove BACKLOG 16 (the
    lead's file).

## Compatibility

- **Output:** changes only under the bridge profiles, and only for these types: play `java.util.Date`/`java.sql.*` (`string` →
  `integer`), upickle `Long` (→ `oneOf`), `BigInt`/`BigDecimal` (`number` → patterned `string`), json4s with json4s-ext
  serializers (`string` → `integer` / an object, per `Formats`), and in phase 2 play maps with non-`String`/`AnyVal` keys
  (`object` → an array of pairs). Neutral, Legacy, circe and spray: byte-exact. No golden file changes (goldens render under
  Neutral and Legacy). A user-written `Predef`/`Schema` for these types is untouched (it records no type). The new schemas
  accept every value the library writes. What they now reject is what the library never writes: a date string under play,
  a JSON number for a `BigInt`/`BigDecimal` under upickle (upickle's own reader rejects it too).
- **Source:** additive: `Schema.scalaType`/`withScalaType`, `JsonSchemaProfile.typeSchemas`/`withTypeSchemas`/`withTypeSchema`,
  phase 2 `dictionary.keySchema`, `KeyPattern#keySchema`, `MapStyle`. Trees are equal to today's (the attribute is outside
  equality), so tree specs and pattern matches don't change.
- **Binary:** new members on the sealed `Schema` trait (a field plus methods: all implementors are in core, compiled
  together), on `JsonSchemaProfile` (methods with bodies: interface default methods on 2.12+/3, so profiles compiled against
  0.9.0 still link), on `KeyPattern` (an abstract class: a concrete method). No signature changes. (No MiMa in the build.)
- **PVP:** a change of a library profile's output. B3 (#470) and D2 (#491) treat a default-output change as breaking (0.10.0).
  This changes no default (Neutral) output, only what the bridge profiles render, and each change makes a schema accept what
  the library writes. So it's a bug fix in 0.9.x unless the owner wants it bundled into 0.10.0 with the other output changes.
  Phase 2 likewise.
- **Legacy:** unaffected by construction (no type schemas, no map style). The lead's Legacy policy doesn't apply: Legacy isn't a
  library profile, and its 0.7 output is right for 0.7's own users.
- **Core rule:** unchanged. Predefs are still inlined, Schemas still become definitions. The tag is data on the inlined node.

## Risks and neighbours (not in scope)

- refined builds its own nodes (Scala 2 `refined/AST.scala`, Scala 3 `RefinedMacro.scala:282`: `integer` for `Int` *and*
  `Long`), so `Long Refined Positive` isn't re-shaped under upickle. A follow-up can tag the refined base schema. Noted for
  the lead.
- play writes value-class keys (`Map[Id, V]`, `Id extends AnyVal`) through `anyValKeyWrites`, i.e. `toString`: `{"Id(a)": 1}`.
  The schema accepts it (`^.*$`), but the key isn't the value. That's play's quirk. Phase 2 leaves value-class keys as objects,
  which is what play writes.
- json4s-ext `JLocalTimeSerializer` writes nanos in its `millis` field (`500000000`). The schema says `integer`, which is right.
  An upstream oddity.
- json4s without json4s-ext can't write `UUID`/`LocalTime`/`Duration` on JDK 17+ (reflection, `InaccessibleObjectException`):
  nothing to describe.
- json4s-ext writes `LocalDateTime`/`ZonedDateTime`/`OffsetDateTime` as UTC strings with `Z`. D1 (#490) owns their patterns.
- D2 (#491), E7 (Either style), E8 (#485) and D4 (field naming) also add knobs to `Knobs`: textual conflicts only.
- B6 (#479) rewrites the `KeyPattern` block that phase 2 extends, and adds the UUID/Boolean `KeyPattern`s phase 2 needs.

## Review log
