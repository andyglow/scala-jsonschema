# java.time Predefs and FiniteDuration  (issue #474, item E3; also #305 question 3)
Status: draft

## Problem
These types have no built-in schema, so a model that uses them doesn't compile:
`java.time.OffsetDateTime`, `OffsetTime`, `Period`, `Year`, `YearMonth`, `MonthDay`, `ZoneOffset`, `ZoneId` and
`scala.concurrent.duration.FiniteDuration`.

```scala
case class Timeouts(connect: FiniteDuration)
Json.schema[OffsetDateTime]; Json.schema[ZoneId]; Json.schema[FiniteDuration]; Json.schema[Timeouts]
```

Actual, the same on 2.12.21, 2.13.18 and 3.3.9 (both macros), JVM and JS:

```
schema for java.time.OffsetDateTime is not supported,
schema for java.time.ZoneId is not supported,
schema for scala.concurrent.duration.FiniteDuration is not supported,
schema for scala.concurrent.duration.FiniteDuration is not supported, ...JavaTimeSchemaSpec.Timeouts
```

(and the same for the other five). `FiniteDuration` fails although `Duration` has a `Predef`: `Predef[T]` is invariant,
so `Predef[Duration]` doesn't cover the subtype, also as a field. `ZoneId` is abstract with two subclasses
(`ZoneOffset`, `ZoneRegion`), so without a Predef the macro would try (and fail) to treat it as a family.

The repro is `api/src/test/scala/com/github/andyglow/jsonschema/JavaTimeSchemaSpec.scala` (it states the target
behavior, so on master it doesn't compile).

## Why it is a problem
`OffsetDateTime` is the most common `java.time` type in APIs after `Instant`; `FiniteDuration` is the usual Scala
type for timeouts and intervals (#305, and the closed #11). Users have to write a `Predef` per type, and get the
format wrong easily (see "What the codecs write" below). The README's "Supported types" lists `java.time` as
supported "out of the box".

## What the codecs write
The schema has to accept what a JSON library really writes. Measured on 2.13 (circe 0.14.17, play-json 3.0.6,
upickle 4.4.3; `toString` is what a hand-written codec usually uses):

| type | `toString` | circe | play-json | upickle |
|---|---|---|---|---|
| OffsetDateTime | `2024-01-01T10:00+02:00` | `"2024-01-01T10:00:00+02:00"` | same as circe | no codec |
| OffsetTime | `10:15Z` | `"10:15:00Z"` | no codec | no codec |
| Period | `P1Y-2M3D` (`P0D`, weeks as days) | `"P1Y-2M3D"` | `"P1Y-2M3D"` | no codec |
| Year | `5`, `12345` | `"0005"`, `"+12345"` | no codec | no codec |
| YearMonth | `2024-03`, `-0005-01`, `+12345-12` | same | no codec | no codec |
| MonthDay | `--12-03` | same | no codec | no codec |
| ZoneOffset | `Z`, `+01:00`, `+01:00:30` | same | (ZoneId writes) | no codec |
| ZoneId | `Europe/Paris`, `UTC+01:00`, `Z` | same | same | no codec |
| FiniteDuration | `1500 milliseconds` | no codec | no codec | `"1500000000"` (nanos) |

How networknt treats the candidate formats (it asserts `format` on draft-04/07 and treats it as an annotation on
2020-12):

| text | `date-time` | `time` | `duration` |
|---|---|---|---|
| `2024-01-01T10:00:00+02:00` / `10:15:00Z` | ok | ok | |
| `2024-01-01T10:00+02:00` / `10:15Z` (`toString`, no seconds) | rejected | rejected | |
| `2024-01-01T10:00:00` / `10:15:30` (no offset) | rejected | rejected | |
| `P1Y2M3D`, `P0D`, `P14D`, `P2W`, `PT1S` | | | ok |
| `P1Y-2M3D`, `-P1Y` (signed), `PT1.5S` (fraction) | | | **rejected** |

So `format: duration` is wrong for `Period`: RFC 3339's duration grammar has no signs, and a negative component is a
normal `Period` value that circe and play write.

## Options
### A. Core Predefs; `format` where the format matches the text, else a `pattern` of the written form — WON
Add to `json.schema.Predef` (core, shared by both macros, both platforms):

| type | schema | rendered (every draft unless noted) |
|---|---|---|
| OffsetDateTime | `string(date-time)` | `{"type":"string","format":"date-time"}` |
| OffsetTime | `string(time)` | `{"type":"string","format":"time"}` |
| FiniteDuration | `string(duration)`, the same as `Duration` | `{"type":"string"}`; `+ "format":"duration"` from 2019-09 |
| Period | `string` + `pattern ^P(?!$)(?:-?[0-9]+Y)?(?:-?[0-9]+M)?(?:-?[0-9]+D)?$` | `{"type":"string","pattern":...}` |
| Year | `string` + `pattern ^[-+]?[0-9]+$` | ditto |
| YearMonth | `string` + `pattern ^[-+]?[0-9]{4,}-(?:0[1-9]\|1[0-2])$` | ditto |
| MonthDay | `string` + `pattern ^--(?:0[1-9]\|1[0-2])-(?:0[1-9]\|[12][0-9]\|3[01])$` | ditto |
| ZoneOffset | `string` + `pattern ^(?:Z\|[-+][0-9]{2}:[0-9]{2}(?::[0-9]{2})?)$` | ditto |
| ZoneId | `string` | `{"type":"string"}` |

- The patterns accept exactly what `toString` and the codecs above write (the "written form"), not every text
  `parse` accepts. `Year` takes both the unpadded `toString` (`5`) and the padded ISO text (`0005`, `+12345`).
  `[0-9]` instead of `\d` (ECMA 262 vs Java regex Unicode classes), as the KeyPatterns do. `(?!$)` keeps out a bare `P`.
- `OffsetDateTime`/`OffsetTime` carry an offset, so RFC 3339 `date-time`/`time` fit the ISO text the codecs write.
  (`toString` drops `:00` seconds, which the formats reject; the codecs use the ISO formatters, which don't. Same as
  the existing `ZonedDateTime`/`Instant` Predefs.)
- `FiniteDuration` gets the `Duration` schema: a model with a `Duration` and a `FiniteDuration` field describes both
  the same way. No codec writes ISO 8601 for either, but that is the existing `Duration` choice, not this item's.
- `ZoneId` is a plain string: a region id is any `Area/City[/...]` name, `Z`, an offset, or `UTC`/`GMT`/`UT` with an
  offset; a pattern loose enough for all of these validates nothing. Being a Predef, it also wins over the sum-type
  derivation the macros would otherwise attempt.
- To attach `pattern` to a `string[Period]` (etc.), core needs `validation.Magnet[Period, String]` for the five
  patterned types, like the existing `uuidMagnet`/`chrMagnet`. They are public implicits in `object Magnet`, so users
  can add validations to these schemas too (`Json.schema[Year].withValidation(maxLength := 4)`).

Pros: every new schema accepts every value the codecs write and rejects most junk, on every draft. No change to any
existing type or output. Cons: the patterns are a small maintenance surface; a hand-written `"P2W"` (which
`Period.parse` accepts) is rejected.

### B. Formats only, plain strings elsewhere — LOST
`Period` → `format: duration` (as `java.time.Duration`), `Year`/`YearMonth`/`MonthDay`/`ZoneOffset`/`ZoneId` → plain
`string`. Simpler and "more uniform", but `Period`'s schema rejects negative periods under any validator that
asserts formats (networknt does on the drafts where it asserts), and the plain strings validate nothing (`"x"` is a
valid `YearMonth`). Lost on correctness.

### C. Parse-compatible patterns — LOST
Patterns that accept everything `Period.parse`/`ZoneOffset.of`/`YearMonth.parse` accept (`-P1Y`, `P2W`, lowercase
`p1y`, `+01`, `+0100`, ...). Friendlier to hand-written input, but the patterns get much longer, and what a decoder
accepts differs per codec, while what encoders write doesn't. Easy to switch to later (patterns only widen).

### D. FiniteDuration via variance or a generic Predef — LOST
`Predef[-T]`, or `implicit def durationSubS[D <: Duration]: Predef[D]`. Contravariance changes implicit resolution for
every Predef (e.g. a `Predef[Any]` would apply everywhere) and is binary-incompatible; the generic def is ambiguous with
`durationS` at `D = Duration` unless it gets its own priority level, and only buys `Duration.Infinite`, which has no
standard text. One more `implicit val` is simpler.

### Per-type alternatives (easy to choose instead)
- `Year` as `integer` (Jackson's `JavaTimeModule` writes a number) instead of a string: no bridged library writes
  that, circe writes a string. A user who needs it declares their own `implicit val y: Predef[Year]`, which wins
  over the built-in one (see Compatibility).
- `ZoneOffset` as plain `string` (as tapir does) or `ZoneId` with a loose pattern: see A.
- `Period` with both `format: duration` and the pattern: the format would still reject negative periods. No.

## Decision
A: nine new core Predefs (two `date-time`/`time` formats, `FiniteDuration` = `Duration`, five written-form patterns,
`ZoneId` plain) and five String Magnets. Additive only; `LocalDateTime`/`LocalTime` stay as they are (item D1).

## Implementation plan
1. `core/src/main/scala/json/schema/validation/Magnet.scala`: `periodMagnet`, `yearMagnet`, `yearMonthMagnet`,
   `monthDayMagnet`, `zoneOffsetMagnet` (`Magnet[T, String]`), next to `uuidMagnet`.
2. `core/src/main/scala/json/schema/Predef.scala`: `offsetDateTimeS`, `offsetTimeS`, `periodS`, `yearS`, `yearMonthS`,
   `monthDayS`, `zoneOffsetS`, `zoneIdS`, `finiteDurationS`, next to the existing java.time ones. The patterns as
   private vals in `object Predef` (core's `Constants` is `private[jsonschema]`, not visible from `json.schema`).
   Nothing in either macro changes: both find Predefs by implicit search (rule 2 of the core design rule).
3. Tests: `api/.../JavaTimeSchemaSpec.scala` (committed with this doc): trees (also as fields, so inlined, never a
   definition), rendering on all five drafts under Neutral and Legacy, networknt validation of the written forms and of
   junk (patterns on every draft; formats on draft-04/07), and a check that the sample texts are what java.time writes
   (runs on JS too, against scala-java-time, which `jsSettings` already has as a test dependency). To add during
   implementation: a local `implicit Predef[Year]` and a local `implicit Schema[OffsetDateTime]` still win over the
   built-ins (the Compatibility claim), on both Scala versions.
4. Golden files: none change (no golden model has these types; they didn't compile). `golden-legacy`: none.
5. Scala.js: the Predefs only name the types (`string[T]` erases `T`), so the core links without a java.time
   implementation, like the existing java.time Predefs. No `scala-js` source needed.
6. Docs: README "Supported types" table and `docs/src/main/paradox/types.md` (same table): add `OffsetDateTime` to the
   `date-time` row, `OffsetTime` to the `time` row, `FiniteDuration` to the `duration` row, a row "`Period`, `Year`,
   `YearMonth`, `MonthDay`, `ZoneOffset` → `string` with a pattern of their ISO text", a row "`ZoneId` → `string`".
   Run `makeDocs`.
7. Gates: `clean test` on 2.12/2.13/3.3.9, `rootJS/test` on all three (core is touched), `scalafmtCheckAll` and
   `rootJS/scalafmtCheckAll`.

## Compatibility
Additive: new implicit vals in `object Predef` and `object Magnet`.
- Source: a user's own `implicit Predef[OffsetDateTime]` (etc.), defined or imported in lexical scope, keeps winning:
  the companion `object Predef` is implicit scope, searched only when lexical scope has no candidate (both Scala 2
  and 3). A user's implicit `Schema[OffsetDateTime]` wins too (rule 1 is looked up before rule 2). So no existing
  user code changes output or becomes ambiguous. The same holds for the new Magnets.
- Binary: new members only.
- Output: no existing type's schema changes; models with these types didn't compile before.
- PVP: additive, so a patch-level release (0.9.x) is enough. Legacy: no impact (0.7 had none of these types).

## Review log
