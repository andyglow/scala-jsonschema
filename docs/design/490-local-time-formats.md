# Time types whose `format` rejects the written form  (issue #490, item D1)
Status: draft

## Problem
Seven built-in schemas give a string a JSON Schema `format` that the text JSON libraries write for that type doesn't
match. A validator that asserts formats rejects every real value: networknt does on draft-04/06/07 by default, and on
2019-09/2020-12 when format assertions are on. ajv with ajv-formats does too.

```scala
case class Meeting(start: java.time.LocalDateTime, at: java.time.LocalTime)
Json.schema[Meeting]   // start: {"type":"string","format":"date-time"}, at: {"type":"string","format":"time"}
```
circe and play-json write `{"start":"2024-01-01T10:00:00","at":"10:15:30"}`, and networknt 1.5.9 rejects both
fields (draft-07; 2020-12 with assertions).

Output today (0.9.0). It's the same on 2.12.21, 2.13.18 and 3.3.9, because these are shared Predefs (`core/.../json/schema/Predef.scala`)
and `JodaTimeSupport` Schemas, not macro output. It's also the same on JS:

| type | tree | draft-04/06/07 | 2019-09 / 2020-12 |
|---|---|---|---|
| `java.time.LocalDateTime` | `string(date-time)` | `format: date-time` | same |
| `java.time.LocalTime`, `java.sql.Time` | `string(time)` | `format: time` | same |
| `java.time.ZonedDateTime` | `string(date-time)` | `format: date-time` | same |
| `java.time.Duration` | `string(duration)` | plain `string` (`Pre09` drops `duration`) | `format: duration` |
| `scala.concurrent.duration.Duration` | `string(duration)` | plain `string` | `format: duration` |
| joda `LocalDateTime` / `LocalTime` / `Duration` | `def(string(date-time / time / duration))` | as the java ones, as a definition | same |

What the libraries write for these types, measured on 2.13 (circe 0.14.17, play-json 3.0.6 + play-json-joda, upickle 4.4.3,
json4s 4.0.7 + json4s-ext `JavaTimeSerializers`), checked with networknt 1.5.9 against today's format
(probe: `notes/scratch/c-d1/probe/Probe.scala`, output `out.txt`):

| type | `toString` | circe / play (ISO formatters) | json4s(-ext) | today's format |
|---|---|---|---|---|
| LocalDateTime | `2024-01-01T10:00`, `…T10:00:00.123`, `+12345-01-01T10:00` | `2024-01-01T10:00:00`, `…00.123456789` | `2024-01-01T10:00:00Z` (as if UTC) | rejects all but json4s |
| LocalTime | `10:00`, `10:15:30.500` | `10:00:00`, `10:15:30.5` | `{"hour":10,…}` (an object) | rejects all |
| java.sql.Time | `10:15:30` | no codec | `{}` | rejects |
| ZonedDateTime | `2024-01-01T10:00+01:00[Europe/Paris]` | `2024-01-01T10:00:00+01:00[Europe/Paris]`; `…00Z`, `…00+02:00` for offset zones | `2024-01-01T09:00:00Z` (UTC) | rejects every region zone (and `toString`, which drops `:00` seconds) |
| java.time.Duration | `PT1.5S`, `PT-5S`, `PT0S`, `PT48H` | same as `toString` | `1500` (millis, a number) | rejects fractions and signs |
| scala Duration | `1500 milliseconds`, `Duration.Inf` | no codec | fails (reflection) | upickle writes `"1500000000"` (nanos), `"inf"`; rejects all |
| joda LocalDateTime | `2024-01-01T10:00:00.000` | play-joda: no writer | `{}` | rejects |
| joda LocalTime | `10:00:00.000` | play-joda: `10:00:00.000` | object | rejects |
| joda Duration | `PT1.500S`, `PT-5S`, `PT172800S` | no codec | — | rejects fractions and signs |

## Why it is a problem
The schema describes these types wrongly for every library we bridge. A service that validates its own responses
or requests with the generated schema rejects a normal `LocalDateTime` or `LocalTime`. That's the most common local
type in Scala APIs after `Instant`. `ZonedDateTime.now()` has a region zone, so it's rejected too. The schema
looks right on paper (`date-time` reads as "a timestamp"), which is why nobody reported it. The README says these
types are supported "out of the box".

RFC 3339 is the reason: `date-time` and `time` require an offset (`Z` or `±hh:mm`), `[zone]` suffixes aren't part of it,
and its `duration` grammar (Appendix A) has no fractions or signs.

## Options
### A. A `pattern` of the written form, no `format`  — WON
Each affected Predef/Schema becomes `string` with no format plus a `pattern` that accepts what `toString`, the
ISO formatters (circe, play) and json4s write. Same rule as E3 (#474, approved): "a format where the format matches
the text, else a pattern of the written form". The patterns are ECMA 262 and Java compatible: `[0-9]` instead of `\d`,
no lookaround (RE2 validators refuse it), `{4,}` for extended years.

```
LocalDateTime, joda LocalDateTime
^[-+]?[0-9]{4,}-(?:0[1-9]|1[0-2])-(?:0[1-9]|[12][0-9]|3[01])T(?:[01][0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9](?:\.[0-9]{1,9})?)?Z?$
LocalTime, java.sql.Time, joda LocalTime
^(?:[01][0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9](?:\.[0-9]{1,9})?)?$
ZonedDateTime
^[-+]?[0-9]{4,}-(?:0[1-9]|1[0-2])-(?:0[1-9]|[12][0-9]|3[01])T(?:[01][0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9](?:\.[0-9]{1,9})?)?(?:Z|[-+][0-9]{2}:[0-9]{2}(?::[0-9]{2})?)(?:\[[^\]]+\])?$
java.time.Duration, joda Duration
^PT(?:-?[0-9]+H(?:-?[0-9]+M)?(?:-?[0-9]+(?:\.[0-9]{1,9})?S)?|-?[0-9]+M(?:-?[0-9]+(?:\.[0-9]{1,9})?S)?|-?[0-9]+(?:\.[0-9]{1,9})?S)$
scala.concurrent.duration.Duration
(no pattern, no format: plain string)
```

Notes on the choices:
- Seconds and the fraction are optional because `toString` drops `:00` seconds and a zero fraction, while the ISO
  formatters write the seconds. joda always writes `.000`, and the fraction is 1–9 digits.
- `LocalDateTime` takes an optional trailing `Z` because json4s's `DefaultFormats.dateFormat` writes one. That makes
  the pattern a little looser than ISO, but it keeps json4s's output valid, and that output is valid today. The cost:
  the schema accepts `2024-01-01T10:00:00Z`, which the circe and play decoders (`LocalDateTime.parse`) reject. A request
  can pass validation and then fail decoding. Accepting too much is the lesser fault here: today the schema rejects
  every value circe and play write. The alternative is the strict ISO form without `Z?`. json4s users would then go
  from valid to invalid. The plan tests the json4s case (step 6).
- `Duration` uses the three branches (starts with H, with M, with S) to keep out the bare `PT` without a lookahead, the
  way E3's `Period` pattern does. `Duration.toString` never writes days (`PT48H`), so `P1D` is rejected, even though
  `Duration.parse` accepts it. That follows the written-form rule (E3 option C, parse-compatible patterns, lost).
- `scala.concurrent.duration.Duration` has no common written form: `toString` writes `1500 milliseconds`, upickle writes
  nanos, and circe and play have no codec. A plain `string` states nothing false. On drafts 04/06/07 nothing changes,
  because they already render a plain `string`.

Checked (`notes/scratch/c-d1/probe-pat/Pat.scala`): 40 written forms accepted and 25 junk strings rejected, by networknt on
draft-07 and 2020-12, and by `new RegExp(p, 'u')` on Node and on Bun. Junk includes `2024-01-01 10:00:00`, `2024-13-01T10:00`, `10:15:30Z`,
`24:00`, `2024-01-01T10:00:00[Europe/Paris]`, `PT`, `P1D` and `1500 milliseconds`.

Sample output, every draft and every profile (Neutral, Legacy, the bridge profiles):
```json
{"type": "string", "pattern": "^(?:[01][0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9](?:\\.[0-9]{1,9})?)?$"}
```

Pros: every value the libraries write validates and junk is still rejected, on every draft and with any validator.
It's consistent with E3's new Predefs. No new public API except five string magnets (Implementation plan, step 2). Cons: a reader sees a regex
instead of a format name. Tools that map `format` to a type, such as OpenAPI generators turning `date-time` into
`OffsetDateTime`, lose the hint. For the local types that hint was wrong anyway: there's no offset to put into an
`OffsetDateTime`. For **`ZonedDateTime`** the hint is *right*. `OffsetDateTime` is a fine target for it, and
offset-zone values (`Z`, `+02:00`) do match `date-time`. So there the trade-off is real: region-zone values rejected
(today) against the documentation tools' hint lost (A). A still picks the pattern, because a schema that rejects what
`ZonedDateTime.now()` writes is the worse failure. The per-type table keeps `date-time` as a one-line alternative.
The same holds, more weakly, for `java.time.Duration`: `duration` is a meaningful hint, but no tool maps it to a type.
The patterns are a small maintenance surface.

**This supersedes a note in E3's approved doc** (`docs/design/474-java-time.md`, PR #477). There, the extended-year case
says "a pattern would lose the format's meaning for documentation tools. Don't 'fix' it with a pattern" about
`Instant`/`ZonedDateTime`, and the `OffsetDateTime` note calls the format a fit "Same as the existing
`ZonedDateTime`/`Instant` Predefs". Both are still true for `Instant`, which this item leaves alone. For `ZonedDateTime`
they only hold for offset zones. Whichever of #477 and this item's implementation lands second drops `ZonedDateTime`
from those two sentences in E3's doc. The lead coordinates; see step 1.

### B. Plain `string`, no format and no pattern (tapir)  — LOST
tapir does this for `LocalDateTime`, `LocalTime`, `OffsetTime` and `java.time.Duration`. Never rejects a real
value and takes no regexes, but validates nothing: `"x"` becomes a valid `LocalDateTime`. It's inconsistent with E3,
which lost this option on the same grounds. It stays the right choice for `scala.concurrent.duration.Duration`
(see A), where no written form exists. A one-line switch per type if the owner prefers it.

### C. Keep the formats, document the limitation  — LOST
No output change. Users who validate write their own `implicit val p: Predef[LocalDateTime] = ...`. A local
Predef wins over the built-in one; verified on 2.13 and 3.3 (`notes/scratch/c-d1/probe-ver/Override.scala`). LOST
because the default schema is wrong for every library: the fix would be on every user, and the lead policy is that
Legacy freezes 0.7's *style*, not its bugs.

### D1. Semantic `Format` values, lowered to A's pattern at render time  — LOST (close second)
The tree says what the type is: new `string.Format` case objects, say `local-date-time`, `local-time`,
`zoned-date-time` and `java-duration`. The Predefs become `` `string`[LocalDateTime](Format.`local-date-time`) ``. The
two string renderers (`Pre09.mkStr`, `Post09.mkStr`) never write them as a `format`. They write option A's pattern
instead, unless the schema already has a `pattern`, so a user's own one wins. The output is the same as A on every
draft and profile, Legacy included. There's a precedent: `Pre09.mkStr` already drops `uuid` and adds a `pattern`.
Pros:
- The tree stays readable and matchable: `string(local-time)` instead of a `pattern` validation, both in users' tests
  and in `AsTree`/`toString` output.
- Hand-written schemas can name the format.
- The patterns live in one public place, so they aren't duplicated between core and joda.
- `JsonSchemaProfile.Lowering` could later follow a library: drop the `Z?` for circe and play, keep it for json4s.
  That would remove A's `Z?` compromise.

Cons:
- New public `Format` vocabulary (additive) that is never a JSON Schema format.
- Both renderers, and anything else that prints a `Format` (`AsTree`, the docs), must know the new values. A user's
  custom renderer that writes `format: productPrefix` would emit `local-date-time`, which ajv in strict mode refuses.
- It's a second mechanism next to E3's: E3's `Period`, `Year`, `YearMonth`, `MonthDay` and `ZoneOffset` are plain
  `pattern` validations. Choosing D1 means either redoing E3 the same way or having two ways to say "a pattern of the
  written form".
- A user matching on `` `string`(`date-time`) `` breaks either way.

LOST to A on consistency with E3 and the smaller surface. If the owner prefers it, the plan changes in steps 1–3 only
(Format objects, the two `mkStr`s, Predefs naming the formats). The tests (steps 4–7) stay the same, except 4(a),
which asserts the tree.

### D2. D1 + Legacy keeps the 0.7 format  — LOST
As D1, but `Lowering` (which knows the profile) turns the new formats back into `date-time` / `time` / `duration` under
Legacy. LOST: all of D1's surface just to reproduce a schema that rejects valid data, which the Legacy policy
(2026-11-23) treats as a bug to fix, not a style to keep. No golden-legacy file has these types, so Legacy's tested
output doesn't change under A anyway.

### E. Keep the format on 2019-09/2020-12 (where it is an annotation by default), pattern before  — LOST
Two outputs for one type, and it's still wrong for format-asserting setups on 2020-12 (ajv-formats, networknt with
assertions on, as in the probe).

### Per-type alternatives (for the owner)
| type | winner (A) | easy alternative |
|---|---|---|
| LocalDateTime (java, joda) | pattern, with `Z?` for json4s | pattern without `Z?` (strict ISO); or plain string (B) |
| LocalTime, java.sql.Time, joda LocalTime | pattern | plain string (B) |
| ZonedDateTime | pattern with an optional `[zone]` (rejects nothing real; loses the right `date-time` hint for doc tools) | keep `date-time` (keeps the hint; rejects every region zone, i.e. `ZonedDateTime.now()`); or plain string |
| java.time.Duration, joda Duration | pattern of `toString` | keep `duration` from 2019-09 (wrong for fractions and negatives); or plain string |
| scala Duration (and E3's FiniteDuration) | plain string | keep `duration` from 2019-09 (no library writes ISO 8601 for it) |

## Decision
Option A: the affected Predefs (core) and Schemas/Predefs (joda) render a `string` with a `pattern` of what the
libraries write, and `scala.concurrent.duration.Duration` becomes a plain `string`. This applies on every draft and under
every profile, Legacy included. It's the E3 rule applied to the existing types. A user who wants the old schema declares
a local `Predef` (or, for joda, a `Schema`).

Out of scope, noted for the lead:
- json4s-ext writes `LocalTime` as an object and `java.time.Duration` as a millis number. play writes `java.util.Date` and
  `java.sql.Date` as epoch-millis numbers. These are a library's *shape*, not a format, so they belong to the bridge profiles.
- `java.sql.Timestamp` (`date-time`): its `toString` writes `2024-01-01 10:00:00.123`, with a space and no offset, which
  `date-time` rejects. None of the probed libraries writes that (json4s writes `…T10:00:00Z`; circe and play have no
  codec), so it only hits hand-written `toString` codecs. Same class of problem, left for the lead to file.
- Extended years (`+12345-…`) fail `date-time` for `Instant` and `OffsetDateTime` as well, because RFC 3339 allows only 4-digit years.
  That's an edge case, not handled here.

## Implementation plan
1. **core: `json/schema/Predef.scala`.** Replace `localDateTimeS`, `localTimeS`, `jsqlTimeS`, `zonedDateTimeS` and
   `jDurationS` with `` `string`[X].withValidation(`pattern` := P) ``, using the patterns of option A. Make `durationS`
   `` `string`[Duration] `` with no format. Keep the val names and types, so the change stays binary compatible. Put the
   pattern strings in one place in core. Constants is `private[jsonschema]` in `com.github.andyglow.jsonschema`, so
   `json.schema.Predef` can't reach it. Use private vals in `object Predef` instead, commented with the
   design-doc link as E3 does.
   **E3 coordination (#477), either order.** E3's doc says `FiniteDuration` follows `Duration`. If E3 merges first, this
   change also makes its `finiteDurationS` a plain `string`, updates E3's FiniteDuration expectations in its spec, and
   drops `ZonedDateTime` from the two sentences in E3's doc quoted under A. If this lands first, E3's rebase does the same
   on its side: `finiteDurationS` becomes a plain `string`, with its spec expectation and those two doc sentences. The lead
   tells whichever PR is second.
2. **core: `json/schema/validation/Magnet.scala`.** Add magnets `Magnet[X, String]` for `java.time.LocalDateTime`,
   `LocalTime`, `ZonedDateTime`, `java.time.Duration` and `java.sql.Time` (as E3 adds them for `Period` and the rest). They are
   needed for `withValidation`, and they're additive: users can now add validations to these schemas. On JS they're type arguments only, as
   in the existing Predefs, so the linker doesn't need `java.sql`/`java.time` classes.
3. **joda-time: `JodaTimeSupport.scala`.** `jtLocalDateTimeSchema`, `jtLocalTimeSchema` and `jtDurationSchema` get the same
   patterns. They stay `Schema`s, so they stay definitions with a `$ref`, and the `predef._` variants follow because they
   wrap these. Add private implicit magnets for the joda types in the object. The core vals are private, so repeat the
   three literals in the joda module, and add a test that they equal the core schemas' patterns (step 5). An alternative
   is a small public object holding the patterns, say `json.schema.Predef.TimePatterns`, used by core and joda. It
   would also let users build their own Predefs from it, for example one that keeps a `format` and adds the pattern. It
   costs public API, so it's not in the plan; it's a cheap switch at implementation time if the owner wants it.
4. **Tests, `TimeFormatsSpec` (new)**, split by platform. The shared part is
   `api/src/test/scala/com/github/andyglow/jsonschema/TimeFormatsSpec.scala`, which runs on the JVM and JS. The JVM-only
   part is `api/src/test/scala-jvm/com/github/andyglow/jsonschema/TimeFormatsJvmSpec.scala`, and it holds everything that
   needs a value that can't exist on JS:
   - `java.sql.Time` values: there's no `java.sql` on Scala.js.
   - region-zone `ZonedDateTime`s (`ZoneId.of("Europe/Paris")`): the JS test classpath has `scala-java-time` but not
     `scala-java-time-tzdb` (build.sbt L68), so it throws `ZoneRulesException`.

   The shared part uses only offset zones (`ZoneOffset.UTC`, `ZoneOffset.ofHours(2)`, which need no tzdb). Region-zone
   *texts* stay in it as string literals for (d), because they need no `ZoneId`. Only the schema of `java.sql.Time` (a
   type argument) is shared. Adding tzdb for JS was considered and rejected: it makes the JS test link much bigger for
   one sample. For each changed type:
   (a) the tree (`string(None)` plus the `pattern` validation); (b) the rendering on all five drafts under Neutral
   and Legacy, as full JSON; (c) the sample texts: what `toString` and the `DateTimeFormatter.ISO_*` formatters write
   for a fixed set of values equals hard-coded strings. On JS that runs against scala-java-time, which is already a
   test dependency, and shows that it writes the same text; (d) `java.util.regex.Pattern.compile(p).matcher(s).matches`
   for the written forms and the junk list of option A. On Scala.js `Pattern` compiles to a JS `RegExp`, so this also proves
   the patterns are ECMA compatible; (e) networknt validation of the written forms and the junk through
   `petshop.JsonValidator` (canceled on JS, as the shim does). Then two more cases: (f) the user-override case, where a
   local `implicit Predef[LocalDateTime]` with the old `date-time` still wins, on both Scala versions; and (g) json4s's
   `LocalDateTime` text `2024-01-01T10:00:00Z` validates. This is the reason for `Z?`. Step 6 checks it against the real
   codec.
5. **Tests, updated:** `api/.../GenericSchemaMacroSpec.scala` L23–29 expects `` `string`(`date-time`) `` for
   `LocalDateTime`, so expect the pattern schema. In `modules/joda-time/.../JodaTimeSupportSpec.scala`, the LocalDateTime and LocalTime
   expectations (L36–61) get the new definitions. Add a Duration case (none today) and a check that the joda patterns equal core's.
   `AsDraft04Spec` L90–102 builds `string(time)` by hand, and that is renderer behaviour, so it stays as it is.
6. **Bridges' acceptance (petshop).** Keep this out of the shared `object Samples`, which initializes every val, so a
   `ZoneId.of` there would break every existing JS acceptance spec.
   - Add the model `final case class Slot(start: LocalDateTime, at: LocalTime, zoned: ZonedDateTime, length: java.time.Duration)`
     to `api/src/test/scala/petshop/Model.scala`. It's a type only, so it's harmless on JS. Add `Schemas.slot` to
     `api/src/test/scala/petshop/Schemas.scala`, also shared. Add the 0.9 schema as a literal, `Schemas.slot09`, for the
     negative check.
   - Add the samples in a JVM-only object, `api/src/test/scala-jvm/petshop/TimeSamples.scala`: `TimeSamples.slots`, one
     region-zone `ZonedDateTime` with fractions and one offset zone with a negative duration.
   - Add new JVM-only specs `modules/circe-json/src/test/scala-jvm/petshop/CirceTimeAcceptanceSpec.scala` and
     `modules/play-json/src/test/scala-jvm/petshop/PlayTimeAcceptanceSpec.scala`. Both modules get `api % test->test`, and
     `platformDirs("jvm")` wires `scala-jvm`. Each encodes `TimeSamples.slots` with the library's built-in codecs and runs
     `Acceptance.assertValid` against `Schemas.slot` under the library's profile, on every draft. It also runs
     `assertInvalid` against `Schemas.slot09`, to show that the change matters.
   - json4s, for `Z?` (R4): in `modules/json4s-json/src/test/scala-2/petshop/Json4sAcceptanceSpec.scala` (JVM-only
     module), write a `LocalDateTime` with `DefaultFormats ++ JavaTimeSerializers.all` and validate it against
     `Json.schema[LocalDateTime]`. This needs `"org.json4s" %% "json4s-ext" % "4.0.7" % Test` in build.sbt, so run
     `scalafmtSbtCheck`. Only `LocalDateTime` and `ZonedDateTime` are checked there. json4s's object `LocalTime` and
     millis `Duration` are out of scope (Decision).
   - upickle and spray have no java.time codecs, so they get no `Slot` test.
7. **Golden files:** none contain these types (`grep -rl 'date-time\|"time"\|duration' api/src/test/resources` is empty).
   Run `GOLDEN_UPDATE=1 sbt clean api/test` on each Scala version and confirm that no golden or golden-legacy file changes.
8. **Docs:** in README.md, the "Supported types" table (L277–280) gets new rows: `LocalDateTime` (moves out of the
   `date-time` row), `LocalTime`/`java.sql.Time` and `ZonedDateTime` (`string` + pattern of the ISO text), and
   `java.time.Duration` / `scala Duration`. Add one sentence on why there's a pattern and not a format (RFC 3339 needs an
   offset), and that a local `Predef` brings the old schema back. The joda paragraph (L803–804) changes the same way.
   In the paradox site, `docs/src/main/paradox/types.md` L11–14 and `modules/joda-time.md` L24–30 get the same wording. No CHANGELOG
   edit; the lead writes that at release.
9. **Scala 2 vs 3:** no macro change. The Predefs are shared sources, so the change is the same on 2.12, 2.13 and 3.3.
   The macro call sites only refer to the Predef values, but run `clean` before the gates anyway (CLAUDE.md), so no stale call site hides a difference.
   **JS:** see 2, 4 (the platform split, (c), (d)) and 6 (JVM-only samples). `rootJS/test` on all three versions, because core changes.
10. **Gates** (for whoever implements it): the full set from PROTOCOL-gaps step 8, including `makeDocs`.

Risks: a validator with an unusual regex dialect. The patterns avoid lookaround, `\d` and named groups, and were checked on
networknt (Java), V8 and JavaScriptCore. A user who matched on `` `string`(`date-time`) `` in a tree breaks; see Compatibility.

## Compatibility
- **Output:** changes for `LocalDateTime`, `LocalTime`, `java.sql.Time`, `ZonedDateTime` and `java.time.Duration` (every draft),
  for `scala.concurrent.duration.Duration` (2019-09 and 2020-12 only: `format` dropped), and for joda `LocalDateTime`,
  `LocalTime` and `Duration` (inside their definitions). This applies under every profile, including Legacy, per the lead's
  Legacy policy (a schema that rejects valid values is a bug). No golden-legacy file has these types. The alternative
  that keeps Legacy's output is option D2.
- **Source:** `Json.schema[LocalDateTime]` is a different tree, so code or tests comparing it to
  `` `string`(`date-time`) `` break. The new magnets are additive. To get the old schema back, declare a local `implicit val ... : Predef[X]`
  (verified to win on 2.13 and 3.3).
- **Binary:** compatible. The same vals with the same types, and new implicit defs in `object Magnet`.
- **PVP:** an output change to existing types, so it ships with the other D items in the next breaking release
  (0.10.0), not in a 0.9.x patch.
- **Core rule:** unchanged. Predefs stay inlined and joda Schemas stay definitions.

## Review log
Round 1 (r-d1, at ca831c4):
- **D1-R1 (must) ZonedDateTime reverses E3's note.** Accepted. Under A, a paragraph now says this supersedes E3's two
  sentences and says what changes in E3's doc. A's cons now cover the ZonedDateTime trade-off: the `date-time` hint is
  right there. The ZonedDateTime row of the per-type table now gives both sides. Step 1 covers the E3 doc update. The
  verdict for ZonedDateTime is still the pattern.
- **D1-R2 (must) Split option D.** Accepted. D is now D1 (semantic Formats lowered by `Pre09`/`Post09`, same output as A)
  and D2 (D1 + Legacy keeps). The ajv con is gone; D1's pros are listed. D1 loses to A on consistency with E3's pattern
  validations and the smaller surface. It's marked close second, with the steps that would change if the owner picks it.
- **D1-R3 (must) Tests can't run on JS.** Accepted. Step 4 is split into a shared spec and a `scala-jvm` spec (region
  zones need tzdb; `java.sql` doesn't exist on JS). Step 6 puts the samples in JVM-only `TimeSamples`, outside
  `Samples`, with JVM-only circe/play specs, and names `Schemas.slot`/`slot09`. tzdb for JS is noted as considered and rejected.
- **D1-R4 (nit) `Z?` cost and test.** Accepted. The decoder mismatch is stated in A. A json4s literal test is 4(g), and
  a real json4s-ext check is in step 6, with the Test dependency.
- **D1-R5 (nit) Public pattern object.** Accepted as an alternative in step 3. The plan keeps the private literals plus
  an equality test.
- **D1-R6 (nit) `java.sql.Timestamp`.** Accepted. It's added to the out-of-scope list with what the libraries write.
- **D1-R7 (nit) FiniteDuration, both orders.** Accepted. Step 1 covers both orders.
