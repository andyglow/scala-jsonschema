# Field defaults of built-in types  (issue #510, item B18)
Status: draft

## Problem
A case class field with a default fails to compile when the field's type is a built-in type that core gives a schema
(a `Predef`) but no `ToValue`:

```scala
case class Timeouts(connect: scala.concurrent.duration.Duration = 5.seconds)
case class Item(id: java.util.UUID = UUID.fromString("123e4567-e89b-12d3-a456-426614174000"), b: Byte = 2)
Json.schema[Timeouts]
```
```
Can't infer json value for default value of `Timeouts(..., connect: scala.concurrent.duration.Duration = 5.seconds)`.
Please provide `ToValue[scala.concurrent.duration.Duration]` type class. ...
```

Both macros turn a default into JSON with an implicit `ToValue[F]` of the field type `F` (Scala 2:
`UProductTypes.scala` ~L122, `c.inferImplicitValue`; Scala 3: `derivation/Products.scala` `defaultValue`,
`Expr.summon`), and abort with the error above when there is none. Core's `ToValue` companion
(`core/src/main/scala/com/github/andyglow/json/ToValue.scala`) has instances for `Boolean`, `Int`, `Long`, `Short`,
`Float`, `Double`, `BigInt`, `BigDecimal`, `Number`, `String`, `Null`, `Iterable`s of those (`ArrV`) and
`Map[String | Int, _]` of those. No other `Predef` type has one.

Measured on master `4b2aff3` (probe: `notes/scratch/c-b18/B18ProbeSpec.scala`, output `probe-<ver>.txt`). The result
is the same on 2.12.21, 2.13.18 and 3.3.9. It's a compile error, so Scala.js is the same:

| field default | compiles? |
|---|---|
| `Int`, `Short`, `List[Int]`, `Option[UUID] = None` | yes |
| `Byte` (`= 2` and `= 2.toByte`, on Scala 3 too), `Char`, `Character` | **no** |
| `UUID`, `URI`, `URL` | **no** |
| `java.util.Date`, `java.sql.Timestamp`, `java.sql.Date`, `java.sql.Time` | **no** |
| `Instant`, `LocalDateTime`, `ZonedDateTime`, `LocalDate`, `LocalTime`, `java.time.Duration` | **no** |
| `scala.concurrent.duration.Duration` | **no** |
| `List[UUID]`, `Map[String, UUID]`, `Array[Int]` (`ArrV` needs an `Iterable`) | **no** |

The types that open PRs add have the same gap: E3 #477 (`OffsetDateTime`, `OffsetTime`, `Period`, `Year`,
`YearMonth`, `MonthDay`, `ZoneOffset`, `ZoneId`, `FiniteDuration`), E5 #480 (`Locale`, `Currency`, `Pattern`,
`Regex`, `InetAddress`/`Inet4Address`/`Inet6Address`, `Path`, `File`, `Charset`), E8 #485 (`java.util.List/Set/Map`,
`Array[Byte]`). E4 #478 adds its own `ToValue`s (boxed numbers, `java.lang.Boolean`, `java.math`), so it is done.
(c-e4 noted that `b: Byte = 2` compiles on Scala 3. It doesn't on master, for `= 2` or for `= 2.toByte`.)

With a bridge import the defaults compile, because the bridge turns the library's encoder into a `ToValue`
(`AsCirce.toValue[T: Encoder]`, `AsPlay.toValue[T: Writes]`, ...). circe 0.14 on 2.13 and 3.3 (probe
`B18CirceProbeSpec.scala`, `circe-probe-<ver>.txt`) writes `"10:15:00"` for `LocalTime.of(10, 15)`,
`"2024-01-02T03:04:00"` for a `LocalDateTime`, `"PT1.5S"` for a `java.time.Duration` and `toString` for `UUID`,
`Instant` and `LocalDate`. circe has no encoder for scala `Duration` or `java.util.Date`, so those still fail with the import.

## Why it is a problem
- The README says built-in types work out of the box and that defaults are written into the schema. A config class
  such as `Timeouts(connect: FiniteDuration = 5.seconds)` (#11, #305), `Settings(id: UUID = ..., since: LocalDate = ...)`
  or `Packet(flags: Byte = 0)` is a compile error. The schema of the field is built in, but its default isn't.
- The error asks for a JSON-library bridge. A user who only generates schemas (no JSON library, or one we don't
  bridge) has to write a `ToValue` for each JDK type.
- Every new Predef (E3, E5, E8) widens the gap. A user can write `Json.schema` for the new type, but not a default for it.

## Options
### A. A core `ToValue` for every built-in Predef type  — WON
Add instances to the `ToValue` companion (its `LowPriority*Implicits` traits), one per Predef type, plus `Array[T]`.
They live in implicit scope, so the macros find them with no macro change. The rule for the JSON text:

1. it must be valid under the type's own core schema (after D1/E3/E5, which describe the written form);
2. among the valid forms, use what circe and play-json write (their ISO formatters for `java.time`);
3. otherwise use `toString`.

| type | written as | sample |
|---|---|---|
| `Byte` | `num(x)` | `2` |
| `Char`, `Character` | `str(x.toString)` | `"a"` |
| `UUID`, `URI`, `URL` | `toString` | `"123e4567-…"`, `"https://example.com/a"` |
| `java.util.Date` | `Instant.ofEpochMilli(x.getTime).toString` | `"2024-01-02T03:04:05.123Z"` (`toString` is `Mon Jan 01 19:04:05 PST 2024`) |
| `java.sql.Timestamp` | `x.toInstant.toString` (keeps nanos) | `"2024-01-02T11:04:05.123456789Z"` (`toString` has a space and no offset) |
| `java.sql.Date`, `LocalDate` | `toString` | `"2024-01-02"` |
| `java.sql.Time` | `toString` | `"10:15:00"` |
| `Instant` | `toString` | `"1970-01-01T00:00:00Z"` |
| `LocalDateTime` | `ISO_LOCAL_DATE_TIME` | `"2024-01-02T03:04:00"` (`toString` drops `:00`) |
| `LocalTime` | `ISO_LOCAL_TIME` | `"10:15:00"` (`toString`: `10:15`) |
| `ZonedDateTime` | `ISO_ZONED_DATE_TIME` | `"2024-01-02T03:04:00Z"`, `"…+01:00[Europe/Paris]"` |
| `java.time.Duration` | `toString` | `"PT1.5S"` |
| scala `Duration` and subtypes | `toString`, one `implicit def DurationV[D <: Duration]` (`ToValue` is invariant) | `"5 seconds"`, `"Duration.Inf"` |
| `Array[T]` (given `ToValue[T]`) | `arr(xs.map(to))` | `[1,2]` |
| E3 `OffsetDateTime` / `OffsetTime` | `ISO_OFFSET_DATE_TIME` / `ISO_OFFSET_TIME` | `"2024-01-02T03:04:00+02:00"`, `"10:15:00Z"` |
| E3 `Period`, `Year`, `YearMonth`, `MonthDay`, `ZoneOffset`, `ZoneId` | `toString` (E3's patterns accept it) | `"P1Y2M"`, `"--12-03"`, `"+02:00"` |
| E3 `FiniteDuration` | `DurationV` above | `"5 seconds"` |
| E5 `Locale` | `toLanguageTag` (play's form) | `"en-US"` |
| E5 `Currency` / `Pattern` / `Regex` / `Charset` | `getCurrencyCode` / `pattern` / `regex` / `name` | `"USD"`, `"^a+$"`, `"UTF-8"` |
| E5 `InetAddress`, `Inet4Address`, `Inet6Address` | `getHostAddress` (`toString` is `host/1.2.3.4`) | `"127.0.0.1"` |
| E5 `Path` / `File` | `toString` / `getPath` | `"/tmp/a b/c.txt"` |
| E8 `java.util.List[T]`, `java.util.Set[T]` | an array of the elements | `[1,2]` |
| E8 `java.util.Map[String, V]` | an object | `{"a":1}` |
| E8 `Array[Byte]` | base64 (E8's schema), more specific than `Array[T]` | `"AQI="` |

Measured forms: `notes/scratch/c-b18/forms/out.txt`. A `null` reference is written as `null` (as E4's `boxed`
helper does), so a direct call never throws. B17 #505 makes the macros skip a `null` default before calling the `ToValue`.

How it interacts with what's in scope (measured, `circe-probe-<ver>.txt`, 2.13 and 3.3). A `ToValue` the user
imports or defines (lexical scope) is found before the companion's (implicit scope). When a type had a `ToValue` in
its companion and circe's imported `toValue` could also encode it, circe's won (`"default":"circe:a"`). So:
- with a bridge import, a type the library can encode keeps the library's text (today's output, unchanged);
- a type the library can't encode (circe: scala `Duration`, `java.util.Date`) falls back to the core instance
  instead of failing;
- a user's own `implicit ToValue[UUID]` keeps winning.

**Scala.js.** core is shared and must link without scala-java-time (build.sbt: "the library doesn't depend on it").
The new instances that reach `java.time`, `java.sql` or `java.net.URL` must be **`implicit def`s, not `val`s**.
Measured (`notes/scratch/c-b18/jsreach`, sbt-scalajs 1.23.0, no scala-java-time): a `val` holding a
`DateTimeFormatter`-based instance fails to link for any program that touches the companion (`Referring to
non-existent method java.time.format.DateTimeFormatter.ISO_LOCAL_TIME`, called from the module initializer). The same
instance as a `def` links and runs. A `def` is reachable only from the expansion of a default of that type, so
the user who writes it already links the type. For uniformity, all new instances are `def`s (the existing `val`s stay).
`java.util.Date` reaches `java.time.Instant`: on JS a `Date` default needs scala-java-time (a link error naming
`Instant` otherwise). That's acceptable, since a `Date` default on JS is rare, and documented.

Pros: fixes every reported case with no macro change, no change to the core design rule and no change to any
output that compiles today. The defaults are valid under their schemas (after D1, see Risks), and they are the
text the main libraries write. Small, additive (PVP minor).
Cons: about 30 new public members. The text chosen for a type nobody encodes the same way (scala `Duration`,
`java.util.Date`, `Locale`) is a choice. Each future Predef must remember its `ToValue` (the plan adds a test that
pairs them).

### B. No default when there is no `ToValue` (with a compile warning)  — LOST
When the implicit search fails, emit the field without `default` (not required, `hasDefault = true`, as for
`= None`) and warn: `c.warning(c.enclosingPosition, ...)` / `report.warning(...)` with today's text. Changes in
`UProductTypes.scala` ~L122–150 and `Products.scala` `defaultValue`/`noToValue`.
Pros: covers every type, user types too (case classes, enums, value classes). It never blocks compilation.
Cons: a schema that silently loses information. The warning is the only trace, and it's noisy for anyone who
doesn't care about defaults. It's a hard error under `-Werror`/`-Xfatal-warnings`, which is the same as today for
those users. On Scala 3 a macro warning's position decides whether a call-site `@nowarn` can silence it (cf.
B21). It turns today's compile error for user types into new output, which B17 #505 pins as the current verdict
(its option F: `n: Inner = null` without a bridge still fails). Also, built-in types still get no default, which is
what the item asks for.
Lost because it hides the problem instead of fixing it for the built-in types. It's a valid choice for *user*
types, as a separate decision for the owner (see C).

### C. A, plus B for types that still have no `ToValue`  — LOST (owner alternative)
A for the built-in types, and B's "warn and drop" for everything else (user types). It's the most permissive
option. It lost for the same reasons as B for user types (silent loss, B17's verdict). It can be added later as a
separate change on top of A: A doesn't preclude it. If the owner prefers C, plan steps 1–6 stay, and a step adds B's
macro change and flips the compile-error tests in `characterization` to "no default, warning".

### D. Opt-in "drop defaults without a `ToValue`" flag  — LOST
B behind `json.schema.Flag` (`implicit val f: Flag with Flag.DropUnencodableDefaults`, as `EnumsAsOneOf` does).
Pros: no silent change by default. Cons: a new public flag for a case A already fixes for built-in types. Users
of their own types already have a better fix (an implicit `ToValue`, or a bridge). Lost.

### E. A `Predef` carries how to write its values  — LOST
`Predef[T](schema, write: Option[T => Value])`, and the macro uses the writer when there's no `ToValue`. Cons: it
mixes the schema type class with an encoding. It changes `Predef`'s public shape (a case class) and every user
`Predef`, and it duplicates `ToValue`. Lost.

### Per-type alternatives (inside A)
- **`toString` everywhere.** It's simpler, but `LocalTime` gives `10:15` (no seconds: RFC 3339 `time` and every
  library write `10:15:00`), `java.util.Date` gives `Mon Jan 01 …`, `Timestamp` has a space, `InetAddress` gives
  `host/1.2.3.4`. Lost for those types; kept where it equals the written form.
- **scala `Duration` as ISO 8601 (`"PT5S"`).** It's valid under today's `format: duration` on 2019-09+, while
  `"5 seconds"` isn't. But no codec writes ISO for it (D1's table: `toString`, upickle nanos), and D1 #494 (approved)
  makes the schema a plain `string`. A client that sends the default back to a `Duration(...)` parser fails on
  `PT5S`. Lost: `toString`, consistent with D1.
- **`java.util.Date` as epoch millis (play's form).** It's invalid under the core schema (`string`, `date-time`). Lost.
  A play user who imports `AsPlay._` gets play's number anyway (bridge first). B24 owns that mismatch.
- **`Locale` as `toString` (`en_US`, Jackson).** E5's schema is a plain string, so both are valid. `toLanguageTag` is
  what play writes, it's BCP 47 and `Locale.forLanguageTag` reads it back. Kept `toLanguageTag`.
- **Instances as `val`s.** They break Scala.js linking (above). Lost.

### Out of scope (stated in the doc and the PR)
- `Option[X] = Some(x)`: needs `ToValue[Option[X]]`. Core has none on purpose (how `Option` is written depends on the
  library), so it's unchanged. Bridges provide one.
- `Map[K, V]` with keys other than `String`/`Int` (`UUID`, `Long`, ...): it needs a key-to-text type class that
  core doesn't have (B6 owns key schemas). Unchanged.
- `java.util.Optional` defaults (E8): an empty `Optional` should give no default, like `None`, which a `ToValue`
  can't say. E8 leaves it a compile error; a macro change like `optionDefault` is E8's follow-up.
- Module types (joda-time `Schema`s/`Predef`s, cats `NonEmpty*`, refined `T Refined P`): these modules are imported
  (`import JodaTimeSupport._`), so their `ToValue`s would sit in lexical scope *next to* a bridge's `toValue`.
  The non-generic one would then win over the bridge's or be ambiguous. Either way, today's output for joda+circe
  users would change. They need an opt-in import object (e.g. `JodaTimeSupport.defaults._`). Left for a follow-up item (mailed the lead).

## Decision
A: core gets a `ToValue` for every built-in Predef type (and `Array[T]`), as `implicit def`s in the `ToValue`
companion, so the macros find them unchanged. They write the text that's valid under the type's schema and that
circe/play write (ISO formatters for `java.time`, `toString` otherwise). A bridge import or a user's own
`ToValue` still takes precedence. If the owner wants user types covered too, C is A plus a later "warn and drop".

## Implementation plan
1. **`core/src/main/scala/com/github/andyglow/json/ToValue.scala`** (shared, all Scala versions, JVM + JS):
   - in `LowPriorityPrimitiveImplicits`: `implicit val ByteV: ToValue[Byte] = mk(num.apply)` (`Value.num(Byte)`
     exists), `implicit val CharV: ToValue[Char] = mk(c => str(c.toString))`. These are primitives and safe as `val`s.
   - a new trait `LowPriorityJdkImplicits` (mixed into `LowPriorityImplicits`), with every instance an
     **`implicit def`** (comment: "defs, not vals: a val would make every Scala.js program link java.time; see
     docs/design/510-predef-defaults.md"). A private helper `def text[T <: AnyRef](f: T => String): ToValue[T] =
     mk(x => if (x == null) `null` else str(f(x)))`. If E4 has merged, reuse/rename its `boxed` helper. Instances:
     `JCharacterV`, `UUIDV`, `URIV`, `URLV`, `JuDateV`, `JsqlTimestampV`, `JsqlDateV`, `JsqlTimeV`, `InstantV`,
     `LocalDateTimeV`, `ZonedDateTimeV`, `LocalDateV`, `LocalTimeV`, `JDurationV`,
     `DurationV[D <: scala.concurrent.duration.Duration]`. Texts are those in the table of option A. Use the
     formatter constants (`DateTimeFormatter.ISO_LOCAL_TIME.format(x)`), not `toString`, where the table says so.
   - `Array`: in `LowPriorityArrayImplicits` (both `scala-2.12-` and `scala-2.13+` copies), add
     `implicit def ArrayV[T](implicit to: ToValue[T]): ToValue[Array[T]] = mk(xs => arr(xs.toSeq.map(to(_))))`.
     Check that `ArrV`'s `C[_] <: Iterable[_]` doesn't make `Array[T]` ambiguous on 2.12 (`Array` isn't an `Iterable`,
     but the implicit `wrapRefArray` conversion doesn't apply to type-class search. Verify with a test).
   - E3 / E5 / E8 types: add their rows of the table **only if that PR has merged** when this is implemented.
     Otherwise mail the lead so the later PR adds them (same file, same helper, same tests). `Array[Byte]` (E8)
     is a more specific `implicit def BytesV: ToValue[Array[Byte]]` placed in a trait that *extends*
     `LowPriorityArrayImplicits`, so it wins over `ArrayV` by the subclass rule on Scala 2 and 3.
     `java.util.Base64` is in the Scala.js javalib.
2. **No macro change.** Both macros already summon `ToValue[F]`: Scala 2 `UProductTypes.scala` ~L122, Scala 3
   `Products.scala` `defaultValue`. Still run `clean` before testing (CLAUDE.md), because expansions must re-resolve.
3. **Tests (shared)**: `api/src/test/scala/com/github/andyglow/jsonschema/PredefDefaultsSpec.scala`, with models at
   the top level of the file (Scala 2 can't take defaults of block-local classes):
   - for each type that is in the Scala.js javalib or scala-java-time (`Byte`, `Char`, `Character`, `UUID`, `URI`,
     `java.util.Date`, every `java.time` type, scala `Duration`/`FiniteDuration`/`Duration.Inf`, `Array[Int]`,
     `List[UUID]`, `Set[LocalDate]`, `Map[String, Instant]`): the field of `Json.schema[D]` has
     `required == false` and `default == Some(<the table's text>)`. One rendered draft-07 check (`"default": "10:15:00"`).
     Fixed samples, zone-independent (`new Date(1704164645123L)`, `Instant.ofEpochMilli`). Runs on JS through `apiJS`,
     which checks that scala-java-time's formatters write the same text as the JDK.
   - `api/src/test/scala-jvm/.../PredefDefaultsJvmSpec.scala` for the JVM-only types (`URL`, `java.sql.*`; E5's
     `InetAddress`/`Path`/`File`/`Currency`/`Locale` if merged).
   - **Pairing test** (JVM): for every `implicit` member of `json.schema.Predef` whose type has no type parameter,
     `ToValue` resolves for that type. Written as one `implicitly[ToValue[X]]` line per Predef in the spec, with a
     comment in `Predef.scala` next to the Predefs ("a new Predef needs a ToValue: PredefDefaultsSpec"). It's a
     compile-time pairing, so a missing one fails the build.
   - **Self-validation** (JVM, uses `petshop.JsonValidator`, canceled on JS): for each type, the rendered field
     schema accepts the field's own default, on draft-07 (networknt asserts formats there) and on 2020-12 with
     format assertions on. That needs an optional `formatAssertions` parameter on `JsonValidator.errors`
     (`SchemaValidatorsConfig.builder().formatAssertionsEnabled(true)`) and the same parameter on the JS counterpart.
     Known exceptions, tagged `TRIAGE-A` with the item that fixes them, until it merges: `LocalDateTime`, `LocalTime`,
     `java.sql.Time`, a region `ZonedDateTime` and scala `Duration` on 2020-12 (D1 #494), and `Char` (B5 #468, whose
     pattern rejects `"a"`). Whichever of D1/B5 merges after this removes its exception.
   - **Precedence** (`modules/circe-json/src/test/.../AsCirceSpec.scala` and `modules/play-json/.../AsPlaySpec.scala`):
     with `import AsCirce._`, a `UUID`/`LocalTime` default has circe's text, and a scala `Duration` default (no circe
     encoder) has the core text. With `import AsPlay._`, a `java.util.Date` default is play's number (bridge first).
     Plus a user `implicit val ToValue[UUID]` in lexical scope wins over core's, in api.
   - Keep the existing compile-error tests for types with no `ToValue` (`ErrorPhrases.NoToValueForDefault`). If a test
     used a built-in type as its "no ToValue" example, switch it to a user class (grep `NoToValueForDefault`).
4. **Golden files**: none change. No golden model can have such a default today, since it doesn't compile.
   `golden-legacy` is unchanged. Verify with `GOLDEN_UPDATE=1 sbt clean api/test` that there's no diff.
5. **JS link check** (once, by the implementer; not a CI job): adapt `notes/scratch/c-b18/jsreach` to depend on
   `coreJS` (`publishLocal`) **without** scala-java-time and call `ToValue[Int]` and `ToValue[UUID]`. `fastLinkJS`
   must pass. Record it in the PR. That's the guard against a future `val`.
6. **Docs**: README ~L115 and `docs/src/main/paradox/getting-started.md` ~L76 ("A default is written into the
   schema when it can be turned into JSON: primitives, strings and collections/maps of them always can, ..."):
   say that every built-in type can, with a short table or list of the texts for the non-obvious ones (`java.time`
   with ISO formatters, `java.util.Date`/`Timestamp` as an ISO instant, scala `Duration` as `toString`), that a
   bridge import or your own `ToValue` takes precedence, and that a `java.util.Date` default on Scala.js needs
   scala-java-time. Mention that `Option = Some(..)` and maps with non-string keys still need a `ToValue`. Same
   wording in both files. `makeDocs` after (snippets as `mdoc:compile-only` are fine: plain case classes).
7. **Gates**: protocol step 8. core is touched, so `clean test` on 2.12.21, 2.13.18 and 3.3.9, `rootJS/test` on all
   three, `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`, `makeDocs`.

Risks:
- An `implicit def` placed by mistake as a `val` passes every test (the JS test build has scala-java-time) and breaks
  JS users. Mitigations: the source comment, plan step 5, and the reviewer checks it.
- Before D1 merges, some defaults fail their own schema's `format` (the TRIAGE exceptions). That's not new
  invalidity: the same values already fail, which is D1's bug.
- `java.sql.Date`/`Time` `toString` use the JVM zone. That's the type's meaning, and tests build samples with `valueOf`.
- Scala 3 / 2 implicit search: if a lexical candidate (a bridge's `toValue[T: Encoder]`) fails with an *ambiguity*
  in its nested search rather than "not found", the search may report it instead of falling back to the core
  instance. The precedence tests cover the common case (no encoder). An ambiguous-encoder case stays a compile error, as today.

## Compatibility
- Source: additive. Code that compiled before resolves the same `ToValue` as before, because a lexical
  (imported/local) instance precedes the companion's (measured on 2.13 and 3.3). Code that didn't compile now does.
- Binary: new members in `ToValue`'s companion and its `LowPriority*Implicits` traits; nothing removed or changed.
  Those traits exist to be mixed into `object ToValue`, and a user class that mixed them in would need recompiling.
  MiMa (if run) reports only additions.
- Output: no output that exists today changes (no golden diff). New output appears only where there was a compile error.
- PVP: a minor bump (new public API, no breakage). It could ship in 0.9.x as an addition. The lead decides.
- Legacy profile: unaffected. Profiles don't touch `default`.
- Core design rule (Schema → `$ref`, Predef → inlined): untouched. `ToValue` is independent of it.

## Review log
