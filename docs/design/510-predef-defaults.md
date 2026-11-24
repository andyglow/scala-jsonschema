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

A related bug in the same instances (found by r-b18, B18-R1). On 2.13 and 3.3, `ArrV[T, C[_] <: Iterable[_]]` also
matches `Map[K, V]`, with `C = Map[K, *]` and `T = V`. For a key type other than `String`/`Int` (`Long`, `Short`,
`Byte`, `Char`, an enum, B6's `UUID`/`Boolean`) that compiles today and is wrong:
- `m: Map[Long, Int] = Map.empty` renders `"default": []` on an `object` schema;
- `m: Map[Long, Int] = Map(1L -> 2)` throws `ClassCastException: scala.Tuple2 cannot be cast to java.lang.Integer`
  when `Json.schema` runs.

2.12 is a compile error. The only thing hiding it is the missing `ToValue[V]` for most `V`. Once A adds
`ToValue[UUID]`, `Map[Long, UUID] = Map(...)` would move from a compile error to that crash. So A must fix it too.

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
| `Map[Long \| Short \| Byte \| Char, V]` (given `ToValue[V]`) | an object, keys as `toString`, like `IntMapV` | `{"1":2}`, `{}` |

**`ArrV` no longer matches a `Map`** (B18-R1). It becomes
`implicit def ArrV[T, C[_]](implicit ev: C[T] <:< Iterable[T], to: ToValue[T]): ToValue[C[T]]` (`Traversable` in the
`scala-2.12-` copy). `Map[K, V]` isn't an `Iterable[V]`, so `ArrV` drops out for maps. The primitive keys
that have a core `KeyPattern` (`Long`, `Short`, `Byte`, `Char`; `String`/`Int` already have `StrMapV`/`IntMapV`) get
their own instances: their `toString` is the key every library writes. Any other key (an enum, B6's `UUID`/`Boolean`)
gets the ordinary "Can't infer json value" compile error instead of `[]` or a crash. That needs a key-text type class,
which is B6's area. Measured with a standalone mimic of the instances on 2.12.21, 2.13.18 and 3.3.9
(`notes/scratch/c-b18/arrv`, `arrv-neg`): `List`, `Set`, `Vector`, `Seq`, `Iterable`, `Queue`, `List[List[_]]` and
`Array` resolve as before. `Map[String|Int|Long, Int]` writes an object, and `Map.empty[Long, Int]` writes `{}`.
`Map[Short, Int]` without a `Short`-key instance is a compile error on all three versions, where it isn't `ArrV` any more.

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

Pros: fixes every reported case with no macro change and no change to the core design rule. Output that compiles
today changes only for the broken `Map[Long|…, V]` defaults (`[]` → `{}`, crash → object). The defaults are valid
under their schemas (after D1, see Risks), and they are the text the main libraries write. Small and additive (see
Compatibility).
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
C changes more than B17 decided (B18-R6). B17's verdict is only about `= null`. C's "drop" would also cover every
non-null default of a user type that has no `ToValue` (`c: Color = Red`, `id: Id = Id(1)`, `p: Point = Point(0, 0)`),
which today is a compile error that tells the user to add a `ToValue` or a bridge.

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
- **`java.util.Date` through a platform shim on JS** (B18-R5), like `Decimals`: `scala-js` writes
  `new js.Date(ms).toISOString()`, normalized to `Instant.toString`'s fraction rule (it omits `.000`, while
  `toISOString` always writes milliseconds). Then a `Date` default would need no scala-java-time on JS. Lost: it's a
  second code path for a rare case, and its output must be kept equal to the JVM's by hand. The `def` + "needs
  scala-java-time" note is enough. Reconsider if someone reports it.
- **`ToValue[Map[K, V]]` for every `KeyPattern` key** (enums, `UUID`) instead of only the primitive keys: needs a
  key-text type class (an enum key's text is its name or its `ToValue`). That's B6's area. Lost here.

### Out of scope (stated in the doc and the PR)
- `Option[X] = Some(x)`: needs `ToValue[Option[X]]`. Core has none on purpose (how `Option` is written depends on the
  library), so it's unchanged. Bridges provide one.
- `Map[K, V]` with a key other than `String`, `Int`, `Long`, `Short`, `Byte` or `Char` (an enum, `UUID`, `Boolean`):
  it needs a key-to-text type class that core doesn't have (B6 owns key schemas). After A it's a compile error.
  Today it's `"default": []` or a `ClassCastException` (B18-R1).
- `java.util.Optional` defaults (E8): an empty `Optional` should give no default, like `None`, which a `ToValue`
  can't say. E8 leaves it a compile error; a macro change like `optionDefault` is E8's follow-up.
- Module types (joda-time, cats, refined) aren't in this phase: see **Phase 2** below (lead, 2026-11-23: same doc,
  a separate follow-up phase).

## Decision
A: core gets a `ToValue` for every built-in Predef type (and `Array[T]`), as `implicit def`s in the `ToValue`
companion, so the macros find them unchanged. They write the text that's valid under the type's schema and that
circe/play write (ISO formatters for `java.time`, `toString` otherwise). A bridge import or a user's own
`ToValue` still takes precedence. The `Iterable` instance stops matching `Map`s (a new `IterableV`, while `ArrV`
stays as a deprecated non-implicit forwarder). Maps keyed by `Long`/`Short`/`Byte`/`Char` get object instances, and other
keys get the usual compile error instead of `[]` or a crash. If the owner wants user types covered too, C is A
plus a later "warn and drop".
The module types (joda-time, cats, refined) follow as Phase 2, a separate PR. They get opt-in `toValues` import
objects, because the module objects users import already share lexical scope with the bridges.

## Implementation plan (Phase 1: core)
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
   - `LowPriorityArrayImplicits` (both copies, `scala-2.12-` and `scala-2.13+`):
     - narrow `ArrV` (B18-R1), keeping binary compatibility: add `implicit def IterableV[T, C[_]](implicit ev: C[T] <:< Iterable[T], to: ToValue[T]): ToValue[C[T]] =
       mk(xs => arr(ev(xs).map(to(_)).toSeq))` (`Traversable` in the 2.12 copy; no `asInstanceOf`). Keep `ArrV` with its
       current signature, but no longer `implicit`, `@deprecated("use IterableV", "0.9.1")`, forwarding to its current
       body. A macro expansion compiled against 0.9.0 calls `ToValue.ArrV(to)`, which must still link (erased
       `ArrV(ToValue): ToValue`). Changing `ArrV`'s parameter list in place would be a binary break (`NoSuchMethodError`).
     - add `implicit def ArrayV[T](implicit to: ToValue[T]): ToValue[Array[T]] = mk(xs => arr(xs.toSeq.map(to(_))))`.
     The mimic in `notes/scratch/c-b18/arrv` measured both on 2.12/2.13/3.3: collections and `Array` resolve, maps don't
     match `ArrV`.
   - `LowPriorityMapImplicits`: `LongMapV`, `ShortMapV`, `ByteMapV`, `CharMapV`, the same shape as `IntMapV` (keys as
     `toString`). Generic `def`s over `V`, so they're JS-safe. A private helper `keyed[K, V](to: ToValue[V]): ToValue[Map[K, V]]`
     serves the six key types (`IntMapV`/`StrMapV` can use it too; same output).
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
   - **Pairing test** (B18-R2), `api/src/test/scala-jvm/.../PredefToValuePairingSpec.scala`, with two halves:
     - compile time: a `Map[String, ToValue[_]]` from each Predef member's name to its instance, e.g.
       `"uuidS" -> implicitly[ToValue[UUID]]`, `"arrayS" -> implicitly[ToValue[Array[Int]]]`, `"dictS" -> implicitly[ToValue[Map[Long, Int]]]`.
       A missing `ToValue` fails the build.
     - run time (it enumerates): reflect over `Class.forName("json.schema.Predef$").getMethods` (inherited
       `LowPriorityPredefs` members included). `Predef` is a value class (`case class Predef[T](schema: Schema[T]) extends AnyVal`),
       so its accessors erase to the underlying `json.Schema` (B18-R7, `javap`: `public json.Schema<java.util.UUID> uuidS()`).
       Collect the names of the public methods whose return type is `json.Schema`. Drop names containing `$`
       (`copy$extension`, `copy$default$1$extension`, ...) and the case-class companion's `apply`/`unapply`. That leaves the
       vals' accessors and the generic `def`s (`listS(json.Schema<T>)`). Check the resulting set on 2.12, 2.13 and 3.3:
       print it once and compare it with `Predef.scala`. Assert that this set equals the map's key set. A new Predef
       then fails the test until its `ToValue` and its line are added. Use method names, not generic return types:
       Scala emits `Predef<Object>` for `Predef[Int]`/`Predef[Byte]`, so the type argument doesn't identify a primitive.
       Explicitly exempt, with the reason in a comment, any Predef that intentionally has no `ToValue` (none in Phase 1).
     - one comment in `Predef.scala` above the instances: "a new Predef needs a ToValue (PredefToValuePairingSpec)".
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
   - **Maps** (B18-R1): `Map[Long, Int] = Map.empty` → `{}`, `= Map(1L -> 2)` → `{"1":2}`, `Map[Char, UUID] = Map('a' -> u)` →
     `{"a":"…"}`. A `Map` keyed by an enum or `UUID` (`Map[Color, Int] = Map.empty`) is a compile error with
     `NoToValueForDefault`. Use `CompileErrors.errorOf(assertCompiles(...))` in api's characterization style.
     `List`/`Set`/`Vector`/`Seq`/`Iterable` defaults are unchanged (the existing `SchemaMacroSpec.Bar9` golden covers lists and sets).
   - **`null` defaults** (B18-R4): `id: UUID = null`. If B17 #505 has merged: no `default`, the field is not required.
     If not: the instance writes `"default": null` on a `string` schema (invalid). Pin it as `TRIAGE-A` pointing to
     B17, and whichever of B17/B18 merges second updates the expectation.
   - Keep the existing compile-error tests for types with no `ToValue` (`ErrorPhrases.NoToValueForDefault`). If a test
     used a built-in type as its "no ToValue" example, switch it to a user class (grep `NoToValueForDefault`).
4. **Golden files**: none change. No golden model has a new-type default (it didn't compile) or a map default
   with a non-String/Int key (grep of `api/src/test/resources`: the `"default": [` entries are lists and sets).
   `golden-legacy` is unchanged. Verify with `GOLDEN_UPDATE=1 sbt clean api/test` that there's no diff.
5. **Guard against a `val`** (B18-R3):
   - in CI, `core/src/test/scala-jvm/com/github/andyglow/json/ToValueFieldsSpec.scala`: reflect over
     `Class.forName("com.github.andyglow.json.ToValue$").getDeclaredFields`, keep the fields whose type is
     `ToValue`, and assert that their names are a subset of an allow-list. The list holds today's vals (`BoolV`, `IntV`,
     `LongV`, `ShortV`, `FloatV`, `DoubleV`, `BigDecimalV`, `BigIntV`, `NumberV`, `StringV`, `NullV`), `ByteV`, `CharV`,
     and E4's `J*V` if merged (`java.lang`/`java.math` are in the Scala.js javalib). A trait `val` compiles to a field
     of the class that mixes it in, a `def` to no field. So a new instance written as a `val` fails the test, and the
     failure message points to this doc. Check the field names on 2.12, 2.13 and 3.3 when writing it (they're plain
     names for public trait vals). A `lazy val` isn't caught on Scala 3 (its field is an `Object`), but it's JS-safe
     anyway: its initializer runs only when accessed.
   - Considered instead: a tiny unpublished `jsLinkCheck` project (depends on `coreJS`, no scala-java-time,
     aggregated by `rootJS`) whose test links a program that touches `ToValue`. It's the end-to-end check, but it
     adds a build project. The reflection test catches the same mistake.
   - once, by the implementer: adapt `notes/scratch/c-b18/jsreach` to depend on `coreJS` (`publishLocal`)
     **without** scala-java-time, call `ToValue[Int]` and `ToValue[UUID]`, `fastLinkJS` must pass. Record it in the PR.
6. **Docs**: README ~L115 and `docs/src/main/paradox/getting-started.md` ~L76 ("A default is written into the
   schema when it can be turned into JSON: primitives, strings and collections/maps of them always can, ..."):
   say that every built-in type can, with a short table or list of the texts for the non-obvious ones (`java.time`
   with ISO formatters, `java.util.Date`/`Timestamp` as an ISO instant, scala `Duration` as `toString`), that a
   bridge import or your own `ToValue` takes precedence, and that a `java.util.Date` default on Scala.js needs
   scala-java-time. Mention that `Option = Some(..)` and maps keyed by an enum or `UUID` still need a `ToValue`. Same
   wording in both files. `makeDocs` after (snippets as `mdoc:compile-only` are fine: plain case classes).
7. **Gates**: protocol step 8. core is touched, so `clean test` on 2.12.21, 2.13.18 and 3.3.9, `rootJS/test` on all
   three, `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`, `makeDocs`.

Risks:
- An `implicit def` placed by mistake as a `val` passes the JS tests (the JS test build has scala-java-time) and breaks
  JS users. Mitigations: `ToValueFieldsSpec` (step 5, CI), the source comment, the one-off link check.
- Merge order with B17 #505 (`null` defaults): see the `null` test in step 3. Before B17, `id: UUID = null` renders
  `"default": null` (today it's a compile error). After B17, there's no default.
- Before D1 merges, some defaults fail their own schema's `format` (the TRIAGE exceptions). That's not new
  invalidity: the same values already fail, which is D1's bug.
- `java.sql.Date`/`Time` `toString` use the JVM zone. That's the type's meaning, and tests build samples with `valueOf`.
- Scala 3 / 2 implicit search: if a lexical candidate (a bridge's `toValue[T: Encoder]`) fails with an *ambiguity*
  in its nested search rather than "not found", the search may report it instead of falling back to the core
  instance. The precedence tests cover the common case (no encoder). An ambiguous-encoder case stays a compile error, as today.

## Phase 2 (follow-up, separate PR): module types
Design only, as asked by the lead. Implement after Phase 1 merges (it reuses Phase 1's helper style and tests).

### Problem
The same error for the module types. Measured on master `4b2aff3`, 2.13.18 and 3.3.9, with the module's usual
import in scope (probes `notes/scratch/c-b18/modules/*.scala`, output `out-<ver>.txt`):

| module | field default | today |
|---|---|---|
| joda-time (`import JodaTimeSupport._`) | `x: org.joda.time.LocalDate = new LocalDate(2024, 1, 2)` | "Can't infer json value for default value" |
| cats (`import CatsSupport._`) | `x: NonEmptyList[Int] = NonEmptyList.of(1, 2)` | same error |
| refined (`import RefinedSupport._`) | `x: Int Refined Positive = Refined.unsafeApply(5)` | same error |

Phase 1 doesn't help. Core can't name these types, and their companions aren't ours, so the instances can't be in
implicit scope. They have to be imported, which puts them in **lexical** scope, the same place as a bridge's
`toValue[T: Encoder]`.

### Options
#### P2-A. Opt-in import objects `JodaTimeSupport.toValues`, `CatsSupport.toValues`, `RefinedSupport.toValues`  — WON
A nested object per module, like joda's existing `JodaTimeSupport.predef` (the opt-in for Predefs instead of
Schemas). A user writes `import JodaTimeSupport.toValues._` to get defaults without a JSON library. Nobody's
current imports change, so no output changes. With a bridge *and* `toValues` imported, both candidates are
lexical. Under the 2.12/2.13/3.3 rules, specificity is expected to prefer the module's instance (non-generic, or
generic in a narrower type) over the bridge's `toValue[T]`. Newer Scala 3 versions (3.5/3.6+) changed how givens are
prioritized, and users compile the expansion with their own compiler, so the result may differ there (B18-R8).
The plan's test pins the result on 2.12/2.13/3.3. Because the import is opt-in, either winner is acceptable: the
user chose to import it, and an ambiguity is a compile error that names both candidates. Texts:
- joda (JVM only, so `val`s are fine): `toString` for all six, which is ISO and what play-joda writes. Measured:
  `DateTime` `2024-01-02T03:04:00.000Z`, `Instant` `1970-01-01T00:00:00.000Z`, `LocalDateTime`
  `2024-01-02T03:04:00.000`, `LocalDate` `2024-01-02`, `LocalTime` `10:00:00.000`, `Duration` `PT1.500S`.
  D1 #494's joda patterns accept these.
- cats: arrays of the elements' `ToValue` for `NonEmptyList`, `NonEmptyVector`, `NonEmptySet`, `NonEmptyChain`,
  `Chain`, `NonEmptyLazyList` (2.13+) / `NonEmptyStream` (2.12, its own `scala-2.12-` file), `OneAnd[F, T]`
  (head + `Foldable[F].toList(tail)`); an object for `NonEmptyMap[String, V]` (as `StrMapV`). These are generic
  `def`s, safe on JS.
- refined: `implicit def refinedV[T, P](implicit to: ToValue[T]): ToValue[T Refined P] = ToValue.mk(r => to(r.value))`
  in a shared trait (`src/main/scala`), mixed into `object toValues` inside both `RefinedSupport`s (`scala-2`, `scala-3`).

#### P2-B. Put the instances in the objects users already import (`JodaTimeSupport._`, ...)  — LOST
No new import to learn. But for a user who imports a module and a bridge today (joda + `AsPlay._`, cats +
`AsCirce._`), the default's text would switch silently from the library's encoder to ours (specificity, above),
or the two would be ambiguous. That's an output change for code that compiles today. Lost.

#### P2-C. Leave it to the bridges  — LOST
Today's state. A user without a bridge or without the library's codec for the type (circe has no joda codecs)
can't get a default. Lost, though it remains the answer for bridge users.

### Plan
1. `modules/joda-time/.../JodaTimeSupport.scala`: `object toValues { implicit val jtDateTimeV: ToValue[DateTime] = ToValue.mk(x => Value.str(x.toString)); ... }` (six).
2. `modules/cats/.../CatsSupport.scala` (+ `scala-2.12-`/`scala-2.13+` `ScalaVersionSpecific*` for the lazy list / stream):
   `object toValues` with the instances above.
3. `modules/refined/src/main/scala/.../RefinedToValues.scala` (trait) + `object toValues extends RefinedToValues` in
   both version-specific `RefinedSupport.scala`.
4. Tests per module (`modules/<m>/src/test/...`): with `import <M>.toValues._`, the default renders the text above
   and the field is not required. Without the import, the error stays (pin it, so nobody moves the instances into
   the main object). For joda and cats, a precedence test with the bridge also imported, in `modules/play-json`
   tests (which can depend on joda only if the build allows; else in the joda module with a hand-written lexical
   `toValue[T]` standing in for a bridge) pins which wins on 2.12/2.13/3.
5. Docs: README "joda-time" / "cats" / "refined" sections (~L751, ~L777, ~L800) and the paradox pages: one line
   each, "for default values without a JSON-library bridge: `import ...toValues._`".
6. Gates: `clean test` on the three versions; `rootJS/test` (cats, refined are cross-built); `scalafmtCheckAll` both.
Compatibility: additive (new nested objects), no output change, not breaking under PVP (0.9.x).

## Compatibility (Phase 1; Phase 2 above)
- Source: additive for everything except maps. Code that compiled before resolves the same `ToValue` as before,
  because a lexical (imported/local) instance precedes the companion's (measured on 2.13 and 3.3). Code that didn't
  compile now does. **The exception (B18-R1, 2.13 and 3.3 only):** a `Map[K, V]` default whose key isn't
  `String`/`Int`/`Long`/`Short`/`Byte`/`Char` (an enum, `UUID`, `Boolean`) compiled through `ArrV` and stops compiling.
  It only ever produced `"default": []` or a `ClassCastException`, so it's a fix of an invalid schema, stated in the PR.
  An explicit `ToValue.ArrV[...]` call still compiles (deprecated).
- Binary: new members in `ToValue`'s companion and its `LowPriority*Implicits` traits. `ArrV` keeps its signature
  (no longer implicit, deprecated), so 0.9.0-compiled expansions still link. Nothing is removed. Those traits exist
  to be mixed into `object ToValue`, and a user class that mixed them in would need recompiling. MiMa (if run)
  reports only additions.
- Output: for code that compiles today, the only change is `Map[Long|Short|Byte|Char, V]` defaults on 2.13/3.3:
  `= Map.empty` goes from `"default": []` to `{}`, and a non-empty map goes from a run-time `ClassCastException` to
  `{"1":2}`. There's no golden diff (no golden model has one). Everything else is new output where there was a compile error.
- PVP: not breaking in the API sense (additions plus a deprecation; the map change fixes output that was invalid), so
  it fits 0.9.x (0.9.1). If the owner counts the `Map[<enum>, V]` source change as breaking, it's 0.10.0; the lead decides.
- Legacy profile: profiles don't touch `default`. The map fix applies under Legacy too (lead policy: Legacy freezes
  style, not bugs), and no `golden-legacy` file has such a default.
- Core design rule (Schema → `$ref`, Predef → inlined): untouched. `ToValue` is independent of it.

## Review log
Lead L1 (2026-11-23): cover the joda/cats/refined module defaults in this doc as a separate follow-up phase, with
the opt-in import object option. → done: **Phase 2** (measured: all three fail on 2.13/3.3 today).
`java.util.Optional` defaults go to the backlog (E8 follow-up), not B18.

Round 1 (r-b18, on f59dbe3):
- B18-R1 (must) `ArrV` matches `Map[Long, V]`: today `[]` or a `ClassCastException`, and A would widen it → accepted.
  Problem section, option A (narrowed `IterableV` plus `Long`/`Short`/`Byte`/`Char` key instances, measured with a
  mimic on 2.12/2.13/3.3 in `notes/scratch/c-b18/arrv`, `arrv-neg`), out-of-scope bullet, plan steps 1/3/4, and
  Compatibility (the source change for enum/`UUID`-keyed map defaults, the output change for primitive-keyed ones).
  One addition beyond the comment: changing `ArrV`'s parameter list in place would break binary compatibility for
  0.9.0-compiled expansions, so `ArrV` stays as a deprecated non-implicit forwarder and the narrowed instance is
  `IterableV`. I chose instances for the primitive keys over plain narrowing, so `Map[Long, Int] = Map.empty`
  keeps compiling (now `{}`) instead of becoming a source break.
- B18-R2 (must) the pairing test didn't enumerate → accepted. It reflects over `json.schema.Predef$`'s methods
  returning `Predef` and compares their names with the spec's `name -> implicitly[ToValue[X]]` map (names, because
  Scala erases primitive type arguments in generic signatures to `Object`).
- B18-R3 (must) a durable guard against a `val` → accepted. `ToValueFieldsSpec` in core (JVM, CI) checks the
  `ToValue`-typed fields of `ToValue$` against an allow-list. The `jsLinkCheck` project is listed as the considered
  alternative, and the one-off link check stays.
- B18-R4 (nit) merge order with B17 → accepted: a `= null` test (TRIAGE-A to B17 until it merges) plus a line in Risks.
- B18-R5 (nit) `js.Date` platform text → accepted as a listed per-type alternative (lost, with the reason).
- B18-R6 (nit) C covers more than B17 decided → accepted: a paragraph under option C with non-null user-type examples.

Round 2 (r-b18, on a282f05; R1–R6 resolved):
- B18-R7 (must) `Predef` is a value class, so the pairing reflection must look for `json.Schema` return types → accepted:
  filter on `json.Schema`, drop `$`-names and `apply`/`unapply`, and check the set on all three versions.
- B18-R8 (nit) P2-A precedence wording → accepted: "expected under 2.12/2.13/3.3 rules", newer Scala 3 given
  prioritization noted, and either winner is acceptable for an opt-in import.
