# Boxed `java.lang` numbers/Boolean and `java.math` types  (issue #473, item E4)
Status: draft

## Problem
Core has Predefs for `Int`, `Long`, `Short`, `Byte`, `Double`, `Float`, `Boolean`, `BigDecimal` and `BigInt`
(`core/src/main/scala/json/schema/Predef.scala:35-43`), but none for their Java counterparts. None of these compile:

```scala
case class Boxed(i: java.lang.Integer, l: java.lang.Long, d: java.lang.Double, f: java.lang.Float,
                 s: java.lang.Short, b: java.lang.Byte, z: java.lang.Boolean)
case class BigMath(d: java.math.BigDecimal, i: java.math.BigInteger)
case class BoxedDefault(i: java.lang.Integer = 5)

Json.schema[java.lang.Integer]          // and the other six boxed types
Json.schema[java.math.BigDecimal]       // and java.math.BigInteger
Json.schema[Boxed]; Json.schema[BigMath]; Json.schema[BoxedDefault]
Json.schema[Map[java.lang.Integer, String]]
Json.schema[java.lang.Integer].withValidation(`minimum` := 1)
```

Repro: `api/src/test/scala/com/github/andyglow/jsonschema/BoxedBigMathSpec.scala` (14 `assertCompiles` tests), run on
2.12.21, 2.13.18 and 3.3.9 (JVM): **14 of 14 fail on each**. The errors:

| case | Scala 2.12 / 2.13 | Scala 3.3 |
|---|---|---|
| root / field | `schema for Integer is not supported, ` (`Long`, `Double`, ... unqualified) / `schema for java.math.BigDecimal is not supported, ` | `schema for java.lang.Integer is not supported, ` / `... java.math.BigDecimal ...` |
| `= 5` default | `Can't infer json value for default value of `...BoxedDefault(..., i: Integer = 5)`` (checked before the type) | `schema for java.lang.Integer is not supported, ...BoxedDefault` |
| Map key | `a Map key must be a String, a value class of String, an enum or a type with an implicit json.Schema.dictionary.KeyPattern` | same |
| `withValidation` | (fails on the schema first) | same |

So five pieces are missing for each type: a `Predef`, a `ToValue` (defaults), a `validation.Magnet` (numeric
validations), a `KeyPattern` (map keys) and, on Scala 3, the IR's `KeyKind`. Scala.js: the same; all nine types are
in the Scala.js javalib (`java.math` included).

## Why it is a problem
Models shared with Java code, or generated from Java / OpenAPI / Avro / protobuf sources, use boxed types (often for
nullable numbers) and `java.math.BigDecimal` (money). Users hit a compile error and have to write nine Predefs by hand,
with casts (the `integer` and `boolean` nodes are `Schema[Int]` / `Schema[Boolean]`; `number[T]` needs a `Numeric[T]`,
which the Java types don't have). The README's "Supported types" list doesn't mention them either way.
gaps-types.md, checklist "Primitives & boxed" and ranked gap 4/5.

## Options

### A. Monomorphic instances next to the Scala ones, reusing the Scala type's schema  — WON
For each type, one `implicit val` in the same object as the Scala equivalent:

```scala
// json.schema.Predef
implicit val jIntegerS: Predef[java.lang.Integer]       = Predef(intS.schema.asInstanceOf[Schema[java.lang.Integer]])
implicit val jLongS: Predef[java.lang.Long]             = Predef(longS.schema.asInstanceOf[Schema[java.lang.Long]])
// ... jShortS, jByteS, jDoubleS, jFloatS, jBooleanS (from boolS)
implicit val jBigDecimalS: Predef[java.math.BigDecimal] = Predef(bigDecimalS.schema.asInstanceOf[Schema[java.math.BigDecimal]])
implicit val jBigIntegerS: Predef[java.math.BigInteger] = Predef(bigIntS.schema.asInstanceOf[Schema[java.math.BigInteger]])
```

- The cast is sound: a schema's type parameter is phantom for these nodes (`integer`, `boolean`, `number` hold no `T`
  values; `number`'s `Numeric` evidence is never used after construction), and the node is the very one the Scala type
  gets, so trees compare equal (`Json.schema[java.lang.Long] == Json.schema[Long]`).
- **Follows the Scala type by construction**: D2 (integer types) will change `longS` / `bigIntS`; the boxed and
  `java.math` Predefs then change with them, with no second edit and no chance to drift.
- `ToValue` (`com.github.andyglow.json.ToValue`, low-priority primitives): `JIntegerV`, ..., `JBooleanV`,
  `JBigDecimalV`, `JBigIntegerV`, each converting **exactly** to the `Value` the Scala type gives, through the Scala
  type's own `num` overload: `num(x.intValue)`, `num(x.longValue)`, `num(x.shortValue)`, `num(x.byteValue)`,
  `num(x.doubleValue)`, `num(x.floatValue)`, `bool(x.booleanValue)`, `num(BigDecimal(x))`, `num(BigInt(x))`. Never the
  generic `num(x: Number)`: it goes through `doubleValue`, which loses `Long.MaxValue` and every 20-digit `java.math`
  value (E4-R2). The bridges' `ToValue`s (from `Writes` / `Encoder` / ...) are imported, so they win over these, as
  they already do over `IntV`.
- `null` defaults (E4-R1). A boxed field can default to `null` (`i: java.lang.Integer = null`, common in Java-shaped
  models). The plan: the `ToValue`s map `null` to `Value.null`, so the field renders `"default": null`. Drawback: that
  default doesn't validate against the field's own `"type": "integer"` (JSON Schema says a default SHOULD), so a tool
  that applies defaults would insert an invalid value. Still better than today's alternative for a naive `ToValue`
  (an NPE in the macro's generated code at schema construction).
  The other real choice is **omit `default` when the value is `null`**: the field then just isn't required (it has a
  default) and has no `default` keyword. That needs a macro change in both macros (emit `Option(x).map(toV)` instead of
  `Some(toV(x))`, or let `ToValue` say "absent"), i.e. a change to how every default is rendered, for an edge case; so
  it's left as a follow-up the owner can pick. This choice is E4's own: it doesn't follow `String`, whose `= null`
  default renders `"default": "null"` (a JSON string, a separate bug r-e4 reported to the lead).
- `Magnet` (`json.schema.validation.Magnet`): one monomorphic `Magnet[X, Number]` per numeric type, like `chrMagnet`.
  Not a generic `[X <: java.lang.Number]` magnet: `scala.BigDecimal` / `BigInt` extend `java.lang.Number`, so it would be
  ambiguous with `numericMagnet` for them.
- `KeyPattern` (`json.Schema.dictionary.KeyPattern`): `java.lang.Integer/Long/Short/Byte`, each with the pattern of the
  Scala type's object (`extends KeyPattern[java.lang.Integer](IntKeyPattern.pattern)`), so B6's pattern fix applies to
  them too. Not for `Double` / `Float` / `BigDecimal` / `BigInteger` / `Boolean`: their Scala types have no
  `KeyPattern` today (B6 adds `Boolean`; whichever of B6/E4 merges second adds the `java.lang.Boolean` one, see plan 4).
- Scala 3 IR: `Collections.kindOf` classifies the four boxed integrals as `KeyKind.Integral` (today they would be
  `Custom`). Output doesn't depend on it (the IR keeps it for later profiles), but it should say the truth.

Pros: additive, no macro change for the main fix, one place per concern, follows D2 automatically.
Cons: nine casts (contained in core, invisible to users); about 30 new implicit members across four objects.

### B. Fresh nodes per type (no cast for `number`)  — LOST
`Predef(`number`[java.lang.Long])` etc., with `Numeric` instances for the Java types (private, built from the Scala
ones, e.g. via `Numeric.LongIsIntegral` and boxing), and still a cast for `integer` / `boolean` (fixed to `Int` /
`Boolean`). Pros: `number[java.lang.Long]` is honestly typed. Cons: nine `Numeric` instances (about 150 lines of
boilerplate, or casts there instead), and the Java Predefs no longer follow the Scala ones: D2 would have to change both
places, and nothing checks they agree. The type honesty buys nothing at run time.

### C. One generic "boxing" rule  — LOST
A new type class `Boxing[J, S]` (instances for the nine pairs) and
`implicit def boxedS[J, S](implicit b: Boxing[J, S], p: Predef[S]): Predef[J]` (plus the same for `ToValue`, `Magnet`,
`KeyPattern`). Pros: picks up a user's own `Predef[Long]` from lexical scope for `java.lang.Long` too. Cons: a new public
type class; a generic implicit def is tried for every type the macros look up (both macros search `Predef[T]` for every
field type), with `S` inferred from `J`, which is fragile on Scala 3 (`TT >: T`-style open type parameters already
caused ambiguities, see `Magnet`); a user overriding `Predef[Long]` but not `Predef[java.lang.Long]` is rare and can
override the latter just as well.

### D. Map the Java types to Scala types inside both macros  — LOST
Before the implicit lookup, rewrite `java.lang.Integer` to `Int` etc. in Scala 2's `UImplicits` and Scala 3's
resolution. Pros: covers every derived position at once. Cons: two macros to change (and `clean` hazards), `Json.schema`
of the root type would still need special-casing, user-defined `Schema[java.lang.Integer]` (step 1 of the core rule)
would be silently shadowed by `Schema[Int]` lookups or the other way round, and it breaks the documented model that
built-ins are just Predefs a user can read and override.

## Decision
A: monomorphic `Predef`, `ToValue`, `Magnet` and `KeyPattern` instances beside the Scala ones, each Predef reusing its
Scala counterpart's schema node (so the boxed and `java.math` types always render exactly like `Int`, `Long`, ...,
`BigDecimal`, `BigInt`, now and after D2), plus the Scala 3 `KeyKind` classification.

## Implementation plan
1. `core/src/main/scala/json/schema/Predef.scala`: `jIntegerS`, `jLongS`, `jShortS`, `jByteS`, `jDoubleS`, `jFloatS`,
   `jBooleanS`, `jBigDecimalS`, `jBigIntegerS` (option A's form), after the Scala ones. Today's output:
   `java.lang.Integer` → `integer`; `Long`/`Short`/`Byte`/`Double`/`Float` → `number`; `Boolean` → `boolean`;
   `java.math.BigDecimal`, `java.math.BigInteger` → `number`.
2. `core/src/main/scala/com/github/andyglow/json/ToValue.scala` (`LowPriorityPrimitiveImplicits`): the nine `ToValue`s,
   exact (per-type `num` overloads, never `num(x: Number)`), `null` → `Value.null`.
3. `core/src/main/scala/json/schema/validation/Magnet.scala`: eight numeric magnets (`Boolean` has no validations).
4. `core/src/main/scala/json/Schema.scala` (`KeyPattern`): `JIntegerKeyPattern`, `JLongKeyPattern`,
   `JShortKeyPattern`, `JByteKeyPattern` delegating to the Scala objects' `pattern`. If B6 has merged by then, also
   `JBooleanKeyPattern` from B6's `BooleanKeyPattern`; otherwise note it for B6 (mail the lead).
5. `core/src/main/scala-3/.../derivation/Collections.scala` `kindOf`: boxed integrals → `KeyKind.Integral`. `clean` before testing.
6. Tests (shared, all three Scala versions, JVM + JS): turn `BoxedBigMathSpec` from the repro into the spec:
   - each type at the root equals its Scala counterpart's tree (`Json.schema[java.lang.Long] shouldEqual Json.schema[Long]`)
     and the expected node (`integer` / `number` / `boolean`). These nine equalities are **the D2 guard** (E4-R6): D2
     must change the Scala Predefs (or the nodes), not special-case the Scala types in the macros or renderers, and these
     tests fail if the two ever drift;
   - golden `shared/boxed/Boxed`, `shared/boxed/BigMath` (case classes with all nine as fields, `Option[java.lang.Integer]`,
     `List[java.math.BigDecimal]`), all drafts, Neutral and Legacy (new files only);
   - defaults: `java.lang.Integer = 5`, `java.lang.Long = 5L`, `java.lang.Double = 1.5`, `java.lang.Boolean = true`,
     `java.math.BigDecimal = new java.math.BigDecimal("12.50")`, `java.math.BigInteger = java.math.BigInteger.TEN`,
     and `java.lang.Integer = null` (renders `"default": null`); compared with the Scala types' defaults;
   - exact defaults at values a `Double` can't hold (E4-R2), each compared with the Scala type's default rendering:
     `java.lang.Long = Long.MaxValue` (`9223372036854775807`), `java.lang.Float = 0.1f` (as `Float` renders it),
     `new java.math.BigDecimal("12345678901234567890.123")`, `new java.math.BigInteger("123456789012345678901234567890")`.
     Shared spec, so it runs on JVM and JS (where `Decimals` is shimmed);
   - Option defaults (E4-R4): `o: Option[java.lang.Integer] = Some(5)` and `= None` (Scala 2's `c.eval` path with
     `int2Integer` in the tree). If `Some(5)` hits the known Scala 2 re-parse crash (TRIAGE-A, `FieldsCharSpec` F4), the
     test pins that as for `Option[String]`, not as an E4 failure;
   - Map keys through both routes (E4-R3), for all four boxed integrals: `Map[java.lang.Integer, String]` (core
     `Predef.dictS`) and `Map[java.lang.Integer, SomeCaseClass]` (each macro's dictionary family: Scala 2
     `UDictionaries`, Scala 3 `Collections`, where `kindOf` applies), each with the same key pattern as the `Int` /
     `Long` / ... map;
   - `withValidation(minimum := 1, maximum := 10)` compiles and records the validations for each numeric type
     (in `ValidationMagnetSpec`), and that spec also covers `Int` / `Long` / `scala.BigDecimal` / `BigInt` numeric
     validations, so new magnets making those ambiguous on Scala 3 would show (E4-R5). On Scala 3 boxed validations
     already compiled through `identityMagnet[java.lang.Number]`; with the new magnets the output is unchanged.
   - a bridge check: `import AsCirce._` / `AsPlay._` defaults for a boxed field still compile (bridge `ToValue` wins, no
     ambiguity), in the circe and play modules' tests if the library has an encoder for the type (circe has
     `Encoder[java.lang.Integer]`, `Encoder[java.math.BigDecimal]`).
7. Docs: README "Supported types" table and `docs/src/main/paradox/types.md`: add the boxed types to the `Boolean` /
   `Int` / `number` rows and `java.math.BigDecimal` / `BigInteger` to the `number` row; the map-key sentence gets the
   boxed integrals. (No CHANGELOG edit, per protocol.)

Not in scope (noted for the lead): Scala 2 prints the boxed types unqualified in errors (`schema for Integer`), B11.
`Schema.enum.of(...)` with Java numbers (`ValueAdapter` needs a `Numeric`): unchanged. `java.lang.Character` already has
a Predef but no `KeyPattern` (not in E4's list; one line if the reviewer wants it).

## Compatibility
- Source: additive. A user's own `implicit Predef/ToValue/KeyPattern` for one of these types in lexical scope still wins
  over the companion's (both Scala versions search lexical scope first); a user `Schema[java.lang.Integer]` still becomes
  a definition (core rule step 1, unchanged). No public signature changes. Binary: new members on objects only.
- Output: only code that didn't compile before gets output. No existing golden file changes; Legacy untouched
  (golden-legacy gets new files only).
- PVP: non-breaking addition, fits 0.9.x.
- Core design rule unchanged: the new built-ins are Predefs, so they are inlined, like the Scala ones.

## Review log
