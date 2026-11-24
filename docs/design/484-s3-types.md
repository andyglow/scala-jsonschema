# Union, opaque, literal and singleton types; intersection types  (issue #484, item E6)
Status: approved (r-e6, 2026-11-23)

Depends on #470 (B3): unions use its `Schema.anyof` node. This branch is stacked on `gap/b3-sealed-collapse` (lead,
2026-11-23); it is rebased onto `master` once #470 merges.

## Problem

The derivation has no rule for these type forms, so each one falls through to `schema for X is not supported`:

| form | Scala | example | today (compile error) |
|---|---|---|---|
| union | 3 | `Int \| String`, field `x: A \| B` | `schema for scala.Int \| scala.Predef.String is not supported` |
| union of literals | 3 | `"a" \| "b"` | `schema for "a" \| "b" is not supported` |
| opaque type | 3 | `object O { opaque type Id = String }`, `Json.schema[O.Id]`, field `id: O.Id` | `schema for ...O.Id is not supported` (no hint that a `given Predef[O.Id]` fixes it) |
| intersection | 3 | `A & B` | `schema for A & B is not supported` |
| literal type | 2.13, 3 | `"a"`, `1`, field `kind: "circle"` | `schema for "a" is not supported`, `schema for "circle" is not supported, ...L` |
| case-object singleton type | 2.12, 2.13, 3 | `Red.type`, field `color: Red.type`, `None.type` | `schema for ...Red.type is not supported` (Scala 3: `...Red is not supported`, `scala.None is not supported`) |

Repro (committed, all fail today; JVM, 2.12.21 / 2.13.18 / 3.3.9 as applicable; Scala.js is the same code path):
- `api/src/test/scala-3/.../types/Scala3TypesSpec.scala`: union of primitives, union of case classes as fields,
  union of two members of a sealed family (`S1 | S2`: no family claims it either), union of literals, opaque type,
  opaque field, intersection (7 failures);
- `api/src/test/scala-2.13+/.../types/LiteralTypesSpec.scala`: `"a"`, `1`, `case class L(kind: "circle", n: 1)` (3 failures on 2.13 and 3);
- `api/src/test/scala/.../types/SingletonTypesSpec.scala`: `Red.type`, `case class Paint(color: Red.type)`,
  `None.type` (3 failures on 2.12 and 3).

What does compile today (r-e6's probes, 3.3.9 unless noted):
- `Json.schema[Color.Red.type]` for a Scala 3 `enum Color { case Red, Green, Blue }` renders the **whole** enum
  (`{"type":"string","enum":["Red","Green","Blue"]}`), as a root and as a field: `classSymbol` widens the `TermRef`
  to `Color`. The value's own singleton type is lost.
- A `given Predef`/`Schema` for an intersection or a literal type works (implicits come first).
- A `given Predef[Int | String]` or `given Schema[Pet]` (`type Pet = Int | String`) **aborts**: `lookupImplicit`
  computes `definitionName(tpe)` for every implicit it finds, the inlined `Predef` path included, and `signature`
  has no case for `OrType` ("a definition can't be named after this type"). `Json.sig[Int | String]` aborts the same way.
- `WeekDay.Mon.type` (an `Enumeration` value) and a field `x: X.type` of a standalone case object: not supported
  (2.13 and 3).

Two more things are in the way once the schema itself is derived:
- **Defaults.** A field default is converted with an implicit `ToValue[FieldType]`, which is invariant: there is no
  `ToValue["circle"]` and no `ToValue[O.Id]`, so `kind: "circle" = "circle"` and `id: O.Id = O.Id("x")` would fail
  with "Please provide `ToValue[...]`" (Scala 3 `Products.defaultValue`, Scala 2 `UProductTypes` ~L122).
- **refined is an opaque type on Scala 3.** `eu.timepit.refined.api.Refined[T, P]` is `opaque type Refined[T, P] = T`
  in refined_3 (no `Refined` class in the jar, only `Refined$package`). Iron's `IronType[A, C]` is the same. A naive
  "opaque → underlying" would turn `Json.schema[Int Refined Positive]` without the refined module into a plain
  `integer` (the predicate silently dropped), and it would hide the error that README documents for an
  unsupported predicate ("the derivation's `is not supported` error followed by refined's own message").

## Why it is a problem

These are ordinary Scala 3 model types: unions for "either of these" fields, literal types and case objects for
fixed tags (`kind: "circle"`), opaque types as the idiomatic Scala 3 newtype (where Scala 2 code had value classes,
which the library unwraps). Users have to write a `given Predef` for each one by hand, and for opaque types the error
doesn't even say so. README and the paradox Scala 3 page list them as "Not supported (yet)". The singleton-type part
is the remainder of #291 (B11 fixed only the message).

## Options

### Unions

#### U-A. `anyOf` of the members  — WON
A new family in the Scala 3 derivation, tried first: an `OrType` is flattened (left to right, nested unions
included) into its members, each resolved with the full core rule (an implicit `Schema` member becomes a definition +
`$ref`, a `Predef` is inlined, otherwise derived). Then:
- literal and standalone-singleton members (see L-A) of the same JSON type merge into one `enum` (at the position
  of the first); singleton members of the same enum family merge into that family restricted to those values;
- `Null` members become `Schema.null`;
- exactly one member left: that member (`"a" | "b"` is just `{"type":"string","enum":["a","b"]}`);
- exactly one member X plus `null` (an `Option[X] | Null` counts as `X | Null`: every library writes `None` as
  `null`): `oneof{X, null}`, which the renderer already writes as a nullable X (`"type": [X, "null"]`, the same
  shape as `Option[X]` under `OptionStyle.Nullable`), **unless X itself accepts `null`** (a user `Predef`/`Schema`
  or a definition whose body is nullable, a `$ref`, an `option`, a nested union with `null`): then `anyof{X, null}`,
  since an exclusive `oneOf` would reject `null`. X's schema is only known at runtime for an implicit, so the check
  is a small runtime function in core (`com.github.andyglow.jsonschema.Unions.orNull(x)`; unknown or `$ref`
  members count as nullable, i.e. `anyOf`, the safe side) (E6-R5);
- otherwise `Schema.anyof(members)` in declaration order (B3's node; `anyof` states a shared JSON type at its root).
  An `Option[A]` member stays an `option` node, lowered by the profile inside the `anyOf`.

**Overriding a union with a given** (E6-R1). The core rule applies to a union like to any type, and it has to work:
- an implicit `Predef[A | B]` is inlined without computing a name: `lookupImplicit` computes `definitionName`
  only for a user `Schema` (the `Predef` path never used it);
- an implicit `Schema[A | B]` becomes a definition named after the union: `Signatures` gets an `OrType` case, the
  members' signatures joined by ` | ` (`scala.Int | scala.Predef.String`; `Json.sig` too). The renderers sanitize
  definition names per draft already. An alias (`type Pet = Int | String`) is dealiased like every type the
  derivation resolves, so the name is the union's, not `Pet`'s (as for `type Users = List[User]`); README says to use
  an opaque type or a case class for a name of your own.

**Union-typed defaults** (E6-R6). A field `x: Int | String = 1` has no `ToValue[Int | String]`. Without a user
`ToValue[A | B]`, the macro generates a runtime match over the members: a literal member by equality
(`case v if v == "a"`), a class member without type arguments by type (`case v: Int => ToValue[Int](v)`), each with
its member's `ToValue`. A union with a generic member (`List[Int]`: an unchecked type test) or a member without a
`ToValue` keeps today's error, which names the fix (`provide a ToValue[A | B]`).

```json
Json.schema[Int | String]            {"anyOf": [{"type": "integer"}, {"type": "string"}]}
Json.schema["a" | "b" | Int]         {"anyOf": [{"type": "string", "enum": ["a", "b"]}, {"type": "integer"}]}
Json.schema[String | Null]           {"type": ["string", "null"]}
case class U(x: A | B)               "x": {"type": "object", "anyOf": [{...A...}, {...B...}]}
```

A Scala union is untagged and inclusive: a value of `Int | Long` is both. `anyOf` says exactly that. When the members
are disjoint, `anyOf` and `oneOf` accept the same documents, so `anyOf` is never wrong. No JSON library derives
codecs for unions (circe, play, spray, json4s, upickle: none), so a user codec writes the bare value, and there is no
profile knob to add.
- Pros: always valid; simple (no render-time decision); reuses B3's node and renderer.
- Cons: a disjoint union (`Int | String`) says `anyOf` where a sealed family with the same members would say `oneOf`.
  Code generators that prefer `oneOf` see a weaker statement.

#### U-B. `oneOf` when the members are provably disjoint, `anyOf` otherwise  — LOST
The same as U-A, but decided at render time with B3's `Disjoint.overlapping` check and its `overlappingMembers`
knob, so `Int | String` says `oneOf` like a sealed family would. The check must run on lowered members (they may hold
`option` / `family` nodes), so the macro can't decide: it needs a new public model node (`Schema.union`) that every
profile lowers, plus renderer/AsTree/equality/DefinitionNames support for it.
- Pros: the most precise statement; consistent with B3's families.
- Cons: a new public node for no change in what validates; more code in every walker. Can be added later without
  breaking U-A's output for overlapping unions.

#### U-C. Always `oneOf`  — LOST
Today's node, no dependency on #470. Wrong whenever members overlap: `Int | Long`, `A | B` with optional fields,
`String | Char`: `5` matches both members of `Int | Long`, so `oneOf` rejects it (B3's problem 2, for unions).

### Opaque types

#### O-A. The underlying type's schema, except for refinement-style opaque types  — WON
An opaque type is reached only after the implicit search (step 3 of the core rule) found nothing, so a user
`given Predef[Id]` / `Schema[Id]` still wins, as for any type. A new family (Scala 3) takes a `TypeRef` whose symbol
is an opaque alias (`isOpaqueAlias`), or an `AppliedType` of one, and resolves its right-hand side
(`translucentSuperType`, applied to the arguments) like a type alias: inlined, the same output as the underlying type.
(Inside the opaque type's own scope `dealias` already sees through it; that works today and doesn't change.)

Two guards keep refinement types from losing their constraint silently. The opaque type is **not** derived, and the
error says to provide a `given Predef`/`Schema` (or the library's support module), when:
1. it has a **phantom type parameter**, one that its right-hand side doesn't mention: refined's
   `Refined[T, P] = T`, iron's `IronType[A, C] = A`. The parameter is there to carry a constraint that the
   underlying schema can't express. (`opaque type Tagged[A] = List[A]` is derived: every parameter is used.)
2. an implicit for it **was found but failed** (`implicitFailures`, e.g. refined's macro on an unsupported
   predicate): that failure is reported, as today.

```
object O { opaque type Id = String }; Json.schema[O.Id]       {"type": "string"}
case class W(id: O.Id)                                        "id": {"type": "string"}
Json.schema[Int Refined Positive]  (no refined import)        error: schema for Refined[Int, Positive] is not supported: it is an opaque
                                                              type with a phantom type parameter (a refinement); provide a given
                                                              json.schema.Predef[...] or json.Schema[...], or the library's support module
```
- Pros: the Scala 3 newtype works like a Scala 2 value class (unwrapped); no change for users who already have a
  given; refined/iron keep failing loudly.
- Cons: the guard is a heuristic. A codec that writes an opaque type differently from its underlying type (e.g.
  `opaque type Ts = Long` written as an ISO string) needs a given, exactly as a value class with a custom codec does
  today. An opaque type with a phantom parameter that is *not* a refinement (`opaque type Id[Owner] = String`) needs
  a given too.

#### O-B. Keep requiring a given; better error  — LOST
No derivation; the error names the fix: "`O.Id` is an opaque type: provide a `given json.schema.Predef[O.Id]`
(inlined) or `json.Schema[O.Id]` (a definition)".
- Pros: never guesses; tiny change.
- Cons: every opaque newtype needs boilerplate, where the equivalent value class works out of the box. The owner's
  item lists "→ underlying" first.

#### O-C. The underlying type's schema, as a definition named after the opaque type  — LOST
Like `derives`: `{"$ref": "#/definitions/...O.Id"}`. Breaks the core rule's symmetry: only an implicit `Schema`
makes a definition. Value classes, the closest analogue, are inlined.

### Literal types and singleton types of case objects

#### L-A. A single-value `enum`  — WON
- Literal type (`ConstantType`): `{"type": <T>, "enum": [v]}`, where `<T>` is the JSON type of the widened type's
  built-in `Predef` (`String`/`Char` → `string`, `Int` → `integer`, `Long`/`Float`/`Double` → `number` as the
  Predefs say today, so D2 changes literals together with the plain types; `Boolean` → `boolean`), and `v` the
  constant (`Value.str`, `Value.num`, `Value.bool`; a `Char` literal is a one-character string). A `Float` literal's
  value is built from its decimal string, `BigDecimal(f.toString)`: `Value.num(0.1f)` widens to
  `0.10000000149011612`, and a single-value enum of that rejects the `0.1` that play and circe write (E6-R4).
- Singleton type of a value **that belongs to an enum** (E6-R2, E6-R3): a case object of an all-object sealed
  family, a Scala 3 `enum` singleton case (`Color.Red.type`), an `Enumeration` value (`WeekDay.Mon.type`), a Java
  enum constant: the enum family derived as today, **restricted to that one value**. So the value is exactly what its
  family renders for it: the name, or `ToValue[Root](obj)` with the `@typeHint` schema when the family has a
  `ToValue`; and a sealed family / Scala 3 enum stays a `family` node, shaped by the profile like the whole enum
  (member naming included). `Color.Red.type` → `{"type":"string","enum":["Red"]}` (today: the whole enum, see
  Compatibility); `WeekDay.Mon.type` → `["Mon"]` (the val name, as the Enumeration family writes it).
- Singleton type of any **other** case object (standalone, or a member of a mixed family of case classes and
  objects): `{"type": "string", "enum": ["Red"]}`, the object's name, as an all-object family renders it under the
  default profiles (`SingletonStyle.AsString`). Not profile-shaped: it isn't a family (see L-C). `scala.Enumeration`
  objects (`WeekDay.type`) keep their current meaning (the Enumeration's values; the enum family is tried first).
- `None.type` is the exception: `{"type": "null"}`. Every JSON library writes `None` as `null` (or drops the key);
  the name `"None"` only ever came from 0.7 deriving `Option` as a sealed family.
- A field default on a literal-typed field uses the literal's own value (no `ToValue["circle"]` needed).
- In a union, these merge (U-A): literals and standalone singletons per JSON type (`"a" | "b"` → one enum),
  values of one enum family into that family restricted to them (`Red.type | Green.type` → `["Red", "Green"]`).
- Scala 2: the same, in a new `ULiteralTypes` trait of the cake (ConstantType on 2.13; `SingleType` of a case object,
  `Enumeration` value or Java constant on 2.12 and 2.13, reusing `UEnums`' items for the family case), so 2.13 and 3
  agree byte for byte.
- Pros: valid on every draft (draft-04 has no `const`, and our draft-04 renderer writes `const` anyway: known issue
  4b); the same shape as the case objects of an enum family; merges naturally in unions.
- Cons: a standalone case object under the circe/play bridges is written as `{}` by those libraries, not `"Red"`;
  the profile doesn't change a standalone singleton (it's not a family). Rare (a standalone case object as a field
  type); a `given Predef` covers it.

#### L-B. `const`  — LOST
`{"const": "a"}`. Shorter, and the model node exists, but draft-04 has no `const` (the draft-04 renderer would need
fixing first), and a union of literals would need an extra merge rule anyway.

#### L-C. Every singleton type as a family of one, lowered by the profile  — LOST
Emit a `family` node with one `Singleton` member for *any* case object, so every profile renders `Red.type` the way
it renders the object inside a family (`AsTagged` → `{"type": {"enum": ["Red"]}}`, circe's wrapper object, ...).
A standalone case object (or one in a mixed family) isn't encoded like an enum member by any library (circe-generic
and play write a standalone one as `{}`), so no profile would be right, and literal types would still need L-A. L-A
does take this route where it is right: a value of an all-object family is rendered by its own (restricted) family.

### Intersection types

#### I-A. A clear error  — WON
`A & B` (`AndType`) that no implicit covers is a compile error that says why and what to do:
`schema for A & B is not supported: intersection types have no derived schema; provide a given json.schema.Predef[A & B] or json.Schema[A & B]`.
A user `given` already works today (it's found before the structural step).
- Pros: honest. The members of a derivable intersection are almost never derivable: case classes are final in
  practice (`A & B` of two case classes is empty), and traits aren't derived at all.
- Cons: tagged-type encodings (`String & UserIdTag`) need a given.

#### I-B. `allOf` of the members  — LOST
Derived objects are closed (`additionalProperties: false`), so `allOf[{a}, {b}]` accepts no document that has
both `a` and `b`, i.e. nothing a codec would write. It is only right when every member is an open user schema, which
the macro can't tell.

#### I-C. The schema of the one member that has one  — LOST
For tagged types: `String & Tag` → `string` when exactly one member is derivable or has an implicit. A guess about
which member carries the representation, and it needs a "does this member have a schema?" probe that today aborts
the expansion on failure. Possible later as an addition.

## Decision

Unions become `anyOf` of their members (literals merged into enums, `X | Null` as a nullable X when X can't be
null), and a given `Predef`/`Schema` for a union works; opaque types are derived as their underlying type unless
they look like a refinement (phantom type parameter) or their implicit failed; literal types and singleton types
become single-value enums on Scala 2 and 3 (an enum's value through its own family, `ToValue` included;
`None.type` → `null`); intersection types get a clear error that names the fix.

## Implementation plan

1. **Scala 3 IR** (`core/src/main/scala-3/.../derivation`): a new file `Unions.scala` (union family + `Union` node:
   members `List[Node]`, emits the rules of U-A), a new file `Literals.scala` (literal/singleton family + `Literal`
   node: `tpe`, the widened type, the `Expr[Value]`; emits `Schema.enum(Predef[W].schema, ListSet(v))`, or
   `Schema.null` for `None.type`), a new file `Opaques.scala` (opaque family; returns the underlying's node, or the
   guarded error). Register them in `Resolve.families` **before** `deriveOption` (a union of members of one sealed
   family must not reach `deriveSum`; `Red.type` must not reach the product/value-class families). The intersection
   message goes into `Resolve.structural`'s fallback (an `AndType` case). Singletons of an enum: `Literals` asks the
   enum family (`Enums.scala`) for the widened type's items and keeps the matching one (by symbol); `Unions` merges
   restricted families of the same enum.
2. **Implicits / names** (E6-R1): `Implicits.lookupImplicit` computes `definitionName` only for `UserSchema`;
   `Signatures.sig` gets `case OrType(a, b) => sig(a) + " | " + sig(b)`.
3. **`X | Null`** (E6-R5): a runtime helper `com.github.andyglow.jsonschema.Unions.orNull(x: Schema[_]): Schema[_]`
   in core (shared, not public API by convention like `AsDraftSupport`): `oneof{x, null}` when `x` provably rejects
   `null`, else `anyof{x, null}`.
4. **Defaults** (Scala 3 `Products.defaultValue`, Scala 2 `UProductTypes`): when there is no `ToValue[F]`, try the
   type's representation: a literal type uses its constant; an opaque type uses `ToValue[underlying]` on the
   getter's value (an opaque value *is* its underlying value at runtime, so a cast is safe); a union uses the
   generated match of U-A (E6-R6); otherwise today's error, with the hint `provide a ToValue[F]`.
5. **Scala 2** (`macros/src/main/scala-2`): `ULiteralTypes` (ConstantType → `SchemaType.Enum` with one value and the
   widened type's Predef as the value schema; a `SingleType` of an enum value → `UEnums`' family restricted to that
   item; any other case object → string enum of its name; `None.type` → `null`), mixed into the cake and tried in
   `Macroses.resolve` before `Dict`. Default fallback for literal fields as in 4.
6. **Tests**: rewrite the three repro specs as tree + JSON asserts (order-sensitive where order matters: the golden
   harness sorts arrays, E6-R9):
   - `Scala3TypesSpec` (3): union of primitives / case classes / a sealed family's members / literals (`"b" | "a"`
     keeps `["b","a"]`) / `A | B | C` member order / with `Null` (alone, with two members, `Option[A] | Null`,
     a member whose given `Predef` is nullable → `anyOf`) / `Option[A] | B` / nested `(A | B) | C` / union inside
     `Option` and `List` / recursive member (`case class Node(next: Node | Int)` → `$ref`) / member with an implicit
     `Schema` (definition) / overlapping `Int | Long` validated with networknt (`5` valid); a `given Predef[A | B]`
     winning over U-A; a `given Schema[A | B]` → definition `... | ...`; `Json.sig[Int | String]`; union field
     defaults (`Int | String = 1`, `"a" | "b" = "a"`; a generic member keeps the error).
     Opaque: root / field / generic `opaque type Tg[T] = T` applied (`Tg[Int]`) / `opaque type Tagged[A] = List[A]` /
     bounded `opaque type Pos <: Int = Int` / over a type with a given `Schema` (→ `$ref`) / with a given `Predef`
     (given wins) / a phantom-parameter opaque type and an iron-style nesting (`opaque type Ref[T, P] = T`,
     `opaque type Age = Ref[Int, Pos]`: the error names `Ref`, `Age` in the context) via `typeCheckErrors` (E6-R8) /
     opaque field default. Intersection error text. `Color.Red.type` root and field (`["Red"]`); a value of an enum
     family with a `given ToValue` (`const`/the hinted value, as the family renders it); `Red.type | Green.type`.
   - `LiteralTypesSpec` (2.13, 3): String, Int, Long, Double, `0.1f` (→ `0.1`), Boolean, Char literals; literal
     fields; a literal field with a default.
   - `SingletonTypesSpec` (2.12, 2.13, 3): `Red.type` root and field (standalone and in a mixed family),
     `None.type`, a case object of an all-object family with a `ToValue[Root]`, `WeekDay.Mon.type` (`["Mon"]`),
     `WeekDay.type` of an `Enumeration` unchanged, a Java enum constant.
   - refined module (3): `Int Refined Positive` with the refined import still renders as today; a `typeCheckErrors`
     check that it still fails without it.
   - Golden files: one model per form under `api/src/test/resources/golden` (union, opaque, literal, singleton),
     Neutral + every draft as the harness does; `golden-legacy/` untouched (no 0.7 model uses these types).
7. **JS**: nothing platform-specific (macro-time only, plus the small `Unions.orNull`); `rootJS/test` on 2.12, 2.13 and 3.
8. **Docs**: README "Not supported (yet)" line and the Scala 3 notes; paradox `scala3.md` ("Not supported (yet)" →
   a "Union, opaque, literal types" section) and `types.md` (literal and singleton types in "Supported types"). Both
   say the same things: the rules above, the opaque guard, how to override with a given, and what profiles **don't**
   do (E6-R7): union members are derived standalone, so a sealed family's members in a union carry no tag under a
   `Discriminator`/`WrapperObject` profile (their closed objects reject a tagged document: use the family type or a
   given); a standalone case object's `Red.type` ignores `SingletonStyle`/`withMemberName` (an enum family's value
   doesn't: it is rendered by its family); a union alias doesn't name its definition (use an opaque type or a case class).

## Compatibility

- Source/binary: no public signature changes (new code is macro-internal, plus the internal helper
  `com.github.andyglow.jsonschema.Unions`; the `anyof` node comes from #470).
- Output changes for code that compiles today (E6-R2), listed in the PR with old vs new JSON:
  - the singleton type of a Scala 3 `enum` case, `Color.Red.type`, as a root or a field: the whole enum
    (`["Red","Green","Blue"]`) → that value (`["Red"]`). A fix: the old schema accepted values the type can't hold.
  - Java enum constant singleton types, if they also render the whole enum today (to be checked; listed if so).
  Everything else here was a compile error before, except an opaque type used inside its own scope (unchanged:
  `dealias` already sees through it) and a type that a user `given` covers (unchanged: implicits come first).
  `JsonSchemaProfile.Legacy` output is unchanged (no 0.7 model has these types; `Color.Red.type` is Scala 3 only).
- PVP: an addition plus a bug fix, not breaking (C bump). Error texts for intersections, phantom opaque types and
  union defaults change (messages only; B11 #476 owns the general format, this PR adds specific texts in that format).

## Review log

Round 1 (r-e6, on 6c1e3b2):
- E6-R1 (must) a given for a union aborts (naming) → accepted: names only for a user `Schema`; `OrType` signature
  `A | B`; tests for `given Predef[A | B]` (wins over U-A), `given Schema[A | B]` (definition) and `Json.sig`. An
  alias's name is lost by dealiasing (as for any alias); docs say to use an opaque type or a case class.
- E6-R2 (must) `Color.Red.type` compiles today → accepted: listed as an output change (Compatibility, PR), tests.
  `WeekDay.Mon.type` decided: `["Mon"]` (the val name, as the Enumeration family writes it); Java constants likewise.
- E6-R3 (must) a family member's singleton must render its family's value → accepted: a value of an enum is its enum
  family restricted to that value (ToValue, `@typeHint`, profile shaping all as for the family), on both Scala
  versions; L-A and L-C reworded. Name only for standalone / mixed-family objects.
- E6-R4 (must) Float literal precision → accepted: `BigDecimal(f.toString)`; `0.1f` test. (Field defaults: your
  neighbour note for the lead.)
- E6-R5 (nit) `X | Null` exclusive → accepted: `Option[X] | Null` flattened to `X | Null`; `oneof` only when X
  provably rejects null, decided at runtime (`Unions.orNull`), else `anyof`; tests for `Option[A] | Null`,
  `Option[A] | B` and a nullable given member. Note for E2: its `any` node must count as nullable there.
- E6-R6 (nit) union-typed defaults → accepted: a generated match over literal and non-generic class members; the
  rest keeps the error, which now names the fix.
- E6-R7 (nit) document what profiles don't do → accepted (plan step 8).
- E6-R8 (nit) opaque guard tests → accepted, all four.
- E6-R9 (nit) member order → accepted: order-sensitive asserts for unions and merged enums.
