# An "any JSON value" schema, and schemas for the JSON libraries' own AST types  (issues #295, #289, #286, item E2)
Status: approved (r-e2, 2026-11-23), implemented

## Problem
There is no schema node for "any JSON value" (`{}` in every draft, `true` from draft-06 on). So there is no way to give a field that holds arbitrary JSON a
correct schema, and none of the JSON AST types we bridge to has a schema. Repro specs on the branch: `AnyValueReproSpec` in `api` and in each
bridge module (they become the real specs in the implementation). All of the following is on 0.9.0 (master 4b2aff3), the same on 2.12.21,
2.13.18 and 3.3.9 unless noted:

| `Json.schema[...]` (also as a field of a case class) | 2.12 / 2.13 | 3.3.9 |
|---|---|---|
| `json.Schema.`any`` (any node) | `value any is not a member of object json.Schema` | same |
| our `com.github.andyglow.json.Value` | `Can't infer json value for default value of Value.arr(..., value: Seq[Value])` | same |
| play `JsValue`, `JsArray` | `Can't infer json value for default value of play.api.libs.json.JsArray(..., value: IndexedSeq[JsValue])` | same |
| play `JsObject` | **macro crash** `AssertionError: method underlying$access$0` (B4, #459/#462) | `scala.collection.Map[String, JsValue] is not supported: only immutable Maps` |
| circe `Json` / `JsonObject` | `Supported Type:false / Only Scala case classes/objects are supported for Sum Type leaves` (`BiggerDecimal`, `LinkedHashMapJsonObject`) | same (`io.circe.numbers.BiggerDecimal`, `io.circe.JsonObject`) |
| ujson `Value` / `Obj` / `Arr` | `upickle.core.LinkedHashMap[String,ujson.Value] is not supported: only immutable Maps` | same |
| json4s `JValue`, `JObject` | **compiles, and is wrong**: a sealed family of the AST's case classes, e.g. a JSON string must be `{"s": "..."}`, an object `{"obj": [{"_1": ..., "_2": ...}]}` | same |
| spray `JsValue`, `JsObject` | **compiles, and is wrong**: `JsObject` must be `{"fields": {...}}`, `JsString` `{"value": "..."}`, `JsTrue` the string `"JsTrue"` | same |

The json4s/spray schemas reject every real JSON value that the library writes (`"abc"` is not `{"s": "abc"}`). Their repro specs validate
`"abc"`, `{"a": 1}` and a field holding `5` against today's schemas with the networknt validator: all rejected, on 2.13.18 and 3.3.9.

Workarounds today (#286, #295): `Schema.`object`.Free[JsObject]()` (README "Free objects") covers objects only. For "any value" the user hand-writes a
`oneOf` of six types (#286's comment), or casts another schema. Neither renders `{}`.

## Why it is a problem
- #286 (a user): a DTO field with "some random JSON that we don't care about" (`JsValue`). #295 (a user): typesafe-config's `ConfigValue`,
  asking for `Schema.empty`. #289 (owner): "there is no option to specify an arbitrary value (num, bool, str, null, arr, obj)".
- Every JSON library user has such a field type (`Json`, `JsValue`, `ujson.Value`, `JValue`), and the errors are confusing. On Scala 2,
  play's `JsObject` crashes the compiler. json4s/spray silently produce a schema that rejects all valid data.
- #286's second complaint (covariance: a `Schema[JsNull]` standing in for `Schema[JsValue]`) is already fixed in 0.8.0 (#435: an implicit
  `Schema` is used only for exactly its type; `Predef` is invariant). Nothing to do for it here; the PR says so.

## Options

### 1. The model node

#### 1A. A new leaf `` json.Schema.`any`[T]() `` rendering `{}` — WON
- `core/src/main/scala/json/Schema.scala`: `final case class `any`[T]() extends Schema[T]`, shaped like `const` (type parameter, so it can be
  `Schema[JsValue]`, `Schema[ConfigValue]`, ...): `mkCopy`, `canEqual`, `equals` (`case `any`() => super.equals(obj)`), `hashCode`, `toString`
  `any`. Title, description and validations go through the base trait like every node. `jsonType` is `"any"`, but the renderer never writes
  it (below).
- Renders `{}` in every draft (04, 06, 07, 2019-09, 2020-12, Raw) and under every profile, Legacy included (a new node: no 0.7 output to keep).
  With texts: `{"description": "plain JSON"}` (title/description on every node is B1, #469; see the plan).
- Free extras: `` `not`(`any`) `` has the body `"not": {}`, the "nothing" schema (`false`); a `Map[String, JsValue]` renders
  `"patternProperties": {"^.*$": {}}`; a `List[JsValue]` renders `"items": {}`.
- An `any` in a `oneOf` always overlaps: `oneof{X, any}` matches every X value twice, so the `oneOf` rejects them. Only a hand-built `oneof` or
  a family member over an AST type can get there. E2 doesn't change `oneOf` vs `anyOf` (that is B3, #470, which picks `anyOf` on overlap:
  the lead is told `any` overlaps with everything). The README section says "don't put an `any` into a `oneOf`".
- Pros: one node, one obvious name; `{}` is valid in every draft we render; the renderers' result type stays `obj`.
- Cons: a new case in a sealed ADT. Exhaustive matches outside core break (users matching on `Schema` without a default get a warning, not an
  error: `Schema` is sealed, a match without the case still compiles). See Compatibility.

#### 1B. The same node, named `` `empty` `` (#295's suggestion) — LOST
The spec calls `{}` "the empty schema", but in Scala `empty` reads as "matches nothing" (`Set.empty`, `Option.empty`), the opposite of what it
does. `any` says what it accepts. The choice is one identifier: easy to switch at review.

#### 1C. Render the boolean schema `true` — LOST
Draft-04 has no boolean schemas. The renderers and every bridge return an object (`obj`, `JsObject`, `Json` object, `ujson.Obj`, `JObject`:
`AsValue.schema: obj`, `asPlay: JsObject`, ...), so a bare `true` would change public signatures. `true` can't carry a `title`/`description`
either. `{}` means the same in every draft.

#### 1D. No node: a `oneOf` of the six JSON types (what #286 hand-wrote) — LOST
Verbose (`oneOf[{type:string}, {type:number}, {type:boolean}, {type:null}, {type:array, items: $ref}, {type:object, additionalProperties: $ref}]`),
needs a recursive definition for arrays and objects, and is still only an approximation of `{}` (`integer` vs `number`, keywords leaking into
`oneOf` checks). It's what the issue asks to replace.

### 2. Where the library types' schemas live

#### 2A. One opt-in object per bridge, `AsX.predef` (inlined `Predef`s) — WON
- `import com.github.andyglow.jsonschema.AsPlay.predef._` (Scala 3: `.*`), likewise `AsCirce.predef`, `AsU.predef`, `AsJson4s.predef`,
  `AsSpray.predef`. The name follows joda's `JodaTimeSupport.predef` (inlined Predefs, next to `Schema`s that become definitions).
- They are `Predef`s, not `Schema`s: by the core rule a `Schema` becomes a definition, and a definition named `JsValue` that says `{}` is noise.
  A user who wants a definition writes `implicit val s: Schema[JsValue] = Schema.`any`[JsValue]()` and gets one (a `Schema` is looked up before a `Predef`).
- Measured (2.13.18 and 3.3.9): an implicit `Predef[JsObject]` in an imported object plus the `Predef[JsObject]` that README "Free objects" tells
  users to write is **ambiguous** ("ambiguous implicit values" on 2, "Ambiguous given instances" on 3). An opt-in object can't break code that
  compiles today; a user who imports it and has their own Predef deletes theirs.
- E8 (c-e8, #481) agreed: its null-singleton Predefs (`JsNull.type`, `JNull.type`, `ujson.Null.type`) go into the same `AsX.predef` objects. E2
  doesn't define those (no duplicate implicits). Whichever PR merges second merges the two `object predef` bodies.
- Pros: no ambiguity for existing code; one import per library for all its AST types; works the same on Scala 2 and 3.
- Cons: one more import to know. README and paradox show it next to "Free objects", and the error users hit today names the type, so it can be
  searched for. A better compile-error hint is B11's area (noted for the lead).

#### 2B. In the bridge objects users already import (`AsPlay._`, `profiles.play._`, ...) — LOST
Zero extra imports, but it makes existing code ambiguous (measured above): anyone who followed the README's `Predef[JsObject]` and imports
`AsPlay._` stops compiling. That is a source break in a patch release, for little gain.

#### 2C. Automatic, no import (implicit scope) — LOST (not possible)
The implicit scope of `Predef[JsValue]` is `Predef`'s companion (core, which can't depend on play/circe/...) and `JsValue`'s companion (the
library's). A type class in core with bridge instances has the same problem. Only our own `com.github.andyglow.json.Value` can be automatic: its
Predefs go into `json.schema.Predef`'s companion (core), found with no import, and a user's own local `Predef[Value]` still wins (the companion is
only searched when nothing in lexical scope matches).

### 3. What each AST type maps to

#### 3A. The root type → any; its object/array/string/number/boolean types → the matching schema — WON

| library | `{}` (any) | free object | array of any | string | number | boolean |
|---|---|---|---|---|---|---|
| ours (core `Predef`, no import; also `` Value.`null`.type `` → `` Schema.`null` ``) | `Value` | `Value.obj` | `Value.arr` | `Value.str` | `Value.num` | `Value.bool` |
| play (`AsPlay.predef`) | `JsValue` | `JsObject` | `JsArray` | `JsString` | `JsNumber` | `JsBoolean` |
| circe (`AsCirce.predef`) | `Json` | `JsonObject` | | | `JsonNumber` | |
| ujson (`AsU.predef`) | `ujson.Value` | `ujson.Obj` | `ujson.Arr` | `ujson.Str` | `ujson.Num` | `ujson.Bool` |
| json4s (`AsJson4s.predef`) | `JValue` | `JObject` | `JArray` (and `JSet`, `uniqueItems`) | `JString` | `JDouble`, `JDecimal`, `JLong`, `JInt`, `JNumber` | `JBool` |
| spray (`AsSpray.predef`) | `JsValue` | `JsObject` | `JsArray` | `JsString` | `JsNumber` | `JsBoolean` |

- Free object: `` `object`.Free[T]() `` (`{"type":"object","additionalProperties":true}`). Array: `` `array`[V, IndexedSeq](`any`[V]()) ``
  (`{"type":"array","items":{}}`), cast to `Schema[JsArray]` (the container is phantom; the same cast E8 uses for its null singletons).
- Numbers render `number`, like the Scala number types today (`integer` for `JInt`/`JLong` is D2's decision, not E2's).
- Not covered: the singletons `JsTrue`/`JsFalse`/`ujson.True`/... (could be `const`, nobody asked), the libraries' null singletons (E8; our
  own `` Value.`null` `` is core's, so E2 adds it), json4s `JNothing`. circe's `Json.JString` etc. are private to circe.
- json4s `JNothing`: json4s *omits* a field whose value is `JNothing`, so a required `JValue` field rejects such an object (`required`). That
  is the field's required-ness, not the value schema: README says to make such a field an `Option[JValue]` or give it a default. The json4s
  acceptance test shows it (an object written with `JNothing` fails `required`, documented).
- Defaults. A field default needs a `ToValue` of the field's type:
  - Ours: core gets an identity `implicit def ValueV[T <: Value]: ToValue[T]` in `object ToValue` (`ToValue` is invariant; the generic covers
    `Value`, `obj`, `arr`, ...). `ProductV` (which would also match `obj`, a case class) isn't mixed into the companion, so it can't clash.
  - Bridges: `AsX.toValue` (from the library's writer: `Writes`, `Encoder`, `Writer`, `JsonWriter`) already covers `JsValue = Json.obj()` and
    the like, with `AsX._` or `profiles.x._` imported, as for every other default today. `AsX.predef` adds no `ToValue` (a second instance next
    to `AsX.toValue` is a specificity question per Scala version, for no gain). README says which imports a default needs; tests cover a
    default on each library's root and object type.
- Option 3B, **root type only** (`JsValue`/`Json`/`ujson.Value`/`JValue`/`Value` → any, nothing else): smaller, but a `JsObject` field (the
  README's own example) and the json4s/spray structural schemas stay as they are. LOST. The subtypes are one line each.

### 4. `Option[JsValue]` under the nullable profiles

A `{}` already accepts `null`. The Neutral (and every `OptionStyle.Nullable`) lowering makes `oneof{X, null}` and renders it
`oneOf[{}, {"type":"null"}]`. That **rejects `null`**: both branches match, and `oneOf` needs exactly one.

#### 4A. The lowering drops the `null` branch when X is any — WON
`JsonSchemaProfile.Lowering.optionAt` (Nullable): `Option[X]` where X *admits any value* lowers to X itself. `Option[JsValue]` renders `{}`.
"Admits any value" (one helper, `isAny`, in core, used by the lowering and the renderer) looks through:
- `value-class` (a value class over `JsValue`);
- `def` (a user's `implicit val s: Schema[JsValue] = Schema.`any`[JsValue]()`, which the doc recommends for a definition): `Option[JsValue]`
  then lowers to the `def` itself, so the `$ref` stays, `{"$ref": "#/definitions/JsValue"}`, and `null` passes (the body is `{}`).
A `ref(sig)` (a reference to a definition whose body is elsewhere: recursion) can't be seen through. That case still renders
`oneOf[{"$ref"}, {"type":"null"}]`. It needs a recursive type whose own definition is an `any`, which derivation never makes, so it is only
documented. Required-ness is unchanged (it comes from the field: not required, or required with `optionKeyRequired`). Legacy renders
`Option[X]` as X anyway.

#### 4B. Only in the renderer (`mkNullable`) — LOST as the fix, kept as a guard
Fixing it in the renderer alone would leave the lowered tree (what `withProfile` returns, what tree specs compare) saying `oneof{X, null}`.
As an extra, `mkNullable` renders a hand-built `` `oneof`(Set(`any`(), `null`)) `` (or a def of any) as the any member alone. A `oneOf` of `{}`
and `null` is never what anyone means, and the guard is two lines.

## Decision
A new leaf `` json.Schema.`any`[T]() `` that renders `{}` in every draft and profile (1A). Our own `Value` gets core Predefs (automatic). Each
bridge gets an opt-in `AsX.predef` object with inlined Predefs for its AST types: the root type is `any`, the object type a free object, and so on
(2A, 3A). `Option` of an `any` lowers to the `any` itself (4A). The parser reads `{}` and `true` as `any`.

## Implementation plan
1. **core model** (`json/Schema.scala`): the `any` case class (1A).
2. **core renderer** (`AsDraftSupport.scala`):
   - `apply`'s `base`: `any`, and a `value-class` of `any`, write no `"type"` (today `value-class(x)` writes `x.jsonType`, which would be
     `"type": "any"`). A helper `isAny(s)` (through value classes) in one place.
   - `mkAllOf`: `any` members don't count for the shared type (like `$ref`s today), so `allof{any, any}` can't render `"type": "any"`. This
     is right for an intersection: `allof{any, string}` is a string.
   - `mkPlainOneOf`: `any` (and `bodyType` of a `def` of any) is excluded from `tpe`, the stated type, but it stays in `known` as a type
     that equals nothing. So `oneof{string, any}` is *not* same-typed, and each member keeps its own `"type"`: `oneOf[{"type":"string"}, {}]`.
     Skipping it in `known` too would give `{"type":"string","oneOf":[{}, {}]}`, which rejects every value.
   - `mkEnum`: an `enum` over `any` (hand-built only) writes no `"type"`.
   - `mkNullable`: the 4B guard (`oneof{any-ish, null}` → the any-ish member alone). `singleType` is unchanged (`any` isn't single-typed).
   - No `inferSpecifics` entry (`lift` falls back to `obj.empty`).
   - Per-draft renderers (`AsDraft04`..`AsDraft12`, `AsRaw`) don't match on nodes: no change.
3. **core profile** (`JsonSchemaProfile.scala`): `optionAt` (Nullable), 4A. `Lowering.apply`'s `case other => other` already passes `any` through.
4. **core tree tools**: `AsTree.scala` (exhaustive match: add `any`), `SchemaEquality` (falls back to `==`; nothing to add), `DefinitionNames` (default case).
5. **core Predefs** (`json/schema/Predef.scala`): `Value`, `Value.obj`, `Value.arr`, `Value.str`, `Value.num`, `Value.bool`,
   `` Value.`null`.type `` (3A). **core `ToValue`**: the identity `ValueV[T <: Value]` (3A, defaults).
6. **bridges**: `object predef` in `AsPlay`, `AsCirce`, `AsU`, `AsJson4s`, `AsSpray` (3A). Built by hand: no macro calls in main code.
7. **parser** (`modules/parser/.../ParseJsonSchema.scala`):
   - An object with no `type` and no keyword the parser reads (only annotations: `$schema`, `$id`, `id`, `title`, `description`, `default`,
     `examples`, `$comment`, `definitions`) → `any`, with its title. Anything else without `type` keeps failing "type is not defined".
   - Nested `true` → `any`, nested `false` → `not(any)`, in `items` and in `properties`. A `true` property is silently dropped today
     (`collect { case (k, v: obj) => ...}`).
   - A root `true` stays an error (the root must be an object with a draft-04 `$schema`).
   - `default`/`examples` in an annotation-only object are dropped, as the parser drops them everywhere today. `true`/`false` as schemas are
     not draft-04 (boolean schemas came in draft-06). The parser reads them leniently in a draft-04 document: harmless, and documented in
     `modules/parser.md`.
8. **tests** (the repro specs become these):
   - core `SchemaEqualsHashCodeSpec`: `any` equality/hashCode, with and without texts.
   - api `AnyValueSpec`:
     - the trees for `Value` and its subtypes, as a field, `Option[Value]` (Neutral: `{}`, not required; `optionKeyRequired`; Legacy),
       `List[Value]`, `Map[String, Value]`, a value class over `Value`, `not(any)`, `allof`/`oneof` with `any` members;
     - `Option` of a `Schema[Value]` *definition* (4A through `def`): the `$ref` alone, Neutral and a profile with `optionKeyRequired`,
       `null` accepted by the validator;
     - `oneof{string, any}`, `oneof{def(any), string}` (each member keeps its type), `allof{any, string}`, a hand-built `oneof{any, null}`
       (the 4B guard), rendered per draft and validated;
     - a default `data: Value = Value.obj(...)` (the identity `ToValue`);
     - the rendered JSON per draft (04, 06, 07, 2019-09, 2020-12);
     - `toDebugString`;
     - the networknt validator: `{}` accepts `1`, `"s"`, `null`, `[]`, `{}`, `true`; `Option[Value]` accepts `null`. JVM; canceled on JS by
       the harness.
   - One golden model `AnyValues` (fields of each kind) for the full output per draft. Update mode creates `golden/AnyValues*.json` and the
     missing `golden-legacy/AnyValues*.json` (it adds, never rewrites). The PR lists both sets of new files. No existing golden file changes.
   - `harness/Golden.scala`'s tree dump (exhaustive): an `"any"` kind.
   - Each bridge, `AnyValueSpec`, with `import AsX.predef._`:
     - the trees;
     - a case class with every AST type as a field;
     - the library's real codec output for sample values validated against the schema under its profile (acceptance-style, JVM);
     - a default on the root and the object type, with `AsX._` imported (the library writer's `ToValue`);
     - json4s: a `JNothing` field is omitted by json4s and fails `required` (the documented exception);
     - json4s/spray: the repro specs (today's structural schemas reject `"abc"`) pass with the import.
     - Scala 2 + play: `JsObject` with the import compiles (the Predef is found before derivation, so the B4 crash path isn't reached).
       Without the import it still crashes until B4 (#462) merges.
   - parser: `{}`, `{"title": "t"}`, `"items": true`, a `true`/`false` property, and `{"minimum": 1}` (still an error).
9. **docs** (README + paradox `validation.md`, same words; `modules/json.md` for the imports):
   - A section "Any JSON value" next to "Free objects":
     - `Schema.`any``, `Option`, our `Value`, the per-library imports and table;
     - defaults need `AsX._` (or `profiles.x._`);
     - don't put an `any` into a `oneOf`;
     - json4s `JNothing`;
     - `ConfigValue` (#295) as the do-it-yourself example: `implicit val p: Predef[ConfigValue] = Predef(Schema.`any`[ConfigValue]())`.
   - The README's `JsObject` example gains the import alternative. No CHANGELOG (the lead's).
10. **JS**: shared code only; the Scala.js bridges (play, circe, ujson) get their `predef` too. The validator tests are canceled on JS as usual.
11. **title/description on `any`**: rendered by B1's generic `texts` (#469). If B1 has merged when E2 is implemented, E2 builds on it and tests
    `{"description": ...}`. If not, E2 doesn't render texts itself (that would conflict with B1), and the texts test lands at rebase.

## Compatibility
- Source: additive. One new case in the sealed `Schema` (a user's exhaustive `match` on `Schema` gets a non-exhaustive *warning*, an error only
  under `-Xfatal-warnings`; core's own `AsTree` and the test harness are updated). New implicits only in core's `Predef` companion (for our own
  `Value`, which didn't compile before) and in new opt-in objects. Nothing that compiles today becomes ambiguous (2A).
- Binary: new classes and members only. PVP: minor-compatible (0.9.x).
- Output: no change for any model that compiles today without the new imports. Opt-in changes: json4s `JValue`/`JObject` and spray
  `JsValue`/`JsObject` render the new schemas *only when `AsX.predef._` is imported* (today's structural schema stays without it). `Option[X]`
  with X = `any` is new (it didn't compile).
- Legacy: unchanged for every existing model; `any` renders `{}` under Legacy too.
- Core rule: unchanged (Predefs inline, a user's `Schema[JsValue]` still makes a definition and wins).

## Review log
Round 1 (r-e2, on eeafc85):
- E2-R1 (must) `Option` of a `def` of any still renders `oneOf[$ref, null]` → accepted: `isAny` looks through `def` and `value-class`;
  `Option[def(any)]` lowers to the `def` (the `$ref` stays). A `ref` can't be seen through: documented (4A). Tests added to the plan.
- E2-R2 (must) `oneof{string, any}` would render `"type":"string","oneOf":[{},{}]` → accepted: `any` is excluded from `tpe` but stays in
  `known` as a type equal to nothing (also via `bodyType`). `mkEnum` over any writes no type. Tests for oneof/allof with any (plan step 2, 8).
- E2-R3 (must) defaults → accepted: core identity `ToValue[T <: Value]` (`ProductV` isn't in the companion, so no clash); the bridges' defaults
  use `AsX.toValue` with `AsX._` / `profiles.x._`, documented and tested per library (3A "Defaults").
- E2-R4 (nit) `any` in a oneOf overlaps → accepted: noted in 1A and the README; the lead is told (B3). The `mkNullable` guard is added (4B as an
  extra).
- E2-R5 (nit) json4s `JNothing` → accepted: documented (3A), with an acceptance test showing the `required` failure.
- E2-R6 (nit) `` Value.`null` `` → accepted: core (ours, not a library's: E8 takes the libraries' null singletons), `` Predef[Value.`null`.type] ``
  → `` Schema.`null` `` (cast, as E8's).
- E2-R7 (nit) json4s/spray repro specs passed on master → accepted: they now validate `"abc"`, `{"a":1}` and a field holding `5` against today's
  schemas (all rejected on 2.13.18 and 3.3.9).
- E2-R8 (nit) golden-legacy files are created → accepted: listed in the plan and the PR.
- E2-R9 (nit) parser `default`/`examples` and draft-04 leniency → accepted: dropped as elsewhere; leniency documented (plan step 7).

Implementation notes (no change to the approved plan):
- Correction to E2-R7: the repro specs at 562defa failed on the validator's `$id` check (`'x' is not a valid $id`), not on validation. The
  implementation's json4s/spray `AnyValueSpec` re-checks with a valid `$id`: the structural schemas (derived without the import) do reject
  `"abc"` and `{"a":1}` on 2.13.18 and 3.3.9. The claim holds; the earlier evidence didn't show it.
- Found (pre-existing, every `not`, not E2's): a rendered `not` node writes `"type": "not"` next to `"not"` (`apply`'s base uses `jsonType`),
  which is not a JSON Schema type. So `not(any)` renders `{"type": "not", "not": {}}`. Reported to the lead. README doesn't advertise `not(any)`.
- An `enum` over `any` (hand-built) would also get the base `"type": "enum"`: `apply`'s base writes no type for it (with `mkEnum`'s).
- Parser: an existing test used `{"title": "no type"}` as an invalid property schema. Under 7 it is now `any` with a title, so the test uses
  `{"title": "no type", "minimum": 1}` (still invalid: a keyword the parser doesn't read, without `type`).
- `Schema.`any`.admitsEverything(s)` (public, in the `any` companion) is the one "admits every value" check (`any`, through `value-class` and
  `def`), shared by the lowering and the renderer.

Code review round 1 (r-e2, PR #486 at 5851c6e):
- E2-R10 (must) the collapsed `Option` of an any lost the any's texts/validations (`withExtraFrom` replaces them) → accepted:
  `Lowering.apply` merges for the collapsed case (new `private[json] Schema#withExtraMergedFrom`: the option's texts where it has them, else
  the inner's; validations of both). Tests: tree-level, the option's own description wins, a value class of any, and #286's case (a `Predef`
  of `any.withDescription("plain JSON")` behind an `Option` field) under Neutral, `optionKeyRequired` and Legacy.
- E2-R11 (nit) the parser's `false` → `not(any)` renders the pre-existing `"type": "not"` → accepted as a note (PR Notes): the `not` fix
  covers the parser path too. Kept `false` (`not(any)` is the right tree).
- E2-R12 (nit) a real codec round → accepted: play `Json.writes[Payload]` and circe `deriveEncoder[Payload]` output validated.
- E2-R13 (nit) README table → accepted: `` Value.`null` `` added, columns aligned (README and paradox).
