# An "any JSON value" schema, and schemas for the JSON libraries' own AST types  (issues #295, #289, #286, item E2)
Status: draft

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

The json4s/spray schemas reject every real JSON value that the library writes (`"abc"` is not `{"s": "abc"}`).

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
- Free extras: `` `not`(`any`) `` renders `{"not": {}}`, the "nothing" schema (`false`); a `Map[String, JsValue]` renders
  `"patternProperties": {"^.*$": {}}`; a `List[JsValue]` renders `"items": {}`.
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
| ours (core `Predef`, no import) | `Value` | `Value.obj` | `Value.arr` | `Value.str` | `Value.num` | `Value.bool` |
| play (`AsPlay.predef`) | `JsValue` | `JsObject` | `JsArray` | `JsString` | `JsNumber` | `JsBoolean` |
| circe (`AsCirce.predef`) | `Json` | `JsonObject` | | | `JsonNumber` | |
| ujson (`AsU.predef`) | `ujson.Value` | `ujson.Obj` | `ujson.Arr` | `ujson.Str` | `ujson.Num` | `ujson.Bool` |
| json4s (`AsJson4s.predef`) | `JValue` | `JObject` | `JArray` (and `JSet`, `uniqueItems`) | `JString` | `JDouble`, `JDecimal`, `JLong`, `JInt`, `JNumber` | `JBool` |
| spray (`AsSpray.predef`) | `JsValue` | `JsObject` | `JsArray` | `JsString` | `JsNumber` | `JsBoolean` |

- Free object: `` `object`.Free[T]() `` (`{"type":"object","additionalProperties":true}`). Array: `` `array`[V, IndexedSeq](`any`[V]()) ``
  (`{"type":"array","items":{}}`), cast to `Schema[JsArray]` (the container is phantom; the same cast E8 uses for its null singletons).
- Numbers render `number`, like the Scala number types today (`integer` for `JInt`/`JLong` is D2's decision, not E2's).
- Not covered: the singletons `JsTrue`/`JsFalse`/`ujson.True`/... (could be `const`, nobody asked), the null singletons (E8), json4s `JNothing`
  (means "absent"; a `JValue` covers it). circe's `Json.JString` etc. are private to circe.
- Option 3B, **root type only** (`JsValue`/`Json`/`ujson.Value`/`JValue`/`Value` → any, nothing else): smaller, but a `JsObject` field (the
  README's own example) and the json4s/spray structural schemas stay as they are. LOST. The subtypes are one line each.

### 4. `Option[JsValue]` under the nullable profiles

A `{}` already accepts `null`. The Neutral (and every `OptionStyle.Nullable`) lowering makes `oneof{X, null}` and renders it
`oneOf[{}, {"type":"null"}]`. That **rejects `null`**: both branches match, and `oneOf` needs exactly one.

#### 4A. The lowering drops the `null` branch when X is any — WON
`JsonSchemaProfile.Lowering.optionAt` (Nullable): `Option[X]` where X is `any` (or a value class of `any`) lowers to X itself. `Option[JsValue]`
renders `{}`. Required-ness is unchanged (it comes from the field: not required, or required with `optionKeyRequired`). Legacy renders
`Option[X]` as X anyway.

#### 4B. Do it in the renderer (`mkNullable`) — LOST
It also catches a hand-built `` `oneof`(Set(`any`(), `null`)) ``, but the lowered tree (what `withProfile` returns, what tree specs compare) would
still say `oneof{any, null}`. A user who builds that `oneof` by hand asked for it.

## Decision
A new leaf `` json.Schema.`any`[T]() `` that renders `{}` in every draft and profile (1A). Our own `Value` gets core Predefs (automatic). Each
bridge gets an opt-in `AsX.predef` object with inlined Predefs for its AST types: the root type is `any`, the object type a free object, and so on
(2A, 3A). `Option` of an `any` lowers to the `any` itself (4A). The parser reads `{}` and `true` as `any`.

## Implementation plan
1. **core model** (`json/Schema.scala`): the `any` case class (1A).
2. **core renderer** (`AsDraftSupport.scala`):
   - `apply`'s `base`: `any`, and a `value-class` of `any`, write no `"type"` (today `value-class(x)` writes `x.jsonType`, which would be
     `"type": "any"`). A helper `isAny(s)` (through value classes) in one place.
   - `mkAllOf`: `any` members don't count for the shared type (like `$ref`s today), so `allof{any, any}` can't render `"type": "any"`.
   - `mkPlainOneOf`: the same for `known`/`tpe`. `mkNullable`/`singleType`: unchanged (`any` isn't single-typed; 4A means the lowering never
     sends it there).
   - No `inferSpecifics` entry (`lift` falls back to `obj.empty`).
   - Per-draft renderers (`AsDraft04`..`AsDraft12`, `AsRaw`) don't match on nodes: no change.
3. **core profile** (`JsonSchemaProfile.scala`): `optionAt` (Nullable), 4A. `Lowering.apply`'s `case other => other` already passes `any` through.
4. **core tree tools**: `AsTree.scala` (exhaustive match: add `any`), `SchemaEquality` (falls back to `==`; nothing to add), `DefinitionNames` (default case).
5. **core Predefs** (`json/schema/Predef.scala`): `Value`, `Value.obj`, `Value.arr`, `Value.str`, `Value.num`, `Value.bool` (3A).
6. **bridges**: `object predef` in `AsPlay`, `AsCirce`, `AsU`, `AsJson4s`, `AsSpray` (3A). Built by hand: no macro calls in main code.
7. **parser** (`modules/parser/.../ParseJsonSchema.scala`):
   - An object with no `type` and no keyword the parser reads (only annotations: `$schema`, `$id`, `id`, `title`, `description`, `default`,
     `examples`, `$comment`, `definitions`) → `any`, with its title. Anything else without `type` keeps failing "type is not defined".
   - Nested `true` → `any`, nested `false` → `not(any)`, in `items` and in `properties`. A `true` property is silently dropped today
     (`collect { case (k, v: obj) => ...}`).
   - A root `true` stays an error (the root must be an object with a draft-04 `$schema`).
8. **tests** (the repro specs become these):
   - core `SchemaEqualsHashCodeSpec`: `any` equality/hashCode, with and without texts.
   - api `AnyValueSpec`:
     - the trees for `Value` and its subtypes, as a field, `Option[Value]` (Neutral: `{}`, not required; `optionKeyRequired`; Legacy),
       `List[Value]`, `Map[String, Value]`, a value class over `Value`, `not(any)`, `allof`/`oneof` with `any` members;
     - the rendered JSON per draft (04, 06, 07, 2019-09, 2020-12);
     - `toDebugString`;
     - the networknt validator: `{}` accepts `1`, `"s"`, `null`, `[]`, `{}`, `true`; `Option[Value]` accepts `null`. JVM; canceled on JS by
       the harness.
   - One golden model `AnyValues` (fields of each kind) for the full output per draft, with Legacy generated too. No existing golden file changes.
   - `harness/Golden.scala`'s tree dump (exhaustive): an `"any"` kind.
   - Each bridge, `AnyValueSpec`, with `import AsX.predef._`:
     - the trees;
     - a case class with every AST type as a field;
     - the library's real codec output for sample values validated against the schema under its profile (acceptance-style, JVM).
     - Scala 2 + play: `JsObject` with the import compiles (the Predef is found before derivation, so the B4 crash path isn't reached).
       Without the import it still crashes until B4 (#462) merges.
   - parser: `{}`, `{"title": "t"}`, `"items": true`, a `true`/`false` property, and `{"minimum": 1}` (still an error).
9. **docs** (README + paradox `validation.md`, same words; `modules/json.md` for the imports):
   - A section "Any JSON value" next to "Free objects": `Schema.`any``, `Option`, our `Value`, the per-library imports and table, and
     `ConfigValue` (#295) as the do-it-yourself example: `implicit val p: Predef[ConfigValue] = Predef(Schema.`any`[ConfigValue]())`.
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
