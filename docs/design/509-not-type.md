# A rendered `not` states `"type": "not"`  (issue #509, item B22)
Status: approved (r-b22, 2026-11-23)

## Problem

Every draft renderer writes a `Schema.not` node as `{"type": "not", "not": {...}}`. `"not"` is not a JSON Schema type,
so a validator rejects every value. The cause is the renderer's common base (master 4b2aff3):

```scala
// core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala
// apply, lines 55-61: the "type" every node states unless it is a def/allof/oneof/ref/const
case `value-class`(x) if includeType => obj("type" -> x.jsonType)
case _ if includeType                => obj("type" -> x.jsonType)     // `not`.jsonType == "not" (json/Schema.scala:534)
// mkNot, line 220: the body, without its type
obj("not" -> apply(x.tpe, Some(x), includeType = false, isRoot = false))
```

The same `jsonType` feeds the shared-type logic of `oneOf`/`allOf` (`mkPlainOneOf` via `memberType`, line 164;
`mkAllOf`, lines 201-202): when every member is a `not`, the members "share" the type `"not"` and the `oneOf`/`allOf`
states `"type": "not"` itself (except at the root, where the shared type is dropped: B15, #499).

It has been like this since `mkNot` was added (0.2.5, 2020).

### Who builds `not` nodes

refined (`modules/refined`, Scala 2 `AST.scala:177`, Scala 3 `RefinedMacro.scala:360`). The derivation macros never do;
a user can build one by hand (`Schema.`not``). refined simplifies a `Not` of a numeric bound into the opposite bound
(`Not[Greater[5]]` → `maximum: 5`, `Not[Not[P]]` → `P`), so the `not`s left are the ones with no such inverse:

| refined type | tree |
|---|---|
| `String Refined Not[IPv4]` | `not(string(ipv4))` |
| `Int Refined NonDivisible[2]` | `not(integer{multipleOf 2})` |
| `String Refined Not[StartsWith["a"] Or EndsWith["b"]]` | `not(oneof(string{pattern}, string{pattern}))` |
| `Int Refined (Not[Less[2]] And Less[7])` | `allof(not(integer{exclMax 2}), integer{exclMax 7})` |
| `Int Refined (Not[Less[2]] And Not[Greater[7]])` | `allof(not(integer{exclMax 2}), not(integer{exclMin 7}))` |
| `Int Refined (Greater[7] Or Less[2])` | `not(integer{exclMin 7, exclMax 2})` (wrong bounds: B27) |

### Repro

Probe (not committed): `notes/scratch/c-b22/B22ProbeSpec.scala` (hand-built trees identical to refined's, rendered
with draft-04/07/2020-12 and validated with networknt json-schema-validator 1.5.9, the api tests' `petshop.JsonValidator`)
and `notes/scratch/c-b22/RefinedB22Probe.scala` (refined types rendered). Logs `probe-2.12.21.log`, `probe-2.13.18.log`,
`probe-3.3.9.log`, filtered `p-<ver>.txt`: **identical output on 2.12.21, 2.13.18 and 3.3.9** (the renderer is shared,
so Scala.js renders the same bytes).

```scala
AsValue.schema(Json.schema[String Refined Not[IPv4]], Draft07(id = "http://x/a.json"))
```
```json
{"$schema": "http://json-schema.org/draft-07/schema#", "$id": "http://x/a.json",
 "type": "not", "not": {"format": "ipv4"}}
```

networknt, draft-07 and 2020-12 (draft-04 the same verdicts):

| schema | `"abc"` | `"1.2.3.4"` | `5` |
|---|---|---|---|
| `{"type":"not","not":{"format":"ipv4"}}` (today) | invalid: `string found, unknown expected` | invalid | invalid |

Other shapes from the probe, all draft-07:

- `Int Refined NonDivisible[2]` → `{"type": "not", "not": {"multipleOf": 2}}`: rejects everything.
- a field `a: Int Refined NonDivisible[2]` → `"a": {"type": "not", "not": {...}}`: no object with an `a` validates.
- `Option[...]` field → `"a": {"oneOf": [{"type": "not", "not": {...}}, {"type": "null"}]}`: only `null` validates.
- `allof(not, integer)` → `{"allOf": [{"type": "not", "not": {...}}, {"type": "integer", "exclusiveMaximum": 7}]}`: rejects everything.
- `allof(not, not)` as a field → `{"type": "not", "allOf": [{"not": {...}}, {"not": {...}}]}`: rejects everything.
- `allof(not, not)` at the root → `{"allOf": [{"not": {"exclusiveMaximum": 2}}, {"not": {"exclusiveMinimum": 7}}]}`:
  accepts `3` and rejects `1`, `9`, but **accepts `2.5`** (the root drops the shared type, and the `not` bodies have none).

## Why it is a problem

Every schema with a refined `Not[IPv4]`, `NonDivisible`, `Not[A Or B]`, or a `Not` inside `And` rejects every
document (anywhere but at the root of an `allOf`). Nobody has reported it, probably because few use those predicates;
the refined specs compare trees, not rendered JSON, so the tests never saw it.

## What a `not` node means

`Schema.`not`[T](tpe: Schema[T]) extends Schema[T]`: the body is a schema of `T` and so is the result. refined uses it
for "a `T` that fails `P`", so `Int Refined NonDivisible[2]` is *an integer* not divisible by 2: `3` valid, `4`, `2.5`
and `"abc"` invalid. The integer-vs-number distinction (c-b15's note in ITEMS) is part of it: `2.5` must be rejected.

JSON Schema's `not` is the plain complement: `{"not": S}` accepts everything `S` rejects, of any type. So the type
must be stated **next to** `not`, and the body must not carry it (inside the body it would be complemented too).
Candidate renderings, checked with networknt (draft-07; 2020-12 identical):

| rendering of `not(integer{exclMin 5})` | `3` | `7` | `2.5` | `"abc"` |
|---|---|---|---|---|
| expected | valid | invalid | invalid | invalid |
| A. `{"type":"integer","not":{"exclusiveMinimum":5}}` | **valid** | **invalid** | **invalid** | **invalid** |
| B. `{"not":{"type":"integer","exclusiveMinimum":5}}` | valid | invalid | valid ✗ | valid ✗ |
| C. `{"not":{"exclusiveMinimum":5}}` | valid | invalid | valid ✗ | invalid |

And for the other shapes, A's rendering (all as expected):
`{"type":"string","not":{"format":"ipv4"}}`: `"abc"` valid; `"1.2.3.4"`, `5` invalid.
`{"type":"integer","allOf":[{"not":{"exclusiveMaximum":2}},{"not":{"exclusiveMinimum":7}}]}` and
`{"allOf":[{"type":"integer","not":{"exclusiveMaximum":2}},{"type":"integer","not":{"exclusiveMinimum":7}}]}`:
`3` valid; `1`, `9`, `2.5`, `"abc"` invalid.
`{"oneOf":[{"type":"integer","not":{"exclusiveMinimum":5}},{"type":"null"}]}`: `3`, `null` valid; `7`, `2.5` invalid.

## Options

### A. State the body's JSON type next to `not`; the body without it  — WON

The renderer works out the single JSON type of the value a `not` constrains (its *base type*) and states it on the
`not` node; the body keeps all its other keywords. A private helper in `AsDraftSupport`:

```scala
/** The one JSON type of the values a `not` node constrains: its body's, seen through wrappers; None when the body has none or several. */
private def baseType(s: Schema[_]): Option[String] = s match {
  case _: `boolean` | _: `integer` | _: `number`[_] | _: `string`[_] | _: `array`[_, _] | _: `object`[_] | _: `dictionary`[_, _, _] | _: `null` => Some(s.jsonType)
  case `value-class`(t)  => baseType(t)
  case `def`(_, t)       => baseType(t)          // rendered as a $ref; its body's type is known here
  case `not`(t)          => baseType(t)
  case `enum`(t, _)      => baseType(t)
  case `const`(v)        => Some(v.tpe)
  case `oneof`(ms, _)    => same(ms)             // every member's base type, when they are all one
  case `allof`(ms)       => same(ms)             // (and B3's `anyof` the same way, once it exists)
  case _                 => None                 // `ref` (defined elsewhere), E2's `any`, ...
}
```

`def`/`value-class` of E2's `any` must also give `None` (E2's `` `any`.admitsEverything ``): check that first once E2
is merged, or `not(any)` would get `"type": "any"`.

Rendering rules:

1. **`not` node, `includeType = true`:** `"type": <baseType(body)>` when it is known, then `"not": <body rendered with
   includeType = false>`. When it is unknown (a `$ref`, a mixed `oneOf`), no `"type"` and the body rendered **with**
   its types (`includeType = true`): then the `not` is the plain complement, which is the best that can be said.
2. **`not` node, `includeType = false`** (a parent states the type: only a `not` inside a `not` after rule 4):
   no `"type"`; the body as in rule 1.
3. A **value class** around a `not` (`case class Ip(v: String Refined Not[IPv4]) extends AnyVal`) states the same
   type as the `not` (today it states `"type": "not"` through `value-class.jsonType`).
4. **`allOf`/`oneOf` with a `not` member** (directly or in a value class) take the *mixed* path: every member rendered
   with its own type. This removes `"type": "not"` from an all-`not` `allOf`/`oneOf`, and keeps the output correct at
   the root, where the shared type is dropped today (B15). Letting a `not` share its base type with the other members
   is a later, smaller output (see Option D).
5. `mkNullable` (`Option[T]` → `"type": [X, "null"]`) keeps treating `not` as not single-typed, so an optional `not`
   stays `oneOf[{"type": X, "not": ...}, {"type": "null"}]`: a type array next to a typeless body would let the body
   see `null` and complement it wrongly.

Output (draft-07; `$schema`/`$id` left out):

| tree | today | A |
|---|---|---|
| `not(string(ipv4))` | `{"type":"not","not":{"format":"ipv4"}}` | `{"type":"string","not":{"format":"ipv4"}}` |
| `not(integer{multipleOf 2})` | `{"type":"not","not":{"multipleOf":2}}` | `{"type":"integer","not":{"multipleOf":2}}` |
| `allof(not(int{exclMax 2}), int{exclMax 7})` | `{"allOf":[{"type":"not","not":{..}},{"type":"integer","exclusiveMaximum":7}]}` | `{"allOf":[{"type":"integer","not":{..}},{"type":"integer","exclusiveMaximum":7}]}` |
| `allof(not, not)`, a field | `{"type":"not","allOf":[{"not":{..}},{"not":{..}}]}` | `{"allOf":[{"type":"integer","not":{..}},{"type":"integer","not":{..}}]}` |
| `allof(not, not)`, root | `{"allOf":[{"not":{..}},{"not":{..}}]}` | as for a field |
| `oneof(not, null)` (an Option) | `{"oneOf":[{"type":"not","not":{..}},{"type":"null"}]}` | `{"oneOf":[{"type":"integer","not":{..}},{"type":"null"}]}` |
| `not(oneof(string{p1}, string{p2}))` | `{"type":"not","not":{"type":"string","oneOf":[{p1},{p2}]}}` | `{"type":"string","not":{"type":"string","oneOf":[{p1},{p2}]}}` (the inner `"type"` is the non-root same-typed `oneOf`'s own; redundant, harmless) |
| `not(oneof(string, integer))` (user tree) | `{"type":"not","not":{"oneOf":[{"type":"string"},{"type":"integer"}]}}` | `{"not":{"oneOf":[{"type":"string"},{"type":"integer"}]}}` |
| `not(ref("A"))` (user tree) | `{"type":"not","not":{"$ref":"#/definitions/A"}}` | `{"not":{"$ref":"#/definitions/A"}}` |
| `not(def("A", object{..}))` | `{"type":"not","not":{"$ref":"#/definitions/A"}}` | `{"type":"object","not":{"$ref":"#/definitions/A"}}` |
| `not(not(integer{multipleOf 2}))` (user tree) | `{"type":"not","not":{"not":{"multipleOf":2}}}` | `{"type":"integer","not":{"not":{"multipleOf":2}}}` |

A body with no keyword besides its type (`not(string)`, user tree) renders `{"type":"string","not":{}}`, which accepts
nothing: that is what `Schema[String]` "a string that is not a string" means. Nobody builds it on purpose (refined
never does); someone wanting "anything but a string" has no `Schema[T]` to describe it with anyway.
The same goes for `not[Any](`null`)` (B22-R2): `{"type":"null","not":{}}`, accepts nothing, not "anything but null".
Dropping `null` from `baseType` would make it the plain complement `{"not":{"type":"null"}}` instead; refined never
builds it, so either is defensible: kept for consistency (every leaf alike), and listed as an owner question.

Pros: every `not` refined builds validates as refined means; the integer-vs-number distinction is kept; only the
renderer changes (one file), no public API change; correct whether or not B15 lands first.
Cons: an `allOf`/`oneOf` with a `not` member states the type on every member instead of once (rule 4).

### B. Drop `"type": "not"` and type the body  — LOST

`{"not": {"type": "integer", "multipleOf": 2}}`. Smallest diff (`mkNot` with `includeType = true`, base skips `not`).
It is JSON Schema's own reading of the tree (complement of the body), but it isn't the tree's: `2.5` and `"abc"`
validate (table above), so `Int Refined NonDivisible[2]` accepts strings. Lost: the result is a `Schema[T]` that accepts
non-`T`s.

### C. Drop `"type": "not"`, keep the body typeless  — LOST

`{"not": {"multipleOf": 2}}`: a one-line change in the base. Strings are rejected only by accident (a typeless body
passes them, so `not` rejects them), and `2.5` validates: the `integer` is lost (c-b15's note). Lost.

### D. Option A, plus `not` sharing its base type in `allOf`/`oneOf`  — LOST (for now)

Rules 1-3 and 5 of A, but instead of rule 4, `memberType` / `mkAllOf` use the base type for a `not` member, so
`allof(not(int{..}), int{..})` renders `{"type":"integer","allOf":[{"not":{..}},{"exclusiveMaximum":7}]}`. Could also
be done by making `` `not`.jsonType `` return the body's type (a public method; it would have to avoid `def`/`ref`,
whose `jsonType` is `???`). Shorter output, but at the root the shared type is dropped today, so before B15 (#499)
lands this *loses* the `integer` at the root of `Int Refined (Not[Less[2]] And Less[7])` (`2.5` validates, as the
probe shows for today's root `allOf(not, not)`). Lost for now because it depends on B15's order; once B15 is merged
it is a pure size optimisation and can follow (a few lines in the same functions).

### E. Wrap in `allOf`: `{"allOf": [{"type": "integer"}, {"not": {...}}]}`  — LOST

Equivalent to A, more nesting, no gain. Lost.

### F. Change the ADT: `not[T](base: Schema[T], excluded: Schema[_])`  — LOST

Makes the base explicit, so a `not(string)` could mean "anything but a string" with an `any` base. Breaking (public
case class, refined's macros on both Scala versions, `SchemaEquality`, `AsTree`, the profiles' lowering, the test
harness), for a use nobody has. Lost.

### Legacy: keep `"type": "not"` under `JsonSchemaProfile.Legacy`  — LOST

The lead's policy (2026-11-23): Legacy freezes 0.7's style, not its bugs. `"type": "not"` makes the schema reject
everything, so the fix applies under Legacy too. No `golden-legacy` file has a `not` (nothing in the golden models
builds one), so no Legacy golden file changes. To keep the old output instead, the renderer would need the profile
(it doesn't see it today: `lowered` runs first): not worth it for a schema that never validated.

## Decision

Option A: a `not` states its body's single JSON type next to `"not"`, and the body is rendered without it; when the
body has no single type, no type is stated and the body keeps its own (plain complement). `allOf`/`oneOf` with a
`not` member render every member with its type; `Option` of a `not` stays a `oneOf` with `null`.

## Implementation plan

All code in `core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala` (shared by every draft, Scala
version and platform), plus a scaladoc line in `core/src/main/scala/json/Schema.scala` (step 6); no macro, refined or
ADT change.

1. Add the private helper `baseType(s: Schema[_]): Option[String]` (sketch in Option A), with `same(ms)` = the common
   `baseType` of every member when all are defined and equal. Put it next to `bodyType`/`memberType`.
2. `apply`'s `base` becomes a private recursive `statedType(x: Schema[_], includeType: Boolean): obj` (B22-R1):
   - before the generic cases: `` case n: `not`[_] => if (includeType) baseType(n.tpe).fold(obj.empty)(t => obj("type" -> t)) else obj.empty ``;
   - `` case `value-class`(inner) => statedType(inner, includeType) ``: a value class states what its inner node would
     (rule 3 falls out of it, no second `not`-specific line). Today it states `inner.jsonType` unless a later
     `mk*` overwrites the key; see Compatibility for the other inner nodes this touches.
3. `mkNot`: `obj("not" -> apply(x.tpe, Some(x), includeType = baseType(x.tpe).isEmpty, isRoot = false))`.
4. `mkAllOf` and `mkPlainOneOf` (or whatever B3 renamed it to, see Risks): `sameType` is false when a member is a `not`
   or a `value-class` of one (`isNot(s)`), so every member is rendered with `includeType = true`. In `mkPlainOneOf`
   it must cover both `known` **and** the `tpe` search: excluding a `not` only from `known` isn't enough, since with
   every member a `not`, `tpe` is `"not"` and `sameType` holds. Simplest: `!subTypesSeq.exists(isNot) && ...` in
   both `sameType`s. (A `def` whose body is a `not` has `bodyType` `"not"`, which never equals a real member type, so a
   `def(not)` member already takes the mixed path; an all-`def` alternative has no `tpe` at all.)
5. `mkNullable.singleType`: unchanged (it has no `not` case: an Option of a `not` keeps the `oneOf` form). Add a
   comment saying why (rule 5).
6. Leave `` `not`.jsonType `` (`"not"`) as is; a scaladoc line on it: "a pseudo type; the renderer states the body's".

Tests:

7. `api/src/test/scala/com/github/andyglow/jsonschema/AsDraft04Spec.scala`, `AsDraft06Spec`, `AsDraft07Spec`,
   `AsDraft09Spec`, `AsDraft12Spec`: a test "emit not" each, full JSON per draft, of one `object` with fields covering
   the table in Option A: `not(string(ipv4))`, `not(integer{multipleOf 2})`, `allof(not, integer)`, `allof(not, not)`,
   `oneof(not, null)` (not required), `not(oneof(string, integer))`, `not(ref("A"))`, `not(not(..))`, a value class
   of a `not`, and (B22-R3) `oneof(def("N", not(..)), integer{..})`, an all-`def(not)` `oneof`, and
   `oneof(value-class(not(..)), null)` (`mkNullable.singleType` recurses into value classes: this pins that it still
   takes the `oneOf` form); plus a root `not` and a root `allof(not, not)`. Step 8 validates the same three. Use `multipleOf`/`format`, not `exclusiveMinimum`, so
   draft-04's own exclusive* problem (see Risks) doesn't enter these expectations.
8. New `api/src/test/scala/com/github/andyglow/jsonschema/NotValidationSpec.scala` (shared; `petshop.JsonValidator`,
   so it validates on the JVM and is canceled on Scala.js): the "expected" rows of the tables above on draft-06, 07,
   2019-09 and 2020-12 (draft-04 with the `multipleOf`/`format` cases): `3` valid / `4`, `2.5`, `"abc"` invalid for
   `NonDivisible[2]`; `"abc"` valid / `"1.2.3.4"`, `5` invalid for `Not[IPv4]`; `3` valid / `1`, `9`, `2.5` invalid for
   `allof(not, not)` at the root and as a field; `null`, `3` valid / `4`, `2.5` invalid for the Option;
   `not(oneof(string{^a.*$}, string{^.*b$}))`: `"c"` valid, `"a"`, `"b"`, `5` invalid. With B14 merged (refined `Or`
   → `anyof`), the same with an `anyof` body, plus `"ab"` invalid (asked for by B14's design, which only asserts the
   tree); with today's `oneof` body `"ab"` is *valid* (`oneOf` fails when both match), which is B14's issue, not this one.
9. refined: new `modules/refined/src/test/scala-2.13+/com.github.andyglow.jsonschema.refined/RefinedNotRenderingSpec.scala`
   (literal types, so 2.13 and 3; 2.12 is covered by the api tests on hand-built trees): render draft-07 JSON of
   `String Refined Not[IPv4]`, `Int Refined NonDivisible[2]`, `Int Refined (Not[Less[2]] And Less[7])`,
   `Int Refined (Not[Less[2]] And Not[Greater[7]])`, `String Refined Not[StartsWith["a"] Or EndsWith["b"]]`, and a
   case class with a field of each, compared as JSON (refined's test classpath has no networknt; step 8 covers
   validation). The existing tree specs (`RefinedBooleanSpec`, `RefinedQuirksSpec`) don't change: the trees don't.
10. If E2 is merged: `api/src/test/scala/com/github/andyglow/jsonschema/AnyValueSpec.scala` (lines 60-61 on E2's
    branch) compares only the `not` body because of this bug: compare the whole `{"not": {}}`.
11. Golden files: none change. No golden or golden-legacy JSON contains `"not"` today (`git grep '"not"' -- '*.json'`
    is empty), so `clean api/test` must pass without `GOLDEN_UPDATE`; check it stays empty.

Docs:

12. README "Refined" (the "Supported predicates" list) and `docs/src/main/paradox/modules/refined.md` (same list):
    one line: "`Not[P]` states the type and negates the rest: `String Refined Not[IPv4]` is
    `{"type": "string", "not": {"format": "ipv4"}}`." (plain `scala`/`json` block or inline; no mdoc needed).

Gates when implemented: `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on all three (core and api change);
`scalafmtCheckAll` + `rootJS/scalafmtCheckAll`; `makeDocs` (README/paradox).

Risks and neighbours:

- **B15 (#499, design #504)** changes the `isRoot` guards in the same two functions (`mkAllOf` line 205,
  `mkPlainOneOf.rootType`). Rule 4 doesn't depend on them (mixed members carry their own type), so the two merge in
  either order with a textual conflict at most. After B15, Option D becomes safe.
- **B3 (#461, branch gap/b3-sealed-collapse)** turns `mkPlainOneOf` into a wrapper of
  `mkAlternatives(keyword, x, subTypesSeq, omitRootType)`, shared with a new `mkAnyOf` (an `anyOf` states its type at
  the root too), and adds `` `anyof` `` (`jsonType` `"anyof"`). If B3 lands first: step 4 goes into
  `mkAlternatives` (so it covers `anyOf` too; an all-`not` `anyOf` states `"type": "not"` even at the root there: add
  it to step 7's tests), and `baseType` gets an `anyof` case like `oneof`'s.
- **B14 (#500)** renders refined `Or` as `anyof`, so `Not[A Or B]` becomes `not(anyof(..))`: covered by `baseType`'s
  `anyof` case; the body keeps the `"type"` `mkAlternatives` states for a same-typed non-root alternative
  (`includeType = false` doesn't suppress it): redundant, correct. B14's `Disjoint`/`admitsNull` treat `not` as "any
  type" / "not null", consistent with rule 5.
- **E2 (#295, branch gap/e2-any-value)** adds `` `any` `` (`jsonType` `"any"`, renders `{}`) and
  `` `any`.admitsEverything ``; the parser maps a nested `false` to `not(any)`. `baseType` must give `None` for
  whatever `admitsEverything` accepts, so `not(any)` → `{"not": {}}` (accepts nothing, as `false`). E2's
  `AnyValueSpec` (lines 60-61) asserts only the `not` body because of this bug: tighten it to the whole `{"not": {}}`.
  E2 also reformats `mkAllOf`'s `typed`/`sameType` lines and adds `any` cases to `apply`'s base: put the `not` case
  next to them. Textual conflict only.
- **B27** (refined `Or` bounds) changes the *trees* of `Greater[7] Or Less[2]` etc.; this design only renders whatever
  `not` tree it gets.
- **draft-04 `exclusiveMinimum`/`exclusiveMaximum`** (neighbour, not in scope): draft-04 renders them as numbers,
  where draft-04 needs a boolean next to `minimum`/`maximum`; inside a `not` body this makes networknt complement an
  empty schema. Reported to the lead; step 7/8 avoid it.

## Compatibility

- Source/binary: no public signature changes (`AsDraftSupport` gets a private helper; `mkNot`'s signature stays).
- Output: only schemas that contain a `not` node change, on every draft and profile (Legacy included, see above).
  Each changed schema rejected every document before, except an all-`not` `allOf`/`oneOf` at the root, which
  validated loosely (accepted `2.5` for an integer refinement) and now validates as refined means.
  Golden files: no change.
- Side effect of step 2's recursion (B22-R1), outside `not`: a value class around a node that states no type of its
  own changes too. A mixed `oneof`/`allof` inside a value class loses its `"type": "oneof"`/`"allof"`; a `const` loses
  `"type": "const"`; a `def`/`ref` inside a value class (`jsonType` is `???`) renders its `$ref` instead of throwing
  `NotImplementedError`. Every one of these was invalid or a crash. Value classes around leaves, `enum`s and same-typed
  `oneOf`/`allOf` are unchanged (the `mk*` already overwrote the key with the same value). If the owner wants B22 to
  stay strictly within `not`, step 2's value-class case becomes `` case `value-class`(n: `not`[_]) `` only.
  No golden file states a pseudo type (`git grep -E '"type": ?"(oneof|allof|const|enum|not|anyof)"' -- '*.json'` is
  empty), so this changes no golden file either.
- PVP: a bug fix, patch (0.9.1).

## Open questions for the owner

1. After B15 (#499) is merged, fold in Option D (a `not` shares its base type in `allOf`/`oneOf`: shorter output)? (B22-R4)
2. `not(null)`: "nothing" (as every other leaf, chosen) or "anything but null"? (B22-R2)
3. Value class recursion (step 2) fixes the other pseudo types inside value classes too; or keep B22 to `not` only? (B22-R1)

## Review log

Round 1 (r-b22, on ee3f995; 0 must, 4 nits):

- B22-R1 (nit) make the value-class case a recursion → accepted: step 2 is now a recursive `statedType`; a value class
  states what its inner node would. The side effect on other inner nodes (mixed `oneof`/`allof`, `const`, `def`/`ref`:
  invalid pseudo types or a `???` crash today) is listed in Compatibility, with the `not`-only fallback, and as owner
  question 3. No golden file changes either way.
- B22-R2 (nit) `not(null)` → accepted: kept `null` in `baseType` (every leaf alike), documented next to the
  `not(string)` edge case with the alternative, owner question 2.
- B22-R3 (nit) tests for the passing claims → accepted: step 7 (and 8) add `oneof(def(not), integer)`, an all-`def(not)`
  `oneof`, and `oneof(value-class(not), null)`.
- B22-R4 (nit) Option D as a PR open question → accepted: owner question 1 (also goes into the PR body).
