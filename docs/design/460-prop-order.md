# Object keys keep their order  (issue #460, item B2)
Status: draft

## Problem

Rendered JSON objects lose their key order once they have more than 4 keys. Scala's immutable `Map` keeps insertion
order only up to 4 entries (`Map1`..`Map4`); from 5 on it is a `HashMap`, which iterates in hash order. The
`json.Schema` tree is ordered (`object.fields` is a `List` since 0.8.0, #298), and our own JSON AST `obj` is a
`mutable.LinkedHashMap` when built with `obj(k -> v, ...)`, but several steps go through an immutable `Map` on the way:

| where | code | what loses order |
|---|---|---|
| `AsDraftSupport.mkObj` | `x.fields.map(...).toMap ++ discriminator` | `properties` (case classes, tuples, discriminated members) |
| `AsDraftSupport.ValidationList.json` | `elements.map(...).toMap` | validation keywords of one schema |
| `AsDraftSupport.inferDefinitions` | `extractDefs(x, None).toMap` | `definitions` / `$defs` |
| `Value.obj.+` / `obj.-` | `obj(underlying.toMap + f)` | any object built with them |
| `Value.obj.toString` | iterates `value` (a `toMap`) | debug output |
| `ToValue.StrMapV` / `IntMapV` | `obj(v.toMap)` | a `Map` default (2.12 only: 2.13's `toMap` returns an immutable map as is) |
| `AsCirce`, `AsPlay`, `AsU`, `AsJson4s`, `AsSpray` | `x.toMap` / `x.value.toMap` / `x.value.toList`, `JsObject(Map)` | every object with 5+ keys, in both directions |

`required` is already ordered (built from `x.fields` as a `Seq`). The parser reads `properties` through `obj.fields`,
so it keeps order too.

Repro: `api/src/test/scala/com/github/andyglow/jsonschema/PropertyOrderSpec.scala` and a "keeps key order" property in
each bridge's `As*Spec`. Actual output (0.9.0, the same on 2.12, 2.13, 3 unless noted; JS is the same code):

| case | actual | expected |
|---|---|---|
| `case class Six(a1..a6: Int)`, `properties`, every draft | `a6, a5, a4, a3, a1, a2` | `a1..a6` |
| the same with defaults | `a6, a5, a4, a3, a1, a2` | `a1..a6` |
| `(Int, String, Boolean, Double, Long)` | `_3, _2, _1, _4, _5` | `_1.._5` |
| 5-field member of an `@discriminator` family | `a5, a4, a3, a1, type, a2` | `a1..a5, type` |
| `definitions` of `WithDefs(f1: D1, .., f6: D6)`, each `Di` an implicit `Schema` | `D4, D5, D6, D1, D3, D2` | `D1..D6` |
| `String` with `minLength, maxLength, pattern, contentEncoding, contentMediaType` | 2.13/3: `pattern, contentMediaType, maxLength, contentEncoding, minLength` (2.12 differs) | as added |
| `m: Map[String, Int] = ListMap(e, d, c, b, a)`, `default` | 2.12: `e, a, b, c, d`; 2.13/3: ok | `e, d, c, b, a` |
| `AsCirce`/`AsPlay`/`AsU`/`AsJson4s`/`AsSpray` of `obj(k1..k6)` | `k2, k5, k6, k1, k4, k3` | `k1..k6` |
| `required` of `Six` | `a1..a6` (ok) | |

## Why it is a problem

The output is stable from build to build (hash order is deterministic), and validators don't care about key order. But
the schema is read by people: as documentation, in code review, in diffs of a committed schema file (#108 asked for
exactly this). A 6-field class whose properties come out as `a6, a5, a4, a3, a1, a2` is hard to read and compare with the
model, and it differs from what the JSON libraries' own codecs write (they keep declaration order). Adding a sixth field
reshuffles every existing one, so a one-line model change shows up as a full-object diff. #108 was closed after the tree
and `required` became ordered; `properties` never did.

## Options

### A. Fix the three `.toMap` calls in the renderer only  — LOST
Build `properties`, the validation keywords and the definitions as ordered sequences
(`obj(mutable.LinkedHashMap(seq: _*))`) in `AsDraftSupport`.

- Pros: three lines in one file; fixes `JsonFormatter.format(AsValue.schema(...))`, the main path.
- Cons: the bridges still scramble every object of 5+ keys, so `schema.asCirce(...)`, `asPlay`, `asU`, `asJson4s`,
  `asSpray` users (most users) see no change. `obj.+`/`obj.-` keep dropping order, so the next renderer change that uses
  them reintroduces the bug. 2.12 `Map` defaults stay scrambled.

### B. Keep insertion order everywhere an `obj` is built or converted, without changing `obj`'s API  — WON
`obj` stays `case class obj(private val underlying: Map[String, Value])`. Every place that builds one inside the library
iterates in order and stores a `mutable.LinkedHashMap`:

1. `AsDraftSupport`: `mkObj`, `ValidationList.json`, `inferDefinitions` build ordered sequences (the three sites of A).
   Duplicate keys (a definition met twice) keep the position of the first occurrence, as `LinkedHashMap` does.
2. `Value.obj`: `+`, `-`, `++`, `deepMerge` build their result from `fields` (the ordered view of whatever map is
   underneath) into a `LinkedHashMap`, so they keep order even when one side was built from an immutable `Map`;
   `toString` iterates `fields`. A private helper `obj.ordered(Iterable[(String, Value)])` does it in one place.
   `value` (the `immutable.Map` view for lookups) stays as it is; its scaladoc says it has no order and points to `fields`.
3. `ToValue.StrMapV`/`IntMapV`: keep the default's own iteration order (`LinkedHashMap` instead of `toMap`).
4. Bridges, both directions: iterate `obj.fields` (not `value`/`toMap`) and build the library object from an ordered
   sequence: circe `Json.obj(seq: _*)` / `JsonObject.toList`; play `JsObject(seq)` / `JsObject.fields`; u-json
   `ujson.Obj.from(seq)` / `Obj.value` (a `LinkedHashMap`); json4s `JObject(list)` / `JObject.obj`; spray
   `JsObject(ListMap(seq: _*))` (spray's `JsObject` is a `Map`; its varargs constructor builds a `HashMap`) /
   `JsObject.fields`.

Sample output (Draft 07, `Six`), keys now as written:
```json
{"$schema": "http://json-schema.org/draft-07/schema#", "$id": "six", "type": "object", "additionalProperties": false,
 "properties": {"a1": {"type": "integer"}, "a2": ..., "a6": {"type": "integer"}},
 "required": ["a1", "a2", "a3", "a4", "a5", "a6"]}
```

- Pros: fixes every path users render through (own formatter and all five bridges), on 2.12/2.13/3 and JS, with no
  public signature change. One rule to keep: "an `obj` iterates in insertion order".
- Cons: touches 7 files; `obj.value` still has no order (it never had beyond 4 entries; we document it instead of
  changing a public `Map`'s performance profile).

### C. Make `obj` ordered by construction: `underlying: Seq[(String, Value)]` or an ordered immutable map  — LOST
The #108 thread suggested this. Replace the `Map` field with an ordered immutable structure, so no caller can lose order.

- Pros: order can't be lost by any later code; removes the mutable `LinkedHashMap` that today hides in an immutable-looking
  value (a caller can pass a mutable map to `obj(...)` and mutate it afterwards).
- Cons: `obj`'s constructor and extractor (`obj(map)`, `case obj(m) =>`) are public and used by users and by our
  bridges; changing their type is source- and binary-breaking (PVP: 0.10.0) for a bug fix. 2.12 has no `VectorMap`, so an
  ordered immutable map with fast lookup would need a shim or `ListMap` (O(n) lookup). Worth doing in a future
  breaking release, not here.

### D. Sort keys alphabetically  — LOST
`JsonFormatter.format(v, sorted = true)` already exists. It gives a stable order but not the declaration order users
asked for (#108), and it doesn't reach the bridges. Not a fix.

## Decision

B: keep insertion order in every place the library builds, combines or converts an `obj`, so `properties` follow the
field declaration order (discriminator last), definitions the order they are met (a definition after the ones it
uses), validation keywords the order they were added, on every draft, every Scala version and through every bridge.
No public API changes.

## Implementation plan

1. `core/.../json/Value.scala`: private `obj.ordered` helper; `+`, `-`, `++` (both), `deepMerge` through it; `toString`
   over `fields`; scaladoc on `fields` (ordered) and `value` (lookup, unordered).
2. `core/.../jsonschema/AsDraftSupport.scala`: `mkObj` props in field order, then the discriminator property last. With
   `phantom = false` a field of that name exists; the discriminator's `{"enum": [key]}` replaces it at the field's
   position (today's `toMap ++ discriminator` also lets the discriminator win; only the position is new).
   `ValidationList.json` and `inferDefinitions` ordered.
3. `core/.../json/ToValue.scala`: `StrMapV`/`IntMapV` ordered.
4. Bridges `AsCirce`, `AsPlay`, `AsU`, `AsJson4s`, `AsSpray`: both directions over ordered sequences.
5. Tests:
   - `api/.../PropertyOrderSpec.scala` (the repro, kept): properties on every draft, `required`, defaults, Tuple5,
     discriminated member, definitions, validation keywords, `Map` default; plus one full-text check of
     `JsonFormatter.format` for `Six` on Draft 07 so the whole key order is pinned.
   - `core/.../json/ValueSpec.scala`: `+`, `-`, `++`, `deepMerge`, `toString` keep order with 6+ keys, also when one side
     was built from an immutable `Map`.
   - Each bridge's `As*Spec`: "keeps key order" (added with the repro) and the reverse direction (library object of
     6 keys → `obj.fields` order).
6. Golden files: no change expected. `golden/` and `golden-legacy/` compare canonicalized JSON (keys sorted), and the
   tree dump already records field order. Run the full suites to confirm; any golden diff is a bug in the change.
   `AsDraftXXSpec` and the other specs compare structurally (`obj.equals` ignores order), so they shouldn't move either.
7. Docs: README "Supported types" row `case class` → "`object`, `properties` in declaration order"; the same row in
   `docs/src/main/paradox/types.md`. No CHANGELOG (the lead writes it).
8. Scala 2 vs 3: nothing version-specific; the macros are untouched (the tree was already ordered). 2.12 differs only in
   which keys happened to scramble. JS: `LinkedHashMap`/`ListMap` are plain Scala collections; run `rootJS/test`.

Out of scope (neighbours): `oneOf` members and `enum` values in hash order (B3, B7, #458): they are `Set`s in the
tree, not an `obj` problem.

## Compatibility

- Source and binary: no public signature changes (one new `private` helper). PVP: patch (0.9.1).
- Output: JSON-equal to 0.9.0 (same keys, same values); only the textual key order of objects with 5+ keys changes,
  for every profile, Legacy included. Byte-for-byte, Legacy's text changes for such objects, but 0.7 itself wrote them
  in hash order, which is no contract; the `golden-legacy/` files (canonicalized) don't change. Keeping hash order
  under Legacy would need a profile-aware `obj`, which no one wants.
- Users who diff a committed schema file see one reorder of objects with 5+ keys when they upgrade, then stable diffs.

## Review log
