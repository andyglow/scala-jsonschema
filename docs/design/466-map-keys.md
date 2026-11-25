# Map key schemas: patterns that reject real keys, keys not enforced, no UUID/Boolean key pattern  (issue #466, item B6)
Status: approved (r-b6, 2026-11-23)

## Problem

A `Map[K, V]` (derived by either macro, or `Predef.dictS`, or cats' `NonEmptyMap`) is a `dictionary` node whose key is
described by one `patternProperties` validation. The pattern comes from an implicit `json.Schema.dictionary.KeyPattern[K]`
(`core/src/main/scala/json/Schema.scala:261-274`), or, for enum keys, from the macro (`^(?:A|B)$`). Scala `String` keys
have no validation, and the renderer (`AsDraftSupport.mkDict`, `core/.../AsDraftSupport.scala:112`) writes its default `^.*$`.

Every draft renders the same shape, e.g. `Json.schema[Map[Int, String]]`:

```json
{"type": "object", "patternProperties": {"^[0-9]+$": {"type": "string"}}}
```

Four problems (repro: `api/src/test/scala/com/github/andyglow/jsonschema/MapKeySchemaSpec.scala`, networknt
json-schema-validator on draft-04, draft-07 and 2020-12, Neutral and Legacy; **14 of 20 tests fail on 2.12.21, 2.13.18
and 3.3.9**, JVM; the code is shared, so Scala.js renders the same):

1. **Integral keys: `^[0-9]+$` has no minus sign.** `Byte`/`Short`/`Int`/`Long` keys are written with `toString`. A probe of
   the libraries we bridge (scala-cli, 2.13; circe 0.14.15, play-json 3.0.6, upickle 4.4.3, json4s 4.0.7) writes
   `Map(-1 -> 1)` as `{"-1":1}` in all four.
2. **`Char` keys: `^.{1}$` misses line terminators.** In ECMA-262 (and Java) regexes `.` doesn't match `\n`, `\r`,
   U+2028, U+2029. play-json and upickle write `Map('\n' -> 1)` as `{"\n":1}`. (Same hole B5 / #463 found for the `Char`
   value pattern.)
3. **`String` keys: `^.*$` misses keys with a line break.** `"a\nb"` doesn't match: `.` stops at `\n` and `$` (without
   the `m` flag) only matches at the end.
4. **Key patterns are not enforced.** `patternProperties` only says which schema the values of *matching* keys follow. A
   key that matches no pattern is an additional property, allowed by default, and its value isn't checked at all.

Together (networknt, all three drafts, both profiles, all three Scala versions):

| schema | document | expected | today |
|---|---|---|---|
| `Map[Int, String]` | `{"-1":5}` | invalid (5 isn't a string) | valid (`-1` matches nothing, its value is unchecked) |
| `Map[Int, String]` | `{"abc":"a"}`, `{"1.5":"a"}`, `{"":"a"}` | invalid | valid |
| `Map[Char, String]` | `{"\n":5}` | invalid | valid |
| `Map[Char, String]` | `{"ab":"x"}`, `{"":"x"}` | invalid | valid |
| `Map[Color, Int]` (`Red`, `Green`) | `{"Blue":1}` | invalid | valid |
| `Map[String, Int]`, `Map[java.lang.String, Int]` | `{"a\nb":"x"}` | invalid | valid |
| `Map[IntId, String]` (`IntId(value: Int) extends AnyVal`) | `{"abc":"a"}` | invalid | valid |

5. **No built-in key pattern for `java.util.UUID` or `Boolean`.** `Json.schema[Map[UUID, V]]` / `Map[Boolean, V]` are
   compile errors ("a Map key must be ... Provide an implicit KeyPattern[...]"). circe and upickle write UUID keys as
   object keys (lowercase `toString`); play-json and upickle write Boolean keys as `"true"`/`"false"`. The README's
   example of a custom key pattern *is* UUID.

6. **Enum key names aren't regex-escaped** (r-b6, B6-R1). Both macros (Scala 2 `UDictionaries`, Scala 3
   `Collections.patternOf`) and `KeyPattern.forEnum` (enumeratum) join the raw names: case objects `` `N/A` ``, `` `1+` ``
   give `^(?:N/A|1+)$`, which matches `"1"` and `"11"` but not `"1+"`; a name like `"(none)"` makes an invalid regex.
   Today that only leaves values unchecked; once keys are enforced (4) it would reject real keys.

0.7 has the same output (the `golden-legacy/` files pin `^[0-9]+$`, `^.{1}$` and `^.*$`).

## Why it is a problem

- 1, 2, 3: a schema that rejects (or silently stops checking) documents the libraries really write. With 4 they don't
  even get rejected: the values under those keys just aren't validated, which is worse (silent).
- 4: a validator accepts any key, so the enum / integer / char restriction the README promises
  ("a `Map` key needs a known pattern") has no effect. A consumer generating code or validating input from the schema
  gets no key check.
- 5: two common key types need boilerplate, and the boilerplate in the README (`^[0-9a-f-]{36}$`) is looser than our
  own UUID value pattern.

## Options

Three independent decisions: (I) the patterns, (II) how keys are enforced, (III) Legacy.

### I. Patterns (in `KeyPattern`, so in the tree)

#### I-A. Fix the built-ins, add UUID and Boolean  — WON
| key | today | new |
|---|---|---|
| `Byte`, `Short`, `Int`, `Long` | `^[0-9]+$` | `^-?[0-9]+$` |
| `Char` | `^.{1}$` | `^[\s\S]$` |
| `java.lang.String` (`StringKeyPattern`) and the renderer's default for `String` keys | `^.*$` | `^[\s\S]*$` |
| `java.util.UUID` (new `UUIDKeyPattern`) | compile error | `^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$` |
| `Boolean` (new `BooleanKeyPattern`) | compile error | `^(?:true\|false)$` |

- Integral: deliberately loose (no range, leading zeros allowed): the readers accept more than the writers write (circe's
  `KeyDecoder[Int]` reads `"+1"` and `"01"`), and a per-type range regex (`-128..127` for `Byte`) is unreadable for little
  gain. `-` only, no `+`: no writer emits `+`.
- Char: `[\s\S]` is the portable "any character" class (no `s`/dotAll flag in JSON Schema regexes). Like B5's choice for
  the `Char` value, one non-BMP character (one code point, two `Char`s) is accepted by validators that match code points
  (Java's, ECMA with `u`) and rejected by ECMA without `u`: no `Char` key can be one, so either is harmless.
- UUID: the same regex as the UUID value pattern draft 04/06/07 already render (`Constants.RegEx.uuid`): both cases (writers
  emit lowercase, `UUID.fromString` and circe's `KeyDecoder[UUID]` read uppercase too).
- Picked by implicit scope (the `KeyPattern` companion), so a user's `KeyPattern[UUID]` in lexical scope (the README's
  pattern) still wins, with no ambiguity, on Scala 2 and 3 (tested).
- `BigInt`, `Double`, `Float` keys stay unsupported (not in this item; `Double`'s `toString` has `NaN`, `Infinity`,
  `1.0E10`, which needs its own design).
- Enum names are escaped (6): a new public helper `KeyPattern.escape(name)` backslash-escapes the ECMA-262 syntax
  characters `\ ^ $ . | ? * + ( ) [ ] { }` (not `Pattern.quote`: `\Q...\E` isn't ECMA-262), used by `forEnum`, by the
  Scala 3 derivation (in core) and by the Scala 2 macro at expansion time (`macros` depends on `core`). Names without those
  characters render exactly as today (`^(?:Red|Green)$`, `aa-aa`).
- **play-json and `Map[UUID, V]`** (B6-R2): play has no `KeyWrites[UUID]` and writes such a map as an array of pairs
  (`[["123e...",1]]`). Today `Json.schema[Map[UUID, V]]` needs a user `KeyPattern` and then gives the same object schema;
  with `UUIDKeyPattern` built in it compiles without one, so more play users reach a schema that doesn't describe play's
  output. The profile can't correct it (the tree doesn't keep the key type, and II-E lost). Kept, because circe and upickle
  users (who write UUID keys as object keys) gain, and documented: a play-json note in README / paradox, a play acceptance
  test that pins the mismatch (tagged `TRIAGE-B`), and a profile follow-up mailed to the lead. The other bridges: json4s
  throws for UUID / Boolean keys and circe has no `KeyEncoder[Boolean]` / `KeyEncoder[Char]`, so they write no document
  that a schema could misdescribe; spray only writes `Map[String, V]`.

#### I-B. Strict canonical integers `^-?(?:0|[1-9][0-9]*)$`  — LOST
Matches exactly what `toString` writes, but rejects `"01"` that every reader accepts, and reads worse. A schema describes
the wire format, which readers define as much as writers.

#### I-C. String keys: `additionalProperties: V` instead of `patternProperties {"^.*$": V}`  — LOST
The idiomatic "map with any string key" (and the shape OpenAPI generators turn into `Map<String, V>`). But it changes the
*shape* of every `Map[String, V]` on every profile (Legacy included: a style change, which the lead policy reserves), and
the pattern fix (I-A) is enough for correctness. A good follow-up for a profile knob, if wanted.

#### I-D. String keys: an unanchored `^.*` or `""`  — LOST
Both match every string (patterns are searches, so the empty match at 0 succeeds) and keep the diff tiny, but they read
like a typo, and whoever edits them later "fixes" them back to `^.*$`.

### II. Enforcement (in the renderer; only when the key is constrained)

"Constrained" = the dictionary has a `patternProperties` validation whose pattern is neither the new any-string pattern
`^[\s\S]*$` nor the old `^.*$` (B6-R4: a schema parsed back by `ParseJsonSchema`, or a user `KeyPattern.mk("^.*$")`,
still carries `^.*$` and means "any key"; it keeps its text and gets no restriction). So `Map[String, V]` and
`Map[java.lang.String, V]` render exactly as before (bar the I-A pattern), and so does `Map[StrId, V]` (a value class of `String`).

#### II-A. `propertyNames` on draft 06+, `additionalProperties: false` on draft 04  — WON
```json
draft 06, 07, 2019-09, 2020-12:
{"type": "object", "patternProperties": {"^-?[0-9]+$": {"type": "string"}}, "propertyNames": {"pattern": "^-?[0-9]+$"}}
draft 04 (no propertyNames):
{"type": "object", "patternProperties": {"^-?[0-9]+$": {"type": "string"}}, "additionalProperties": false}
```
- `propertyNames` says what it means ("every key matches P"), and validators report a bad key as a key that doesn't match
  the pattern rather than as a disallowed additional property (the exact networknt messages for both go in the PR).
- A tool that ignores `patternProperties` (some code generators) sees an open object, as today. With II-B it would see a
  closed, empty object.
- Cost: draft 04 renders differently from 06+, a small per-draft hook (`AsDraftSupport.keyRestriction`, overridden in `AsDraft04`).

#### II-B. `additionalProperties: false` on every draft  — LOST
Same validation result, one shape for every draft, one line of code. Lost on the two points above (error messages, and
tools that drop `patternProperties` would see `{"type":"object","additionalProperties":false}`: an object that allows
nothing). Easy to switch to: drop the `AsDraft04`-only override and use its body everywhere.

#### II-C. `propertyNames` and `additionalProperties: V` on 06+ (no `patternProperties`)  — LOST
The cleanest 06+ shape, but it drops `patternProperties`, which every Map schema has had since 0.1; anyone post-processing
our schemas (the parser module reads `patternProperties` back into a `dictionary`) would break. Too big for a bug fix.

#### II-D. Fix the patterns only, don't enforce  — LOST
Smallest change, but leaves 4 (a key check that has no effect), the core of the item.

#### II-E. Put the restriction in the tree (a `propertyNames` validation or a field on `dictionary`)  — LOST
Would let a profile (Legacy) or a user turn it off, but it's a new public API surface (a validation, `Magnet`s, the parser,
`AsTree`, equality) for a choice nobody has asked to make, and the pattern itself already lives in the tree. The renderer
is where `patternProperties` is shaped today.

### III. Legacy

#### III-A. Fix under every profile, Legacy included  — WON
The lead's policy (2026-11-23): Legacy freezes 0.7's style, not its bugs. The patterns live in the tree (built at compile
time from `KeyPattern`s) and the renderer doesn't see the profile, so this is also the only option that needs no new
mechanism. The affected `golden-legacy/` files are regenerated deliberately (`GOLDEN_UPDATE_LEGACY=1`) and listed in the PR.

#### III-B. Legacy keeps the 0.7 patterns and no enforcement  — LOST
Needs the tree to remember which `KeyPattern` produced a pattern (or the renderer to know the profile), and a lowering that
maps `^-?[0-9]+$` back to `^[0-9]+$` etc. Lots of machinery to keep schemas that reject or skip keys the 0.7 users' own
libraries write. Choosing it later means: a `Lowering` case for `dictionary` that rewrites the four patterns and a
renderer flag (or II-E) to skip the restriction.

## Decision

Fix the built-in key patterns (`^-?[0-9]+$`, `^[\s\S]$`, `^[\s\S]*$`), add `UUIDKeyPattern` and `BooleanKeyPattern`, and
regex-escape enum key names (I-A). Enforce constrained keys with `propertyNames {pattern}` on draft 06+ and `additionalProperties: false` on draft 04
(II-A). Under every profile, Legacy included (III-A).

## Implementation plan

1. `core/src/main/scala/json/Schema.scala` (`dictionary.KeyPattern`): new patterns for `Byte`/`Short`/`Int`/`Long`/`Char`/
   `String`; add `implicit object UUIDKeyPattern extends KeyPattern[java.util.UUID](...)` and
   `implicit object BooleanKeyPattern extends KeyPattern[Boolean]("^(?:true|false)$")`. Scaladoc on `KeyPattern` listing
   the built-ins and saying the renderer enforces the pattern. (`Constants` is `private[jsonschema]` in another package:
   the UUID regex literal is shared by moving it to a `private[json]` constant both use, or duplicated with a comment; the
   implementation picks the one that compiles cleanly on all three versions.)
   Add `def escape(name: String): String` (ECMA-262 syntax characters) and use it in `forEnum`.
2. `core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala` `mkDict`: default pattern
   `StringKeyPattern.pattern` instead of the literal `^.*$`; when the pattern is neither that one nor `^.*$`, add
   `keyRestriction(pattern)`. `protected def keyRestriction(pattern: String): obj = obj("propertyNames" -> obj("pattern" -> pattern))`;
   `AsDraft04` overrides it with `obj("additionalProperties" -> false)`.
3. Macros: built-in and user `KeyPattern`s are read at run time and found by implicit search (no change). Enum-key
   patterns: escape each name with `KeyPattern.escape` in Scala 2 `UDictionaries` (both `mkString` sites, at expansion
   time) and Scala 3 `Collections.patternOf`. Update the Scala 3 scaladoc of `Collections` (lines 17-19, 37) that names
   the old patterns. (`KeyKind.Uuid` already exists; Boolean keys are `KeyKind.Custom`.) `clean` before testing.
4. Tests:
   - `MapKeySchemaSpec` (the repro, all green after the fix), plus: UUID keys (lowercase and uppercase valid, `"abc"` and a
     36-char non-UUID invalid), Boolean keys (`"true"`/`"false"` valid, `"yes"`/`"True"` invalid), a value class over
     `UUID` as key, a user `KeyPattern[UUID]` in lexical scope wins over the built-in, `implicitly[Predef[Map[UUID, Int]]]`
     resolves (`dictS`), rendered shapes per draft (draft-04 `additionalProperties:false`, 06/07/2019-09/2020-12
     `propertyNames`, String keys neither).
   - Tree specs that pin the old patterns: `SchemaMacroSpec`, `collections/CollectionsSpec`, `scala-3/.../Scala3CollectionsSpec`,
     `CRLFSourceSpec`, `modules/cats/.../CatsSupportSpec`. Rendered-JSON specs with `^.*$` / a dictionary: `AsDraft04Spec`,
     `AsDraft07Spec`, `ProfileSpec` (and `ValidationMagnetSpec` if it renders). `modules/enumeratum` tree specs are unchanged
     (enum patterns), its rendered specs, if any, gain `propertyNames`.
   - `characterization/TriageBPendingSpec` "Map with a UUID key: clear compile error": UUID now compiles; switch the test to
     another unsupported key type (`java.time.LocalDate`), same assertion. (B11 also edits this file's messages: textual
     conflict at most.)
   - Enum names with metacharacters (B6-R1): sealed case objects `` `N/A` ``, `` `1+` ``, `` `(none)` `` and a Scala 3
     `enum` with backticked cases, on both versions: pattern text and networknt (`{"1+":1}` valid, `{"1":1}`, `{"11":1}`
     invalid); an enumeratum `entryName` with `+` / `.` in `EnumeratumSupportSpec`; `KeyPattern.escape` unit test.
   - The parser module (B6-R4): its specs still pass; a new parse -> render round-trip of `{"patternProperties":{"^.*$":..}}`
     renders no `propertyNames` / `additionalProperties`.
   - Acceptance (B6-R3): a petshop model `Stock(byId: Map[Int, Int], byUuid: Map[UUID, Int], byFlag: Map[Boolean, Int],
     byChar: Map[Char, Int])` (each field only where the library has key codecs) with negative / `'\n'` / UUID keys, written
     by the real library in each bridge's acceptance spec and validated under that bridge's profile on 04/07/2020-12:
     circe (Int, UUID), play (Int, Boolean, Char; UUID pinned as the mismatch of B6-R2, `TRIAGE-B`), upickle (all four),
     json4s (Int), spray (none: `Map[String, V]` only, so not added). Each spec also checks a document with a bad key is
     rejected. Enum keys need user key codecs in every library, so they stay in `MapKeySchemaSpec`.
   - Scala.js (B6-R5): `api/src/test/scala-js/.../KeyPatternRegExpSpec` compiles each built-in pattern and an escaped enum
     pattern with `js.RegExp` and matches the samples (`-1`, `\n`, `a\nb`, uppercase UUID, `true`, `1+`), so ECMA-262 is
     checked, not only the JDK regex networknt uses.
5. Golden files: `GOLDEN_UPDATE=1 sbt clean api/test` and `GOLDEN_UPDATE_LEGACY=1` for `golden-legacy/`, on 2.12, 2.13
   and 3 (check no `@<ver>` override appears). Expected diff: pattern strings, and a `propertyNames` /
   `additionalProperties:false` next to every constrained `patternProperties`; nothing else. Every changed file goes in
   the PR (old -> new pattern per file).
6. Docs: README "Supported types" (Map row: keys enforced), the "A `Map` key needs a known pattern" paragraph (add `UUID`,
   `Boolean`; the custom example becomes a non-built-in type, e.g. `java.time.LocalDate` `^[0-9]{4}-[0-9]{2}-[0-9]{2}$`),
   and the same text in `docs/src/main/paradox/types.md`; a play-json note (`Map[UUID, V]` is an array of pairs in play,
   so its schema doesn't match; give play a `KeyWrites`/`KeyReads` or a `Predef`) in the README's play section and the
   paradox play page. `makeDocs`.
7. JS: shared code only; all regexes are ECMA-262 (no lookbehind, no flags). `rootJS/test` on 2.13 and 3 (core changed:
   all three).

## Compatibility

- Source: compatible. New implicit instances only make previously failing derivations compile; a user `KeyPattern[UUID]`
  or `KeyPattern[Boolean]` in lexical scope keeps priority over the companion's (implicit scope). A user instance placed
  where only implicit scope sees it (e.g. a companion of a type alias) isn't possible for these Java/Scala types.
- Binary: compatible (new members of `KeyPattern`'s companion; changed string constants).
- Output: changes for every `Map` with a non-`String` key, on every draft and profile (pattern + `propertyNames` /
  `additionalProperties:false`), and the pattern of every `String`-keyed map (`^.*$` -> `^[\s\S]*$`). Every document
  valid before stays valid **except** documents with keys the type can't produce (`{"abc": ..}` for `Map[Int, _]`), which
  is the point. Documents that were valid only because a value went unchecked (`{"-1": 5}` for `Map[Int, String]`) become
  invalid, which is also the point.
- PVP: no API break; a bug fix with an output change. Patch (0.9.x) is defensible; the lead/owner may prefer a minor bump
  because the schemas of existing types change.
- Enum keys whose names contain regex syntax characters get an escaped pattern (`^(?:N/A|1\+)$`); others are unchanged.
- play-json: `Map[UUID, V]` now compiles without a user `KeyPattern` and gives a schema that doesn't match play's
  array-of-pairs output (documented, pinned; profile follow-up with the lead).
- Public API: `KeyPattern.UUIDKeyPattern`, `KeyPattern.BooleanKeyPattern`, `KeyPattern.escape` (additive).
- Legacy: changes (III-A), per the lead's policy; listed in the PR.

## Neighbours (not fixed here)
- `scala.Enumeration` `Value("C++")`: the value enum and the key pattern use the `val` name (`CPP`), but libraries write
  `toString` (`"C++"`). Pre-existing, wrong for values too; with enforcement `{"C++":1}` becomes a rejection for keys
  (r-b6 B6-R6, mailed to the lead).
- refined `EndsWith` renders `^.*<v>$`: a string with a line break before the suffix is rejected.
- upickle 4.4.3 writes a `Long` key beyond 2^53 as `""-9223372036854775808""` (invalid JSON; it quotes large longs, then
  quotes the key again). Upstream; the acceptance sample uses a `Long` key within 2^53.
- JDK regex `$` also matches before a final `\n` (networknt accepts the key `"1\n"` for `^-?[0-9]+$`), ECMA-262 doesn't.
  A validator difference; no action.

## Review log
B6-R1 (must) escape enum key names → accepted: `KeyPattern.escape`, used by `forEnum` and both macros; Problem 6, I-A, plan 1/3, tests.
B6-R2 (must) play + `Map[UUID, V]` → accepted, option (a): kept `UUIDKeyPattern`, documented the play mismatch (I-A, Compatibility,
docs step), a pinning `TRIAGE-B` test in the play acceptance spec, follow-up mailed to the lead. Other bridges checked: no document to misdescribe.
B6-R3 (must) petshop acceptance for maps → accepted: `Stock` model per bridge (plan 4, "Acceptance"); enum keys stay in
`MapKeySchemaSpec` because every library needs user key codecs for them.
B6-R4 (nit) `^.*$` from outside → accepted: `^.*$` counts as unconstrained too (keeps its text); parse -> render round-trip test.
B6-R5 (nit) ECMA coverage → accepted: `js.RegExp` spec on Scala.js.
B6-R6 (nit, neighbour) → noted under Neighbours.
