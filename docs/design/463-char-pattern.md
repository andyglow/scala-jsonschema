# Char schema: a pattern that rejects every non-whitespace character  (issue #463, item B5)
Status: approved (r-b5, 2026-11-23); implemented on `gap/b5-char-pattern`

## Problem
The core Predefs for `Char` and `java.lang.Character` (`core/src/main/scala/json/schema/Predef.scala`, `chrS` / `jchrS`) are

```scala
`string`[Char].withValidation(`minLength` := 1, `maxLength` := 1, `pattern` := "^[.\\s]$")
```

and render (draft-04; every draft renders the same keywords, under every profile) as

```json
{ "type": "string", "minLength": 1, "maxLength": 1, "pattern": "^[.\\s]$" }
```

`[.\s]` is a character class: a literal `.` or one whitespace character. The intent was probably "any character, whitespace
included" (`.` or `\s`), but inside `[...]` the dot is a literal. Every inline use of a `Char` / `Character`, whether a
field, a collection element or `Json.schema[Char]` itself, carries this pattern. The 0.7.x releases had it, and so do
0.8.0 and 0.9.0.

Repro: `api/src/test/scala/com/github/andyglow/jsonschema/CharSchemaSpec.scala` (on this branch) validates documents with the
acceptance harness' networknt validator on draft-04, draft-07 and 2020-12, under Neutral and Legacy. Today 6 of its 8
tests fail, identically on 2.12.21, 2.13.18 and 3.3.9 (JVM):

```
neutral: Char accepts any single character *** FAILED ***
  draft-04: "a" is not valid: $: does not match the regex pattern ^[.\s]$
neutral: a Char field accepts any single character *** FAILED ***
  draft-04: {"c":"a","jc":"a"} is not valid: $.c: does not match the regex pattern ^[.\s]$; $.jc: does not match ...
(same for java.lang.Character and for Legacy)
```

On Scala.js the code is the same (a shared Predef), but the validating tests are canceled there.

networknt (Java regex engine), `{"type":"string","minLength":1,"maxLength":1}` plus each candidate pattern, the same
result on draft-04, draft-07 and 2020-12:

| pattern            | `"a"` | `" "` | `"."` | `"\n"` | `"\t"` | `"é"` | U+1F600 (a non-BMP emoji) | `""` | `"ab"` |
|--------------------|-------|-------|-------|--------|--------|-------|---------------------------|------|--------|
| `^[.\s]$` (today)  | NO    | ok    | ok    | ok     | ok     | NO    | NO                        | NO   | NO     |
| none               | ok    | ok    | ok    | ok     | ok     | ok    | ok                        | NO   | NO     |
| `^.$`              | ok    | ok    | ok    | **NO** | ok     | ok    | ok                        | NO   | NO     |
| `^[\s\S]$`         | ok    | ok    | ok    | ok     | ok     | ok    | ok                        | NO   | NO     |
| `^[\u0000-\uFFFF]$`| ok    | ok    | ok    | ok     | ok     | ok    | **NO**                    | NO   | NO     |

## Why it is a problem
Any schema with a `Char` in it rejects real data: `case class HasChar(c: Char)` rejects `{"c":"a"}`. Codecs that write a
`Char` write a one-character string (circe's `Encoder[Char]`, for example), so a validator using our schema rejects
their output for almost every value. There is no workaround short of an implicit `Predef[Char]` override in every
user's scope. The README and the paradox `types.md` say a `Char` is "exactly one character", and that's the intent.

## Surrogate pairs (what "one character" means)
JSON Schema counts string length in characters "as defined by" the JSON RFC (draft-04 §5.2.1 cites RFC 4627; later drafts the same
idea), i.e. Unicode code points, and validators count code points (networknt does, see the table).
A Scala `Char` is one UTF-16 code unit. The two only differ for characters outside the BMP (U+1F600, an emoji): one code point, so
`minLength`/`maxLength` 1 accepts it, but it is two code units, so it doesn't fit in a `Char`. The reverse case, a lone surrogate (a
`Char` like `'\ud83d'`), is one code point either way, so every option below accepts it.
The question is whether the schema should reject the supplementary characters (U+10000 and up) that a `Char` can't hold. That's
option C; the others accept them.

## Options
### A. Drop the pattern: `minLength` 1 + `maxLength` 1  — WON
`chrS` / `jchrS` become `` `string`[Char].withValidation(`minLength` := 1, `maxLength` := 1) ``.

```json
{ "type": "string", "minLength": 1, "maxLength": 1 }
```

- Pros: says exactly what the Predef meant ("exactly one character") with no regex, so there's no regex-dialect
  question for any validator, in any language, on any draft. It accepts every value a `Char` can hold, whitespace and line
  breaks included. It's the smallest change: one line per Predef.
- Cons: it also accepts a supplementary character (U+1F600), which no `Char` can hold. That's a false accept for a rare
  input. A schema that accepts slightly more than the type can hold is the usual trade-off in this library (`Byte` and `Short`
  are a plain `number` with no range today).

### B. Fix the regex: `^.$`  — LOST
Probably what the original author meant. Without the dotAll flag, `.` doesn't match line terminators in ECMA-262 (the
JSON Schema regex dialect) or in Java: networknt rejects `"\n"`, which is a valid `Char`. Line terminators are rare in
a `Char` field, but this would still reject valid data, so it's the same kind of bug, just smaller.

### C. Exact UTF-16 code unit: `minLength` 1 + `maxLength` 1 + `pattern` `^[\u0000-\uFFFF]$`  — LOST
Only BMP code points (or a lone surrogate): exactly the values a `Char` holds. networknt rejects U+1F600 (see the table).
In ECMA-262 the class rejects it both with the `u` flag (one code point above U+FFFF) and without it (two code units, and
`^…$` wants one).
- Pros: precise.
- Cons: a regex in every `Char`, so every consumer has to evaluate it. `\uXXXX` escapes aren't in the
  "recommended subset" that the JSON Schema docs advise for portability, and RE2-based validators (Go) spell them
  `\x{FFFF}`, so for some of them the pattern is unreadable or a compile error. That's a real risk, traded for
  rejecting emoji in a `Char` field, which hardly anyone needs. It also writes a regex where the old bug came from
  one.

### D. No constraints: plain `{"type": "string"}`  — LOST
This option would drop `minLength` and `maxLength` too.
- Pros: simplest output.
- Cons: it loses the information that is the reason `Char` has its own Predef: `""` and `"ab"` become valid. It also makes
  the README's "exactly one character" untrue.

`^[\s\S]$` together with the lengths is the same as A, with a redundant regex, so it isn't a separate option.

### E. Fix it, but keep `^[.\s]$` under Legacy only  — LOST
Legacy reproduces 0.7 byte for byte, and 0.7 had the pattern. But the Predef lives in the derived tree, and the tree is type-erased
(`` `string`[Char] `` is just a `string` at render time), so a profile can't tell a `Char` from a user's `string` with the same
lengths. Gating it would need a new marker node or flag just to bring back a pattern that rejects every letter, and no consumer can
rely on that. The lead agreed to change Legacy too (2026-11-23).

## Decision
A. Keep `minLength: 1` and `maxLength: 1` and drop the `pattern` from both `Char` and `java.lang.Character`, under every
profile, Legacy included. The lead agreed to Legacy changing (2026-11-23): 0.7's pattern rejected every non-whitespace character, so no consumer
can depend on it. The supplementary-character false accept is documented and left as is. If the owner wants option C, it's the
same one-line change with a different `pattern`, plus a row in the tests.

## Implementation plan
1. `core/src/main/scala/json/schema/Predef.scala`: drop `` `pattern` := "^[.\\s]$" `` from `chrS` and `jchrS`. The Scala 2 macro and the
   Scala 3 derivation both inline these Predefs (the repro fails identically on 2.12/2.13/3), so there's no macro change.
2. Tests:
   - `CharSchemaSpec` (shared, new): the validation tests above (JVM; canceled on JS), plus tests that run on
     both platforms: `Json.schema[Char]` and `Json.schema[Character]` equal `` `string`.withValidation(`minLength` := 1, `maxLength` := 1) ``,
     and the rendered JSON for each of the five drafts (no `pattern`), under Neutral and Legacy. A `Char` field
     inside a case class gets the same inline schema, and the invalid cases (`""`, `"ab"`, a number, `null`) stay invalid.
     It documents the supplementary-character false accept with a test (U+1F600, written as a JSON surrogate-pair escape, is accepted, as option A says), and a lone surrogate
     document (a value a `Char` holds) is accepted too, if Jackson parses it.
   - `modules/circe-json/.../petshop/CirceAcceptanceSpec.scala`: a `case class Initials(first: Char, last: Character)` with circe's
     `deriveEncoder` validates circe's real output against the schema (B5-R4).
   - No golden file contains a `Char` today (`git grep` finds no `[.\\s]`), in `golden/` or `golden-legacy/`, so no golden changes.
     `golden-legacy` stays byte-exact.
3. Docs: README ("`Char`: exactly one character") and paradox `types.md` already describe the fixed behavior. I'd add
   nothing to them. No CHANGELOG edit (the lead writes it).
4. Out of scope: the `Map[Char, V]` key pattern `^.{1}$`, which rejects a `"\n"` key. That's map keys, item B6, and the lead has forwarded it there.

## Compatibility
- Source and binary: no change (same vals, same types).
- Output: every schema with an inline `Char` / `java.lang.Character` loses `"pattern": "^[.\\s]$"`, on every draft and under
  every profile, **including `JsonSchemaProfile.Legacy`** (the lead agreed). The schemas only get more permissive, and only
  for documents the old schema wrongly rejected. Every document that was valid before is still valid.
- PVP: a patch (a bug fix, no API change). It can go out in 0.9.1.

## Review log
Round 1 (r-b5, at 0da59d1): approved, 4 nits, no musts.
- B5-R1 (nit) the emoji and the U+FFFF in the doc were mangled → accepted: the emoji is now written as "U+1F600" and option C's
  pattern as the ASCII text `\uFFFF`. The doc has no non-BMP characters left.
- B5-R2 (nit) use JSON escapes in the spec, plus a lone surrogate → accepted: the non-BMP document is `"\"\\ud83d\\ude00\""` in the
  source. Jackson parses a lone `"\\ud83d"` and networknt accepts it, so it's in `chars`. An escaped `é` and `"\r"` are in there too.
- B5-R3 (nit) say why Legacy-only gating lost → accepted: added option E.
- B5-R4 (nit, optional) a bridge check with real codec output → accepted: a circe `deriveEncoder` case in `CirceAcceptanceSpec`.
