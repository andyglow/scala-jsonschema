# String-like Predefs: Locale, Currency, Pattern/Regex, InetAddress, Path, File, Charset  (issue #475, item E5)
Status: draft

## Problem
These types have no built-in schema, so a model that uses one of them doesn't compile:
`java.util.Locale`, `java.util.Currency`, `java.util.regex.Pattern`, `scala.util.matching.Regex`,
`java.net.InetAddress`, `Inet4Address`, `Inet6Address`, `java.nio.file.Path`, `java.io.File`, `java.nio.charset.Charset`.

```scala
case class Settings(locale: Locale, currency: Currency, host: InetAddress)
Json.schema[Locale]; Json.schema[InetAddress]; Json.schema[Settings]
```

Actual, the same on 2.12.21, 2.13.18 and 3.3.9 (both macros; 0.9.0 and master):

```
schema for java.util.Locale is not supported,
schema for java.net.InetAddress is not supported,
schema for java.util.Locale is not supported, Settings
```

and likewise for the other eight. (`InetAddress` is a sealed Java class on JDK 17+, with `Inet4Address`/`Inet6Address`
as its subclasses; with a Predef the macros never try to treat it as a family.)

The repro is `api/src/test/scala/com/github/andyglow/jsonschema/StringLikeSchemaSpec.scala` (target behavior; on
master it doesn't compile) and `api/src/test/scala-jvm/.../StringLikeSamplesSpec.scala` (the sample texts are what
the JDK writes; JVM only, see "Scala.js").

## Why it is a problem
These are ordinary field types in configuration and API models (a user's locale and currency, an allowed-host list, a
file path, a validation regex, an encoding). Every user writes the same one-line `Predef` per type, and has to pick a
format or pattern without guidance. The README lists `UUID`/`URI` as string-likes supported "out of the box"; these
are the next most common.

## What the codecs write
Measured on 2.13 (circe 0.14.17, play-json 3.0.6, upickle 4.4.3, jackson-module-scala 2.18 for comparison;
`toString` is what a hand-written codec usually uses):

| value | `toString` | circe | play-json | upickle | Jackson |
|---|---|---|---|---|---|
| `Locale.US` | `en_US` | no codec | `"en-US"` | no codec | `"en_US"` |
| `Locale.forLanguageTag("sr-Latn-RS")` | `sr_RS_#Latn` | – | `"sr-Latn-RS"` | – | `"sr_RS_#Latn"` |
| `Locale.ROOT` | `` (empty) | – | `"und"` | – | `""` |
| `Currency` USD | `USD` | `"USD"` | – | – | `"USD"` |
| `Pattern` `^a+\d$` | `^a+\d$` | – | – | – | `"^a+\\d$"` |
| `Regex` `^a+\d$` | `^a+\d$` | – | – | – | an object (bean) |
| `InetAddress` 127.0.0.1 | `/127.0.0.1` | – | – | – | `"127.0.0.1"` |
| `InetAddress` ::1 | `/0:0:0:0:0:0:0:1` | – | – | – | `"0:0:0:0:0:0:0:1"` |
| `Inet6Address` fe80::1%1 (scoped) | `/fe80:0:0:0:0:0:0:1%1` | – | – | – | `"fe80:0:0:0:0:0:0:1%1"` |
| `InetAddress` with a host name | `example.com/1.2.3.4` | – | – | – | `"example.com"` |
| `Path` | `/tmp/a b/c.txt` | – | – | – | `"file:///tmp/a%20b/c.txt"` |
| `File` | `rel/c.txt` | – | – | – | the absolute path |
| `Charset` | `UTF-8`, `windows-1252` | – | – | – | `"UTF-8"` |

Hand-written codecs: `getHostAddress` (`127.0.0.1`, `0:0:0:0:0:0:0:1`, `fe80:0:0:0:0:0:0:1%1`), `Locale.toLanguageTag`
or `toString`, `Charset.name`, `Pattern.pattern` / `Regex.regex`, `Path.toString`.

How networknt 1.5.9 (the validator of our acceptance suite) treats the candidate formats. It asserts `format` on
draft-04/06/07 and treats it as an annotation on 2019-09 and later; unknown formats are ignored on every draft
(the acceptance suite runs draft-04, 07 and 2020-12):

| text | `ipv4` | `ipv6` | `regex` |
|---|---|---|---|
| `10.0.0.1` | ok | rejected | |
| `0:0:0:0:0:0:0:1`, `::1` | rejected | ok | |
| `fe80:0:0:0:0:0:0:1%1` (scoped, as Java writes it) | | **rejected** | |
| `example.com` | rejected | rejected | |
| `^a+\d$`, `a++`, `(?i)abc`, `\A\d+\z`, `(?<n>a)`, `\p{javaLowerCase}+` (Java syntax) | | | ok |
| `(`, `[a-`, `a{,3}` | | | rejected |

So networknt's `regex` check is lenient towards Java-only syntax (it checks with a Java-compatible engine), and it
asserts `regex` on draft-04/06 too, although those drafts don't define that format. A strict ECMA 262 validator (ajv
with ajv-formats) would reject Java-only syntax such as `(?i)abc`, `a++` or `\A...\z`.

## Options
### A. Core Predefs: a standard `format` where one exists, a `pattern` where the text has a fixed shape, else a plain string — WON
Add to `json.schema.Predef` (core, shared by both macros and both platforms):

| type | schema | rendered (every draft, every profile) |
|---|---|---|
| `Locale` | `string` | `{"type":"string"}` |
| `Currency` | `string` + pattern `^[A-Z]{3}$` | `{"type":"string","pattern":"^[A-Z]{3}$"}` |
| `Pattern`, `Regex` | `string(regex)`, a new `Format.regex` | `{"type":"string","format":"regex"}` |
| `InetAddress` | `string` | `{"type":"string"}` |
| `Inet4Address` | `string(ipv4)` | `{"type":"string","format":"ipv4"}` |
| `Inet6Address` | `string(ipv6)` | `{"type":"string","format":"ipv6"}` |
| `Path`, `File` | `string` | `{"type":"string"}` |
| `Charset` | `string` + pattern `^[A-Za-z0-9][A-Za-z0-9+:_.-]*$` | `{"type":"string","pattern":...}` |

Per type:
- **Locale**: plain. The two written forms disagree (`en_US` vs `en-US`, `""` vs `und`, `sr_RS_#Latn` vs
  `sr-Latn-RS`), so a pattern for one rejects the other, and one for both (`^[A-Za-z0-9_#-]*$`) validates almost
  nothing.
- **Currency**: ISO 4217 alphabetic code, the only form anyone writes (`toString`, circe, Jackson). The JVM test
  checks that every code in `Currency.getAvailableCurrencies` matches. `[A-Z]` rather than a class, as the other
  Predef patterns do (ECMA 262 vs Java).
- **Pattern / Regex**: `format: regex` is the standard format for "a regular expression" (draft 07 and later). It
  needs a new `case object regex extends Format` in `json.Schema.string.Format` (next to `ipv4`, `uri`, ...). The
  renderers already emit any format by its name (`Pre09.mkStr` writes `productPrefix` for everything except
  `duration`/`uuid`/`idn-hostname`), so it renders on every draft, draft-04/06 included, as `date`/`time` already do
  (they are also draft 07 formats). Validators ignore formats they don't know (networknt does, on every draft), and networknt checks
  `regex` leniently on draft-04/07 (table above).
- **InetAddress**: plain. It is either family member, and `oneOf [ipv4, ipv6]` is wrong: where formats are
  annotations (2019-09 and later; networknt there) both branches match every string, so `oneOf` rejects every value
  ("must be valid to one and only one schema, but 2 are valid"). The model has no `anyOf` node, and an IPv4|IPv6
  pattern is a page of regex. `anyOf [ipv4, ipv6]` is the natural follow-up if an `anyOf` node appears (E6 discusses
  one for union types); it would still reject Jackson's host-name form (`example.com`), a second reason plain wins
  today. (refined's `IP` already renders the `oneOf` form: a neighbouring bug, reported to the lead, not fixed here.)
- **Inet4Address / Inet6Address**: `ipv4` / `ipv6`, the standard formats. Con: Java writes a *scoped* link-local IPv6
  address as `fe80:0:0:0:0:0:0:1%1`, which RFC 4291's text form (and networknt on draft-04/07) rejects. Such addresses
  only come from interface enumeration on the local host; addresses in payloads (parsed from text) have no scope.
- **Path / File**: plain. No standard format; a path is any text on POSIX.
- **Charset**: the legal-name rule from the `Charset` javadoc ("must begin with a letter or digit; may contain
  letters, digits, `-`, `+`, `:`, `_`, `.`"). The JVM test checks every name and alias in `Charset.availableCharsets`.
  Rejects `""`, spaces, `utf8;`.

To attach a `pattern` to `string[Currency]` / `string[Charset]`, core needs `validation.Magnet[Currency, String]` and
`Magnet[Charset, String]` (next to `uuidMagnet`), as E3 (#474) adds for its patterned types. They are public
implicits, so users can add string validations to these schemas too. **The rule (E3's rule, a choice): a Magnet only
where core attaches a validation.** So `Json.schema[Currency].withValidation(maxLength := 3)` compiles, while
`Json.schema[Locale].withValidation(pattern := ...)` doesn't, exactly as for `URI`/`URL` today. (Also, a Magnet for
`InetAddress` and one for `Inet4Address` could both match on Scala 3, where `withValidation`'s `TT >: T` is still open
during the search.)

Pros: every schema accepts what the JDK and the codecs write (except the scoped-IPv6 case), the patterns and formats
reject junk where the text has a fixed shape, and users get the standard formats that tools (OpenAPI generators,
form builders) understand. Additive: no existing output changes. Cons: one new public `Format` member; two patterns to
maintain.

### B. Plain strings only — LOST
Every type → `{"type":"string"}`. No new `Format`, no Magnets, nothing can reject a valid value. But it throws away
information that has a standard spelling (`ipv4`, `ipv6`, `regex`) and validates nothing (`"usd"` passes as a
`Currency`). Easy to choose instead: drop the formats/patterns from the Predefs; the tests' tables say which change.

### C. Strict formats and patterns everywhere — LOST
`Locale` with a BCP 47 pattern (play's form), `InetAddress` as `oneOf`/`anyOf` of `ipv4`/`ipv6` (needs an `anyOf`
model node: new node, renderers, profiles), `Path`/`File` with an absolute-path pattern, `Charset` as an `enum` of the
JDK's charsets. Each rejects values real codecs write (Jackson's `en_US`, a relative path, a charset added by a
provider) or is wrong under annotation-only formats (`oneOf`), and `anyOf` is a model change far beyond this item.

### Per-type alternatives (easy to choose instead)
- `Pattern`/`Regex` as plain strings (if the owner prefers not to add a `Format` member, or worries about strict
  ECMA 262 validators such as ajv with ajv-formats rejecting Java-only syntax like `(?i)` or `\A`): drop the format.
- `Format.regex` only on draft 07+ (dropped on draft-04/06 in `Pre09.mkStr`, like `uuid` is): needs the draft in
  `mkStr` (today `Pre09` is shared by 04/06/07 and doesn't know which one it renders), and `date`/`time` already set
  the precedent of emitting draft-07 formats on draft-04.
- `Inet6Address` as a plain string, if scoped addresses matter: drop the format.
- `Locale` with play's BCP 47 form as a pattern, for users who only use play-json: they declare their own
  `implicit val l: Predef[Locale]`, which wins over the built-in one (see Compatibility).

## Decision
A: ten new core Predefs (four plain strings, `regex` for `Pattern`/`Regex` via a new `Format.regex`, `ipv4`/`ipv6`
for `Inet4Address`/`Inet6Address`, ISO 4217 and charset-name patterns for `Currency`/`Charset`) and two String
Magnets. Additive only.

## Implementation plan
1. `core/src/main/scala/json/Schema.scala`: `case object `regex` extends Format` in `string.Format` (with a comment:
   "added in draft 07; ECMA 262"). `Pre09.mkStr` and the 2019-09+ renderer need no change (they write
   `productPrefix`); a renderer test pins it on every draft.
2. `core/src/main/scala/json/schema/validation/Magnet.scala`: `currencyMagnet`, `charsetMagnet` (`Magnet[T, String]`),
   as a separate block under a `// string-likes (#475)` comment after `uuidMagnet`, apart from where E3 (#474) adds its
   Magnets, so whichever PR lands second rebases trivially.
3. `core/src/main/scala/json/schema/Predef.scala`: `localeS`, `currencyS`, `patternS`, `regexS`, `inetAddressS`,
   `inet4AddressS`, `inet6AddressS`, `pathS`, `fileS`, `charsetS`, as one block under a `// string-likes (#475)` comment
   after `durationS` (E3 adds next to the java.time vals); the two patterns as private vals in `object Predef`. Nothing
   in either macro changes (rule 2 of the core design rule: found by implicit search).
4. Tests:
   - `StringLikeSchemaSpec` (shared, committed with this doc): trees, also as fields (inlined, never a definition);
     rendering on all five drafts under Neutral and Legacy; networknt validation (JVM; canceled on JS) of the written
     forms and of junk: patterns and plain strings on every draft, formats on draft-04/07.
   - `StringLikeSamplesSpec` (JVM only): the sample texts are what the JDK writes; every JDK currency code and every
     charset name/alias matches its pattern.
   - To add during implementation: a local `implicit Predef[Locale]` and a local `implicit Schema[Currency]` still win
     over the built-ins (the Compatibility claim), on both Scala versions; `Json.schema[Currency].withValidation(maxLength := 3)`
     compiles (the Magnet); a `regex` row in `AsDraft04Spec` next to the `ipv6` one (L117). That is the only
     renderer spec that enumerates formats (AsDraft06/07/09/12 only use `uuid` in a model); the other drafts are
     pinned by `StringLikeSchemaSpec`'s every-draft rendering test.
5. Golden files: none change (no golden model has these types; they didn't compile). `golden-legacy`: none.
6. Scala.js: the Scala.js 1.23 javalib has `java.util.regex.Pattern` and `java.nio.charset.Charset` (and
   `scala.util.matching.Regex` is in the Scala library), but not `Locale`, `Currency` (scala-java-locales provides
   them), the `InetAddress` family, `Path` or `File`. The Predefs only name the types (`Predef` is a value class over
   `Schema[T]`, and `string[T]` erases `T`), so core links without them, like the existing `java.sql`/`URL` Predefs;
   compilation uses the JDK's classes. `StringLikeSchemaSpec` runs on JS and proves it links (r-e5 verified this on
   apiJS 2.13 and 3); value-level checks live in the JVM-only spec. README's Scala.js note gets one sentence: "The
   schemas (and models with fields) of `Locale`, `Currency`, `InetAddress`, `Path` and `File` compile and link on
   Scala.js; creating values of `Locale`/`Currency` needs scala-java-locales, the others have no Scala.js
   implementation."
7. Docs: README "Supported types" table and `docs/src/main/paradox/types.md` (same table), new rows:
   `Locale`, `InetAddress`, `java.nio.file.Path`, `java.io.File` → `string`; `java.util.Currency` → `string`, ISO 4217
   pattern; `java.nio.charset.Charset` → `string`, charset-name pattern; `java.util.regex.Pattern`,
   `scala.util.matching.Regex` → `string`, format `regex`; `Inet4Address` / `Inet6Address` → `string`, format
   `ipv4` / `ipv6`. The same Scala.js sentence in `getting-started.md`. Run `makeDocs`.
8. Gates: `clean test` on 2.12/2.13/3.3.9, `rootJS/test` on all three (core is touched), `scalafmtCheckAll` and
   `rootJS/scalafmtCheckAll`, `makeDocs`.

Out of scope (noted in the report):
- Defaults: `charset: Charset = UTF_8` still needs a `ToValue[Charset]` (compile error asking for one), as `UUID`/`URI`
  defaults do today.
- `modules/parser` `parseFormat` doesn't map `"regex"` (nor `uuid`/`duration` today), so a parsed schema with
  `format: regex` becomes a plain string: unchanged; the parser is a draft-04 subset.
- `CharSequence`, `Symbol` (gaps-types row 17) are not in E5's list.
- Map keys: `Map[Currency, V]`, `Map[Locale, V]` (etc.) still need a user `KeyPattern`, as `UUID` keys do (README
  "Supported types"). Key patterns belong to B6; these Predefs cover values only.

## Compatibility
Additive: new implicit vals in `object Predef` and `object Magnet`, a new case object in the (unsealed) `string.Format`.
- Source: a user's own `implicit Predef[Locale]` (etc.), defined or imported in lexical scope, keeps winning: the
  companion `object Predef` is implicit scope, searched only when lexical scope has no candidate (Scala 2 and 3). A
  user's implicit `Schema[Locale]` wins too (rule 1 is looked up before rule 2). `Format` is not sealed, so a user's
  `match` on it gets no new exhaustivity warning. A user's own `regex` format object keeps working; only code that
  wildcard-imports both it and `json.Schema.string.Format._` gets an ambiguous name (unlikely).
- Binary: new members only.
- Output: no existing type's schema changes; models with these types didn't compile before.
- PVP: additive, so a patch-level release (0.9.x) is enough. Legacy: no impact (0.7 had none of these types).

## Review log
Round 1 (r-e5, 186e507): no must, 7 nits, all accepted.
- E5-R1 (nit) networknt is annotation-only on 2019-09 too → accepted: "What the codecs write" and the InetAddress bullet
  now say "2019-09 and later" (and note that the acceptance suite runs 04/07/2020-12).
- E5-R2 (nit) state the Magnet rule → accepted: one bolded line in A ("a Magnet only where core attaches a
  validation", E3's rule), with the Currency-compiles / Locale-doesn't consequence, as for URI/URL.
- E5-R3 (nit) rebase-friendly placement vs E3 → accepted: plan steps 2–3 put both blocks under a
  `// string-likes (#475)` comment after `uuidMagnet` / `durationS`.
- E5-R4 (nit) map keys out of scope → accepted: added to "Out of scope" (B6 owns key patterns).
- E5-R5 (nit) `anyOf` follow-up and Jackson's host-name form → accepted: InetAddress bullet; also the refined `IP`
  `oneOf` neighbour bug, which goes to the lead.
- E5-R6 (nit) concrete renderer rows → accepted, narrowed: only `AsDraft04Spec` enumerates formats (L117); the other
  AsDraftXXSpecs have no format rows to sit next to, and `StringLikeSchemaSpec` pins `regex` on all five drafts.
- E5-R7 (nit) README Scala.js wording → accepted verbatim (plan step 6).
- (from r-e5's pre-design note 2) strict ECMA 262 validators reject Java-only syntax → one line added under the
  networknt table; the "plain string for Pattern/Regex" alternative already covers it.
