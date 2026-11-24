# Draft-04: exclusive bounds as boolean modifiers  (issue #530, item B30)
Status: approved (r-b30, 2026-11-23)

## Problem

Draft-04 output writes `exclusiveMinimum` / `exclusiveMaximum` as numbers, the way draft-06 and later define them.
In draft-04 they are **booleans that modify `minimum` / `maximum`**: `"minimum": 0, "exclusiveMinimum": true` means
"greater than 0". The draft-04 meta-schema types them as `boolean` (default `false`) and has
`"dependencies": {"exclusiveMaximum": ["maximum"], "exclusiveMinimum": ["minimum"]}`.

The renderer has no draft-04 case for them. `AsDraftSupport.apply` (`AsDraftSupport.scala:53-65`) writes every
validation as `name -> value` through `ValidationList.json` (`AsDraftSupport.scala:283-285`), on every draft.

```scala
import json._
import json.schema.Version._
import json.schema.validation.Instance._
import com.github.andyglow.jsonschema.AsValue

val positive = Json.schema[Int].withValidation(`exclusiveMinimum` := 0)   // = refined's Int Refined Positive
val open     = Json.schema[Double].withValidation(`exclusiveMinimum` := 2, `exclusiveMaximum` := 7)
val mixed    = Json.schema[Int].withValidation(`maximum` := 20, `minimum` := 15, `exclusiveMinimum` := 3, `exclusiveMaximum` := 18)

AsValue.schema(positive, Draft04())
```

Actual draft-04 output, byte-identical on 2.12.21, 2.13.18 and 3.3.9 (JVM; the renderer is shared code with no JS
shim, so Scala.js renders the same):

```json
{ "$schema": "http://json-schema.org/draft-04/schema#", "type": "integer", "exclusiveMinimum": 0.0 }
{ "$schema": "http://json-schema.org/draft-04/schema#", "type": "number", "exclusiveMinimum": 2.0, "exclusiveMaximum": 7.0 }
{ "$schema": "http://json-schema.org/draft-04/schema#", "type": "integer",
  "maximum": 20.0, "minimum": 15.0, "exclusiveMinimum": 3.0, "exclusiveMaximum": 18.0 }
```

(`0.0` instead of `0` is B19's number formatting, #512; not this item.)

Checked with networknt json-schema-validator 1.5.9 (the api tests' validator), repro in
`notes/scratch/c-b30/B30ReproSpec.scala`, log in `notes/scratch/c-b30/b30-repro.log`:

| schema (draft-04 render) | against the draft-04 meta-schema | V4 validator, instance |
|---|---|---|
| `positive` | `$.exclusiveMinimum: number found, boolean expected`; `has an error with dependencies {exclusiveMaximum=[maximum], exclusiveMinimum=[minimum]}` | `0` **accepted** |
| `open` | both keywords `number found, boolean expected`; the dependencies error | `2` **accepted** |
| `mixed` | both keywords `number found, boolean expected` | `19` **accepted** (should be `< 18`) |
| hand-written target `{"type":"integer","minimum":0,"exclusiveMinimum":true,"maximum":18,"exclusiveMaximum":true}` | valid | `0` rejected, `1` and `17` accepted, `18` rejected |

Drafts 06, 07, 2019-09 and 2020-12 are right today (V6: `positive` rejects `0`, "must have an exclusive minimum
value of 0.0").

Where the bounds come from: `withValidation(exclusiveMinimum/exclusiveMaximum := ...)` by hand, and refined
(`Positive`, `Negative`, `Greater[N]`, `Less[N]`, `Interval.Open/OpenClosed/ClosedOpen`, and the
`And`/`Or`/`Not` combinations that normalise to an exclusive bound), both macros (Scala 2 `refined/.../AST.scala:117-154`, Scala 3 `RefinedMacro.scala:195-196`). No golden file
(`golden/`, `golden-legacy/`, petshop) has an exclusive bound. `AsDraft04Spec` "numCase"
(`AsDraft04Spec.scala:308-338`) pins the current output. The README lists it under "Known issues"
(`README.md:967`).

## Why it is a problem

- **Invalid schema.** Every draft-04 schema with an exclusive bound fails the draft-04 meta-schema. Tools that check
  the schema first (strict validators, code generators, OpenAPI 3.0 tooling: OpenAPI 3.0's Schema Object takes
  exclusive bounds from draft-04 as booleans) reject or mis-read it.
- **The bound is not enforced.** A draft-04 validator either rejects the schema or ignores the keyword, so
  `Int Refined Positive` accepts `0`, `Double Refined Interval.Open[2, 7]` accepts `2` and `7`. The schema is weaker
  than the type: silent loss of a constraint.
- refined users get this without writing a bound themselves.

## Options

All options change draft-04 only. Drafts 06+ already render the numeric form, which is right there.

### A. Draft-04 renders each side as `minimum`/`maximum` + boolean modifier; the stricter bound wins  — WON

A render hook in `AsDraftSupport` turns the validation list into JSON; `AsDraft04` overrides it. Per side (lower:
`minimum` + `exclusiveMinimum`, upper: `maximum` + `exclusiveMaximum`):

| validations on the side | draft-04 output |
|---|---|
| none | nothing |
| inclusive `m` only | `"minimum": m` (as today; no `"exclusiveMinimum": false`, it's the default) |
| exclusive `e` only | `"minimum": e, "exclusiveMinimum": true` |
| both, `e >= m` | `"minimum": e, "exclusiveMinimum": true` (`x > e` implies `x >= m`; on a tie `x > e` is stricter) |
| both, `e < m` | `"minimum": m` (`x >= m` implies `x > e`) |

The upper side mirrors it: exclusive `E` wins when `E <= M`, else `"maximum": M`. Values are compared as the
`BigDecimal` inside the rendered `num` (`Def.json`), so `minimum := 15` and `exclusiveMinimum := 15.0` compare equal,
and the result is exact: the one bound draft-04 can state per side is exactly the conjunction of the two. The kept
value is the original `num`, so whatever number formatting B19 settles on carries through.

```json
positive → { "type": "integer", "minimum": 0, "exclusiveMinimum": true }
open     → { "type": "number",  "minimum": 2, "exclusiveMinimum": true, "maximum": 7, "exclusiveMaximum": true }
mixed    → { "type": "integer", "maximum": 18, "exclusiveMaximum": true, "minimum": 15 }
```

The hook sits where `apply` already writes validations, so it covers every place a schema with validations is
rendered: the root, fields, collection items, map values, definitions, `not` bodies, `oneOf`/`allOf` members, and
validations on a value-class node itself (`value-class(...).withValidation(...)`, written by that node's `apply`).
It does not cover validations of the schema *inside* a value class: `mkValueClass` (`AsDraftSupport.scala:244-246`)
renders the inner type's specifics with the outer node's list and never writes the inner node's validations, on any
draft. That is a separate bug, in B13's design (#507), not this item; see Risks.

- Pro: valid draft-04; exactly the constraint the tree states; no API change; one place.
- Pro: two bounds on one side (by hand; refined merges each side into one bound) stay exact without new keywords.
- Con: on draft-04 a reader sees one bound per side where the tree had two. That is all draft-04 can say.

### B. Like A, but a conflicting pair keeps both bounds through `allOf`  — LOST

When a side has both an inclusive and an exclusive bound, keep the inclusive one in place and put the exclusive one in
an `allOf` entry: `{"minimum": 15, "allOf": [{"minimum": 3, "exclusiveMinimum": true}]}`.

- Pro: no number comparison; the tree's two bounds stay visible.
- Con: an `allOf` appears in a schema that had none, and it has to merge with an existing `allOf` (a schema can be an
  `allof` node with validations of its own). More output, more cases, for the same set of valid instances.
- Lost: the comparison in A is exact (`BigDecimal`), so B buys nothing semantically.

### C. Integer shift: exclusive bound on an integer schema becomes `minimum: e + 1`  — LOST

For `"type": "integer"` with a whole bound, `exclusiveMinimum: 0` → `"minimum": 1` (and `maximum: E - 1`); other
cases as in A.

- Pro: the output has no `exclusive*` at all for the most common refined cases (`Int Refined Positive`).
- Con: two forms for one keyword depending on type and value; a fractional bound on an integer (`Greater[2.5]` on
  `Int` can't happen in refined, but `withValidation` allows it) and `number` schemas still need A. More code, more
  tests, and the boolean form is plain draft-04 that every draft-04 validator implements.

### D. Keep the output, document it  — LOST

Today's state (the README's known issue). Lost: the schema is invalid and drops the constraint; nothing about it is
a style a user could depend on.

### E. Rewrite the rendered JSON tree after rendering (`AsDraft04.schema` walks the result)  — LOST

Find every object with a numeric `exclusiveMinimum`/`exclusiveMaximum` and rewrite it.

- Con: it can't tell a schema from data. A `default`, `enum` or `const` value that is an object with an
  `exclusiveMinimum` key (a case class with such a field) would be rewritten too. The renderer knows which objects
  are schemas; the post-pass doesn't.

### Sub-decision: Legacy

- Applies under Legacy too (WON). The lead's policy (2026-11-23): Legacy freezes 0.7's style, not its bugs, and this
  one makes the schema invalid. No `golden-legacy` file has an exclusive bound, so the frozen goldens don't change.
- Legacy keeps the numeric draft-04 form (LOST): the renderer doesn't see the profile (lowering runs first and
  leaves a plain tree), so this would need the profile passed into `AsValueBuilder`/the renderer just to keep an
  invalid schema.

## Decision

Option A. On draft-04 the renderer states each side as one `minimum`/`maximum`, with `exclusiveMinimum`/
`exclusiveMaximum: true` when the stricter bound of that side is exclusive (a tie goes to the exclusive one). Drafts
06+ and the raw renderer are unchanged. It applies under every profile, Legacy included.

## Implementation plan

1. **Hook** (`core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala`)
   - Add `private[jsonschema] def renderValidations(vl: ValidationList): obj = vl.json` with a one-line scaladoc
     ("the validation keywords of one schema node; draft-04 overrides it for exclusive bounds").
   - In `apply(x, parent, includeType, isRoot)` replace `validationList.json` with `renderValidations(validationList)`.
     Keep the order of evaluation: `specifics` is computed first (it `extract`s `patternProperties` from the list),
     then the validations are rendered. Nothing else in the trait writes validation keywords (`Pre09.mkStr` writes the
     uuid `pattern` itself; leave it).
2. **Draft-04 override** (`core/src/main/scala/com/github/andyglow/jsonschema/AsDraft04.scala`)
   - `override private[jsonschema] def renderValidations(vl: ValidationList): obj = exclusiveBounds(super.renderValidations(vl))`,
     with `exclusiveBounds: obj => obj` a private method of `AsDraft04` (pure, so unit-testable through rendering).
     It works on `.fields` (a `Seq[(String, Value)]`; the map behind `json` has already applied "last one of a keyword
     wins", which is today's behaviour for a repeated keyword).
   - Per side: read `minimum`/`exclusiveMinimum` (resp. `maximum`/`exclusiveMaximum`) as `num`. If the side has no
     exclusive keyword, leave its fields alone. Otherwise pick by the table in option A, comparing `num.value`
     (`BigDecimal.compare`). Emit the chosen bound under `minimum` (`maximum`), then `exclusiveMinimum: true`
     (`exclusiveMaximum: true`) if the chosen one is exclusive, at the position of the side's first keyword in the
     list; drop the other keyword(s) of that side. All other fields keep their place. The position only means
     something once B2's `obj.ordered` is in: until then `ValidationList.json` builds its `obj` with `.toMap`
     (hash order past 4 entries), and so does this step's output order. Tests compare `obj`s (Map equality), so
     they don't depend on the order either way.
   - If a value is not a `num` (it always is: the four `Instance`s convert with `num.apply`), leave the side as is.
   - Build the result with an order-keeping constructor: `new obj(mutable.LinkedHashMap(...))` today, `obj.ordered`
     if B2 (#460) is merged first (B2 changes `ValidationList.json` to `obj.ordered`). Use only collection methods
     common to 2.12 and 2.13+ (`indexWhere`, `collectFirst`, `foldLeft`, `flatMap`); no `scalamigration` shim needed.
   - Plain Scala, no reflection, no platform code: JS-clean.
3. **Tests, rendering** (`api/src/test/scala/com/github/andyglow/jsonschema/AsDraft04Spec.scala`, shared)
   - "numCase" (byte, short, int, long, float, double, bigInt, bigDec): expected becomes
     `"type" -> t, "minimum" -> 15, "maximum" -> 18, "exclusiveMaximum" -> true, "multipleOf" -> 3`.
   - New block "exclusive bounds" (full JSON per case): exclusive lower only; exclusive upper only; both exclusive;
     inclusive lower stricter (`minimum 15, exclusiveMinimum 3` → `minimum 15`); exclusive lower stricter
     (`minimum 3, exclusiveMinimum 15` → `minimum 15` + `true`); lower tie (`minimum 5, exclusiveMinimum 5` →
     exclusive); upper tie; mixed types (`minimum := 5`, `exclusiveMinimum := 5.5`); inclusive only (unchanged, no
     `false`). Placement: on a field of an object, as an array item, inside a definition (`.toDefinition`), inside
     `Schema.not` (`not(integer.withValidation(exclusiveMinimum := 5))` → `"not": {"minimum": 5, "exclusiveMinimum": true}`:
     today's body has no `type` (B22's area); this expectation is B30's alone, and whoever of B22/B30 merges second
     adds B22's `type` to it), and on a value-class node with validations of its own
     (`` `value-class`[Pos, Int](`integer`).withValidation(`exclusiveMinimum` := 0) `` with an implicit
     `Magnet[Pos, Number]`, as in the docs' `UserId` example, or `Json.schema[Pos].withValidation(...)`) → `{"type": "integer", "minimum": 0, "exclusiveMinimum": true}`. Validations on the value class's *inner*
     schema are out of scope (they are dropped on every draft today, see Risks); no test for them here.
   - `AsDraft06Spec`/`07`/`09`/`12` "numCase" stay as they are (the numeric form); add one test there that a
     `minimum` + `exclusiveMinimum` pair is rendered as both keywords, unmerged (guards against the hook leaking).
4. **Tests, validator** (shared `api/src/test/scala/com/github/andyglow/jsonschema/Draft04ExclusiveBoundsSpec.scala`)
   - Add `def metaErrors(draft: Version, schema: String): List[String]` to both `petshop.JsonValidator` shims: JVM
     validates `schema` against the draft's meta-schema (`JsonSchemaFactory.getInstance(flag).getSchema(SchemaLocation.of(<draft uri>))`,
     as in the repro); JS calls `cancel()` like `errors`.
   - For `positive`, `open`, `mixed` and the `not` case of step 3 (its `exclusiveMinimum` needs a `minimum` next to
     it inside the `not` body: the meta-schema's `dependencies` rule, which the `not` body is checked against too):
     the draft-04 render has no meta errors; instances via
     `JsonValidator.errors(Draft04(), ...)`: `positive` rejects `0`, accepts `1`; `open` rejects `2` and `7`, accepts
     `2.5`; `mixed` rejects `14`, `18`, `19`, accepts `15`, `17`. On JS these are canceled (the convention).
5. **Tests, refined** (`modules/refined/src/test/scala/com.github.andyglow.jsonschema.refined/`, shared dir: the
   types below need no literal types) a rendered draft-04 check: `Int Refined Positive` →
   `{"type":"integer","minimum":0,"exclusiveMinimum":true}`, `Int Refined Negative` → `maximum` + `true`. The
   2.13+/3 `Interval.Open[2, 7]` case goes in `scala-2.13+` (literal types), the 2.12 one with `W.\`2\`.T` in
   `scala-2.12-`. The tree specs (`RefinedNumbersSpec`, `RefinedBooleanSpec`, `RefinedQuirksSpec`) don't change:
   the tree is unchanged.
6. **Goldens:** none change (no golden file has an exclusive bound). Regenerating with `GOLDEN_UPDATE=1` must give an
   empty diff; say so in the PR.
7. **Docs**
   - `README.md`: drop the known issue at L967. Under the validation table (L506) add: "On draft-04, which has no
     numeric `exclusiveMinimum`/`exclusiveMaximum`, an exclusive bound renders as `minimum`/`maximum` with
     `"exclusiveMinimum": true` / `"exclusiveMaximum": true`. Where one side has both an inclusive and an exclusive
     bound, draft-04 gets the stricter one."
   - `docs/src/main/paradox/validation.md`: the same sentence under its table.
   - `docs/src/main/paradox/modules/refined.md` and README L759: the `Greater[17]` comment shows the draft-06+ form;
     add "(draft-04: `"minimum": 17, "exclusiveMinimum": true`)" in both, or leave the comment and rely on the
     validation note: the implementer picks one and does the same in both files.
   - `makeDocs` must pass.
8. **Gates:** the protocol's standard set (core changes: JVM `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on
   all three; `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`; `makeDocs`).

### Risks and neighbours

- **Value-class inner validations** (B13, #507): `mkValueClass` never writes the inner schema's validations,
  on any draft. Whoever fixes that must write them through `renderValidations` (or `apply`), not `ValidationList.json`
  directly, or draft-04 exclusive bounds come back broken there.
- **Textual conflicts:** D3 (#294) overrides `mkConst` in `AsDraft04.scala` (same file, a different method). B2 (#460)
  rewrites `ValidationList.json`; the hook reads its result, so it works with either version (see step 2). B22 (#509),
  B27, B32 tests that render a draft-04 `not`/bound will see the new form if they land after B30 (and vice versa):
  whoever merges second updates the expected draft-04 JSON.
- **`contentSchema`** renders its nested schema with `Draft04` at `:=` time (`Instance.scala:31-34`, the FIXME,
  known issue 9), whatever draft the outer document is. After B30 an exclusive bound inside a `contentSchema` gets the
  draft-04 form inside, say, a 2020-12 document. The nested schema already states `"$schema": draft-04`, so the new
  form agrees with its own `$schema`, and `contentSchema` is an annotation (2019-09+ validators don't assert it).
  Not fixed here; noted for the lead.
- **Parser module:** `ParseJsonSchema` (draft-04 subset) reads no numeric bounds today, so nothing to round-trip.

## Compatibility

- Source/binary: no public signature changes. `renderValidations` is `private[jsonschema]` (a concrete trait method;
  Scala 2.12+ traits compile to default methods, so classes compiled against 0.9.0 keep linking). The core design rule
  (Schema → `$ref`, Predef → inline) is untouched; the tree is unchanged, only draft-04 rendering.
- Output: **draft-04 only**, every profile (Neutral, Legacy, the library profiles), wherever a schema has an exclusive
  bound: `"exclusiveMinimum": n` → `"minimum": n, "exclusiveMinimum": true` (same for the upper side), and where a
  side has both kinds, only the stricter one is kept. Drafts 06, 07, 2019-09, 2020-12 and `Version.Raw`: unchanged.
  No golden file changes (Legacy goldens included).
- PVP: patch (0.9.x): a bug fix of invalid output, no API change.

## Review log
B30-R1 (must) the hook doesn't cover a value class's inner validations (`mkValueClass` uses `inferSpecifics`, not
`apply`) → accepted: option A now says it covers validations on the value-class node itself and names the inner-schema
drop as a separate bug; step 3's value-class test is the outer form, the inner form is out of scope; Risks has a line
that the fix for the inner drop must go through `renderValidations`.
B30-R2 (nit) line refs → accepted: `apply` :53-65, `ValidationList.json` :283-285.
B30-R3 (nit) keyword position is hash order before B2 → accepted: step 2 says the position only holds once B2's
`obj.ordered` lands; tests compare `obj`s (order-insensitive).
B30-R4 (nit) the `not` case → accepted: step 3 says the expectation is B30's alone (no `type`, B22's area) and who
updates it; step 4's meta-schema check covers the `not` case (`dependencies` inside the body).
B30-R5 (nit) the inner value-class drop is B13's design (#507) → accepted: option A and Risks name B13, #507.
