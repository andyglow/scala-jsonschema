# Render `title` / `description` on every schema, and `@title` on fields  (issues #268, #290; item B1)
Status: approved (r-b1, 2026-11-23)

## Problem
The draft renderers (`core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala`) emit a node's
`title` and `description` only in `mkObj` (L104-105) and `mkConst` (L236-237). Every other node kind keeps them in the
tree and the renderer drops them: `string`, `integer`, `number`, `boolean`, `array`, `dictionary`, `enum`, `oneof`,
`allof`, `not`, `value-class`, `def`, `ref` (and so the lowered `option` and `family`). `AsRaw` does emit them.

Separately, `@title` on a case-class **field** is not read by either macro (only `@description` is), so it never reaches
the tree.

Repro (`api/src/test/scala/com/github/andyglow/jsonschema/TitleDescriptionSpec.scala`,
`modules/enumeratum/src/test/.../EnumeratumSupportSpec.scala`, commit `46d354a`):

```scala
Json.schema[String].withTitle("T").withDescription("D")          // also Int, Double, Boolean, List[Int], Map[String, Int], Option[String]
Json.schema[TdId].withTitle("T").withDescription("D")            // case class TdId(value: String) extends AnyVal
@title("Color") @description("A color") sealed trait TdColor     // case objects Red, Blue      -> enum
@title("Shape") @description("A shape") sealed trait TdShape     // case classes Circle, Square -> oneOf
case class TdPerson(@title("Name") @description("The name") name: String)
implicit val tdItems = Predef(Json.schema[List[String]].withTitle("Items").withDescription("The items")); case class TdBox(items: List[String])
@title("foo") @description("bar") sealed trait Titled extends EnumEntry   // enumeratum, #268
```

Actual output, the same on 2.12.21, 2.13.18 and 3.3.9 (JVM), every draft (04, 06, 07, 2019-09, 2020-12):

```json
{ "$schema": "http://json-schema.org/draft-04/schema#", "type": "string" }
```

no `title`, no `description`, for every case above except `TdPerson`, whose property gets the description only:
`"name": { "type": "string", "description": "The name" }`. The tree does carry the texts (the spec asserts
`Neutral.lower(Json.schema[TdShape]).title == Some("Shape")`, and `Json.schema[Titled].title == Some("foo")`): only the
renderer drops them. Scala.js uses the same shared renderer (not run for the repro).

10 of 12 `TitleDescriptionSpec` cases fail and the new enumeratum case fails, on all three Scala versions; the
remaining case (Legacy stays as 0.7) passes today and must keep passing.

## Why it is a problem
- Silent data loss: the user wrote `@title`/`.withTitle` and the schema doesn't have it. Form generators and doc tools
  (the main consumers of `title`/`description`) show nothing.
- The README says "`.withTitle`/`.withDescription` work on any schema" (README L448, paradox `descriptions.md` L45).
- #268 (enumeratum enum title, owner: "seems like a bug"); #290 asks for field-level `@title` (field `@description`
  works already). #290's second ask, user-defined annotation keywords, is out of scope (needs an extension point).

## Options

### Rendering

#### A. Render the node's own texts centrally in `AsDraftSupport.apply` — WON
`apply(x, parent, includeType, isRoot)` is the one place every node is rendered (properties, items, members,
definition bodies, the root). It adds `description`/`title` of `x` itself:

```scala
val own = texts(x)                         // obj("description" -> x.description, "title" -> x.title)
base ++ validationList.json ++ own ++ specifics ++ own
```

`own` goes in twice on purpose: first, before the specifics, so an object or a const keeps its key order (they always
had the texts right after `type`/validations; `DocumentationSpec` compares pretty-printed strings and stays
byte-identical); then again last, so the node's own texts win over texts the specifics bring from an inner node.
`mkObj` and `mkConst` stop emitting them (no duplication). `mkValueClass` adds the underlying type's texts
(`... ++ texts(x.tpe)`), which the outer `own` then overrides: a value class's own title wins over its underlying type's.
In `mkNullable`, the inner `X` is rendered through `apply`, so it brings its own texts and the `oneof`'s own win.

`def`/`ref`: their texts render next to the `$ref` at that site (`{"$ref": "#/definitions/X", "title": "..."}`); the
definition body renders its own texts in `definitions`. That's what a field `description` on a `$ref` has done since 0.7.

Sample (prototype run, 2.13, Neutral, draft-07):
`Json.schema[String].withTitle("T").withDescription("D")` →
`{"$schema": ..., "type": "string", "description": "D", "title": "T"}`.

Pros: one rule, every node kind (including E2's future "any" node and anything added later) gets it for free; tiny diff.
Cons: none found. The prototype (`notes/scratch/c-b1/renderer-proto.patch`) passes every existing JVM test on 2.13
except the 3 Legacy golden files listed under Compatibility (and the two repro cases that need parts F and L below).

#### B. Add texts in each `mkXxx` method — LOST
Same output, but 12 places to keep in sync, and a new node kind would silently drop texts again (the bug we're fixing).

#### C. Wrap `$ref`-with-texts in `allOf: [{$ref}]` for drafts before 2019-09 — LOST
Before 2019-09, keywords next to `$ref` are ignored by validators. `title`/`description` are annotations: nothing
validates them, and tools that read them generally do read them next to `$ref`. A field `description` beside `$ref`
has been rendered this way since 0.7 without complaints; wrapping would change those outputs too. Not worth it.

### Field `@title` (#290)

#### F2. Put the field's `@title` on the field's schema (`fieldSchema.withTitle`) in both macros — WON
Scala 2 (`UProductTypes`, next to the `fd` descriptions): read `@title` from the field annotations (param and
`@(title @field)`, as `@description`), and set it on the field's `SchemaType` extra (`withExtra(extra.copy(title = ...))`),
which emits `.withTitle(...)` on the field's tree. Scala 3 (`Products.product`): the same on the field's `Node`
(`node.withExtra(node.extra.copy(title = ...))`). The field's title replaces its type's title at that site; for a
definition it sits next to the `$ref` and the definition keeps its own title. Field `@description` stays where it is
(`Field.description`, rendered last in the property, so it already wins over the type's).

Pros: no model change: `object.Field` stays as in 0.9.0 (source and binary compatible, patch release). Works with
part A unchanged. Cons: asymmetry (field description on `Field`, field title on `Field.tpe`); a hand-built tree sets a
field title with `Field("name", schema.withTitle("x"))`. (Under L1 it had a Legacy caveat, see there.)

#### F1. Add `title: Option[String]` to `object.Field` — LOST
Symmetric with `description`, and the Legacy profile could drop exactly the field titles. But `Field` is a case
class: an 8th parameter changes `copy`/`unapply`/`apply` (0.9.0 added `hasDefault` the same way, README L852 tells
users their `case Field(...)` patterns need a new binder). That's a PVP-breaking change (0.10.0) for a doc keyword.
Pick it instead if the owner prefers the symmetric model and a minor bump: the renderer adds `field.title` next to
`field.description` in `mkObj`; macros call a new `withTitle(Some(t))` on the field; Legacy lowering drops it.

### Legacy profile (`JsonSchemaProfile.Legacy`, "the 0.7 output, byte for byte")

Changed after approval (2026-11-23): the lead's Legacy policy (`notes/PROTOCOL-gaps.md`, "Lead policy on Legacy") says
Legacy freezes 0.7's *style*, not its bugs. Dropping a title the user wrote is a bug, so L2 won; L1 stays below as
the alternative (it was implemented and green at `8068d6b`, reverted in the next commit).

#### L2. Render texts under Legacy too — WON
Dropped titles are a 0.7 bug that Legacy doesn't reproduce: no knob, no stripping; the renderer change applies to every
profile. Legacy output gains the texts in the 3 golden-legacy files listed in plan step 5 (rendered sections only; the
tree dump is unchanged). The Legacy docs wording ("0.7 byte for byte", README L606/L873, paradox `rendering.md`,
`JsonSchemaProfile.Legacy` scaladoc) is qualified once for all Legacy-policy PRs by the lead, not here.

#### L1. Legacy lowering keeps texts only where 0.7 rendered them — LOST (the alternative)
To choose it instead: cherry-pick `8068d6b`'s `JsonSchemaProfile.scala`, `harness/Families.scala` and
`FamilyLoweringSpec.scala`, and the Legacy cases of `TitleDescriptionSpec` from that commit. A private knob (`legacyTexts`, true only for `Legacy`, carried by the `with...` builders) makes `Lowering` clear
`title`/`description` on every node that isn't an `object` or a `const` (where 0.7's `mkObj`/`mkConst` rendered them;
field descriptions are on `Field` and untouched). It works for both ways of choosing Legacy (`withProfile(Legacy)` and
an implicit Legacy at render time), because the renderer then has nothing to render.

Caveats: (1) a field `@title` on a field whose type renders as an object (F2 puts it on that object) survives under
Legacy. 0.7 ignored field `@title` entirely, so this is new output for a model nobody could have relied on; F1 would
avoid it. (2) A tree shaped with `withProfile(Neutral)` and then rendered under an implicit Legacy loses its non-object
texts (lowering under Legacy strips them although the option/family shapes are already fixed). (3) The `"schema"` dump
in the 3 affected `golden-legacy` files changes (the Legacy-lowered tree has fewer texts); their rendered draft
sections stay byte-identical. That needs `GOLDEN_UPDATE_LEGACY=1` for those 3 files, reviewed by hand. (4) The
test harness `Families.as07` used `Legacy.lower` for the 0.7 *tree*, which now lacked texts: it needs a
Legacy-shaped profile without the strip (`Neutral.withOption(OptionStyle.Legacy)`), and two `FamilyLoweringSpec`
tree assertions under Legacy flip.

## Decision
Render every node's own `title`/`description` in `AsDraftSupport.apply` (A), read field `@title` in both macros
onto the field's schema (F2), under every profile, Legacy included (L2, per the lead's Legacy policy).

## Implementation plan
1. `core/.../AsDraftSupport.scala`: `texts(x)`; `apply` adds `own` before and after the specifics; drop the texts from
   `mkObj` and `mkConst`; `mkValueClass` adds `texts(x.tpe)`.
2. Field `@title`:
   - Scala 2: `macros/src/main/scala-2/.../UFieldDecorations.scala` gets `fieldTitles(fields)` (like
     `fromFieldAnnotations`, for `T.annotation.title`); `UProductTypes` sets the title on `fieldSchema`'s extra
     (also for `U.Opt`). Check `SchemaTypes` emits `.withTitle` for every `SchemaType` that carries extra.
   - Scala 3: `core/src/main/scala-3/.../derivation/Products.scala`: `fieldTitles(ps)` from `p.annotations`
     (param + field annotations, already merged in `Param.annotations`); `product` sets
     `node.withExtra(node.extra.copy(title = ...))`. Check the recursion pass/emit don't key definitions on node equality
     with extras.
3. (L1 only, dropped with L2) a private `legacyTexts` knob in `JsonSchemaProfile.scala` making Legacy's lowering
   strip non-object/non-const texts; carried by `Knobs(p)`; binary compatible. See `8068d6b`.
3b. (B1-R1) Value class → definition keeps its texts in the tree, in `core/src/main/scala/json/Schema.scala`:
   - `def.adapt` (both macros call it for every implicit `Schema`): `case vc @ value-class(inner)` →
     `def(sig, inner.duplicate(title = vc.title orElse inner.title, description = vc.description orElse inner.description))`;
     `case d @ def(originalSig, inner)` keeps `d`'s own title/description (only those: its validations stay dropped,
     as today), so they render next to the `$ref` like a hand-built definition's.
   - NOT `value-class.toDefinition` (`tpe.withValidationsAddedFrom(this)` starts from `mkCopy()` and keeps neither
     node's texts nor the receiver's validations): that's item B13 (validations-copy), which fixes
     `withValidationsAddedFrom` itself.
   Under Legacy too (L2).
4. Tests:
   - `TitleDescriptionSpec` (already committed) goes green; add: `def`/`ref` site texts (`$ref` sibling), `allof`, `not`,
     `enum` from `Schema.enum.of(...).withTitle`, `oneof` member texts, a value class whose underlying type has its own
     title (outer wins), field `@title` on a definition-typed field, on an `Option` field, `@(title @field)` (Scala 2),
     field `@title` under `Json.objectSchema`, and Legacy cases (L2: the texts render under Legacy too).
   - (B1-R1) a titled value class given as `implicit val s: Schema[VC] = Json.schema[VC]` and used as a field: the
     definition body has the texts, under Legacy too. (`toDefinition` is B13's.)
   - (B1-R3) F2 with definitions and recursion, on both Scala versions: (a) the same definition-typed type in two
     fields, one with `@title` → one definition, the title only next to the titled field's `$ref`, the body untouched;
     (b) `case class Node(@title("kids") kids: List[Node])` (title on the array, the item stays a plain `$ref`);
     (c) `@title` + `@description` on a `def`-typed field → `{"$ref", "title", "description"}`.
   - `AsDraft04/06/07/09/12Spec`: one renderer case each (string + array with texts) in the full-JSON style.
   - Enumeratum #268 case (committed) goes green.
5. Golden files (all versions; `GOLDEN_UPDATE=1 sbt clean api/test`, review the diff): neutral `golden/` gains texts in
   `char/products/Id.json` (root value class: `"title": "Id", "description": "A wrapper"` on the string) and
   `shared/SumTypeModels.FooBarInsideCompanionWithAnnotations{,.EnumsAsOneOf}.json` (the value-class member `M3`:
   `"title": "t3", "description": "d3"`). `golden-legacy/`: the same 3 files gain the same texts in their rendered sections (L2; `GOLDEN_UPDATE_LEGACY=1`,
   reviewed by hand; the tree dump is unchanged).
   Plus new golden cases if the reviewer wants them. Bridge petshop suites: no change in the prototype.
6. Docs (README L479 and paradox `descriptions.md` L78, the annotation table): `@title` row "type (the root, or a
   type given as an implicit `Schema`), field, enum value" (B1-R2), same for `@description`'s type part; one sentence
   that titles/descriptions render on every schema kind, a definition's (or a field's on a definition-typed field) next
   to its `$ref`, which validators before 2019-09 ignore, so it's for doc tools there (B1-R6). The Legacy
   "byte for byte" wording is the lead's (see L2). `makeDocs`. No CHANGELOG (lead).
7. Gates per protocol step 8 (core + macros + api touched: JVM 2.12/2.13/3, `rootJS/test` on all three, fmt checks).

## Out of scope
- (B1-R2) A **nested** derived type's own `@title`/`@description`/scaladoc (`case class Nested(c: Color, i: Inner)`
  with titled `Color`/`Inner`): the derivation doesn't put them in the tree (only the root and implicit-`Schema`
  definitions get type texts; `Derivation.scala` L37 "R8", parity decision 2026-11-20). Changing that is a derivation
  change with output changes for every annotated nested type, not a rendering bug: the lead queued it as item B12
  (nested-texts), to build on this PR. The docs above stop promising it.
- `value-class.toDefinition` / `withValidationsAddedFrom` dropping texts and validations: item B13.
- #290's "customizable annotations" (user-defined keywords): needs an extension point in the ADT and renderers; the
  PR says it stays open (B1-R7).

## Compatibility
- Source/binary: no public signature changes (F2, L2). Patch release under PVP (0.9.1).
- Output (Neutral and the bridge profiles): new `title`/`description` keys wherever the tree has them on a non-object
  node; nothing removed, objects/consts byte-identical. In our suites: 3 golden files (listed in step 5). Users who
  annotate enums/families/value classes or call `withTitle` on non-objects will see the keys appear: that's the fix.
- Legacy: the same new keys as Neutral (L2, a bug fix under the lead's policy): in our suites
  `golden-legacy/char/products/Id.json` and `golden-legacy/shared/SumTypeModels.FooBarInsideCompanionWithAnnotations{,.EnumsAsOneOf}.json`,
  rendered sections only.
- Neighbours: B2 (property order) also edits `mkObj` (adjacent lines: textual conflict only, trivial); E2 adds a node
  that A covers for free.

## Review log
Round 1 (r-b1, on cc4d73e):
- B1-R1 (must) value class → definition drops texts (`def.adapt`, `value-class.toDefinition`) → accepted, narrowed
  after the lead's reply to `def.adapt` (`toDefinition` is B13's): plan step 3b + tests in step 4. Also applied to `def.adapt`'s `def` case (it re-wraps and drops the def node's own texts); only
  title/description are kept there, its validations stay dropped as today.
- B1-R2 (must) nested-type texts → accepted: new "Out of scope" section; README/paradox wording in step 6.
- B1-R3 (must) F2 tests with definitions/recursion → accepted: step 4, cases (a)–(c).
- B1-R4 (nit) drop `AsRaw.render`'s `base` → pushed back: with it the root's texts stay the first keys of AsRaw's
  output (`obj ++` keeps a key's first position, the values are the same); without it they move after `type`. Same
  values either way; I keep it, with a comment saying it's there for key order.
- B1-R5 (nit) L1 knob details + Legacy scaladoc → accepted: step 3.
- B1-R6 (nit) `$ref` siblings before 2019-09 in the docs → accepted: step 6.
- B1-R7 (nit) PR body: `Fixes #290`, custom annotations stay open → accepted: "Out of scope" + PR body.
Round 2 (r-b1, on 8750908): approved; R4 pushback accepted.
- B1-R8 (nit) plan step 7 sat under "Out of scope" → accepted: moved back into the plan.
Change after approval (c-b1, lead's Legacy policy of 2026-11-23): L1 → L2; r-b1 OK'd it with conditions, all applied:
Decision/Compatibility/log updated with L1 as the alternative; the "Legacy keeps object-only texts" docs sentence
dropped; the Legacy "byte for byte" wording left to the lead (lead's call); the PR lists each regenerated golden-legacy
file; the knob is fully removed (`JsonSchemaProfile.scala`, `Families.scala`, `FamilyLoweringSpec.scala` are as on master).
