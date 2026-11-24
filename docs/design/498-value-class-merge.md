# A value class keeps its underlying type's validations and texts  (issue #498, item B13)
Status: approved (r-b13, 2026-11-23)

## Problem
A value class (`case class X(v: T) extends AnyVal`) is the tree node `value-class(<schema of T>)`. The node can carry
its own validations and texts, e.g. from `Json.schema[X].withValidation(...)` (with a `Magnet[X, _]`), `.withTitle`,
or `@title`/`@description`/scaladoc on a root value class. The underlying schema carries its own, e.g. `Char`'s
`minLength`/`maxLength`, a user `Predef` with a `pattern`, or a refined type's bounds. Four code paths turn the pair into
one schema, and each one loses part of it:

| path | code (master `4b2aff3`) | keeps | drops |
|---|---|---|---|
| P1 `value-class.toDefinition(sig)` | `Schema.scala` L626-627 `def(sig, tpe.withValidationsAddedFrom(this))` | the vc's validations | the underlying's validations, both nodes' title/description |
| P2 implicit `Schema` of a value class (both macros: `Schema.def.adapt`) | `Schema.scala` L601 `case value-class(innerTpe) => def(sig, innerTpe)` | the underlying's validations and texts | the vc's validations and texts |
| P3 inline render (field, element, root) | `AsDraftSupport.scala` L53-65 (`inferValidations(x)` of the vc node) + L244-246 `mkValueClass` | the vc's validations | the underlying's validations |
| P4 inline render of a value class over a definition | `AsDraftSupport.scala` L59 `obj("type" -> x.jsonType)`, `def.jsonType = ???` (L554); `extractDefs` (L309-319) doesn't look inside a value class | | everything: **throws** `NotImplementedError` |
| P5 value-class member of a `@discriminator` family (Neutral, Legacy: the profiles that honor the annotations) | `JsonSchemaProfile` puts the member's tag (`discriminationKey`) on the member node, i.e. the `value-class`; `mkObj` reads it from the object it renders, the underlying | | the tag's `{"enum": [...]}` property, while `required` still lists the field: **the member matches nothing** |

P1 is the item as filed: `withValidationsAddedFrom(x)` (L91-96) starts from `mkCopy()`, so its result has only `x`'s
validations, and the receiver's own validations, title, description and discriminationKey are gone. Its other call site,
`inferDefinitions.extractDefs` (L311), only walks the structure to find nested definitions, so there it is harmless.

Repro (`notes/scratch/c-b13/B13ReproSpec.scala`, `B13Repro2Spec.scala`; logs `repro.log`, `repro2-master.log`), draft-07,
the same on 2.12.21, 2.13.18 and 3.3.9 (JVM; the code is shared with Scala.js):

```scala
@title("Code T") @description("Code D") case class Code(c: Char) extends AnyVal
final class Slug(val s: String); object Slug { implicit val p: Predef[Slug] = /* string, minLength 3 */ }
case class SlugId(s: Slug) extends AnyVal
case class Plain(v: String) extends AnyVal                               // + implicit Magnet[Plain, String]
case class Person(name: String); object Person { implicit val s: Schema[Person] = Json.schema[Person] }
case class PersonW(p: Person) extends AnyVal
```

| # | input | tree | output today |
|---|---|---|---|
| R1 | `value-class(string{minLength 3} title "inner T"){maxLength 5} title "VC T"` | | inline `{"type":"string","maxLength":5}` (P3); `.toDefinition("vc")` → `def(vc, string{maxLength 5})`, body `{"type":"string","maxLength":5}` (P1) |
| R2 | `Json.schema[Code]` | `value-class(string{minLength 1, maxLength 1, pattern}) title desc` | inline `{"type":"string"}` (P3); `.toDefinition("code")` → `def(code, string)` (P1) |
| R6 | `case class SlugHolder(id: SlugId)` | `id: value-class(string{minLength 3})` | `"id": {"type":"string"}` (P3); `Json.schema[SlugId].toDefinition(..)` body `{"type":"string"}` (P1) |
| R9 | `implicit val s: Schema[Plain] = Json.schema[Plain].withValidation(pattern := "^[a-f0-9]{16}$")`, field `id: Plain` | `id: def(...Plain, string)` | definition `{"type":"string"}`: the `pattern` is gone (P2) |
| R7 | `Json.schema[PersonW]`; `case class H(w: PersonW)`; `Option[PersonW]`, `List[PersonW]` fields | `value-class(def(...Person, object))` | **throws** `scala.NotImplementedError` (P4) |
| Q2 | `sealed trait Fam extends Any; case class FamP(p: Person) extends AnyVal with Fam; case class FamS(s: String) extends AnyVal with Fam` | lowered `oneof(value-class(def(Person)), value-class(string))` | **throws** (P4, via `mkPlainOneOf`'s `memberType` → `jsonType`) |
| D1 | `@discriminator("t") sealed trait F extends Any; case class Inner(a: Int); case class W(p: Inner) extends AnyVal with F; case class O(b: String) extends F` (found by r-b13, `notes/scratch/r-b13/p2-master.log`) | member W: `value-class(object(a))` keyed `t` | W = `{"additionalProperties":false,"properties":{"a":{"type":"integer"}},"required":["a","t"]}` (P5): `t` is required but not allowed; O is fine. Neutral and Legacy, the same |

What works today: the README pattern `Json.schema[UserId].toDefinition("user-id").withValidation(pattern := ...)`
(the validation goes into the definition's body after `toDefinition`), and any value class whose underlying schema has
no validations (every value class in the test suite: no golden file has a value class over a validated type).

## Why it is a problem
- Silent data loss in the schema: a value class over `Char`, over a refined type, or over a user `Predef` with a
  `pattern` makes a schema that accepts values the type can't hold. Wrapping a validated type in a value class is the
  usual way to make it a domain type (`case class Age(v: Int Refined Positive) extends AnyVal`), so the loss hits
  exactly the users who bothered to write validations.
- `Json.schema[X].withValidation(...)` given as an implicit `Schema` (R9) is the direct way to validate a value class
  everywhere it's used, and it silently does nothing. (It needs a user `Magnet[X, String]`, as in the README's `UserId`
  example: no built-in magnet lets `withValidation` onto a value class. The README's
  `Json.schema[UserId].toDefinition(..).withValidation(..)` is the form that works today, with the same magnet.)
- A `@discriminator` family with a value-class member (D1) renders a member that no document matches, under the
  default profile and Legacy.
- P1 contradicts the method's name (`withValidationsAddedFrom` adds nothing to the receiver; it replaces it).
- P4 is a crash at render time, for a model that compiles: a value class over any type with an implicit `Schema`.

## Options

The merge rule is the same in all options; they differ in where it lives. **Rule:** a value class stands for its
underlying schema: the validations are the underlying's followed by the value class's own, and on the same keyword the
value class's wins (it's the more specific one, and later `withValidation` calls on a node win today too, since
`ValidationList.json` is a `toMap`). `title`/`description` are the value class's, falling back to the underlying's (the
rule B1 #469 already uses in `def.adapt` and `mkValueClass`). `discriminationKey` is the underlying's, falling back to
the value class's: the profiles put a family member's tag on the member node, which for a value-class member is the
`value-class`, while the renderer reads it from the object it renders (the underlying). Moving it down is what fixes
D1; an underlying that already has its own key keeps it.

### A. One helper, `value-class.unwrapped`, used by every path  — WON
- `json.Schema.withValidationsAddedFrom(x)`: start from `duplicate()` (keeps the receiver's texts and
  discriminationKey) and set `this._validations ++ x._validations`. That's what the name says, and what its only
  user-visible caller needed.
- New `def unwrapped: Schema[I]` on `` `value-class` ``: the underlying schema with the rule above applied (validations of
  the underlying minus the keywords the value class sets, then the value class's; texts and key as above).
- P1: `toDefinition(sig) = def(sig, unwrapped)`.
- P2: `def.adapt`'s value-class case: `def(sig, vc.unwrapped)`.
- P3/P4: the renderer treats a value class as transparent: `AsDraftSupport.apply` renders `unwrapped` in place of a
  value-class node, and the places that read a child's `jsonType` before rendering it (`mkPlainOneOf`, `mkAllOf`,
  `mkNullable`) look at `unwrapped` too. A value class over a definition then renders as the definition's `$ref` (the
  existing `def` code), and `extractDefs` gets a `value-class` case that walks into the underlying, so the definition is
  emitted. `extractDefs`' `def` case walks `x.tpe` directly (the `withValidationsAddedFrom` there never mattered).

Output after, from a throwaway prototype of A (`notes/scratch/c-b13/proto-v2.patch`; it appends the validations
without the per-keyword override, which only matters when both nodes set the same keyword). `clean test` with it: all
green, 0 warnings, no golden or golden-legacy change: 1042 tests on 2.12.21, 1058 on 2.13.18, 1105 on 3.3.9 (counts
include the two repro specs; logs `proto2-test-*.log`, repro output `repro2-proto.log`, `proto-after-*.txt`, the
same on 2.13 and 3):

```json
// R2 Json.schema[Code] inline (the pattern is B5's to fix, see below)
{"type": "string", "minLength": 1, "maxLength": 1, "pattern": "^[.\\s]$"}
// R1 .toDefinition("vc"): body; tree def(vc, string{minLength 3, maxLength 5} title "VC T" desc "VC D")
{"type": "string", "minLength": 3, "maxLength": 5}           // + "title"/"description" once B1 #469 renders them on strings
// R6 SlugHolder
"id": {"type": "string", "minLength": 3}
// R9 PlainHolder: the definition
"...Plain": {"type": "string", "pattern": "^[a-f0-9]{16}$"}
// R7 Json.schema[PersonW]; H(w: PersonW) is the same with "w": {"$ref": ...}
{"$ref": "#...Person", "definitions": {"...Person": {"type": "object", ...}}}
// R7 Option[PersonW] field: "o": {"oneOf": [{"$ref": "#...Person"}, {"type": "null"}]}
// Q2 FamHolder (Neutral)
"f": {"oneOf": [{"$ref": "#...Person"}, {"type": "string"}]}, "definitions": {"...Person": {...}}
// D1 member W, Neutral and Legacy (r-b13, notes/scratch/r-b13/p2-proto.log)
{"additionalProperties": false, "properties": {"a": {"type": "integer"}, "t": {"enum": ["...W"]}}, "required": ["a", "t"]}
```

A value-class member **over a definition** in a `@discriminator` family (`case class FP(p: Person) extends AnyVal with F`,
`Person` with an implicit `Schema`) renders, with A, as a bare `{"$ref": "#...Person"}`: the tag moves onto the `def`
node, and a shared definition can't take one member's tag (`mkDef` emits only the `$ref`). By the profile code
(`withDefinition(m, typed.duplicate(discriminationKey = ...))`), that is what a case-class member with an implicit
`Schema` renders today, so A adds no new gap there; master throws (P4). The step 4 tests pin it.

Pros: one rule in one place (the tree), used by the tree API (`toDefinition`), both macros (through `adapt`, no macro
change) and the renderer; fixes the crash as a side effect of the same step; no change to the tree the macros build or
to the lowered tree (`withProfile`), so profiles and bridges are untouched. Cons: a value class's own validations next
to a `$ref` (value class over a definition, with `withValidation` on the value class) render beside the `$ref`, which
validators ignore before 2019-09 (B1 places texts there too; the definition itself must not change, it's shared).
`AsDraftSupport.mkValueClass` and the value-class cases in `apply`'s `base` and `inferSpecifics` become unreachable.

### B. Fix each path on its own  — LOST
`withValidationsAddedFrom` fixed as in A; `toDefinition` adds the texts; `mkValueClass` renders with the merged
`ValidationList` (vc's + underlying's); `base` skips `type` for a value class over a definition and `extractDefs`
descends; `adapt` copies the vc's validations; `mkPlainOneOf`/`mkAllOf`/`mkNullable` special-case value classes in
their `jsonType` lookups. Same output as A. Lost: the rule ends up written four or five times (B1 #469 already added
a texts-only copy in `adapt` and another in `mkValueClass`), and the next path that meets a value class gets it wrong
again; A's helper is the same amount of code.

### C. Unwrap value classes when lowering (`JsonSchemaProfile.lower`)  — LOST
`lower` replaces `value-class(t)` with `unwrapped`, so no renderer ever sees one; `toDefinition`/`adapt` still need the
helper (they run before lowering). Lost: the lowered tree is public (`schema.withProfile(p)`, tree specs compare it),
and the family lowering reads value-class members (`own`/`taggedProvided`, `Kind.ValueClass`: a library profile
writes `{"<field>": value}` for them), so the unwrap has to run after that and the two interact. Same output as A,
with more moving parts, in the profile code that several other gap items (B3, D3, D4, D5, E7) are changing.

### D. Merge in the macros  — LOST
Both derivations emit the underlying schema with the merged validations (or keep `value-class` but copy the inner's
validations up). Lost: two macros to change (Scala 2 `UValueTypes`/`SchemaTypes`, Scala 3 `ValueClasses.scala`),
hand-built trees, `withValidation` after derivation (R1, R9) and `toDefinition` stay broken, and P4 isn't touched.

### Scope alternative: the inline render (P3/P4) as its own item
The lead folded P3 and P4 into B13 (same root cause, shared helper). The alternative is B13 = P1 + P2 only (the
tree API), P3/P4 a separate item. Only worth it if the inline output change needs a separate owner decision; with A it's
one helper, and splitting leaves `toDefinition` and inline rendering of the same value class disagreeing in between.

### Legacy alternative
Under the lead's Legacy policy (0.7's style is frozen, its bugs aren't) the fix applies under `JsonSchemaProfile.Legacy`
too: dropping a validation the user's types declare is a bug, not a style. No golden-legacy file changes (none has a
value class over a validated type). The alternative, "Legacy keeps dropping them", would need the renderer to know the
profile (it doesn't today) or Legacy's lowering to strip the underlying's validations from every value class; not
recommended.

## Decision
Option A: fix `withValidationsAddedFrom` to keep its receiver, add `value-class.unwrapped` (underlying's validations,
then the value class's, the value class's winning per keyword; the value class's texts over the underlying's), and use
it in `toDefinition`, `def.adapt` and the renderer, which treats a value class as transparent. This also fixes the
crash for a value class over a definition. Applies under every profile, Legacy included.

## Implementation plan
1. `core/src/main/scala/json/Schema.scala`
   - `withValidationsAddedFrom(x)`: `val copy = duplicate(); copy._validations = this._validations ++ x._validations`.
     Scaladoc: "this schema with `x`'s validations added after its own; keeps this schema's title, description and
     discriminationKey".
   - `` `value-class` ``: add
     ```scala
     /** This value class as its underlying schema: the underlying's validations, then this one's (on the same keyword this one's wins);
       * this one's title/description over the underlying's; the underlying's discriminationKey, else this one's. */
     def unwrapped: Schema[I]
     ```
     Build it from `tpe.duplicate(title = title orElse tpe.title, description = ..., discriminationKey = tpe.discriminationKey orElse discriminationKey)`
     and set `_validations = tpe.validations.filterNot(v => ownKeys(v.validation)) ++ validations`, where `ownKeys` is
     the set of this value class's `validation`s. The override is done here, in the tree, on purpose: don't rely on
     `ValidationList.json`'s `.toMap` letting the later entry win (B2 #465 may make it an ordered map, and
     `ValidationList.extract` takes the first match).
     (`_validations` is a `private var` of the trait, accessible from the companion `Schema`'s nested classes;
     if a compiler disagrees, add a `private[json] def withValidations(vs: Seq[V.Def[_, _]]): Self`).
     Keep it non-recursive: an underlying `value-class` (only hand-built trees have one) stays as is and the renderer
     unwraps it again.
   - `` `value-class`.toDefinition ``: `def[TT](sig, unwrapped)`.
   - `` `def`.adapt ``: `case vc: `value-class`[_, _] => `def`(sig, vc.unwrapped)`. If B1 #469 is merged first, this
     replaces its texts-only `innerTpe.duplicate(title = vc.title orElse ..., ...)` (same texts, plus the validations).
2. `core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala`
   - `private def transparent(s): Schema[_]`: `case vc: value-class => transparent(vc.unwrapped); case s => s`.
   - `apply(x, parent, includeType, isRoot)`: render `transparent(x)`. Drop the `value-class(x) if includeType` case of
     `base` (unreachable). Keep `mkValueClass` and its `inferSpecifics` case (public members of a public trait; binary
     compatibility), with a comment that `apply` no longer sends value classes there.
   - `mkPlainOneOf`, `mkAllOf`, `mkNullable`: `x.subTypes.toSeq.map(transparent)` before the `jsonType`/`def` lookups;
     `singleType`'s value-class case becomes unreachable (drop it).
   - `inferDefinitions.extractDefs`: `case x: def => extractDefs(x.tpe, par) :+ inferDefinition(x, par)`; add
     `case x: value-class => extractDefs(x.tpe, par)` (a value class is transparent: same parent).
   - `AsRaw` goes through the same `apply`/`inferDefinitions`: nothing to do.
   - Note for whoever merges second: B3 #470 and B14 rework `mkPlainOneOf`; keep the `map(transparent)` at the top.
3. Not changed, checked: `JsonSchemaProfile` (`own`: a value-class member of a family under a library profile becomes
   `{"<field>": inner}`, which ignores the value class's own validations; derived members have none, so nothing is
   lost; `withProfile` output unchanged). Decision: keep `inner` and add a one-line comment there saying the value
   class's own validations/texts aren't carried (derived members have none). Not `vc.unwrapped`: that would also move
   the member's texts and tag into the field's schema, a second place for them next to the member object.
   Also unchanged: `DefinitionNames` (already walks into value classes), `AsTree`, the macros, the bridges, the parser.
4. Tests (shared `src/test/scala`, so they run on 2.12, 2.13, 3, JVM and JS):
   - `core/src/test/scala/json/SchemaCopySpec.scala` (or `SchemaSpec`): `withValidationsAddedFrom` keeps the receiver's
     validations (receiver's first), title, description, discriminationKey; `value-class.unwrapped`: validations order,
     same keyword → the value class's wins (asserted on `unwrapped.validations`, the tree, not only the JSON), texts
     precedence both ways, discriminationKey; `toDefinition` of R1 →
     `def(vc, string{minLength 3, maxLength 5} title "VC T" description "VC D")`; add `value-class.toDefinition` to
     `SchemaSpec`'s "copy extra" list in its own form (the texts land on the body, not the `def`).
   - `api/src/test/scala/com/github/andyglow/jsonschema/ValueClassSpec.scala` (new), full JSON per draft for 04, 07 and
     2020-12 (`AsDraftXXSpec` style): R1 inline and `toDefinition`; R6 inline field, `List[SlugId]`, `Option[SlugId]`
     (nullable: `"type": ["string","null"], "minLength": 3`); R9 (`def.adapt`, implicit `Schema` with `withValidation`);
     R7 root, field, `Option`, `List` (a `$ref` + the definition, emitted once); Q2 (family with a value-class member
     over a definition, under Neutral); a value class over a definition with its own `withValidation` (keyword beside
     the `$ref`, definition unchanged). Use a non-`Char` underlying so the spec doesn't depend on B5.
   - Same spec, `@discriminator` families, draft-07, under **Neutral and Legacy**: D1 (the value-class member gets its
     `"t": {"enum": [...]}` property; validate `{"a": 1, "t": "...W"}` against it where the JVM validator is available),
     and the value-class member over a definition (`FP(p: Person)`, implicit `Schema[Person]`): a bare `$ref`, the
     `Person` definition untouched.
   - The petshop acceptance suite (`api/src/test/scala/petshop/`): add a value class over a validated `Predef`
     (e.g. `minLength`) to the model so every library profile checks the merged schema against real codec output
     (JVM; canceled on JS).
   - `modules/refined`: `case class Age(v: Int Refined Positive) extends AnyVal` as a field → the refined bounds render
     (Scala 2 and 3: refined has its own macro on each).
   - No golden-file changes expected (prototype: all suites green, goldens and golden-legacy untouched, on 2.12.21,
     2.13.18 and 3.3.9). If the new models go into the golden harness instead, add `golden/` files deliberately.
5. Docs (README + paradox say the same):
   - README "Supported types" table row `value class | its underlying type` (L287) and `docs/src/main/paradox/types.md`: "its underlying
     type, with its validations, title and description".
   - README "Definition names" (`UserId` example, L231-245) and `docs/src/main/paradox/definitions.md` L74-85: one
     sentence: validations added to a value class come after its underlying type's (on the same keyword, the value
     class's win), whether it's inlined, a definition (`toDefinition`) or an implicit `Schema`.
   - README "Validation" section (L511) / `validation.md` L22: nothing new beyond a link if wanted.
   - No CHANGELOG edit (the lead writes it).
6. Merge order: **after B5 #468** (Char pattern). Today an inline `Char` value class renders no `pattern`; with B13
   alone it would render 0.9.0's `^[.\s]$`, which rejects every letter, so B13 must not ship before B5. With B5 first,
   a `Char` value class renders `minLength: 1, maxLength: 1` (what B5 leaves). With B1 #469: either order; whoever
   lands second reconciles `def.adapt` and `mkValueClass` as in steps 1-2.
7. Gates: `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on all three (core and api change);
   `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`; `makeDocs` (README/paradox change).

Risks: (a) a user who relied on `withValidationsAddedFrom`'s old result (only `x`'s validations); unlikely, the
method is undocumented and the old result ignores its receiver. (b) Schemas get stricter where the underlying type
had validations: values that violate them now fail validation. That is the fix, but it's the user-visible change to
list in the PR and CHANGELOG. (c) An underlying with an invalid or overly strict validation (B5's `Char` pattern)
starts to apply inline: see step 6.

## Compatibility
- Source and binary: no signature changes. Adds a public method `value-class.unwrapped` (an addition, fine in a
  0.9.x release; it's on a final case class). `mkValueClass` stays. `withValidationsAddedFrom` keeps its signature; its
  result now keeps the receiver (a behaviour fix).
- Output: new validation keywords (and, with B1, texts) where a value class's underlying type has validations, inline
  and as a definition; an implicit `Schema` of a value class keeps the value class's own validations; a value class
  over a definition renders a `$ref` instead of throwing; a value-class member of a `@discriminator` family gets its
  tag property (D1: was an unsatisfiable member) under Neutral and Legacy. Nothing that rendered before disappears.
  All drafts, all profiles, Legacy included (lead's Legacy policy: each of these fixes a schema that was wrong), no
  golden or golden-legacy change in the current suite.
- PVP: a bug fix, patch level (0.9.x), like B5. It makes some schemas stricter; the PR lists that.
- Core design rule: unchanged (an implicit `Schema` of a value class is still a definition, a `Predef` is still inlined;
  only what the value class contributes to either changes).

## Review log
Round 1 (r-b13, doc 32cd13c):
- B13-R1 (must) unlisted output change: a value-class member of a `@discriminator` family gets its tag property
  (Neutral, Legacy) → accepted: P5 row + D1 repro in Problem, a "Why" bullet, the D1 output under A, the
  discriminationKey rationale corrected (the tag sits on the value-class node; moving it to the underlying is what
  fixes D1), Compatibility "Output", tests under Neutral and Legacy.
- B13-R2 (must) value-class member over a definition in a discriminated family → accepted: stated under A (a bare
  `$ref`, same as a case-class member with an implicit `Schema`; master throws), added to the tests.
- B13-R3 (nit) R9 needs a user `Magnet` → accepted: said in "Why", with the README form that works today.
- B13-R4 (nit) per-keyword override in the tree, not via `.toMap` → accepted: step 1 says why; the test asserts on
  `unwrapped.validations`.
- B13-R5 (nit) `JsonSchemaProfile.own` → accepted, chose the comment (keep `inner`), and wrote why not `vc.unwrapped`
  (step 3).
