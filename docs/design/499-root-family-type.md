# A sealed family (or `allOf`) at the root states no `type`  (issue #499, item B15)
Status: approved (r-b15, 2026-11-23)

## Problem

The draft renderers write a same-typed `oneOf` (every sealed family of case classes is one) as the shared type,
stated once next to the `oneOf`, plus members rendered **without** their own `type`. At the root the shared type is
dropped, so nothing in the document states it:

```scala
// core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala (master, 4b2aff3)
// mkPlainOneOf, line 180
def rootType = tpe filter { _ => !isRoot } map { t => obj("type" -> `str`(t)) } getOrElse obj()
// mkAllOf, line 205
"type"  -> (if (!isRoot) Some(`str`(tpe)) else None),
```

`isRoot` is `true` only for the document's top node (`apply(x)`, line 49); definition bodies, fields, items and
members are rendered with `isRoot = false` and do get the type.

Repro (probe: `notes/scratch/c-b15/B15ProbeSpec.scala`, not committed; logs `probe-2.13.18.log`, `probe-3.3.9.log`
next to it). Same output and the same validation results on 2.13.18 and 3.3.9 (the renderer is shared code, so 2.12
and Scala.js render the same bytes); networknt json-schema-validator, draft-04, draft-07 and 2020-12 alike:

```scala
sealed trait One;  final case class OnlyOne(a: Int) extends One
sealed trait Ev;   final case class Created(id: String) extends Ev; final case class Deleted(id: String) extends Ev
sealed trait Two;  final case class TA(a: Int) extends Two;         final case class TB(b: String) extends Two
@discriminator sealed trait DOne; final case class D1(a: Int) extends DOne
sealed trait Mixed; final case class MA(a: Int) extends Mixed;      case object MB extends Mixed
final case class W(e: Ev)
```

Root `One`, draft-07:

```json
{"$schema": "http://json-schema.org/draft-07/schema#", "$id": "http://x/s",
 "oneOf": [{"additionalProperties": false, "properties": {"a": {"type": "integer"}}, "required": ["a"]}]}
```

`Ev` as the field `e` of `W`: `"e": {"type": "object", "oneOf": [{...}]}`.

| root schema | `5`, `"s"`, `null`, `true`, `[]` | why |
|---|---|---|
| `One` (1 member) | **valid** | `object` keywords don't apply to non-objects: the one member matches |
| `Ev` (2 equal members, render as 1 entry) | **valid** | same |
| `DOne` (`@discriminator`, 1 member) | **valid** | same |
| `One` under `SumStyle.WrapperObject` | **valid** | same |
| `One` under `JsonSchemaProfile.Legacy` | **valid** | same (Legacy = 0.7 output) |
| `Two` (2 disjoint members), also with `SumStyle.Discriminator` | invalid, but "must be valid to one and only one schema, but 2 are valid" | rejected by accident: every member matches |
| `Mixed` (object + case object) | invalid: "integer found, object expected" | not same-typed: each member states its type |
| `W` (the family nested) | invalid: "integer found, object expected" | the nested `oneOf` states `type` |
| `allOf.of(object{a}, object{b})` by hand | **valid** | `allOf` of members that all match: every member count |
| `allOf.of(string{minLength 1}, string)` by hand | **valid** (also `{}`, `5`) | same |

The real `allOf` producer is refined's `And` (`modules/refined`: `Int Refined (GreaterEqual[5] And LessEqual[5])` is
`allof{integer(minimum 5), integer(maximum 5)}`, `RefinedQuirksSpec:53`): at the root it renders (by reading
`mkAllOf`; the hand-built `allOf`s above are the run evidence) `{"allOf": [{"minimum": 5}, {"maximum": 5}]}`, which
accepts `"s"`.

History: the two guards come from two commits, neither with a test or changelog entry that asks for them.
d29c3f6 ("refactor Macroses", 2020-11-10, first tag v0.6.1) added `isRoot` and `mkAllOf`'s `if (!isRoot)`; `mkOneOf`
still wrote `"type" -> tpe` unconditionally (`git show v0.6.1:core/.../AsDraftSupport.scala`). ee9fe0b ("definitions,
discriminations", 2020-12-01, first tag v0.7.1) added `mkOneOf`'s `filter { _ => !isRoot }`. So a root `oneOf` stated
its type up to 0.6.x, and a root `allOf` never did after 0.6.0. The 0.7 goldens (`golden-legacy/`) recorded the result.

## Why it is a problem

- **The schema accepts documents of the wrong type** whenever the root composite has one member (a single-case
  family, two members with equal schemas) or is an `allOf`.
  A service validating requests against `Json.schema[Command]`, where `Command` has one case so far, accepts `5`.
- **For two or more members the rejection is an accident.** It relies on more than one member matching. The error
  ("2 are valid with indexes '0, 1'") points at the wrong thing; a type error is what the user needs.
- **Root and nested output disagree.** The same family is `{"type": "object", "oneOf": ...}` in a field and
  `{"oneOf": ...}` at the root. Tools that read `type` (form generators, code generators, OpenAPI tooling) see an
  untyped root.
- B3 (#461, PR #470) adds `anyOf` and already states the type at the root for it (B3-R3), because an `anyOf` has no
  accidental rejection to hide behind. After B3, `oneOf` and `allOf` are the only composites that drop it.

Found by r-b3 (B3-R3, B3-R7); no user issue reported it.

## Options

### A. State the shared type at the root too, under every profile  — WON

Delete the two `!isRoot` guards. A same-typed `oneOf` / `allOf` states its type the same way at every depth:

```json
{"$schema": "...", "type": "object", "oneOf": [{"additionalProperties": false, "properties": {"a": ...}, ...}]}
```

Nothing else moves: members stay without their own `type`, a family whose members have different types still lets
each member state its own, and a family of only definitions / `$ref`s still states none (each definition body has its
type; see "Non-cases" below).

Effect, measured by applying the change on 2.13.18 (`clean test` of every JVM module, then `GOLDEN_UPDATE=1
GOLDEN_UPDATE_LEGACY=1 clean api/test`; diff kept in `notes/scratch/c-b15/exp-golden.diff`, then reverted):
- only `api` tests fail; every bridge, refined, cats, derived, parser and enumeratum suite passes unchanged;
- 13 golden files and the same 13 golden-legacy files change, each by one line per draft render (78 lines in all):
  66 × `"type": "object"`, 12 × `"type": "string"` (the two `EnumsAsOneOf` roots, a `oneOf` of string `const`s);
- the hand-written "emit const" test in `AsDraft04/06/07/09/12Spec` (a root `oneOf` of two string `const`s) gains
  `"type": "string"`.

Pros: one rule, root = nested; fixes every validation hole above; better errors for 2+ members; no profile knob, no
API change; the change is two lines. Cons: 13 golden-legacy files change (Legacy's 0.7 bytes), 12 of them in a way
that doesn't change what validates (see B).

### B. A, but `JsonSchemaProfile.Legacy` keeps the 0.7 output  — LOST (the explicit alternative)

Same as A for Neutral and the library profiles; Legacy renders the root without the type, as 0.7 did.

The renderer doesn't know the profile: `AsValue.schema` lowers the tree with it (`profile.lower(x)`) and hands the
lowered tree to `AsDraftXX.schema`, which has no profile parameter. B needs a new channel: a knob on
`JsonSchemaProfile` (e.g. `rootCompositeType: Boolean`, `false` only for Legacy) and a way into `AsDraftSupport`
(a thread-local like `definitionNames`, or a new `schema(x, profile)` overload on `AsValue`, which is a public trait).
Legacy would keep accepting `5` for a one-member root family, which the lead's Legacy policy (2026-11-23: Legacy
freezes 0.7's style, not its bugs) counts as a bug to fix.

Variant B': Legacy gets the type only where it changes validation (1-member `oneOf`, any `allOf`): byte-exact for 12
of the 13 golden-legacy files, but still needs the plumbing, and Legacy ends up with a rule no other profile has.

Lost on cost (new API surface for one keyword) and on the Legacy policy. It wins if the owner rules that a root `type`
is style under Legacy.

### C. Only where validation needs it, under every profile  — LOST

State the root type only for a one-member `oneOf` and for `allOf`; leave 2+ member `oneOf`s as they are. No plumbing,
and Legacy changes in one golden file (`char/fields/DiscOrder`). But root and nested keep disagreeing, the
"2 are valid" error stays, and the rule ("type at the root only if there's one member") is something a reader of the
output can't guess. It saves golden churn and nothing else.

### D. Unwrap a one-member `oneOf` into its member  — LOST

Render `oneof{X}` as `X` (with its type). Fixes the one-member family, but not `allOf`, and changes the nested
output too (`{"type": "object", "oneOf": [X]}` → `X`), which the bridges' and the golden expectations encode. It's
a separate simplification; not needed for the bug.

## Decision

A: drop the `!isRoot` guards in `mkPlainOneOf` and `mkAllOf`, so a same-typed `oneOf` / `allOf` states its type at
the root as it does everywhere else, under every profile, Legacy included (13 golden-legacy files regenerated
deliberately). If the owner rules that the root type is Legacy *style*, B is the fallback; C is the low-churn fallback
if the owner wants the golden-legacy bytes kept for 2+ member families without new API.

Legacy: the lead (2026-11-23) chose A for Legacy too (one renderer rule), with "Legacy keeps 0.7" (B) kept as the
alternative and put to the owner in the PR: 12 of the 13 golden-legacy changes are additive and don't change what
validates; the 13th (`DiscOrder`, one member) is the validation fix.

## Implementation plan

The change is in shared code (`core/src/main/scala`): no Scala 2 / Scala 3 macro change, no JS-specific code.

1. **Base.** Implement on top of master **after B3 (#470) is merged** if it merges first (likely: B3 is approved).
   B3 refactors `mkPlainOneOf` into `mkAlternatives(keyword, x, members, omitRootType)` with
   `mkPlainOneOf(...) = mkAlternatives("oneOf", ..., omitRootType = isRoot)` and `mkAnyOf` passing `false`.
   - On B3: drop the `omitRootType` parameter (always state the type), update the comment
     "a type all members share is stated once, next to `oneOf`, but not at the root (as in 0.7)" to
     "... next to `oneOf`/`anyOf`, at every depth"; `mkAnyOf`'s scaladoc keeps its explanation of why `anyOf`
     needs the type, generalised to `oneOf`.
   - On master without B3: in `mkPlainOneOf` (AsDraftSupport.scala:180) replace `tpe filter { _ => !isRoot } map`
     with `tpe map`; drop the `isRoot` parameter of the private `mkPlainOneOf`.
   - Either way, in `mkAllOf` (line 205): `"type" -> Some(`str`(tpe))`, i.e. drop the `if (!isRoot) ... else None`.
   - Other open PRs on the same lines (merge, don't redesign):
     - E2 (#486, `Schema.any`) edits `mkPlainOneOf`'s `tpe` lookup (skips `any`) and `mkAllOf`'s `typed`, next to both
       guards. A textual conflict only: A keeps E2's rule (`oneof{string, any}` isn't same-typed, so no root type);
     - B14 (design PR #500) assumes B15 for a root refined `oneOf` ("today a root refined `oneOf` states none, B15"):
       nothing to do here beyond landing A;
     - E6 (#489): unions become `anyof`, which B3 already types at the root. No interaction.
2. **Keep the public signatures.** `AsDraftSupport.apply(x, parent, includeType, isRoot)`, `mkOneOf(vl, x, isRoot, par)`,
   `mkAllOf(vl, x, isRoot, par)` and the `inferSpecifics` tuple are public (the trait is public and the bridges/users
   may extend it). Keep `isRoot` in them, now unused by the rendering; add a one-line scaladoc on `apply`: "`isRoot`:
   the document's top node; no longer changes the output (kept for source/binary compatibility)". The build has no
   unused-params warning (`project/CompilerOptions.scala`: `-Ywarn-unused:imports,...` without `params`; Scala 3
   `-Wunused:imports`), so this stays warning-free. Removing the parameter is a later, breaking cleanup (not here).
3. **New test** `api/src/test/scala/com/github/andyglow/jsonschema/RootCompositeTypeSpec.scala` (shared; validation
   via `petshop.JsonValidator`, so the validating cases run on the JVM and are canceled on JS, as elsewhere). Models
   `One`, `Ev`, `DOne`, `Two`, `Mixed`, `W` from the Problem section (in the spec's companion/object, not
   `model/`), and the two hand-built `allOf`s. Keep the render-only checks (first two bullets) in tests of their own,
   apart from the tests that call the validator: on Scala.js `JsonValidator` cancels the whole test that calls it, so a
   render check inside it would never run there (the petshop specs split them the same way). For each of draft-04,
   draft-07, draft-2020-12:
   - rendered root of `One`, `Ev`, `DOne`, `Two` has `"type": "object"`; `EnumsAsOneOf` root and the const `oneOf`
     have `"type": "string"`; `Mixed`'s root has no `type` and each member has its own; an all-`$ref` family
     (`DefinitionsSpec.Pet`-like, or B3's `RefEv` if on B3) has no root `type`;
   - root = nested: the root of `Ev` without `$schema`/`$id` equals the `e` property of `W`, compared as JSON;
   - validation: `5`, `"s"`, `null`, `true`, `[]` are rejected by `One`, `Ev`, `DOne`, `Two`, the `allOf`s, and the
     error list contains a type error (`"found, object expected"`), not only the oneOf count; `{"a":1}` (`One`),
     `{"id":"x"}` (`Ev`), `{"a":1,"type":"...D1"}` (`DOne`) stay valid;
   - the same checks for `One` under `JsonSchemaProfile.Legacy`, `withSum(SumStyle.WrapperObject)` and
     `withSum(SumStyle.Discriminator("type"))` (if the owner picks B instead, the Legacy rows assert no root type).
   - Scala 3 only (`api/src/test/scala-3/...`, render-only): the README's root `enum Event { case Created(id: String);
     case Deleted(id: String, reason: Option[String]) }` has `"type": "object"` next to its `oneOf`, so the docs'
     example is pinned by a test.
4. **Refined.** Add to `modules/refined/src/test/scala-2.13+/.../RefinedQuirksSpec.scala` (or a small new spec in
   the same dir): `AsValue.schema(schema[Int Refined (GreaterEqual[5] And LessEqual[5])], Draft07(...))` has
   `"type": "integer"` next to `"allOf"`. Tree-only today; this is the one render assertion. (Validation isn't
   available in that module's tests; the render check is enough.)
5. **Hand-written renderer specs.** In `api/src/test/scala/com/github/andyglow/jsonschema/AsDraft04Spec.scala`,
   `AsDraft06Spec`, `AsDraft07Spec`, `AsDraft09Spec`, `AsDraft12Spec`, test "emit const": add `"type" -> "string"`
   to the expected root `obj`.
6. **Golden files.** `GOLDEN_UPDATE=1 GOLDEN_UPDATE_LEGACY=1 sbt clean api/test` (a CLEAN build), then review: the
   diff must be exactly one `"type"` line per draft render in these files, under both `golden/` and
   `golden-legacy/` (if the owner picks B: `golden/` only, and the golden-legacy set must stay unchanged):
   `char/fields/DiscOrder`, `char/fields/KeyWithoutDiscriminator`, `groupC/Either[Int,String]`,
   `shared/DiscriminatorSpec.case0`, `.case1`, `.case3`, `.case4`, `shared/EnumSpec.case0.EnumsAsOneOf`,
   `shared/EnumSpec.case8.EnumsAsOneOf` (these two: `"string"`), `shared/FooBar`, `shared/FooBarInsideCompanion`,
   `shared/MultiLevelSealedTraitRoot`, `triageB/covariant.Base` (the rest: `"object"`). This list was measured on
   master; on B3's base (or after E2/B14), re-measure it: B3's own new root-`oneOf` goldens change the same way, and
   some of the 13 may come out differently (e.g. a family B3 turns into `anyOf` already has its root type there).
   List in the PR both the files that join and the files that drop out. No `@2.12`/`@2.13`/`@3` overrides exist,
   none needed (same output on all versions). Unchanged and worth a look in the review: `HybridSum`,
   `HybridRecursiveSum`, `AnyFooBar`, `HybridGenericSum[Int]`, `SumTypeModels.*` (mixed types),
   `DiscriminatorSpec.case5`, `DefinitionsSpec.Pet` (only `$ref`s).
7. **Bridges.** No change expected: the experiment ran every bridge suite green on 2.13 (not run on 2.12/3 or JS).
   Re-run them; if a row compares a root family's bytes, update it and list it in the PR.
8. **Docs** (README.md and `docs/src/main/paradox/types.md` say the same thing; update both):
   - README ~L326 / types.md ~L65 (`enum Event` comment) and types.md ~L52 (`Json.schema[Shape]` comment):
     `{"type": "object", "oneOf": [...]}`;
   - README ~L357 / types.md ~L100 (the `@discriminator Pet` JSON): add `"type": "object",` above `"oneOf"`;
   - README ~L397 / types.md ~L141 (`EnumsAsOneOf` `Gender` JSON): add `"type": "string",`;
   - one sentence in the sealed-family bullets (README ~L317, types.md alongside): "The members' shared JSON type is
     stated next to the `oneOf`, at the root too." B3 edits the same paragraphs (member order, `anyOf`): rebase on it
     and merge the wording. `sbt makeDocs` must pass.
   - **The "Legacy = 0.7 byte for byte" promises** (under A they become false; say "0.7 plus the listed fixes"):
     - `core/src/main/scala/json/schema/JsonSchemaProfile.scala:146` (`/** The 0.7 output, byte for byte. */`). B3
       rewrites it to "byte for byte, except where 0.7's schema was wrong: ..."; add "a root `oneOf`/`allOf` states its
       members' shared type" to that list (whichever PR lands second merges the wording);
     - `README.md:835` ("render sealed families exactly as 0.8 and 0.7 did") and `README.md:873` ("Legacy reproduces
       0.7 byte for byte"), and their paradox twins `docs/src/main/paradox/migration-0.9.md:8` and `migration.md:19`.
       The 0.9.0 migration list's "Two renderer bugs are fixed, also under Legacy" (README:841) is the natural home
       for one more line, or the next version's migration section if the lead starts one;
     - `api/src/test/resources/golden/README.md:16` ("a verbatim copy of the 0.7 goldens ... proves Legacy reproduces
       0.7 byte for byte"), the scaladoc of the Legacy set in `api/src/test/scala/.../harness/Golden.scala:48`, and
       CLAUDE.md ("`golden-legacy/` is the frozen 0.7 output that `JsonSchemaProfile.Legacy` must reproduce").
9. **Gates** (for the implementer): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on all three (core and api
   change); `scalafmtCheckAll` and `rootJS/scalafmtCheckAll`; `makeDocs`.

Non-cases (checked, unchanged by A):
- a family whose members have different JSON types (case classes + case objects; `HybridSum`): not same-typed, each
  member states its own type, the root states none, as today;
- members that are all definitions / `$ref`s (`DefinitionsSpec.Pet`, `DiscriminatorSpec.case5`): `tpe` is `None`, so
  no root type; each definition body states `"type": "object"`, so `5` is rejected (r-b3 verified this for B3's
  `RefEv`);
- `Option[Family]` at the root: `mkNullable`, which already renders the family with `isRoot = false`;
- `Schema.not` (`mkNot`) never stated a root type; its `"type": "not"` is B22's.

Risks: low. The only output change is an added `type` that each member already implies. A user who post-processes
the root (e.g. merges our root into an OpenAPI component and adds its own `type`) gets a duplicate key only if they
build JSON by concatenation; `obj ++` replaces it.

## Compatibility

- Source and binary: no change (no public signature changes; `isRoot` stays in the public methods, unused).
- Output: every profile (Neutral, the library profiles, Legacy under A) adds `"type"` next to a root `oneOf` whose
  members share a type, and next to a root `allOf`. Documents of that type validate as before; documents of another
  type are now rejected with a type error (before: accepted for 1 member and `allOf`, rejected by the oneOf count for
  2+ members).
- PVP: a bug fix in output, no API change → **patch** (0.9.x). It can go out with B3 in whatever version B3 ships.
- Legacy: under A, 13 golden-legacy files change by one `"type"` line per draft (listed in step 6; to be listed in the
  PR). Under B, none.
- Legacy's **documented contract** changes: "the 0.7 output, byte for byte" becomes "0.7's output, except where 0.7's
  schema was wrong" (step 8 lists every place that says so). Under the lead's Legacy policy (2026-11-23: style frozen,
  bugs fixed) that is still a patch, the same treatment as B3's fixes under Legacy.

## Review log
B15-R1 (must) the "Legacy = 0.7 byte for byte" promises A breaks → accepted: step 8 lists them (JsonSchemaProfile:146,
README 835/841/873, migration-0.9.md:8, migration.md:19, golden/README:16, Golden.scala:48, CLAUDE.md) with the new
wording; Compatibility states the contract change and why it's a patch.
B15-R2 (nit) history: two commits → accepted: Problem's History paragraph corrected (`allOf` guard d29c3f6 / v0.6.1,
`oneOf` guard ee9fe0b / v0.7.1). Issue #499's "affected versions" corrected the same way.
B15-R3 (nit) other open PRs; re-measure goldens on B3's base → accepted: step 1 names E2 (#486), B14 (#500), E6 (#489);
step 6 says to re-measure on the actual base and list files that join or drop out.
B15-R4 (nit) render checks out of validating tests → accepted: step 3 splits them; Scala 3 root `enum Event` test added.
