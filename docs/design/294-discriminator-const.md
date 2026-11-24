# `const` for the discriminator value  (issue #294, item D3)
Status: draft

## Problem

A tagged sealed family renders each member's tag (its discriminator value) as a one-value `enum`, on every draft and
under every profile. #294 asks for an option to render it as `const` (draft-06 and later).

```scala
@discriminator sealed trait Shape
object Shape {
  @discriminatorKey("c") final case class Circle(r: Int)       extends Shape
  final case class Square(side: Int)                           extends Shape
  @definition("Tri") final case class Triangle(a: Int, b: Int) extends Shape
}
Json.schema[Shape]   // rendered with AsValue.schema(_, Draft06("x")) / Draft12("x")
```

Actual output, draft-06 and draft-2020-12 (draft-04 is the same apart from `$schema`/`$id`/`$ref` style), Neutral and Legacy alike:

```json
"oneOf": [
  { "additionalProperties": false,
    "properties": { "r": { "type": "integer" }, "type": { "enum": ["c"] } },
    "required": ["r", "type"] },
  { "additionalProperties": false,
    "properties": { "side": { "type": "integer" }, "type": { "enum": ["com.github.andyglow.jsonschema.D3ReproSpec.Shape.Square"] } },
    "required": ["side", "type"] },
  { "$ref": "#Tri" }
],
"definitions": { "Tri": { "$id": "#Tri", "type": "object", ...,
  "properties": { "a": ..., "b": ..., "type": { "enum": ["com.github.andyglow.jsonschema.D3ReproSpec.Shape.Triangle"] } } } }
```

All three paths that produce a tag end in the same place:
- a derived family with `@discriminator` (Neutral, Legacy, spray);
- a library profile with `SumStyle.Discriminator(field)`: play `_type`, upickle `$type`, circe configured, json4s hints. For example,
  `Neutral.withSum(Discriminator("_type")).withSingleton(AsTagged)` gives `"_type": {"enum": ["...Pet.Dog"]}` and, for a case object,
  `{"properties": {"_type": {"enum": ["...Pet.Fish"]}}, "required": ["_type"]}`;
- a hand-written `` `oneof`(...).discriminatedBy("kind") `` whose objects carry `.withDiscriminationKey("A")`.

The lowering (`JsonSchemaProfile.Lowering`) only sets `discriminationKey` on the member object (or on the body of its
definition) and `discriminationField` on the `oneof`. The renderer writes the tag in one place,
`AsDraftSupport.mkObj` (`AsDraftSupport.scala:92`): `df -> obj("enum" -> arr(dk))`. Nothing configures it, and a
user can't work around it short of hand-writing the whole family's schema. With `phantom = false`, the member's own
`type: String` field is replaced by the same `enum`.

Reproduced with an uncommitted spec (`notes/scratch/c-d3/D3ReproSpec.scala`, outputs `repro-*.out`): the output is
byte-identical on 2.12.21, 2.13.18 and 3.3.9. The renderer is shared code with no JS shim, so JS renders the same.

Related, found while reproducing: **draft-04 has no `const`, yet the renderer writes `"const"` on draft-04**
(`AsDraftSupport.mkConst` has no draft-04 override; known issue 4b). A draft-04 validator ignores the unknown
keyword, so each `{"const": "V1"}` becomes `{}`. `Flag.EnumsAsOneOf` families therefore render on draft-04 as
`"oneOf": [{"const": "V1"}, {"const": "V2"}]`, which matches every instance twice and so **rejects every instance**.
Goldens that lock this in: `EnumSpec.case0.EnumsAsOneOf`, `EnumSpec.case8.EnumsAsOneOf`,
`SumTypeModels.FooBarInsideCompanionWithAnnotations.EnumsAsOneOf` (in `golden/` and `golden-legacy/`). Any option
here that writes `const` for the tag has to handle draft-04 too, so the plan includes this fix as its own step
(lead's call, 2026-11-23).

## Why it is a problem

- `const` is the idiomatic keyword for "exactly this value" from draft-06 on. Tools that read the schema (code
  generators, OpenAPI 3.1 converters, documentation renderers) may treat a one-value `enum` as an enumeration type
  rather than a literal. #294's author asks for a config option without naming the tool; there is one reporter.
- Both forms validate the same: this is about style, not correctness. That is why it needs a knob, not a silent switch.
- The draft-04 `const` (4b) is a correctness bug: a valid document is rejected.

## Options

### A. Per-draft switch in the renderer: `const` on draft-06+, `enum` on draft-04 — LOST
`mkObj` writes `df -> obj("const" -> dk)` unless `isDraft04`. One line, no API.

- Pro: smallest change; everyone gets the idiomatic form.
- Con: changes the default output for every `@discriminator` user and every library profile on draft-06+, with no
  way back.
- Con: **breaks Legacy's byte-exact 0.7 output.** The renderer doesn't know the profile: lowering runs first and
  leaves only plain nodes. So Legacy can't keep `enum` unless the renderer learns the profile, and that is option C.
  A style change under Legacy needs the lead, and the issue asks for an option, not a new default.

### B. A profile knob; the lowering writes the tag as a `const` field — WON
A new knob on `JsonSchemaProfile`, following `optionStyle`/`sumStyle`/`singletonStyle`:

```scala
sealed trait TagStyle
object TagStyle {
  /** `"kind": {"enum": ["cat"]}` on every draft (0.7, the default). */
  case object Enum extends TagStyle
  /** `"kind": {"const": "cat"}`; on draft-04, which has no `const`, `{"enum": ["cat"]}`. */
  case object Const extends TagStyle
}
def tagStyle: TagStyle = TagStyle.Enum                                       // trait JsonSchemaProfile
final def withTag(x: TagStyle): JsonSchemaProfile = Knobs(this).copy(tagStyle = x).profile
```

Under `Const`, `Lowering` turns every discriminated `oneof` (from a family, or hand-written) into plain nodes. For
each member object that carries a `discriminationKey` (directly, or as the body of a `def`, as far down as `def`s
nest), it puts the field `df: const(key)` (required) in the object's fields and clears the object's
`discriminationKey`. It replaces an existing field of that name in place (`phantom = false`), else appends one.
The renderer then writes the field with its normal `mkConst`: `{"const": "c"}`. On draft-04, step 2 of the plan
renders it as `{"enum": ["c"]}`. Under `Enum` nothing changes: the key stays on the object and `mkObj` writes the
`enum` as today.

```json
"properties": { "r": { "type": "integer" }, "type": { "const": "c" } }, "required": ["r", "type"]
```

- Pro: the issue's "config option". Default output is unchanged for every profile, Legacy included.
- Pro: keeps the profile contract. The profile decides at lowering, so `schema.withProfile(Neutral.withTag(Const))`
  renders `const` wherever it is rendered later, just like the other knobs. The renderer stays profile-agnostic.
- Pro: one mechanism covers derived families, library profiles (including `taggedProvided`'s copy of an implicit
  object, and `AsTagged` singletons) and hand-written discriminated `oneof`s.
- Pro: it's additive under PVP (a new knob with a default), so it can ship in 0.9.x.
- Con: two code paths for the tag: the renderer's `enum` (kept for `Enum` and for trees rendered without lowering)
  and the lowering's `const` field.
- Con: a hand-written discriminated `oneof` lowered under `Enum` (`withProfile(Neutral)`) still carries its keys, so
  a `Const` profile at render time converts it. The keys are "not decided yet" under `Enum`. That is harmless, but
  worth a line in the scaladoc.

### C. Pass the profile (or a render option) to the renderer — LOST
`AsValue.schema(x, v)(builder, profile)` hands `profile.tagStyle` to the renderer, for example as a new
`AsValue.schema(x, opts)` overload or a ThreadLocal like `definitionNames`. `mkObj` picks `enum` or `const`.

- Pro: no new tree shape; all tag logic stays in `mkObj`.
- Con: breaks the documented contract that `withProfile(p)` has shaped the schema. A pre-shaped schema would follow
  the render-time profile's tag style instead of `p`'s.
- Con: more API surface (`AsValue` is a public trait; renderers are also called directly, as `AsDraft07(...).apply`,
  and through the bridges' `asPlay`/`asCirce`/...). Every entry point needs the option, or it silently falls back.

### D. Option B, but `Neutral` (and so every library profile built from it) defaults to `Const` — LOST (owner's alternative)
The same mechanism with a different default: `Knobs.tagStyle = Const` for Neutral, and `Legacy.withTag(Enum)`
explicitly.

- Pro: idiomatic output by default, which is what the 0.9 Neutral profile is for.
- Con: an output change for every `@discriminator` and library-profile user on draft-06+. In the repo that means 6
  golden files (`DiscriminatorSpec.case0/1/3/4/5`, `char/fields/DiscOrder`), plus the tree/JSON expectations in
  `DiscriminatorSpec`, the bridges' `As*Spec`/petshop expectations that spell out a tag, and the README/paradox
  examples. It isn't a correctness fix, so it doesn't belong in a patch release.
- How to choose it instead: change one default in `Knobs` and regenerate the goldens (step 7b). Legacy's files don't
  change. It fits 0.10.0 better than 0.9.x.

### Draft-04 sub-decision: fold the draft-04 `const` fix (4b) into D3 — WON; own item — LOST
- Fold in (WON, lead 2026-11-23): `AsDraft04` renders every `const(v)` as `{"enum": [v]}` (with its
  `title`/`description`). It's needed for `TagStyle.Const` to be correct on draft-04, it touches the same keyword and
  drafts, and it's a few lines.
- Own item (LOST): D3 would emit `{"enum": [k]}` for the tag on draft-04 by itself. The lowering can't, because it
  doesn't know the draft. So the renderer would need to tell a tag `const` apart from other `const`s (a marker), or D3
  would ship knowing that `Const` is broken on draft-04 until the other item lands. Either is worse than the general
  fix.
- Legacy keeps the old draft-04 `"const"` (LOST): the lead's policy is that Legacy freezes 0.7's style, not its bugs,
  and this one rejects every instance. The three `golden-legacy` files are regenerated deliberately (step 7a).

## Decision

Option B: a `TagStyle` knob (`Enum` default, `Const` opt-in) built with `withTag`. Under `Const`, the lowering writes
each tag as a required `const` field, so the choice survives `withProfile` and covers derived, library-tagged and
hand-written discriminated `oneof`s alike. Draft-04 renders every `const` as a one-value `enum`, which fixes 4b. No
default output changes except the draft-04 renders of `EnumsAsOneOf` families. Option D (Neutral defaults to
`Const`) is the alternative for the owner, for 0.10.0.

## Implementation plan

1. **Knob** (`core/src/main/scala/json/schema/JsonSchemaProfile.scala`)
   - `sealed trait TagStyle` with `case object Enum` / `case object Const` in `object JsonSchemaProfile`, next to
     `SingletonStyle`, with scaladoc as in option B (Const: "on draft-04, which has no `const`, a one-value `enum`").
   - `def tagStyle: TagStyle = TagStyle.Enum` on the trait. Scaladoc: "How a member's tag (the discriminator field's
     value) is stated. Applies wherever a tag is written: `@discriminator`, `SumStyle.Discriminator`, hand-written
     `oneof.discriminatedBy`."
   - `final def withTag(x: TagStyle): JsonSchemaProfile = Knobs(this).copy(tagStyle = x).profile`.
   - `Knobs`: a new field `tagStyle: TagStyle = TagStyle.Enum`, put in `profile` (`override val tagStyle`) and read in
     `Knobs.apply(p)`. Neutral and Legacy are unchanged (`Enum`). The bridge profiles are built from `Neutral` with the
     builders (Play/Circe/UPickle/Json4s/Spray), so they inherit `Enum` and need no change.
   - `Lowering`: a helper used in two places:
     ```scala
     // TagStyle.Const: the tag as a field, `"<df>": {"const": key}`, so the renderer's one-value `enum` doesn't apply
     private def tagsAsConst(xs: Set[Schema[_]], df: String): Set[Schema[_]] = xs map (constTag(_, df))
     private def constTag(s: Schema[_], df: String): Schema[_] = s match {
       case o: `object`[_] if o.discriminationKey.isDefined =>
         val tag = `object`.Field(df, `const`[Any](Value.str(o.discriminationKey.get)), required = true)
         val withTag = if (o.fields.exists(_.name == df)) o.withFieldsUpdated { case f if f.name == df => tag } else o.withField(tag)
         withTag.duplicate(discriminationKey = None)
       case d @ `def`(sig, body) => `def`[Any](sig, constTag(body, df)).withExtraFrom(d)
       case other                => other   // where the renderer writes no tag today (a value class, a $ref): unchanged
     }
     ```
     (`duplicate(discriminationKey = None)` clears the key: `Schema.duplicate` assigns the argument, whose default is
     the current value; `Schema.scala:68-76`.) Use it (a) in `apply`'s `` case `oneof`(xs, df) ``: when `p.tagStyle == Const && df.isDefined`, it becomes
     `` `oneof`(tagsAsConst(xs map apply, df.get), df) ``; and (b) at the end of `familyAt`: the `` `oneof`[Any](schemas.toSet, tagField) ``
     case under `Const` with `tagField.isDefined`. Keep `discriminationField` on the `oneof`: `mkObj` appends it to
     `required` and `distinct`s, so nothing changes, and `AsTree`/user code can still read it.
     Member order: `Lowering.apply` already maps `oneof.subTypes` (a `Set`) the same way, so the order is whatever it is
     today. If B2's ordered-members change lands first, map over whatever replaces the Set.
     Mirror exactly where `mkObj` reads the key today, an object member or the body of a definition member, so `Const`
     differs from `Enum` only in `enum` → `const`. A replaced `phantom = false` field drops its own
     default/description/readOnly, as the renderer's property overwrite does today.
     Idempotent: a converted object has no key left. Extend the class comment of the "sealed families" section with one line.
2. **Draft-04 `const`** (`core/src/main/scala/com/github/andyglow/jsonschema/AsDraft04.scala`): `override def mkConst`
   → `obj(("description", x.description), ("title", x.title), ("enum", arr(x.value)))`. `AsDraftSupport.apply` keeps
   omitting `type` for a const, which is right: `enum` alone is the exact equivalent. Check that `mkPlainOneOf`'s
   `memberType` for consts still states the shared `type` on a same-typed oneOf; it does, and that is unchanged.
3. **Tests, tag** (new `api/src/test/scala/com/github/andyglow/jsonschema/TagStyleSpec.scala`, shared; full JSON
   comparisons as in `AsDraftXXSpec`):
   - the `Shape` family above (`@discriminatorKey` member, default-key member, `@definition` member whose definition body
     gets the `const`) plus a `phantom = false` family (`DiscriminatorSpec.case1`/`case3` models), under
     `Neutral.withTag(Const)`: draft-04 `{"enum": [k]}`; draft-06, 07, 2019-09, 2020-12 `{"const": k}`; `required` the same as under `Enum`;
   - `Neutral.withSum(Discriminator("_type")).withSingleton(AsTagged).withTag(Const)`: a case class member and a case
     object member (`{"properties": {"_type": {"const": ...}}, "required": ["_type"]}`);
   - a member with an implicit `Schema` of an object (the `taggedProvided` copy): the `oneOf` entry has the `const`, and
     the implicit schema's own definition, used elsewhere, doesn't;
   - a hand-written `` `oneof`(...).discriminatedBy("kind") `` with keyed objects;
   - `Enum` (and the default) leave the output byte-identical (render the same models without the knob, compare with
     `Neutral.withTag(Enum)`);
   - `lower` idempotent under `Const`; `schema.withProfile(Neutral.withTag(Const))` rendered with no profile in scope
     keeps `const`;
   - builders: `Neutral.withTag(Const).withSum(...).withSingleton(...).tagStyle == Const`; `Legacy.tagStyle == Enum`;
     `Neutral.withTag(Const) == Neutral.withTag(Const)` and `!= Neutral` (Knobs equality).
4. **Tests, draft-04** (`AsDraft04Spec`): a new case `oneof.of(const("foo").withTitle(..), const("bar"))` on `Draft04`
   → `{"enum": ["foo"], "title": ...}`. The existing "emit const" test renders 2019-09 (`Draft09`) and stays as is.
   Grep the module tests (`AsPlaySpec`, `AsCirceSpec`, ... all render `Draft04`) for a draft-04 `const` expectation
   and update it if any.
5. **Acceptance** (networknt, validates on draft-04, 07 and 2020-12): in `NeutralLegacyAcceptanceSpec` add "Neutral with
   `TagStyle.Const` follows `@discriminator`": `assertValid` with the existing `Schemas.shape` documents, and
   `assertInvalid` with a wrong tag (`{"kind":"petshop.Nope","r":1.5}`). Optionally, in one bridge petshop spec (play
   `_kind`): `PlayProfile....withTag(Const)` gives the same cross-check row as without it. Add an `EnumsAsOneOf` draft-04
   acceptance check that the oneOf of consts accepts `"V1"` (it fails today), if a model is at hand in petshop;
   otherwise the golden change in step 7 covers it.
6. **Scala 2 vs 3, JS**: no macro change; everything is in shared `core` sources. No platform code. Gates as usual
   (`clean test` on 2.12/2.13/3, `rootJS/test` on all three since core changes; validating acceptance tests are
   canceled on JS).
7. **Goldens**
   - a. Draft-04 (step 2): the `draft-04` render of `EnumSpec.case0.EnumsAsOneOf`, `EnumSpec.case8.EnumsAsOneOf`,
     `SumTypeModels.FooBarInsideCompanionWithAnnotations.EnumsAsOneOf` changes from `{"const": "V1"}` to
     `{"enum": ["V1"]}`, in `golden/` (`GOLDEN_UPDATE=1 sbt clean api/test`) and in `golden-legacy/`
     (`GOLDEN_UPDATE_LEGACY=1`, deliberately, per the lead's Legacy policy). The diff must contain only those six
     files' `draft-04` sections. (Found by loading each golden's `draft-04` render and looking for `"const"`; no other
     golden has one.)
   - b. Tag (step 1): no golden changes under the default. (Under option D instead: the `draft-07` and
     `draft-2020-12` renders of the 6 tagged goldens, `DiscriminatorSpec.case0/1/3/4/5` and `char/fields/DiscOrder`;
     `golden-legacy/` must not change.)
8. **Docs** (README.md and paradox `docs/src/main/paradox/types.md` "Discriminators", `rendering.md` profile knob
   table; the two say the same things):
   - after the `@discriminator` JSON example: "The tag is a one-value `enum` on every draft. To get
     `"kind": {"const": "cat"}` (draft-06 and later; draft-04 has no `const`), render with a profile that has
     `.withTag(TagStyle.Const)`.", with a short `implicit val profile = JsonSchemaProfile.Neutral.withTag(TagStyle.Const)` snippet;
   - the knob table: `| withTag | TagStyle.Enum, Const | the tag as a one-value enum or a const (a one-value enum on draft-04) |`;
   - `makeDocs` must pass (the snippet can be `mdoc:compile-only`).
   - No CHANGELOG edit (the lead writes it).
9. **Risks**: field order in `required` for a
   `phantom = false` field that wasn't required: its tag now sits at the field's position, not at the end. Both are
   valid; only `Const` output is affected. A user's custom `JsonSchemaProfile` subclass (not built from `Knobs`)
   gets `Enum` from the trait default.

## Compatibility

- Source/binary: additive. New `TagStyle` ADT, `tagStyle` (concrete trait method with a default; Scala 2.12+ traits
  compile to interfaces with default methods, so subclasses compiled against 0.9.0 keep linking), `withTag`. `Knobs`
  is private. No public signature changes, and the core design rule (Schema → `$ref`, Predef → inline) is untouched.
- Output: unchanged under every built-in profile on draft-06+. Draft-04 only: a `const` renders as a one-value `enum`
  (a bug fix: the old output rejected every instance of an `EnumsAsOneOf` family). It applies under Legacy as well
  (three `golden-legacy` files).
- PVP: patch (0.9.x). Option D would be a default-output change for 0.10.0.

## Review log
