# `@discriminatorKey` on case objects in a `@discriminator` family  (issue #293, item D5)
Status: draft

## Problem

In a family annotated `@discriminator`, case classes are tagged objects, but a case object stays a plain string.
Its `@discriminatorKey` is silently ignored. This is the issue's model:

```scala
import json.schema._

@discriminator("type", phantom = true)
sealed trait Command
object Command {
  @discriminatorKey("m") case class Move(meters: Int)    extends Command
  @discriminatorKey("r") case class Rotate(degrees: Int) extends Command
  @discriminatorKey("j") case object Jump                extends Command   // the key is ignored
}
```

Derived tree (the same on 2.12.21, 2.13.18 and 3.3.9; the key is recorded, so the macros are not the problem):

```
family(Move:Product(object(meters: integer)) discriminatorKey=`m`,
       Rotate:Product(object(degrees: integer)) discriminatorKey=`r`,
       Jump:Singleton(string = "Jump") discriminatorKey=`j`
       | Discriminator(type,true))
```

Rendered with `JsonSchemaProfile.Neutral` (the default) and with `Legacy`, draft-07. The output is identical on
2.12, 2.13 and 3.3.9 (the lowering is shared code in `core`, with nothing platform-specific, so JS is the same):

```json
{ "oneOf": [
  { "type": "object", "additionalProperties": false,
    "properties": { "meters": { "type": "integer" }, "type": { "enum": ["m"] } }, "required": ["meters", "type"] },
  { "type": "object", "additionalProperties": false,
    "properties": { "degrees": { "type": "integer" }, "type": { "enum": ["r"] } }, "required": ["degrees", "type"] },
  { "type": "string", "enum": ["Jump"] }
] }
```

networknt (draft-07): `{"type":"j"}` is **invalid**, `"Jump"` is valid.

`schema.withProfile(JsonSchemaProfile.Neutral.withSingleton(SingletonStyle.AsTagged))` already gives what the
issue asks for. The third member becomes
`{"additionalProperties": false, "properties": {"type": {"enum": ["j"]}}, "required": ["type"]}`, and networknt
accepts `{"type":"j"}` and rejects `"Jump"`. The code: `JsonSchemaProfile.Lowering.familyAt`
(`core/src/main/scala/json/schema/JsonSchemaProfile.scala`) decides per *profile*
(`val asString = p.singletonStyle == SingletonStyle.AsString`), never per family. Under `AsString` a singleton
becomes an `enum` value (its `ToValue` value, else its name), and `@discriminatorKey` is never read
(`singletonValue`, `singletonSchema`).

Related shapes, from the same repro (`notes/scratch/c-d5/D5ReproSpec.scala`; full output in `out-2.13.txt`):

| model | Neutral / Legacy today | `withSingleton(AsTagged)` |
|---|---|---|
| issue model (above) | `"Jump"` | `{"type": "j"}` |
| `@discriminator` family, case object **without** a key (`Stop`) | `"Stop"` | `{"type": "<pkg>.Command.Stop"}` (the signature, the same default as case classes) |
| `@discriminator`, `phantom = false`, `@discriminatorKey("j") case object Jump` | `"Jump"` | `{"kind": "j"}` (the field is added: a case object has no field to hold it) |
| `@discriminator` family of case objects **only** (`North`, `South` with keys `n`, `s`) | `enum ["North", "South"]`: `@discriminator` is ignored entirely (characterized as 0.7 behavior: `FieldsAnnotationsCharSpec` "A4") | `oneOf [{"type": "n"}, {"type": "s"}]` |
| no `@discriminator`, `@discriminatorKey("j") case object Jump` | `"Jump"` | `{}`: untagged, the key is unused (case-class members don't use it either) |

## Why it is a problem

- The user states the wire shape with an annotation, and the default output contradicts it, silently. The schema
  rejects every document a codec writes as `{"type": "j"}`. That is what a hand-written codec that follows the
  annotations writes, and the shape play-json (`{"_type": "petshop.Fish"}`) and circe configured with a
  discriminator give a case object (README "Library profiles"). upickle is the exception: it writes `"Fish"`. This is #293 (4 comments, reported on 0.7.9; still true on 0.9.0).
- The README says `@discriminatorKey` is "the member's discriminator value (default: its full name)" and doesn't
  say the annotation does nothing on a case object.
- The workaround (`withSingleton(AsTagged)`) is a profile setting, so it applies to every family the profile
  renders. A user with one discriminated family and one plain enum of case objects can't get both right with one
  profile: `AsTagged` turns the plain enum into objects too.

## Options

### A. Document only — LOST
README/paradox: "under the default profile a case object is a string; to tag it, render with
`withSingleton(SingletonStyle.AsTagged)`". Close #293 with a pointer.
- Pro: no output change, no API change.
- Con: the annotation keeps being silently ignored by default, and the workaround is global (last bullet above).
  It documents the trap instead of removing it.

### B. Fail loudly — LOST
In `familyAt`, under `AsString`, throw `IllegalArgumentException` when a singleton of an honored-`@discriminator`
family has a `@discriminatorKey`: "Jump has @discriminatorKey("j") but this profile writes case objects as
strings: render with withSingleton(SingletonStyle.AsTagged)". (A compile-time warning is impossible: the macro
doesn't know the profile, which is chosen at render time.)
- Pro: nothing changes silently; the error names the fix.
- Con: existing schemas that render today (with a dead annotation) start throwing at render time, which is worse
  than a changed schema. It only catches the "has a key" case: a key-less case object in a `@discriminator`
  family is still a string. Under Legacy it would break the 0.7 byte-exact output (it would throw).

### C. A new `SingletonStyle.TaggedIfDiscriminated`, the default of Neutral — WON
A third singleton style: **a singleton is tagged (`AsTagged`) in a family whose `@discriminator` the profile
honors, and a string (`AsString`) everywhere else.** `JsonSchemaProfile.Neutral` uses it (and so does
`SprayProfile.default`, which is `Neutral.withName("spray")`). `Legacy` stays `AsString`, so it keeps 0.7's
output byte for byte. The library profiles (play, circe, upickle, json4s) set their own style and don't honor
`@discriminator`, so they are unaffected.

The mechanism is one line in `familyAt`: `asString` becomes a per-family decision.

```scala
val asString = p.singletonStyle match {
  case SingletonStyle.AsString              => true
  case SingletonStyle.AsTagged              => false
  case SingletonStyle.TaggedIfDiscriminated => annotated.isEmpty   // `annotated`: the family's @discriminator, if honored
}
```

The rest already exists. A tagged singleton goes through `shaped(m, object())`. Its tag is
`m.discriminatorKey getOrElse m.sig` (`label`), the same rule as the case classes. `@title`/`@description` and
`@definition` apply as for a case class (`withTexts`, `withDefinition`), and the renderer adds the field to
`properties`/`required` (`mkObj`), whether `phantom` is true or false.

Output for the issue's model under Neutral (draft-07), the third member:

```json
{ "type": "object", "additionalProperties": false, "properties": { "type": { "enum": ["j"] } }, "required": ["type"] }
```

(At the **root**, the members of an all-object `oneOf` render without `"type": "object"`: that is the existing
root-`type` bug B15, which already affects every all-case-class family. As a field, `"type": "object"` is stated
on the `oneOf`. D5 doesn't change this. See "Interactions".)

- Pro: the annotation means what it says by default. The choice is per family, so a discriminated family and a
  plain case-object enum both come out right under one profile. Users who want the old shape write
  `withSingleton(SingletonStyle.AsString)`, or use `Legacy`. No macro change, no tree change, no annotation API
  change. Measured with a prototype (below): one golden file and three tests change.
- Con: an output change of the default profile (PVP: see Compatibility). A third value in a sealed trait. A user
  whose own codec writes the case object as a string *inside* a discriminated family (upickle's style, for
  example) now has to say so with `withSingleton(AsString)`.

### D. Change what `AsString` means under an honored `@discriminator` (no new value) — LOST
The same rule as C, but hard-wired: any profile that honors `@discriminator` tags the singletons of such a family,
whatever `singletonStyle` says.
- Pro: no new API.
- Con: it changes Legacy (0.7's string is a *style*: the lead policy keeps style under Legacy). And
  `withSingleton(AsString)` no longer gives a string everywhere, so the user in C's last "con" has no way back.

### E. Per member: tag only the case objects that carry `@discriminatorKey` — LOST
`asString` stays as it is, and a singleton with a `@discriminatorKey` in an honored `@discriminator` family is
tagged.
- Pro: the smallest output change: only families with a currently-dead annotation change.
- Con: one family gets two shapes for its case objects (`{"type": "j"}` and `"Stop"`). No codec writes that, so
  the schema matches neither the tagged nor the string encoding of `Stop`. Adding a key to one object would
  change how its siblings validate, which is hard to explain.

### F. A parameter on the annotation: `@discriminator(field, phantom, singletons = ...)` — LOST
- Pro: per family, explicit.
- Con: grows the annotation API. Both macros (Scala 2 `UTypeAnnotations.scala`/`USumTypes.scala`, Scala 3
  `derivation/Annotations.scala`) and `family.Discriminator` need a new field, and the tree dumps and golden schemas change.
  It duplicates the profile knob that already exists. The default for the parameter has to be chosen anyway, which
  is C's question again.

## Decision

C: add `SingletonStyle.TaggedIfDiscriminated` and make it Neutral's (and so spray's) singleton style. Within a
family whose `@discriminator` applies, a case object (or a parameterless Scala 3 `enum` case) is an object tagged
with its `@discriminatorKey`, else its signature, exactly like the case classes. Elsewhere it is a string, as
today. Legacy keeps `AsString` and its 0.7 output. `withSingleton(AsString)` is the opt-out.

Two sub-choices in C, each one condition in the code (owner: see Open questions):
1. A family of case objects **only**, annotated `@discriminator`, is tagged too (`oneOf` of tagged objects
   instead of a string `enum`). C does this: the user annotated the family, and "every member is a tagged object"
   is the rule that is easy to state. The alternative is to add `&& !singletonsOnly` to the rule, which keeps
   characterization A4 ("ignored") under Neutral.
2. `@discriminatorKey` on a case object in a family **without** `@discriminator` stays unused (as for case
   classes). Out of scope. Using it as the string value would need the tree to say whether `Member.value` came
   from a `ToValue` (today it is always `Some`: the `ToValue` value or the name), so it is a separate change.

## Implementation plan

Apply after B3 (#470) if that merges first: B3 adds a `Knobs` field (`overlappingMembers`) next to
`singletonStyle`, so the `Knobs`/builder lines will conflict textually (no semantic overlap). A prototype of steps
1–2 (`notes/scratch/c-d5/prototype.diff`) was run with `clean test` (JVM) on 2.12.21, 2.13.18 and 3.3.9. On each
version, exactly these failed: `FamilyLoweringSpec` "keep Neutral's and Legacy's knobs", `FamilyTreeSpec` "Neutral
and Legacy shape a derived family the same way", and `FieldsAnnotationsCharSpec` "A4" (golden). Every bridge
module passed, spray and the petshop acceptance suites included.

1. **`core/src/main/scala/json/schema/JsonSchemaProfile.scala`**
   - `object SingletonStyle`: add
     ```scala
     /** [[AsTagged]] in a family whose `@discriminator` applies (the profile [[honorsSchemaAnnotations]]), [[AsString]] in every other family. The default of [[JsonSchemaProfile.Neutral]]:
       * `@discriminatorKey` tags a case object as it tags a case class. A family of case objects only, annotated `@discriminator`, is a `oneOf` of tagged objects, not an `enum`.
       */
     case object TaggedIfDiscriminated extends SingletonStyle
     ```
     and update the `AsString` scaladoc ("0.7, upickle, Legacy").
   - `val Neutral = Knobs("neutral", OptionStyle.Nullable, singletonStyle = SingletonStyle.TaggedIfDiscriminated).profile`.
     Update its scaladoc ("the 0.7 shapes, with every `Option[X]` as "X or null" and the case objects of a
     `@discriminator` family tagged"), and the `Legacy` one if needed.
   - Keep the trait default `def singletonStyle = SingletonStyle.AsString` and the `Knobs` default `AsString`:
     `Legacy` is built from the `Knobs` defaults, and a user's profile that implements the trait directly keeps its
     current output. State this in the trait method's scaladoc ("[[JsonSchemaProfile.Neutral]] overrides it with
     [[SingletonStyle.TaggedIfDiscriminated]]").
   - `Lowering.familyAt`: replace `val asString = p.singletonStyle == SingletonStyle.AsString` with the `match`
     shown in option C (exhaustive, no wildcard). `singletonsOnly` already depends on `asString`, so a
     singletons-only discriminated family goes to the plain tagged `oneOf` with `tagField`.
   - Update the comment block above `familyAt` ("a family of singletons only is that `enum` ...; `@discriminator`
     doesn't apply to it") to say this holds under `AsString` (Legacy), while `TaggedIfDiscriminated` tags.
   - `honorsSchemaAnnotations = false` + `TaggedIfDiscriminated`: `annotated` is `None`, so it behaves as
     `AsString`. Say so in the scaladoc and test it.
   - `memberName`'s scaladoc mentions `AsString`. Check that it still reads right.
2. **`core/src/test/scala/json/schema/FamilyLoweringSpec.scala`**
   - "keep Neutral's and Legacy's knobs": Neutral's `singletonStyle` is `TaggedIfDiscriminated`, Legacy's is
     `AsString` (split the tuple per profile).
   - The builder test: add `withSingleton(TaggedIfDiscriminated)` round-trip.
   - New lowering cases on hand-built `family` trees, each asserting the lowered tree:
     a) issue shape (two products + keyed singleton, `Discriminator("type", true)`): Neutral tags the singleton
        `object()` with key `j`; Legacy and `Neutral.withSingleton(AsString)` give today's `enum`;
     b) key-less singleton: tagged with its `sig`;
     c) singletons only + `@discriminator`: `oneof(..., Some("type"))` of tagged empty objects under Neutral; the
        string `enum` under Legacy;
     d) no `@discriminator`: Neutral equals `AsString` (string enum, merged as today);
     e) `withHonorsSchemaAnnotations(false)` + `TaggedIfDiscriminated` + `@discriminator` on the tree: strings
        (as `AsString`);
     f) `enumsAsOneOf = true` + `@discriminator`: tagged objects (the flag concerns the string form only);
        without `@discriminator`: `const`s as today;
     g) a tagged singleton with `title`/`description`/`definition`: they land on the object / the `def`, as for a
        case class.
3. **`api/src/test/scala/com/github/andyglow/jsonschema/DiscriminatorSpec.scala`**: a `case6` (the issue's
   model) and a `case7` (`phantom = false` with a keyed case object). Assert the tree (`as07`) and the Raw/draft
   JSON with `containJson`, as cases 0–5 do: the object member `{"type": {"enum": ["j"]}}` required `["type"]`.
   Also add them to `api/src/test/scala/.../golden/GoldenSharedModelsSpec.scala`
   (`check("shared/DiscriminatorSpec.case6", ...)`, as for cases 4 and 5). This creates new
   `golden/shared/DiscriminatorSpec.case6.json` / `case7.json` (tagged), and their `golden-legacy/` twins hold
   the string form, which also pins Legacy.
4. **`api/src/test/scala/com/github/andyglow/jsonschema/FamilyTreeSpec.scala`** "Neutral and Legacy shape a
   derived family the same way": drop `Json.schema[Tagged]` from the list, and add a test that Neutral and Legacy
   differ for `Tagged` in exactly the `T2` member (Neutral: `object` with key
   `com.github.andyglow.jsonschema.FamilyTreeSpec.T2`; Legacy: `enum("T2")`).
5. **`api/src/test/scala/com/github/andyglow/jsonschema/characterization/FieldsAnnotationsCharSpec.scala`** A4:
   rename to "A4: @discriminator on an all-case-object family: ignored under Legacy (0.7), tagged under Neutral".
   Regenerate `golden/char/fields/DiscriminatedEnum.json` (`GOLDEN_UPDATE=1 sbt clean api/test`, CLEAN build).
   Its three drafts go from `{"type": "string", "enum": ["X", "Y"]}` to a `oneOf` of two tagged objects
   (`{"type": {"enum": ["<sig>.X"]}}`, required `["type"]`). The prototype's exact diff is in
   `notes/scratch/c-d5/proto-golden.diff`. `golden-legacy/char/fields/DiscriminatedEnum.json` is **unchanged**,
   which is the Legacy check. No other golden file changes (measured).
6. **Acceptance (networknt)**, `api/src/test/scala/petshop/`: add a `@discriminator("kind")` family with a keyed
   case object to `Model.scala` (or a local model in `NeutralLegacyAcceptanceSpec`). Under Neutral `{"kind": "j"}`
   is valid and `"Jump"` invalid, and under Legacy the reverse, on the suite's drafts. Validation is JVM only: on
   JS these tests show as canceled, which is expected.
7. **Scala 3** (`api/src/test/scala-3/.../sums/Scala3EnumsSpec.scala`):
   `@discriminator enum Cmd { case Move(m: Int); @discriminatorKey("j") case Jump }`. Under Neutral, `Jump` is a
   tagged object with key `j`. (The family node is the same as for a sealed trait, so no Scala 3-specific code is
   expected. The test guards that the enum case's annotation reaches `Member.discriminatorKey`, which
   `FamilyTreeSpec` checks only for Scala 2-style case objects.)
8. **Docs** (README and paradox say the same things):
   - README "Discriminators" and `docs/src/main/paradox/types.md` (same section): add a case object to the `Pet`
     example (`@discriminatorKey("fish") case object Fish extends Pet`) with its rendered member, and one sentence:
     "A case object in a `@discriminator` family is tagged like the case classes (an object with only the
     discriminator field). A family of case objects only, annotated `@discriminator`, is a `oneOf` of such
     objects. Under `JsonSchemaProfile.Legacy`, or with `withSingleton(SingletonStyle.AsString)`, a case object is
     a string, as in 0.7." Regenerate the JSON with the real output (the README's JSON is real output).
   - README "Library profiles" knob table and `docs/src/main/paradox/rendering.md` table: `withSingleton` values
     `AsString`, `AsTagged`, `TaggedIfDiscriminated` (Neutral's default).
   - README annotation table row `@discriminatorKey`: "sealed member (case class or case object)".
   - `SprayProfile` scaladoc: it inherits Neutral's singleton style (no code change).
   - `sbt makeDocs` (the `rendering.md` mdoc snippet uses `withSingleton(AsTagged)` explicitly, so it is unaffected).
9. **Gates** (for the implementer): `clean test` on 2.12.21 / 2.13.18 / 3.3.9; `rootJS/test` on all three
   (core changed); `scalafmtCheckAll` + `rootJS/scalafmtCheckAll` (touch sources first); `makeDocs`.

Risks:
- A user whose hand-written codec writes case objects as strings inside a `@discriminator` family gets a schema
  that rejects their output after upgrading. Mitigation: the CHANGELOG "Breaking" entry names
  `withSingleton(SingletonStyle.AsString)` (or `Legacy`). The lead writes the CHANGELOG at release.
- `SingletonStyle` gets a third case: a user's exhaustive `match` on it gets a non-exhaustive warning (an error
  under `-Werror`). Rare: users build profiles, they don't usually match on knobs.

### Interactions with other items
- **B15** (root-family-type): an all-object `oneOf` at the root has no `"type"` (members drop theirs, and the
  root doesn't state it). C makes more families all-object (the issue's model, A4's). Then a root schema with
  only **one** member would accept any non-object, for example `"j"`. With two or more members, a non-object
  matches all of them and `oneOf` rejects it, by accident. B15's fix covers this, and D5 needs nothing extra.
  Mention it in the B15 doc/PR.
- **D3** (#294, discriminator as `const`): orthogonal. Whatever D3 renders for the tag applies to tagged
  singletons too.
- **B3** (#470): textual conflict in `Knobs` only (see the start of the plan).

## Compatibility

- **Output:** changes under `JsonSchemaProfile.Neutral` (the default) and `SprayProfile.default`, only for
  families annotated `@discriminator` that have at least one case object / parameterless enum case: those members
  go from a string `enum` to a tagged object, and a case-object-only `@discriminator` family goes from a string
  `enum` to a `oneOf` of tagged objects. Unchanged: `Legacy` (byte-exact 0.7: its golden-legacy files don't
  change), the play/circe/upickle/json4s profiles, families without `@discriminator`, families without case
  objects, and any profile built with an explicit `withSingleton(...)`.
- **Source:** additive (a new `SingletonStyle` case). A user's exhaustive match on `SingletonStyle` warns.
- **Binary:** compatible (a new case object; `Neutral` is still a `val JsonSchemaProfile`; no signature changes).
- **PVP:** the default profile's output changes, so under the project's rule (output changes bump the second
  number, as 0.7 → 0.8 did) this is **breaking: 0.10.0**. If the owner treats #293 as a bug fix, it can ship in
  0.9.x with a CHANGELOG note. Owner's call.
- **Core design rule:** untouched (no change to the Schema → `$ref` / Predef → inline / derive lookup).

### Open questions for the owner
1. C (new default) vs A (document only). The doc recommends C.
2. A `@discriminator` family of case objects only: tagged (C as written) or keep the string `enum` (add
   `&& !singletonsOnly`).
3. The name: `TaggedIfDiscriminated` (descriptive) vs a shorter `Auto`.
4. Ship in 0.9.x as a fix, or hold for 0.10.0.

## Review log
