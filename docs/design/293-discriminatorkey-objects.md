# `@discriminatorKey` on case objects in a `@discriminator` family  (issue #293, item D5)
Status: draft (round 2)

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
| `@discriminator @typeHint[Int]` family with an implicit `ToValue` (`V1` → `1`, `V2` → `2`) | `{"type": "integer", "enum": [1, 2]}` | `{"kind": "<sig>.V1"}`, `{"kind": "<sig>.V2"}`: the `ToValue` values are dropped |
| a profile's own `withSum(Discriminator("type"))`, family without `@discriminator` | case classes tagged, case objects `"Jump"` | all tagged |

## Why it is a problem

- The user states the wire shape with an annotation, and the default output contradicts it, silently. The schema
  rejects every document a codec writes as `{"type": "j"}`. That is what a hand-written codec that follows the
  annotations writes, and the shape play-json (`{"_type": "petshop.Fish"}`) and circe configured with a
  discriminator give a case object (README "Library profiles"). upickle is the exception: it writes `"Fish"`.
  This is #293 (4 comments, reported on 0.7.9; still true on 0.9.0).
- The README says `@discriminatorKey` is "the member's discriminator value (default: its full name)" and doesn't
  say the annotation does nothing on a case object.
- The workaround (`withSingleton(AsTagged)`) is a profile setting, so it applies to every family the profile
  renders. A user with one discriminated family and one plain enum of case objects can't get both right with one
  profile: `AsTagged` turns the plain enum into objects too, and it also drops a family's `ToValue` values (last
  rows of the table).
- The same mixed shape (tagged case classes, string case objects) comes from a profile whose own `sumStyle` tags:
  `Neutral.withSum(SumStyle.Discriminator("type"))` without `withSingleton`. No codec writes that mix unless it
  writes case objects as strings on purpose (upickle).

## Options

The question has two parts: **which families** tag their case objects by default (options A–F), and what happens
to a family's **`ToValue` values** in such a family (options T1–T3).

### A. Document only — LOST
README/paradox: "under the default profile a case object is a string; to tag it, render with
`withSingleton(SingletonStyle.AsTagged)`". Close #293 with a pointer.
- Pro: no output change, no API change.
- Con: the annotation keeps being silently ignored by default, and the workaround is global. It documents the trap
  instead of removing it.

### B. Fail loudly — LOST
In `familyAt`, under `AsString`, throw `IllegalArgumentException` when a singleton of an honored-`@discriminator`
family has a `@discriminatorKey`: "Jump has @discriminatorKey("j") but this profile writes case objects as
strings: render with withSingleton(SingletonStyle.AsTagged)". A compile-time warning is impossible: the macro
doesn't know the profile, which is chosen at render time. A runtime warning is impossible too: lowering has no
channel for warnings.
- Pro: nothing changes silently; the error names the fix.
- Con: existing schemas that render today (with a dead annotation) start throwing at render time, which is worse
  than a changed schema. It only catches the "has a key" case: a key-less case object in a `@discriminator`
  family is still a string. Under Legacy it would break the 0.7 byte-exact output (it would throw).

### C. A new `SingletonStyle.FollowFamily`, the default of Neutral — WON
A third singleton style: **a singleton is written the way its family writes the other members.** It is tagged
(`AsTagged`) when the family is tagged, that is when the family's *resolved* sum style is
`Discriminator(field)` or `WrapperObject`, whether that comes from an honored `@discriminator`, from the
profile's `sumStyle`, or from `PerFamily`. It is a string (`AsString`) when the family is `Untagged`. The
`ToValue` exception (T2 below): when the profile honors annotations and the family's singletons have values from
a `ToValue`, they stay values.

`JsonSchemaProfile.Neutral` uses it, and so does `SprayProfile.default`, which is `Neutral.withName("spray")`.
`Legacy` stays `AsString`, so it keeps 0.7's output byte for byte. The library profiles (play, circe, upickle,
json4s) each set `withSingleton(...)` explicitly (checked in their `*Profile.scala`), so they are unaffected.

The mechanism, in `familyAt`, after `style` is resolved:

```scala
// the family's singletons are written as values (a ToValue of the family) when one has a value other than its name, or a value schema other than a plain string
def valueDriven = honors && f.members.exists { m =>
  m.kind == `family`.Kind.Singleton && (m.schema != `string`[String](None) || !m.value.contains(Value.str(m.name)))
}
val asString = p.singletonStyle match {
  case SingletonStyle.AsString     => true
  case SingletonStyle.AsTagged     => false
  case SingletonStyle.FollowFamily => style == SumStyle.Untagged || valueDriven
}
```

The rest already exists. A tagged singleton goes through `shaped(m, object())`. Its tag is `label(m)`: under an
honored `@discriminator` that is `@discriminatorKey`, else the signature, the same rule as the case classes.
`@title`/`@description` and `@definition` apply as for a case class (`withTexts`, `withDefinition`), and the
renderer adds the field to `properties`/`required` (`mkObj`), whether `phantom` is true or false. Under
`WrapperObject`, a singleton becomes `{"<label>": {}}` (what circe-generic writes).

Output for the issue's model under Neutral (draft-07, the real render, as the prototype prints it), the third
member:

```json
{ "additionalProperties": false, "properties": { "type": { "enum": ["j"] } }, "required": ["type"] }
```

All members are objects now, so the renderer hoists `"type": "object"` off them (`mkPlainOneOf`). As a field it
is stated on the `oneOf`. At the root, nothing states it: that is the existing bug B15, which already affects every
all-case-class family (see "Interactions").

- Pro: the annotation means what it says by default, and so does a profile's own tagging `sumStyle`. The choice is
  per family, so a discriminated family and a plain case-object enum both come out right under one profile. No
  macro change, no tree change, no annotation API change. The README's hand-written-codec example
  (`withSum(Discriminator("type")).withMemberName(SimpleName).withSingleton(AsTagged)`) no longer needs its
  `withSingleton`, though it stays valid. Opt-out: `withSingleton(SingletonStyle.AsString)`, or `Legacy`.
- Con: an output change of the default profile (PVP: see Compatibility). A third value in a sealed trait. A user
  whose own codec writes case objects as strings in a tagged family (upickle's style) now has to say so with
  `withSingleton(AsString)`. User profiles built from `Neutral` with `withSum(Discriminator/WrapperObject)` and
  no `withSingleton` change too (see Compatibility).

### C1. The same, keyed on the annotation only (`TaggedIfAnnotated`) — LOST
Tag a family's singletons only when an honored `@discriminator` is present
(`case TaggedIfAnnotated => annotated.isEmpty || valueDriven`). A profile's own `sumStyle` doesn't count.
- Pro: a smaller output change. Only `@discriminator` families change. A user profile
  `Neutral.withSum(Discriminator(...))` keeps its mixed shape.
- Con: that mixed shape (tagged case classes next to string case objects) is the shape #293 complains about, and
  E is rejected for producing it. Here it would be reached through the knob instead of the annotation. The
  `familyAt` lowering already resolves `@discriminator`, `sumStyle` and `PerFamily` into one `style`, and keying
  on that is the simpler rule to state: "case objects follow their family". The prototype's first round used this
  rule, and the review (D5-R2) showed the gap.

### D. Change what `AsString` means in a tagged family (no new value) — LOST
The same rule as C, hard-wired: any profile tags the singletons of a tagged family, whatever `singletonStyle`
says.
- Pro: no new API.
- Con: it changes Legacy (0.7's string is a *style*: the lead policy keeps style under Legacy), and upickle's
  profile (`Discriminator("$type")` + `AsString`, the correct description of upickle) breaks. And
  `withSingleton(AsString)` no longer gives a string everywhere, so there is no way back.

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
  `derivation/Annotations.scala`) and `family.Discriminator` need a new field, and the tree dumps and golden schemas
  change. It duplicates the profile knob that already exists. The default for the parameter has to be chosen anyway,
  which is C's question again.

### The `ToValue` values of a tagged family (review D5-R1)
A family can carry an implicit `ToValue` (from the user's codec through a bridge, or hand-written). With a
`@typeHint`, or values that are not strings, it shapes each singleton's value: `Member.value` and `Member.schema`
(`string` with the name, a hinted type, or a `const`). Under `AsTagged` today, and under C without an exception,
those values are dropped. The `@discriminator @typeHint[Int]` family above goes from `enum [1, 2]` to two tagged
objects, and a codec that writes `1`, as the `ToValue` says, is rejected.

#### T1. Tagging wins; a `ToValue` no longer shapes a tagged family's singletons — LOST
- Pro: one rule, no detection.
- Con: it silently drops the user's explicit `ToValue`, the same kind of silent override this item removes. A
  `ToValue` taken from the user's real codec (bridges derive it from the codec) is the most accurate fact the tree
  has about how the case objects are written.

#### T2. Detect a `ToValue` from the member facts the tree has; such a family keeps its values — WON
A family is *value-driven* when the profile honors annotations and one of its singletons has a value schema other
than a plain `string`, or a value other than its own name. `valueDriven` in the code above. Such a family keeps
the `AsString` path: the merged `enum` of values, or a `const` each. The check is per family, because a `ToValue`
is per family (`ToValue[Root]`), so its singletons are never split between two shapes.

Measured with the prototype (2.13.18): `@discriminator @typeHint[Int]` + `ToValue` (1, 2) stays
`oneOf[{P, kind: "..P"}, {"type": "integer", "enum": [1, 2]}]` under Neutral, and `1` validates. A string
`ToValue` (`Up` → `"up"`) stays `{"type": "string", "enum": ["up", "down"]}`.
- Pro: no tree or macro change; the user's `ToValue` keeps winning, as it does today.
- Con: a heuristic. A `ToValue` that writes exactly each case object's name as a string can't be told apart from
  no `ToValue`, so such a family is tagged. The user's way out is `withSingleton(AsString)`. The case is rare:
  such a `ToValue` only restates the default. Under profiles that don't honor annotations (the library profiles),
  values are already ignored (`singletonValue` uses `label`), and `valueDriven` is false there.

#### T3. Record the fact in the tree — LOST
Add `Member.fromToValue: Boolean` (or a `family`-level flag), set by both macros (Scala 2 `UEnums`/`USumTypes`,
Scala 3 `derivation/Enums.scala`/`Sums.scala`).
- Pro: exact.
- Con: `Member` and `family` are public case classes, so a new field changes `apply`/`copy`/`unapply`. That is a
  binary break, and a source break for users who pattern-match them. It touches both macros, the tree dumps and
  every golden file's `schema` section. That cost outweighs the one case T2 gets wrong.

## Decision

C with T2. Add `SingletonStyle.FollowFamily` and make it Neutral's (and so spray's) singleton style. A case object
(or a parameterless Scala 3 `enum` case) is written the way its family writes the other members:
- In a tagged family (an honored `@discriminator`, or the profile's `Discriminator`/`WrapperObject`), it is an
  object tagged with its `@discriminatorKey`, else its signature (or memberName, per profile), like the case
  classes.
- In an untagged family it is a string, as today.
- A family whose `ToValue` shapes its singletons keeps those values.

Legacy keeps `AsString` and its 0.7 output. `withSingleton(AsString)` is the opt-out.

Two sub-choices, each one condition in the code (owner: see Open questions):
1. A family of case objects **only** in a tagged family (`@discriminator sealed trait Dir; case object North ...`,
   and on Scala 3 `@discriminator enum Dir { case N, S }`, probably the most common way to hit this there): C tags
   it (a `oneOf` of tagged objects instead of a string `enum`). The user annotated the family, and "case objects
   follow their family" has no exception to explain. The alternative is to add `|| singletonsOnly` to the
   `FollowFamily` case, which keeps characterization A4 ("ignored") under Neutral.
2. `@discriminatorKey` on a case object in an **untagged** family stays unused (as for case classes). Out of
   scope. Using it as the string value would collide with T2 (is `"j"` a key or a `ToValue` value?), so it is a
   separate change.

## Implementation plan

Apply after B3 (#470) if that merges first: B3 adds a `Knobs` field (`overlappingMembers`) next to
`singletonStyle`, so the `Knobs`/builder lines will conflict textually (no semantic overlap). Prototypes of step 1
(round 1: annotation-only, `notes/scratch/c-d5/prototype.diff`; round 2: C + T2,
`notes/scratch/c-d5/prototype2.diff`) were run with `clean test` (JVM) on 2.12.21, 2.13.18 and 3.3.9. With the
C + T2 prototype, exactly these failed on each version. Every other test passed, including all bridge modules,
spray and the petshop acceptance suites.
- core `FamilyLoweringSpec`: "keep Neutral's and Legacy's knobs"; and, under "the upickle profile", "write
  singletons as strings of their names (upickle)" and "use the member name, not the ToValue value, for string
  singletons". The spec's local `upickle` fixture is `Neutral.withSum(Discriminator("$type"))` without
  `withSingleton`, which is the user-profile case in Compatibility. The real `UPickleProfile` sets
  `AsString` and passes.
- api `FamilyTreeSpec` "Neutral and Legacy shape a derived family the same way".
- api `FieldsAnnotationsCharSpec` "A4" (golden).

1. **`core/src/main/scala/json/schema/JsonSchemaProfile.scala`**
   - `object SingletonStyle`: add
     ```scala
     /** Written the way the family writes its other members: [[AsTagged]] when the family is tagged ([[SumStyle.Discriminator]] or [[SumStyle.WrapperObject]], from an honored `@discriminator`,
       * [[sumStyle]] or [[SumStyle.PerFamily]]), [[AsString]] when it is [[SumStyle.Untagged]]. A family whose singletons have values from a `ToValue` (a value other than the name, or a value
       * schema other than `string`) keeps those values when the profile [[honorsSchemaAnnotations]]. The default of [[JsonSchemaProfile.Neutral]].
       */
     case object FollowFamily extends SingletonStyle
     ```
     Update the `AsString` scaladoc ("0.7, upickle, [[JsonSchemaProfile.Legacy]]") and the `AsTagged` one ("for
     every family, tagged or not").
   - `val Neutral = Knobs("neutral", OptionStyle.Nullable, singletonStyle = SingletonStyle.FollowFamily).profile`.
     Update its scaladoc: "the 0.7 shapes, with every `Option[X]` as "X or null", and case objects written like
     the other members of their family".
   - Keep the trait default `def singletonStyle = SingletonStyle.AsString` and the `Knobs` default `AsString`.
     `Legacy` is built from the `Knobs` defaults, and a user's profile that implements the trait directly keeps its
     current output. Say so in the trait method's scaladoc ("[[JsonSchemaProfile.Neutral]] sets
     [[SingletonStyle.FollowFamily]]").
   - `honorsSchemaAnnotations` scaladoc, first bullet: add "and its case objects are tagged the same way under
     [[SingletonStyle.FollowFamily]]".
   - `memberName` scaladoc: "the name a member is tagged with ..., or written as ([[SingletonStyle.AsString]] when
     [[honorsSchemaAnnotations]] is false)". Still right; check it after the edit.
   - `Lowering.familyAt`: replace `val asString = p.singletonStyle == SingletonStyle.AsString` with the `match`
     and `valueDriven` from option C (exhaustive, no wildcard), placed after `style` is resolved.
     `singletonsOnly` already depends on `asString`, so a singletons-only tagged family goes to the plain tagged
     `oneOf` with `tagField`. Under `WrapperObject`, `tagField` is `None` and the members are wrapper objects.
   - Update the comment block above `familyAt`: the "singletons ... merge into one `enum`" and "a family of
     singletons only is that `enum` ...; `@discriminator` doesn't apply to it" lines hold under `AsString` (Legacy)
     and for untagged or value-driven families under `FollowFamily`.
2. **`core/src/test/scala/json/schema/FamilyLoweringSpec.scala`**
   - "keep Neutral's and Legacy's knobs": Neutral's `singletonStyle` is `FollowFamily`, Legacy's is `AsString`
     (split the tuple per profile).
   - The local `upickle` fixture: add `.withSingleton(SingletonStyle.AsString)`, as the real `UPickleProfile`
     does, so its two tests keep testing upickle.
   - The builder test: add a `withSingleton(FollowFamily)` round-trip.
   - New lowering cases on hand-built `family` trees, each asserting the lowered tree:
     a) issue shape (two products + keyed singleton, `Discriminator("type", true)`): Neutral tags the singleton
        `object()` with key `j`; Legacy and `Neutral.withSingleton(AsString)` give today's `enum`;
     b) key-less singleton: tagged with its `sig`;
     c) singletons only + `@discriminator`: `oneof(..., Some("type"))` of tagged empty objects under Neutral; the
        string `enum` under Legacy;
     d) no `@discriminator`, Neutral (Untagged): equals `AsString` (string enum, merged as today);
     e) no `@discriminator`, `Neutral.withSum(Discriminator("type"))`: singletons tagged with `memberName`/key;
        `Neutral.withSum(WrapperObject)`: `{"<label>": {}}`; `PerFamily` choosing `Discriminator`: tagged;
     f) value-driven (T2): a singleton with `value = Some(num(1))`, `schema = integer` in a `@discriminator`
        family stays `enum[integer](1, ...)`; a string value other than the name (`"up"`) stays a string enum; a
        `const` schema (a `ToValue` without a hint) stays a `const`; the same tree under
        `withHonorsSchemaAnnotations(false)` + `Discriminator` is tagged (values ignored, as today);
     g) `enumsAsOneOf = true` + `@discriminator`: tagged objects (the flag concerns the string form); without
        `@discriminator`: `const`s as today;
     h) a tagged singleton with `title`/`description`/`definition`: they land on the object / the `def`, as for a
        case class.
3. **`api/src/test/scala/com/github/andyglow/jsonschema/DiscriminatorSpec.scala`**: a `case6` (the issue's
   model) and a `case7` (`phantom = false` with a keyed case object). Assert the tree (`as07`) and the Raw/draft
   JSON with `containJson`, as cases 0–5 do: the object member `{"type": {"enum": ["j"]}}` required `["type"]`.
   Also add them to `api/src/test/scala/.../golden/GoldenSharedModelsSpec.scala`
   (`check("shared/DiscriminatorSpec.case6", ...)`, as for cases 4 and 5). This creates new
   `golden/shared/DiscriminatorSpec.case6.json` / `case7.json` (tagged), and their `golden-legacy/` twins hold
   the string form, which also pins Legacy.
   Plus, derived (Scala 2 and 3 share this spec):
   - `case8`: `@discriminator("kind") @typeHint[Int]` family with a case class and two case objects and an
     implicit `ToValue` (the D5-R1 model). Neutral keeps `{"type": "integer", "enum": [1, 2]}`.
   - `case9`: `@definition("jump") @discriminatorKey("j") case object Jump` in a `@discriminator` family. Under
     Neutral, Jump becomes `{"$ref": "#jump"}` with the tagged object in `definitions` (prototype output). Under
     Legacy it stays a string, and `@definition` is ignored as in 0.7.
4. **`api/src/test/scala/com/github/andyglow/jsonschema/FamilyTreeSpec.scala`** "Neutral and Legacy shape a
   derived family the same way": drop `Json.schema[Tagged]` from the list, and add a test that Neutral and Legacy
   differ for `Tagged` in exactly the `T2` member (Neutral: `object` with key
   `com.github.andyglow.jsonschema.FamilyTreeSpec.T2`; Legacy: `enum("T2")`).
5. **`api/src/test/scala/com/github/andyglow/jsonschema/characterization/FieldsAnnotationsCharSpec.scala`** A4:
   rename to "A4: @discriminator on an all-case-object family: ignored under Legacy (0.7), tagged under Neutral".
   Regenerate `golden/char/fields/DiscriminatedEnum.json` (`GOLDEN_UPDATE=1 sbt clean api/test`, CLEAN build).
   Its three drafts go from `{"type": "string", "enum": ["X", "Y"]}` to a `oneOf` of two tagged objects
   (`{"type": {"enum": ["<sig>.X"]}}`, required `["type"]`). The exact diff is in
   `notes/scratch/c-d5/proto-golden.diff`. `golden-legacy/char/fields/DiscriminatedEnum.json` is **unchanged**,
   which is the Legacy check. No other golden file changes (measured). A3 ("`@definition` is ignored on case
   objects") stays true for Legacy and for untagged families. Rename it to say so.
6. **Acceptance (networknt)**, `api/src/test/scala/petshop/`: add a `@discriminator("kind")` family with a keyed
   case object to `Model.scala` (or a local model in `NeutralLegacyAcceptanceSpec`). Under Neutral `{"kind": "j"}`
   is valid and `"Jump"` invalid; under Legacy the reverse; on the suite's drafts. Also the `case8`-style
   `ToValue` family: `1` valid under both. Validation is JVM only: on JS these tests show as canceled, which is
   expected.
7. **Scala 3** (`api/src/test/scala-3/.../sums/Scala3EnumsSpec.scala`):
   - `@discriminator enum Cmd { case Move(m: Int); @discriminatorKey("j") case Jump }`: under Neutral, `Jump` is
     a tagged object with key `j`;
   - `@discriminator enum Dir { case N, S }` (parameterless cases only, sub-choice 1): a `oneOf` of tagged
     objects under Neutral, the string `enum` under Legacy.

   The family node is the same as for a sealed trait, so no Scala 3-specific code is expected. The tests guard that
   an enum case's annotation reaches `Member.discriminatorKey`. `FamilyTreeSpec` checks that only for case
   objects.
8. **Docs** (README and paradox say the same things):
   - README "Discriminators" (~l.341) and `docs/src/main/paradox/types.md` (same section, ~l.80): add a case object
     to the `Pet` example (`@discriminatorKey("fish") case object Fish extends Pet`) with its rendered member (the
     real output), and this text: "A case object in a tagged family is tagged like the case classes (an object with
     only the discriminator field). A family of case objects only, annotated `@discriminator`, is a `oneOf` of such
     objects. A family whose `ToValue` gives its case objects values keeps those values. Under
     `JsonSchemaProfile.Legacy`, or with `withSingleton(SingletonStyle.AsString)`, a case object is a string, as
     in 0.7."
   - README ~l.315–318 and `types.md:55-56` ("A sealed family of case objects is a string enum of their names"
     / "case objects among them are merged into one enum member"): add "unless the family is tagged (see
     Discriminators)".
   - README annotation table row `@discriminatorKey` (~l.484) and its twin `docs/src/main/paradox/descriptions.md:83`:
     "sealed member (case class, case object, enum case)".
   - README "Library profiles" knob table (~l.727) and `docs/src/main/paradox/rendering.md` (~l.188):
     `withSingleton` values `AsString`, `AsTagged`, `FollowFamily` (Neutral's: "like the family's other
     members").
   - `rendering.md:173` and its README twin ("They apply under Neutral and Legacy, and under the spray profile
     ..."): add "a case object in a `@discriminator` family is tagged under Neutral and spray, and is a string under
     Legacy".
   - `SprayProfile` scaladoc (`modules/spray-json/.../SprayProfile.scala`): it inherits `FollowFamily`. Its
     example can drop `.withSingleton(SingletonStyle.AsTagged)`, though keeping it is harmless. Do the same for the
     README/`rendering.md` hand-written-codec snippet. Recommendation: drop it in both, so the example shows the
     default.
   - `sbt makeDocs`.
9. **Gates** (for the implementer): `clean test` on 2.12.21 / 2.13.18 / 3.3.9; `rootJS/test` on all three
   (core changed); `scalafmtCheckAll` + `rootJS/scalafmtCheckAll` (touch sources first); `makeDocs`.

Risks:
- A user whose hand-written codec writes case objects as strings inside a tagged family gets a schema that rejects
  their output after upgrading. Mitigation: the CHANGELOG "Breaking" entry names
  `withSingleton(SingletonStyle.AsString)` (or `Legacy`). The lead writes the CHANGELOG at release.
- The T2 heuristic: a `ToValue` that restates the names exactly is treated as "no `ToValue`" (tagged). Documented
  in the `FollowFamily` scaladoc.
- `SingletonStyle` gets a third case: a user's exhaustive `match` on it gets a non-exhaustive warning (an error
  under `-Werror`). Rare: users build profiles, they don't usually match on knobs.

### Interactions with other items
- **B15** (root-family-type): an all-object `oneOf` at the root has no `"type"`, because the members drop theirs
  and the root doesn't state it. C makes more families all-object (the issue's model, A4's). Then a root schema with
  only **one** member would accept any non-object, for example `"j"`. With two or more members, a non-object
  matches all of them, and `oneOf` rejects it by accident. B15's fix covers this, and D5 needs nothing extra.
  Mention it in the B15 doc/PR.
- **B14** (oneof-overlap): `AsTagged` + `Untagged` writes every singleton as the same
  `{"additionalProperties": false}`. `FollowFamily` never produces that: an untagged family keeps strings. The
  json4s-without-hints profile (`AsTagged`, its own setting) still does; that belongs to B14 (r-d5 reported it to
  the lead).
- **D3** (#294, discriminator as `const`): orthogonal. Whatever D3 renders for the tag applies to tagged
  singletons too.
- **B3** (#470): textual conflict in `Knobs` only (see the start of the plan).

## Compatibility

- **Output:** changes under `JsonSchemaProfile.Neutral` (the default) and `SprayProfile.default`, and under any
  user profile derived from `Neutral` through other builders (`withSum`, `withMemberName`, `withOption`, ...),
  since those inherit `FollowFamily`. It changes only tagged families (an honored `@discriminator`, or a profile
  `sumStyle` of `Discriminator`/`WrapperObject`) that have at least one case object / parameterless enum case and
  no value-giving `ToValue`. Those members go from a string `enum` to a tagged object. A case-object-only tagged
  family goes from a string `enum` to a `oneOf` of tagged objects. `@title`/`@description`/`@definition` on
  such a case object now apply, as on a case class (characterization A3 "ignored on case objects" then holds for
  Legacy and untagged families only).
  Unchanged: `Legacy` (byte-exact 0.7: its golden-legacy files don't change), the play/circe/upickle/json4s
  profiles (each sets its singleton style), untagged families, families without case objects, `ToValue`-driven
  families, and any profile with an explicit `withSingleton(...)` or a hand-implemented trait.
- **Source:** additive (a new `SingletonStyle` case). A user's exhaustive match on `SingletonStyle` warns.
- **Binary:** compatible (a new case object; `Neutral` is still a `val JsonSchemaProfile`; no signature changes;
  no change to `Member`/`family`).
- **PVP:** the default profile's output changes, so under the project's rule (output changes bump the second
  number, as 0.7 → 0.8 did) this is **breaking: 0.10.0**. If the owner treats #293 as a bug fix, it can ship in
  0.9.x with a CHANGELOG note. Owner's call.
- **Core design rule:** untouched (no change to the Schema → `$ref` / Predef → inline / derive lookup).

### Open questions for the owner
1. C (new default) vs A (document only). The doc recommends C.
2. The scope of "tagged family": the resolved style (C, recommended) or only an honored `@discriminator` (C1).
3. A tagged family of case objects only (`@discriminator enum Dir { case N, S }`): tagged (C as written) or keep
   the string `enum` (`|| singletonsOnly`).
4. `ToValue` in a tagged family: keep the values by heuristic (T2, recommended), drop them (T1), or record the fact
   in the tree (T3, binary-breaking).
5. The name: `FollowFamily` vs `Auto`.
6. Ship in 0.9.x as a fix, or hold for 0.10.0.

## Review log

Round 1 (r-d5 on 1cb09fe; comments in `notes/reports/r-d5.md`):

- D5-R1 (must) ToValue/`@typeHint` values of a `@discriminator` family dropped silently → **accepted**. Added
  "The `ToValue` values of a tagged family" with T1/T2/T3. The winner is T2: per-family detection from
  `Member.schema`/`Member.value`, no tree change. It is in the mechanism, and the prototype run shows
  `enum [1, 2]` and the string `ToValue` kept. Added tests: lowering 2f, derived `case8`, acceptance in step 6.
  The heuristic's blind spot is listed under Risks.
- D5-R2 (must) the rule keyed on the annotation only, but was named "discriminated" → **accepted, (ii)**. The
  style is now `FollowFamily`, keyed on the resolved `style` (`@discriminator`, `sumStyle`, `PerFamily`). The
  annotation-only rule is option C1 (LOST). Re-measured: two more tests change, both on core's local `upickle`
  fixture built from `Neutral` without `withSingleton`. The real `UPickleProfile` sets `AsString`, and all
  library profiles set their style explicitly. Plan step 2 fixes the fixture.
- D5-R3 (must) docs list incomplete → **accepted**. Step 8 now lists `descriptions.md:83`, `types.md:55-56` +
  README ~l.315, `rendering.md:173` + its README twin, the `honorsSchemaAnnotations`/`singletonStyle`/`AsString`/
  `AsTagged` scaladocs (step 1), the spray scaladoc, and the hand-written-codec example.
- D5-R4 (nit) option C's sample output had `"type": "object"` on the member → **accepted**. It now shows the
  real render and explains the hoisting (and B15).
- D5-R5 (nit) `@title`/`@description`/`@definition` now apply to tagged case objects; A3 → **accepted**.
  Added to Compatibility, plus step 5 (rename A3) and derived test `case9` (`@definition` case object →
  `$ref`, prototype output).
- D5-R6 (nit) Scala 3 enums with parameterless cases only → **accepted**. Named in sub-choice 1 and open
  question 3; test in step 7.
- D5-R7 (nit) say who else changes → **accepted**. Compatibility names user profiles derived from `Neutral`
  through other builders. Only an explicit `withSingleton`, `Legacy` or a hand-implemented trait keeps the old
  style.
