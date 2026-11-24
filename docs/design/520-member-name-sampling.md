# Member names chosen by sampling: a selective renamer is ignored  (issue #520, item B25)
Status: approved (r-b25, 2026-11-23)

## Problem

`CirceProfile.configured` (and Scala 3 `CirceProfile.fromConfig`, which calls it) and `UPickleProfile.fromApi` pick
the profile's `memberName` (the tag of a sealed-family member) by trying the library's renaming function on a fixed
list of sample names:

```scala
// CirceProfile.scala
val memberName = renamed(transformConstructorNames).fold[MemberName](MemberName.SimpleName)(_ => MemberName.Custom(m => transformConstructorNames(m.name)))
private def renamed(f: String => String): Option[(String, String)] =
  List("name", "fooBar", "FooBar", "foo_bar", "foo-bar", "x1", "URL", "a").iterator.map(n => n -> f(n)).find { case (in, out) => in != out }

// UPickleProfile.scala: the same list plus "petshop.Dog", on n => api.objectTypeKeyWriteMap(n).toString
val memberName = renamed(n => api.objectTypeKeyWriteMap(n).toString) match {
  case None    => name                                  // SimpleName or FullName
  case Some(_) => MemberName.Custom(m => if (m.kind == `family`.Kind.Singleton) name(m) else api.objectTypeKeyWriteMap(name(m)).toString)
}
```

If no sample name changes, the function is taken for identity and dropped. A renamer that changes only some names
(one member, a legacy tag, a fixed table) leaves all samples alone, so the profile tags with the Scala name while the
codec writes the renamed one.

Repro (`notes/scratch/c-b25/B25CirceReproSpec.scala`, `B25UPickleReproSpec.scala`, the petshop models
`Pet(name, age: Option[Int], animal: Animal)`, `Animal = Dog(name) | Fish`):

```scala
CirceProfile.configured(Some("type"), { case "Dog" => "dog"; case n => n }).memberName   // SimpleName

object DogOnlyTags extends upickle.AttributeTagged {
  override def objectTypeKeyWriteMap(s: CharSequence): CharSequence = if (s.toString == "Dog") "doggo" else s
  override def objectTypeKeyReadMap(s: CharSequence): CharSequence  = if (s.toString == "doggo") "Dog" else s
}
UPickleProfile.fromApi(DogOnlyTags).memberName                                            // SimpleName
```

Actual output, the same on 2.12.21, 2.13.18 and 3.3.9 (shared code, so JS too):

- circe with the discriminator: `"animal":{"type":"object","oneOf":[{..."properties":{"name":{"type":"string"},"type":{"enum":["Dog"]}},"required":["name","type"]},{..."properties":{"type":{"enum":["Fish"]}}...}]}`.
  The codec writes `{"name":"Rex","age":3,"animal":{"name":"Rex","type":"dog"}}`; networknt (every draft):
  `$.animal.type: does not have a value in the enumeration ["Dog"]`.
- circe without a discriminator (wrapper objects): the wrapper key is `"Dog"`, the codec writes `{"dog":{"name":"Rex"}}`:
  `property 'dog' is not defined in the schema and the schema does not allow additional properties`.
- upickle: `DogOnlyTags.write` gives `{"name":"Rex","age":3,"animal":{"$type":"doggo","name":"Rex"}}`, the schema has
  `"$type":{"enum":["Dog"]}`: `$.animal.$type: does not have a value in the enumeration ["Dog"]`.

Not affected: `PlayProfile.fromConfig` (it compares `cfg.typeNaming == JsonNaming.Identity`, by reference),
`Json4sProfile` (always `Custom` over its `TypeHints`), `SprayProfile` (no setting).

## Why it is a problem

The failure is silent: no exception, no warning, a schema that rejects every value of the renamed members. The
renamers that hit it are the realistic ones for tags: keeping a wire name after a Scala rename
(`{ case "DogV2" => "Dog"; ... }`), a lookup table, or one odd member. circe's `transformConstructorNames` and
upickle's `objectTypeKeyWriteMap` are plain functions, and the scaladoc and README promise the profile "reads" them,
without a word about samples. The case-changing renamers (snake, kebab, lower) change at least one sample name, which
is why the gate passes the acceptance suite.

The same sample gate decides the field-renaming *throw* (`transformMemberNames`, `objectAttributeKeyWriteMap`,
play `naming`); D4 (#292, design PR #493) replaces that throw with a `fieldName` knob chosen by reference, for the
same reason. This item is the member-name half.

## Options

### A. `Custom` unless the function is the known identity by reference  — WON

Drop the sample gate. A profile uses `SimpleName` / `FullName` only when it *knows* there is no renaming, by reference
equality with the library's own default; otherwise `MemberName.Custom(...)` applies the function to each member at
lowering, so it is exact for any renamer, and equal output to `SimpleName` when the function happens to be identity
(the lowering calls `memberName(m)` and nothing else; nothing in main code matches on the `MemberName` case).

| profile | `memberName` |
|---|---|
| `CirceProfile.configured(d, transformConstructorNames, ...)` | `SimpleName` if `transformConstructorNames eq Same`, where `Same` is a stable `private val Same: String => String = identity` that becomes the parameter's default; else `Custom(m => transformConstructorNames(m.name))` |
| Scala 3 `CirceProfile.fromConfig(c)` | passes `Same` when `c.transformConstructorNames eq Configuration.default.transformConstructorNames` (`Configuration.default` is a `val`; `withDiscriminator`, `withDefaults`, ... are `copy`s and keep the reference), else the function |
| `UPickleProfile.fromApi(api)` | `SimpleName` if `api eq upickle.default`; else `Custom(m => if (m.kind == Singleton) name(m) else api.objectTypeKeyWriteMap(name(m)).toString)`, `name` being `FullName` or `SimpleName` per `objectTypeKeyWriteFullyQualified` as today |
| `PlayProfile`, `Json4sProfile`, `SprayProfile` | unchanged |

`Same` is the same stable value D4 introduces for `transformMemberNames` (one `private val` in `CirceProfile`, the
default of both parameters); whichever PR lands second reuses it.

Output: unchanged for every configuration that renders correctly today (`Custom` over a function that leaves a name
alone yields that name). Changed, and now right, for selective renamers: `"type":{"enum":["dog"]}` /
`"properties":{"dog":{...}}` / `"$type":{"enum":["doggo"]}`.

Visible besides output: `profile.memberName` is `Custom(...)` instead of `SimpleName` / `FullName` for a configured
profile that doesn't rename (Scala 2 `configured(cfg.discriminator, cfg.transformConstructorNames, ...)` with
generic-extras' `Configuration.default`, whose function we can't compare with; a Scala 3 `Configuration(...)` built
with the constructor; any upickle API object other than `upickle.default`, e.g. `object MyPickler extends
AttributeTagged { override def tagName = "kind" }` or one with `objectTypeKeyWriteFullyQualified = true`). The
defaults (`CirceProfile.default`, `profiles.circe._`, Scala 3 `fromConfig(Configuration.default.withDiscriminator(...))`,
`UPickleProfile.default`, `profiles.upickle._`) keep `SimpleName`. `MemberName.Custom` profiles "don't compare equal"
(its scaladoc), but profiles are anonymous classes and never compared in main code; a user who matched on
`profile.memberName` of a configured profile would see `Custom` — unlikely, and the knob's meaning (what the tag is) is
unchanged.

Pros: exact for every function; no heuristic left; same rule as D4, so the two halves read the same. Cons: the case
object you get back is less informative for configured profiles.

### B. Always `Custom` (no identity check at all)  — LOST

`memberName = Custom(m => transformConstructorNames(m.name))` unconditionally; upickle likewise. Simplest code, also
exact. Lost to A only because the default profiles would report `Custom` instead of `SimpleName`, which is what a
user printing or asserting `CirceProfile.default.memberName` expects, and A costs one `eq`. Easy to pick instead: drop
the `eq` branches from A's table, nothing else changes.

### C. Smarter sampling  — LOST

More samples, or the real member names: the profile is built before any family is seen, so the only "real names"
are the ones the lowering meets, and evaluating the function there *is* `Custom` (A/B). A longer list still misses
`{ case "Dog" => "dog" }` for any name not on it. Lost: a heuristic that can't be made right.

### D. Keep the gate, document it  — LOST

Say in the scaladoc and README that a renamer must change one of the sample names, and point to
`.withMemberName(MemberName.Custom(...))` for others. Zero code, but leaves a silent wrong schema for an API that
claims to read the setting. Lost.

## Decision

A: the circe and upickle profiles stop sampling. They use `SimpleName` / `FullName` only when the renamer is the
library's default by reference (`Same` / `Configuration.default`'s function / `upickle.default`), and `Custom` over the
library's function otherwise, which is exact per member. Output changes only for selective renamers, which become
correct.

## Implementation plan

1. `modules/circe-json/src/main/scala/com/github/andyglow/jsonschema/CirceProfile.scala`:
   - add `private[jsonschema] val Same: String => String = identity` (shared with D4: if #493's implementation landed,
     reuse its value; `private[jsonschema]` so `CirceProfilePlatform` on Scala 3 can reach it). Declare it **above**
     `val default = configured()`: object vals initialize in order, so below it `configured()` would run with
     `Same == null` as the default argument, and the `renamed(transformMemberNames)` throw kept until D4 would call
     `null("name")`, an NPE in `CirceProfile.<clinit>` (Scala 2 accepts the forward reference silently). Comment it so;
     the "render with the import's own asCirce" test and every `profiles.circe._` test touch `CirceProfile.default` and
     would catch it;
   - `configured(..., transformConstructorNames: String => String = Same, ...)`;
   - `val memberName = if (transformConstructorNames eq Same) MemberName.SimpleName else MemberName.Custom(m => transformConstructorNames(m.name))`;
   - keep `renamed` only for the `transformMemberNames` throw until D4 removes it (if D4 already landed, delete `renamed`);
     reword its comment: it only gates the field-renaming throw now;
   - scaladoc of `configured`: "names are `transformConstructorNames(<simple name>)`, for any function (it isn't compared with identity)".
2. `modules/circe-json/src/main/scala-3/com/github/andyglow/jsonschema/CirceProfilePlatform.scala`: pass
   `if (c.transformConstructorNames eq Configuration.default.transformConstructorNames) CirceProfile.Same else c.transformConstructorNames`.
   (D4 does the same for `transformMemberNames`; keep both lines side by side.) Scala 2 has nothing to change: there is
   no `fromConfig` (generic-extras is a test dependency only), users pass the fields to `configured`.
3. `modules/u-json/src/main/scala/com/github/andyglow/jsonschema/UPickleProfile.scala`:
   `val memberName = if (api eq _root_.upickle.default) name else MemberName.Custom(...)` (the existing `Custom` body,
   unchanged: case objects not mapped). Drop `"petshop.Dog"` from the sample list (it was only for the tag map);
   `renamed` stays for the `objectAttributeKeyWriteMap` throw until D4 removes it, its comment reworded as in step 1.
   Scaladoc of the object and of `fromApi`: the tag is `objectTypeKeyWriteMap(<name>)` for any function (it isn't
   compared with identity); case objects not mapped.
4. Tests (shared `src/test/scala`, so 2.12/2.13/3, JVM and JS; on JS the validating asserts are canceled as usual,
   the `memberName(...)` asserts still run):
   - `modules/circe-json/src/test/scala/petshop/CirceAcceptanceSpec.scala`, under "CirceProfile.configured":
     selective `{ case "Dog" => "dog"; case n => n }` with `Some("type")` (assertValid on
     `{"animal":{"name":"Rex","type":"dog"}}` / `{"animal":{"type":"Fish"}}`) and with `None` (wrapper `{"dog":{...}}`,
     `{"Fish":{}}`), plus `memberName(Schemas.animalMember("Dog")) shouldBe "dog"`;
     `CirceProfile.default.memberName shouldBe MemberName.SimpleName`.
   - `modules/circe-json/src/test/scala-3/petshop/Circe3Spec.scala`: a `Circe3DogOnlyCodecs` with
     `given Configuration = Configuration.default.withDiscriminator("t").withTransformConstructorNames { case "Dog" => "dog"; case n => n }`
     and `ConfiguredEncoder.derived`; assertValid of `fromConfig(that)` against the real encoder output;
     `fromConfig(Configuration.default.withDiscriminator("kind")).memberName shouldBe SimpleName`.
   - `modules/circe-json/src/test/scala-2/petshop/CirceExtrasSpec.scala`: generic-extras
     `Configuration.default.withDiscriminator("type").copy(transformConstructorNames = { case "Dog" => "dog"; case n => n })`,
     `deriveConfiguredEncoder`, `configured(cfg.discriminator, cfg.transformConstructorNames)`, assertValid on the real output.
   - `modules/u-json/src/test/scala/petshop/UPickleAcceptanceSpec.scala`: the `DogOnlyTags` API object + codecs from
     the repro, its map extended to `Fish -> fishy` too (and the read map back), assertValid on `DogOnlyTags.write`
     output, `memberName(Dog) shouldBe "doggo"`, and the schema still has `"Fish"` (upickle writes the case object
     unmapped: `"Fish"`; this pins the singleton branch with a selective map);
     `UPickleProfile.default.memberName shouldBe MemberName.SimpleName`. The existing `FullNames`, `LowerTags` and
     `MyPickler` cases stay green (now through `Custom`).
   - "`Custom` over identity = the old output", one direct check per bridge (`Schemas.pet` rendered under each, compared
     byte for byte on every draft): circe shared `configured(Some("type"), identity)` vs `configured(Some("type"))` and
     `configured(None, identity)` vs `CirceProfile.default`; Scala 3
     `fromConfig(Configuration(identity, identity, false, Some("type")))` vs
     `fromConfig(Configuration.default.withDiscriminator("type"))`; upickle `fromApi(MyPickler)` (only `tagName` set)
     vs `UPickleProfile.default.withSum(SumStyle.Discriminator("kind"))`, and an `object Plain extends AttributeTagged`
     vs `UPickleProfile.default`.
   - No golden files change (goldens are rendered under `Neutral` / `Legacy`, not under the bridge profiles); a golden
     diff is a bug. `golden-legacy/` untouched.
5. Docs: README "Library profiles" table (circe and upickle rows: "read from" stays; add "any function, applied per
   member") and the same row in `docs/src/main/paradox/rendering.md` (the two say the same thing). The "(tried on
   sample names)" sentence at README ~L703 / rendering.md ~L163 is about field renaming, D4's; if D4 hasn't landed,
   leave it.
6. Risks: (a) D4 and B25 edit the same lines of `CirceProfile.configured`, `CirceProfilePlatform.fromConfig` and
   `UPickleProfile.fromApi`: implement them in one PR or rebase the second, sharing `Same`. (b) A Scala 2
   generic-extras user's profile now reports `Custom`; output identical. (c) JS: `eq` on function values is reference
   equality on Scala.js too; nothing platform-specific.

## Compatibility

- Source/binary: no signature changes. `configured`'s default argument value changes from `identity` to the stable
  `Same` (still identity); a default argument is a method in the companion, its signature unchanged. Binary-compatible.
- Output: unchanged for every configuration whose renamer is identity or changes a sample name (all of today's
  tests); changed, to the codec's real tags, for selective renamers (a bug fix). `profile.memberName` reports `Custom`
  for configured profiles that aren't the defaults.
- PVP: patch (0.9.x). Legacy profile: not involved (bridge profiles only); golden-legacy untouched.

## Review log
- B25-R1 (must) init order: `Same` declared after `default` is `null` during `CirceProfile`'s init, and the kept
  field-renaming throw NPEs in `<clinit>` → accepted: step 1 says to declare `Same` above `default`, why, and which
  tests would catch it (D4's implementation has the same constraint; noted where `Same` is shared).
- B25-R2 (nit) pin "`Custom` over identity renders the same" → accepted: step 4 adds one byte-equal check per bridge
  (circe shared, Scala 3 `Configuration(...)` constructor, upickle `MyPickler` / a plain `AttributeTagged`).
- B25-R3 (nit) singleton branch with a selective map → accepted: `DogOnlyTags` also maps `Fish`; the test asserts the
  schema keeps `"Fish"` and validates upickle's `"Fish"`.
- B25-R4 (nit) scaladocs → accepted: step 3 updates the object's and `fromApi`'s scaladoc; steps 1 and 3 reword the
  `renamed` comments (field-renaming throw only).
