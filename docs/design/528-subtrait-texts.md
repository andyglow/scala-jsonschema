# A sealed sub-trait inside a family keeps its texts and its implicit Schema  (issue #528, item B28)
Status: draft

## Problem
A sealed sub-trait of a sealed family is flattened into the family's members. Both macros build the `Schema.family`
from the leaves only (Scala 2 `UCommons.resolveSumTypeRecursively`, Scala 3 `Sum.leaves` in `FamilyEmit.emitSumFamily`
and `Enums.singletonLeaves`), so nothing about the sub-trait reaches the tree. Two facts are lost:

1. **Its texts.** `@title` / `@description` / scaladoc on the sub-trait. (Scala 3 records them on the `NestedFamily`
   `SumMember`, then `leaves` drops them. Scala 2 never reads them.)
2. **Its implicit `Schema`.** The core rule says an implicit `Schema[T]` becomes a definition + `$ref`. For a sealed
   sub-trait inside a family it's ignored: both macros check `isSealed` before the implicit lookup and recurse into the
   leaves. (The lead folded this in from r-b28's probe. Repro: `notes/scratch/r-b28/ZzB28ProbeSpec.scala`.)

Repro (`notes/scratch/c-b28/B28ReproSpec.scala`, run on 2.13.18 and 3.3.9, JVM; both versions give the same trees):

```scala
sealed trait Fam
object Fam {
  @title("Sub T") @description("Sub D")
  sealed trait Sub extends Fam
  case class C(c: Int)     extends Sub
  case class D(d: String)  extends Sub
  case class A(a: Boolean) extends Fam
}

sealed trait Imp
object Imp {
  sealed trait ISub extends Imp
  object ISub { implicit val iSubSchema: Schema[ISub] = Json.schema[ISub] }
  case class C4(c: Int)     extends ISub
  case class D4(d: String)  extends ISub
  case class A4(a: Boolean) extends Imp
}
case class ImpHolder(s: Imp.ISub)

sealed trait En
object En {
  @title("G T") sealed trait G extends En
  case object X extends G
  case object Y extends G
  case object Z extends En
}
```

| expression | tree today (2.13 = 3.3) |
|---|---|
| `Json.schema[Fam]` | `family(C:Product(..), D:Product(..), A:Product(..))`: no `Sub T` / `Sub D` anywhere |
| `Json.schema[Fam.Sub]` | `family(C, D) description=Sub D title=Sub T`: as the root it keeps them |
| `Json.schema[Imp]` | `family(C4, D4, A4)`: the implicit is ignored, the leaves are derived again |
| `Json.schema[ImpHolder]` | `object(s: def(Imp.ISub, family(C4, D4)))`: as a field the implicit is used |
| `Json.schema[En]` | `family(X:Singleton, Y:Singleton, Z:Singleton)`: `G T` dropped |

Draft 07, Neutral, `Json.schema[Fam]` today: a flat `oneOf` of the three objects. There's no title in it (and none
on master even for `Fam.Sub` as the root, because the renderer writes texts only on objects and consts; B1 / #469
fixes that).

## Why it is a problem
- The README promises that `@title` / `@description` work on any type. B12 (#501, design #508) makes a type's own texts
  apply wherever it's derived. It has to make an exception for exactly this case ("not on a sealed sub-trait inside a
  family"). Scaladoc on a sub-trait is lost the same way. Documentation tools that show a schema's titles lose the grouping.
- The implicit case breaks the core design rule (`CLAUDE.md`: implicit `Schema` → definition + `$ref`). A user who
  writes `implicit val s: Schema[Sub]` to get one definition for `Sub` gets it for fields of type `Sub`, but the family
  inlines `Sub`'s leaves again. That's the duplication the definition was meant to avoid. It also can't serve as an
  override: a hand-written `Schema[Sub]` (a different shape, a `Predef` that inlines something else) is ignored inside
  the family, so the family's schema disagrees with the field's.
- Nothing becomes invalid. The flat leaves validate the same documents, so this is lossy output, not a wrong schema.

## Options

The fix has three parts: where the facts live in the tree, what the profiles make of them, and what Legacy does.

### A. The leaves record their enclosing sub-traits (`Member.parents`): WON
The `family` stays a flat list of leaves, exactly as today. Each `family.Member` gets one new fact: the sealed types
between the root and the leaf, outermost first, each with what the profile needs:

```scala
final case class Parent(name: String, fullName: String, sig: String,
                        title: Option[String] = None, description: Option[String] = None,
                        provided: Option[Schema[_]] = None)
final case class Member(..., valueField: Option[String] = None, parents: List[Parent] = Nil)
```

`provided` is the sub-trait's implicit `Schema` / `Predef`, if one was in scope where the family was derived. As for
every member, the implicit is used as it is: a `def` for a `Schema`, the inlined body for a `Predef`.

The **lowering** (`JsonSchemaProfile.familyAt`) turns contiguous runs of leaves into groups. In declaration order the
leaves of one sub-trait are contiguous on both versions, because the walk is depth first. The rules:
- a run under a sub-trait with a `provided` schema → `apply(provided)` in place of the whole run (the core rule). Two
  exceptions: the family has an honored `@discriminator` (see "Discriminator" below), or the profile flattens (Legacy);
- else a run under a sub-trait with a title or description → one group: the run lowered with the same rules (its
  members shaped, tagged and merged exactly as today, but only among themselves), wrapped as
  - `oneof(members, tagField)` (or B3's `anyof` when they overlap) with the sub-trait's `title` / `description`, or
  - when the run is only singletons that merge into one `enum`: that `enum` with the texts, without a `oneOf` around it;
- a sub-trait with neither texts nor an implicit is transparent: its leaves stay where they are (today's output);
- nesting recurses: `Sub1 > Sub2 > leaf` with both titled gives a group in a group. An untitled `Sub1` around a
  titled `Sub2` gives just the `Sub2` group.

Neutral output of `Fam` after B1 + B3 + this (draft 07; key order as B1 renders it):

```json
{ "oneOf": [
    { "description": "Sub D", "title": "Sub T", "type": "object", "oneOf": [
        { "additionalProperties": false, "properties": { "c": { "type": "integer" } }, "required": ["c"] },
        { "additionalProperties": false, "properties": { "d": { "type": "string" } }, "required": ["d"] } ] },
    { "type": "object", "additionalProperties": false, "properties": { "a": { "type": "boolean" } }, "required": ["a"] } ] }
```

`Imp`: `{ "oneOf": [ { "$ref": "#/definitions/...Imp.ISub" }, { "type": "object", ...A4 } ] }` plus the definition. Under B3 the
`$ref` makes the members not provably disjoint, so that outer group is an `anyOf` (B3's rule for a `$ref` member; still
correct). `En`: `{ "oneOf": [ { "title": "G T", "type": "string", "enum": ["X", "Y"] }, { "type": "string", "enum": ["Z"] } ] }`.

Shapes per sum style. Grouping doesn't change what a leaf looks like, so the documents that validate don't change:
- **Untagged**: as above.
- **WrapperObject** (circe/play/upickle defaults): each leaf is wrapped by its own label as today (`{"C": {...}}`),
  and the group is a `oneOf` of the wrapped leaves. The libraries flatten nested families (the petshop `Vehicle > Car`
  acceptance tests prove it for every bridge), so nothing is wrapped by `"Sub"`.
- **Discriminator(field)**: the group's `oneof` carries the same `tagField`, so the renderer (`mkObj`, parent
  `oneof(_, Some(f))`) still puts the tag into each leaf object. The group itself isn't an object, so it gets no tag.
- A `provided` run under a style-driven tag (library profiles): the implicit is lowered by the same profile. Its leaves
  get the same labels they'd get inside the parent (`label` depends only on the member, the profile and an annotated
  discriminator), so `$ref` describes what the library writes.

**Discriminator.** With `@discriminator` on the root (honored), the root's tag is the annotation's, keyed by each
leaf's `@discriminatorKey` or signature. A `Schema[Sub]` derived on its own doesn't carry it. Its leaves are closed
objects without the tag field, so they'd reject every tagged document. So under an honored root `@discriminator` the
implicit is not used and the run is grouped by texts, or stays flat. That's today's behavior for the implicit. A
provided *leaf* under `@discriminator` is a compile error today; the sub-trait doesn't get one, because the leaves are
there to fall back on.

Why A: everything that reads `family.members` today, and that's a lot, keeps working unchanged. That covers the
library profiles' `PerFamily` choosers and `memberName` sampling (`CirceProfile`, `UPickleProfile`, B25), json4s
hints by `cls`, the singletons-only check, Map keys from `knownString`, `SchemaEquality`, `Families.as07` and users'
own profiles. Legacy can still produce 0.7 output, because the leaves are all there. The facts stay facts and the
profile decides, as `family` was designed. Con: `Member` gains a field, which breaks binary compatibility (a case class).

### B. A nested member (`Kind.Family`, its schema a nested `family`): LOST
Scala 3's own IR already has it (`MemberKind.NestedFamily`). `members` would become a tree: a sub-trait is one member
whose `schema` is its `family` (or the implicit). The lowering gets simpler (recursion instead of runs). But every
consumer of `members` now sees non-leaves: the library profiles' choosers and name sampling, json4s class hints (a
trait's `cls`), the singletons-only rule, `knownString` Map keys, and user profiles. Each would need a `leaves` helper
and would be silently wrong until it adopts one. A new `Kind` also makes users' exhaustive matches warn. Same binary
break as A, and a much larger blast radius. Lost.

### C. Copy the sub-trait's texts onto each of its leaves: LOST
No tree change: a leaf without texts of its own takes the sub-trait's (as `withTexts` does for members). It's wrong
in meaning, because the title names the group, not each alternative (`C` and `D` would both be titled "Sub T"). It's
inconsistent too: a leaf with its own title keeps it, so the sub-trait's title shows up on some alternatives only.
It also doesn't help the implicit. Lost.

### D. Document the limitation only: LOST
README / paradox say a sub-trait's texts and implicit `Schema` are ignored inside a family. The workaround would be a
hand-written `Schema` for the whole family. Nothing changes and nothing breaks. But the core-rule violation stays, and
B12's "a type's texts apply wherever it's derived" keeps its exception. Kept as the fallback if the owner doesn't
want a binary break for this.

### The implicit part: record it and keep the leaves (P1, WON) vs. one opaque member (P2, LOST)
- **P1** (in A): the macro still derives the leaves under a provided sub-trait and records the implicit on their
  `Parent`. Legacy and the annotated-discriminator case fall back to the leaves, and consumers keep every leaf fact.
  Con: a sub-trait whose leaves can't be derived (a non-case class leaf, the usual reason to write the implicit) still
  doesn't compile inside the family. That's no regression: it doesn't compile today either.
- **P2**: an implicit stops the derivation (rule 3 only applies "otherwise"), and the sub-trait is one `Provided`-like
  member. This rescues underivable sub-trees. But Legacy can't give 0.7 output any more, consumers see a non-leaf (B's
  problems), and the annotated discriminator has nothing to fall back to (a compile error for code that compiles today).
  Lost. If the owner prefers P2, it's a local change: the macro skips the leaves when `provided` is set, and the
  member gets a new `Kind`.
- **Own item** (the lead's alternative): implement only the texts here and file the implicit as a separate item. It
  would touch the same macro lines (`resolveSumTypeRecursively`, `Sums.member`), the same new `Parent` and the same
  lowering loop, so splitting it means designing the data twice. Recommended against, but the split is clean: drop
  `Parent.provided` and plan steps 3c / 5b.

### Legacy: keep 0.7 (L-a, WON) vs. group under Legacy too (L-b, LOST)
The lead's policy freezes Legacy's *style*, sum shapes included, and fixes its *bugs*. Nothing here makes a 0.7
schema invalid: the flat leaves validate the same documents. Grouping is a change of sum shape. So Legacy keeps the
flat 0.7 output (texts dropped, the implicit ignored), byte for byte. That's done with a profile knob, so either
answer is a one-word change:

```scala
sealed trait SubFamilies
object SubFamilies { case object Grouped extends SubFamilies; case object Flattened extends SubFamilies }
def subFamilies: SubFamilies = SubFamilies.Grouped    // Legacy overrides: Flattened
final def withSubFamilies(x: SubFamilies): JsonSchemaProfile
```

L-b (Legacy `Grouped` too) is the alternative. The lead can pick it by deleting Legacy's override. Users who want
today's flat output under any profile write `.withSubFamilies(SubFamilies.Flattened)`.

## Decision
A + P1 + L-a. Each leaf of a family records the sealed sub-traits it sits under (names, signature, texts, implicit
schema). The profile groups a titled or described sub-trait's leaves into a nested `oneOf`/`anyOf`/`enum` carrying
its texts, and uses the sub-trait's implicit `Schema` in place of its leaves (unless the root's `@discriminator` is
honored). A knob `subFamilies` (default `Grouped`, Legacy `Flattened`) keeps 0.7's output under Legacy.
Binary-breaking (new `Member` field): ship with B3 in 0.10.0.

## Implementation plan
Prerequisites: B1 (#469, texts rendered on every node, needed for a group's title to show) and B3 (#470: members in
declaration order (contiguous runs), `anyof`, `Disjoint`) merged. Branch from master after both. Read B12's doc (#508)
for which texts function to use: the sub-trait's texts are "a type's own texts", with annotations over scaladoc.

1. **Core model**, `core/src/main/scala/json/Schema.scala` (`object family`):
   a. `final case class Parent(name, fullName, sig, title, description, provided: Option[Schema[_]])` with scaladoc
      (what each field is, "outermost first", "`provided`: the sub-trait's implicit Schema/Predef, used as it is"),
      and a `toString` (`Sub` plus `title=`/`description=`/`provided=` as `Member.toString` does).
   b. `Member`: new last parameter `parents: List[Parent] = Nil`. Add it to the `hashCode` tuple and print
      `in Sub/Sub2` in `toString` when non-empty.
   c. `withMemberSchemas(f)` also maps every `parents[i].provided` (deepCopy and `Families.as07` go through it).
   d. The `family` comment block: "nested sealed families flattened" → "flattened; each leaf lists the sub-traits it
      sits under (`parents`)".
2. **Profile**, `core/src/main/scala/json/schema/JsonSchemaProfile.scala`:
   a. `SubFamilies { Grouped, Flattened }`, `def subFamilies = Grouped`, `withSubFamilies`, the `Knobs` field, and
      `Legacy` overriding it with `Flattened`. Scaladoc the knob with the rules from option A.
   b. `familyAt`: today's fold over `f.members` becomes `lowerRun(members, depth): List[Schema[_]]`. It walks members
      in order. For a member whose `effectiveParents` (the parents with texts or a usable `provided`; under `Flattened`:
      none) has an entry at `depth`, it takes the maximal contiguous run sharing that parent (by `sig`) and emits
      `apply(provided)` when `provided` is usable (`Grouped` and no honored discriminator). Otherwise it emits the group
      of `lowerRun(run, depth + 1)` (one merged `enum` → that enum with the texts; else `oneof(_, tagField)`, or `anyof`
      per B3's overlap check *among the run*). Members with no parent at `depth` go through today's entry logic
      (`Single` / `Merged`). Merging singletons into one `enum` stays within one run.
      The texts go on the group with `duplicate(title = ..., description = ...)`. A provided run gets none (an implicit
      is used as it is, as for a `Provided` member).
   c. The final `schemas match` (one enum / singletons-only `oneof` / `anyof` / `oneof`) applies to the top run, as
      today. `singletonsOnly` still reads all `f.members` (the leaves).
   d. B3's `Disjoint`: a group (`oneof`/`anyof` built for a run) is disjoint from `b` iff every one of its members is,
      and two groups iff every pair is. Otherwise a nested group would count as `anyType` and turn every family with
      a titled sub-trait into an `anyOf`. Mark group nodes for this, e.g. a private `Group` wrapper resolved before
      returning, or check the run's member schemas before wrapping them.
   e. A leaf under two sub-traits of the same root (a diamond, `case class X extends S1 with S2`) is listed twice
      today. Each copy goes to its own run. B3's overlap check then sees two groups sharing `X`: `anyOf`, valid. Pin
      it in a test; no special handling.
3. **Scala 2 macro** (`macros/src/main/scala-2/com/github/andyglow/jsonschema/`):
   a. `UCommons`: `resolveSumTypeLeaves(tpe, include, otherwise): Seq[(Type, List[Type])]`, the leaf plus the sealed
      types strictly between `tpe` and it, outermost first. The same walk as `resolveSumTypeRecursively`, which becomes
      `resolveSumTypeLeaves(..).map(_._1)` (or goes away).
   b. `SchemaTypes.Family`: `case class Parent(name, fullName, sig, texts: Option[Texts], provided: Option[SchemaType])`
      with a `tree` (`$F.Parent(...)`). `Member` gets `parents: List[Parent] = Nil`, and `Member.tree` passes
      `parents = List(..)`. `transformSchema` maps `provided` too (recursion and definitions see it).
   c. `USumTypes.resolveOneOf` and `UEnums` (the all-singleton family at ~L169) build the parents once per distinct
      parent type (a `Map[Type, Parent]`): `name`/`fullName` as for members, `signature(p)`, `TypeAnnotations(p).texts`,
      and `Implicit.get(p)` for `provided`. The implicit search for a sealed parent uses the same guards as for a
      leaf (`Json.auto` mustn't count: test it).
   d. `Either` (`eitherOneOf`) is unchanged.
4. **Scala 3 derivation** (`core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/`):
   a. `Sums.member`, `isSealed` branch: also `lookupImplicit(c.tpe)` → a new `SumMember.provided: Option[Node]`. The
      nested `Sum` is still built, for the leaves.
   b. `Sum.leaves` → `leavesWithParents: List[(SumMember, List[SumMember])]` (the enclosing `NestedFamily` members).
      `validateDiscriminator` keeps using the leaves.
   c. `Sum.children` / `mapChildren` include the `provided` nodes, so the recursion pass sees them.
   d. `FamilyEmit.emitSumFamily` emits `Schema.family.Parent(name, scala2FullName, sig, title, description,
      provided)` per enclosing member (`emit(provided)`), the same values Scala 2 gives (`name` = simple name, `sig` =
      `signature(tpe, dealiased = true)`).
   e. `Enums.singletonLeaves` returns the parents too, `EnumFamily` items carry them, and `emitEnumFamily` emits them
      (the `En` case). Scala 3 `enum`s have no nested sealed types: `Nil`.
   f. Texts: `typeAnnotations(c.tpe).texts`, as `NestedFamily` already records. Check `docsFromClasspath` matches what
      B12 picked for nested types (B12-R6).
5. **Core utilities**:
   a. `AsTree` (family case): print `parents` per member.
   b. `DefinitionNames.signatures`: also the `provided` schemas of the parents (a `def` there is a definition).
   c. `SchemaEquality` (family case): it compares `lm.copy(schema = rm.schema) != rm`, so `parents` takes part. Add
      the `provided` schemas to the structural compare like member schemas.
6. **Tests**:
   a. New shared `api/src/test/scala/com/github/andyglow/jsonschema/SubFamilySpec.scala`, models in its companion:
      `Fam` (titled + described sub-trait), `DocFam` (scaladoc only; Scala 2 reads it via the sbt-scaladoc plugin the
      api tests use), `En` (singleton group), two levels (`S1 > S2` both titled; untitled `S1` > titled `S2`), `Imp`
      (implicit `Schema`), an implicit `Predef[Sub]` (inlined), `Imp` with `@discriminator` on the root (falls back to
      the leaves), a diamond leaf, and `import Json.auto._` around an untitled nested family (output unchanged).
      Assert per model:
      - the tree (`family` members with their `parents`) on both versions;
      - Neutral draft 07 JSON (whole document);
      - `JsonSchemaProfile.Legacy`: today's flat output;
      - a `WrapperObject` profile and a `Discriminator("type")` profile (`Neutral.withSum(..)`): the group holds the
        wrapped / tagged leaves;
      - validity via `petshop.JsonValidator` (networknt; canceled on JS): sample documents valid
        (`{"c":1}`, `{"a":true}`, `"X"`), invalid (`{"x":1}`, `"Q"`) under Neutral, Legacy and the two styles;
      - `withSubFamilies(Flattened)` on Neutral equals today's Neutral output.
   b. `core/src/test/scala/json/schema/FamilyLoweringSpec.scala`: hand-built families with `parents`. Cover runs,
      transparency of untitled parents, nesting, a provided run, the annotated-discriminator fallback,
      singleton-merge-within-a-run, and `Disjoint` over groups (a titled group of disjoint objects next to a disjoint
      object stays `oneOf`).
   c. Existing tree specs that spell out `family.Member`s of nested families gain `parents`: `FamilyTreeSpec`
      (`Six` / `SNested`), and any of `SchemaMacroSpec` / `SumType*Spec` that build `MultiLevelSealedTraitRoot` /
      `HybridRecursiveSum` members by hand (`as07` comparisons are unaffected). `core` `SchemaEqualsHashCodeSpec` /
      `SchemaCopySpec`: a member with `parents`.
   d. Bridges: add `@title("Car")` to `petshop.Car` (`api/src/test/scala/petshop/Model.scala`), so every bridge's
      acceptance suite validates real codec output against a grouped family. If a petshop golden or spec spells out the
      `Vehicle` schema, update it, or add a separate titled family to the model instead.
   e. Golden files: none should change. No golden model has a sub-trait with texts or an implicit (checked:
      `MultiLevelSealedTraitRoot`, `HybridRecursiveSum`, `FamilyTreeSpec.Six`, `petshop.Vehicle` have none).
      Run `GOLDEN_UPDATE=1 sbt clean api/test` on 2.13 and 3.3 and expect an empty diff; `golden-legacy/` must not change.
7. **Docs** (README and paradox say the same):
   - README "Sealed families" / annotations section and `docs/src/main/paradox/types.md`: a sub-trait's
     `@title`/`@description`/scaladoc give its alternatives a nested group; an implicit `Schema` of a sub-trait is used
     in the family (definition + `$ref`), except under the root's `@discriminator`; the Neutral `Fam` example.
   - `docs/src/main/paradox/rendering.md` (profiles): the `subFamilies` knob, Legacy = `Flattened`.
   - Drop B12's caveat "(not on a sealed sub-trait inside a family: it's flattened, its texts are dropped)" if B12 has
     merged by then.
   - `sbt makeDocs`. No CHANGELOG (the lead).
8. **JS**: no platform code. The shared specs run on JS (`rootJS/test` on all three: core + macros + api touched).
   Validating tests are canceled there.
9. **Gates**: as in the protocol (core + macros + api: all three Scala versions, JVM and JS, scalafmt).

Risks:
- Contiguity: grouping assumes the leaves of one sub-trait are adjacent. True for the depth-first walk on both versions
  (after B3's ordering). The lowering should not reorder, only group. A test with an interleaved declaration order
  (`A`, `Sub`'s `C`, `B`, `Sub`'s `D` declared in that order in the file) pins that `Sub`'s leaves still come out
  together (the walk visits `Sub` once).
- Implicit searches for every sealed parent cost compile time. Cache per parent type. They inherit leaf lookups'
  guards (`Json.auto`, B20's generic low-priority implicits: whatever applies to a leaf applies here).
- Root type (B15) and the outer `oneOf`'s `type`: a group member has no single JSON type (`oneof.jsonType`), so the
  outer `oneOf` writes `type` on each member instead of once. That's valid. Hoisting it would change other outputs:
  not here.

## Not in scope (reported to the lead)
- `@definition`, `@discriminatorKey` and `@discriminator` *on a sub-trait* inside a family (Scala 3 records them on
  `NestedFamily`, both drop them). A `@definition` group would collide with the sub-trait's own definition when it's
  also a field type (tagged vs untagged bodies, one name). Needs its own design.
- A family-typed field (`Holder(f: Fam.Sub)`) losing `Sub`'s texts: B12 (nested types' own texts).
- A root `oneOf` without `type`: B15.

## Compatibility
- **Binary**: breaking. `family.Member` gets a parameter (constructor, `apply`, `copy`, `unapply`), and there's a new
  `family.Parent` and a profile knob (`Knobs` gains a field). Under PVP that's a minor bump: 0.10.0, together with B3
  (also breaking). If a 0.9.x patch is required, only option D (docs) fits.
- **Source**: compatible for named or positional construction of `Member` (new last parameter with a default).
  Positional `Member(...)` extractor patterns (12 fields) break. Custom profiles are unaffected unless they rebuild members.
- **Output** (Neutral and the library profiles; nothing changes for families without a texted or implicit sub-trait):
  - a sub-trait with `@title`/`@description`/scaladoc: its leaves become a nested `oneOf`/`anyOf` (or a titled
    `enum` for case objects) carrying its texts. Users with doc-commented sub-traits get a new `description` and the
    nesting. The same documents validate;
  - a sub-trait with an implicit `Schema` in scope: a `$ref` to its definition in place of its leaves, unless the root
    has `@discriminator`. The outer group may become an `anyOf` (B3: a `$ref` isn't provably disjoint).
- **Legacy**: unchanged, byte for byte (`subFamilies = Flattened`). Alternative L-b above.
- **Core rule**: restored for sub-traits (an implicit `Schema` of a sub-trait becomes definition + `$ref`), with one
  documented exception (an honored root `@discriminator`).

## Open questions for the owner
1. Ship in 0.10.0 with B3 (recommended), or docs-only (D) for 0.9.x?
2. Legacy: keep flat (L-a, recommended) or group too (L-b)?
3. Implicit: P1 (recommended) or P2 (rescue underivable sub-trees, Legacy changes)?
4. Should scaladoc alone (no annotation) trigger a group? Recommended yes: a type's scaladoc is its description
   everywhere else (B12). The alternative is "annotations only", which would make scaladoc on a sub-trait the one
   place it doesn't count.

## Review log
