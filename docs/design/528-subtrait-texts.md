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
- For the texts, nothing becomes invalid: the flat leaves validate the same documents, so that part is lossy output,
  not a wrong schema. **The implicit part can be a wrong schema** (B28-R1). A hand-written `Schema[Sub]` usually exists
  because `Sub`'s codec isn't the derived one (a custom `Format[Sub]` writing a string, a different shape). Today the
  family inlines the derived leaves, so the family's schema rejects what the codec writes, while a `sub: Sub` field
  accepts it.

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
- a run under a sub-trait with a `provided` schema → `apply(provided)` in place of the whole run (the core rule), under
  every profile, Legacy included (B28-R1). The one exception: the family has an honored `@discriminator` (see
  "Discriminator" below);
- else a run under a sub-trait with a title or description → one group, unless the profile flattens (Legacy, see
  `subFamilies`) or the family is an enum family (see "Families of singletons only" below). The group is the run
  lowered with the same rules (its members shaped, tagged and merged exactly as today, but only among themselves),
  wrapped as
  - `oneof(members, tagField)` with the sub-trait's `title` / `description`. Untagged, it's B3's `anyof` when the
    run's members overlap. Under a tag field it is always `oneof(_, tagField)`, never `anyof` (B3 makes only untagged
    families `anyOf`, and `anyof` has no tag field), so the nested leaves keep their tag (B28-R6); or
  - when the run is only singletons that merge into one `enum`: that `enum` with the texts, without a `oneOf` around it.
    With `EnumsAsOneOf` it's a `oneof` of their `const`s (each with its own texts) carrying the group's texts;
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
correct). `En` (all singletons): unchanged, `{ "type": "string", "enum": ["X", "Y", "Z"] }`; see below.

**Families of singletons only** (B28-R2). A family whose leaves are all singletons lowers (under `AsString`, without
`EnumsAsOneOf`) to one string `enum`. Code generators and form tools map that to an enum type, and handle a `oneOf` of
enums poorly or not at all. So a sub-trait's texts **don't** split such a family. It stays one `enum` and the
sub-trait's texts are dropped, as today; the docs say so. Where the family isn't one `enum` anyway, the group applies:
- `EnumsAsOneOf`: the family is a `oneOf` of `const`s already. The group nests its `const`s in a titled `oneOf`
  (`{ "oneOf": [ { "title": "G T", "oneOf": [ {"const": "X"}, {"const": "Y"} ] }, {"const": "Z"} ] }`);
- `SingletonStyle.AsTagged` (singletons written as objects): an ordinary group;
- a mixed family (case classes too): the outer is a `oneOf` already. A sub-trait of case objects becomes a titled
  `enum` member. Its values are no longer merged into the root's singletons' `enum`. Example: `OA` at the root and
  `OB` in a titled `OSub` give `oneOf[enum[OA], {title, enum[OB]}, OC]`, where today it's `oneOf[enum[OA, OB], OC]`.

An implicit `Schema` of a sub-trait still wins in an enum family (the core rule, R1). `En` with an implicit
`Schema[G]` becomes `oneOf[enum[Z], {"$ref": G}]`: an `enum` is split only when a schema really differs, never for
texts. The alternative, grouping in enum families too, is an owner question (Q5).

**A recursive sub-trait** (B28-R3). `sealed trait RFam; RA extends RFam; @title("RSub T") sealed trait RSub extends RFam;
case class RC(s: RSub) extends RSub; RD extends RSub` today (both versions): RFam's `oneOf` inlines `RC` and `RD`, and
`RC.s` holds `def(RSub, family(RC, RD))`, the recursion's definition, without the title. Decision: **the group stays
inline**, like any other group: `oneOf[RA, {title "RSub T", oneOf[RC, RD]}]`, and the definition is unchanged. The same
type then renders two ways, an inline group and a definition, but it does that today too (inline leaves and a
definition). Once B12 lands, the definition's body gets `RSub`'s texts too (B12: a nested derived type keeps its own
texts), so both say "RSub T". The alternative is a `$ref` to the definition for the group. It's DRYer, but the
lowering would need to know the recursion pass made a definition for `RSub` (a `provided`-like fact set by the
recursion pass), and the definition's body carries no root `@discriminator` tag: R1's problem again. Kept as the
alternative. Implementation: Scala 3 `leavesWithParents` looks through `nestedSum`'s `Def(_, _, s: Sum, _)`, as `leaves`
does. On Scala 2 the recursion pass (`URecursiveTypes`) runs over the leaves' schemas as today, and `parents` hold only
texts (no `provided` for a recursion definition). `validateDiscriminator` keeps checking all the leaves.

Shapes per sum style. Grouping doesn't change what a leaf looks like, so the documents that validate don't change:
- **Untagged**: as above.
- **WrapperObject** (circe/play/upickle defaults): each leaf is wrapped by its own label as today (`{"C": {...}}`),
  and the group is a `oneOf` of the wrapped leaves. The libraries flatten nested families (the petshop `Vehicle > Car`
  acceptance tests prove it for every bridge), so nothing is wrapped by `"Sub"`.
- **Discriminator(field)**: the group's `oneof` carries the same `tagField`, so the renderer (`mkObj`, parent
  `oneof(_, Some(f))`) still puts the tag into each leaf object. The group itself isn't an object, so it gets no tag.
- A `provided` run under a style-driven tag (library profiles): the implicit is lowered by the same profile. Its leaves
  get the same labels they'd get inside the parent (`label` depends only on the member, the profile and an annotated
  discriminator), so `$ref` describes what the library writes. The *style* is chosen for `Sub`'s family alone,
  though: a `SumStyle.PerFamily` chooser (json4s) runs on `Sub`'s members, not on `Fam`'s (B28-R8). json4s's chooser
  decides by the members' classes and hints, so per leaf the result is the same. One case differs: a partially hinted
  `Fam` throws, while `Sub` alone may not. That's acceptable, because the parent family still throws.

**Discriminator.** With `@discriminator` on the root (honored), the root's tag is the annotation's, keyed by each
leaf's `@discriminatorKey` or signature. A `Schema[Sub]` derived on its own doesn't carry it. Its leaves are closed
objects without the tag field, so they'd reject every tagged document. So under an honored root `@discriminator` the
implicit is not used and the run is grouped by texts, or stays flat. That's today's behavior for the implicit. A
provided *leaf* under `@discriminator` is a compile error today; the sub-trait doesn't get one, because the leaves are
there to fall back on. The fallback isn't silent (B28-R6). When the root has `@discriminator` and a sub-trait has an
implicit, the macro warns: "the implicit Schema[Sub] is not used inside Fam, which has @discriminator: Sub's leaves are
derived (add the same @discriminator to Sub, or give Fam a Schema)". It's a compile-time warning on both versions,
even though a profile that ignores the annotation (json4s and the library profiles) will use the implicit.

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
  `Parent`. The annotated-discriminator case falls back to the leaves, consumers keep every leaf fact, and L-c stays
  possible.
  Con: a sub-trait whose leaves can't be derived (a non-case class leaf, the usual reason to write the implicit) still
  doesn't compile inside the family. That's no regression: it doesn't compile today either.
- **P2**: an implicit stops the derivation (rule 3 only applies "otherwise"), and the sub-trait is one `Provided`-like
  member. This rescues underivable sub-trees. But consumers see a non-leaf (B's problems), the annotated discriminator
  has nothing to fall back to (a compile error for code that compiles today), and L-c becomes impossible. Lost. If the owner prefers P2, it's a local change: the macro skips the leaves when `provided` is set, and the
  member gets a new `Kind`.
- **Own item** (the lead's alternative): implement only the texts here and file the implicit as a separate item. It
  would touch the same macro lines (`resolveSumTypeRecursively`, `Sums.member`), the same new `Parent` and the same
  lowering loop, so splitting it means designing the data twice. Recommended against, but the split is clean: drop
  `Parent.provided` and the implicit parts of plan steps 2b, 3c-3e and 4a-4e.

### Legacy: flat texts, honored implicit (L-a, WON) vs. group too (L-b, LOST) vs. 0.7 for both (L-c, LOST)
The lead's policy freezes Legacy's *style*, sum shapes included, and fixes its *bugs*.
- **The texts** are style. Grouping changes the sum shape, and the flat leaves validate the same documents. So Legacy
  keeps the flat output, texts dropped, as 0.7.
- **The implicit** is a bug fix (B28-R1). Ignoring a hand-written `Schema[Sub]` can make the family reject what the codec
  writes. So Legacy honors it too, like every other profile, as Legacy already does for an implicit *leaf* (0.7 used
  it). No golden-legacy file has a sub-trait with an implicit, so no golden-legacy file changes.
- **L-c**, the alternative for the lead / owner: Legacy ignores the implicit too (0.7 byte for byte). That would be one
  more knob value (`SubFamilies.Flattened` also dropping `provided`) or a separate knob.

The texts part is a profile knob, so either texts answer is a one-word change:

```scala
sealed trait SubFamilies
object SubFamilies { case object Grouped extends SubFamilies; case object Flattened extends SubFamilies }
def subFamilies: SubFamilies = SubFamilies.Grouped    // Legacy overrides: Flattened
final def withSubFamilies(x: SubFamilies): JsonSchemaProfile
```

`subFamilies` governs only the texts groups. An implicit is honored under both values. L-b (Legacy `Grouped` too) is
the alternative: the lead can pick it by deleting Legacy's override. Users who want flat texts under any profile write
`.withSubFamilies(SubFamilies.Flattened)`.

## Decision
A + P1 + L-a. Each leaf of a family records the sealed sub-traits it sits under (names, signature, texts, implicit
schema). The profile does two things with that:
- It groups a titled or described sub-trait's leaves into a nested `oneOf`/`anyOf`/titled `enum` carrying its texts.
  An enum family stays one `enum`; a recursive sub-trait's group stays inline.
- Under every profile, it uses the sub-trait's implicit `Schema` in place of its leaves. The exception is an honored
  root `@discriminator`, where the macro warns.

A knob `subFamilies` (default `Grouped`, Legacy `Flattened`) keeps Legacy's texts flat. Binary-breaking (new `Member`
field): ship with B3 in 0.10.0, after B1 and B3 have merged.

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
      `Legacy` overriding it with `Flattened`. Scaladoc the knob with the rules from option A: it governs the texts
      groups only, and an implicit is honored under both values.
   b. `familyAt`: today's fold over `f.members` becomes `lowerRun(members, depth): List[Schema[_]]`. It walks members
      in order. A parent is *effective* when
      - its `provided` is usable (no honored discriminator, under every `subFamilies` value), or
      - it has texts, `subFamilies == Grouped`, and the family isn't an *enum family*: all members singletons,
        `asString` and not `enumsAsOneOf`, i.e. the family would lower to one `enum` (R2).

      For a member with an effective parent at `depth`, it takes the maximal contiguous run sharing that parent (by
      `sig`). It emits `apply(provided)` when `provided` is usable. Otherwise it emits the group of
      `lowerRun(run, depth + 1)`:
      - one merged `enum` → that enum with the texts;
      - else `oneof(_, tagField)`, or, untagged only, `anyof` per B3's overlap check *among the run*.

      Members with no effective parent at `depth` go through today's entry logic (`Single` / `Merged`). Merging
      singletons into one `enum` stays within one run. The texts go on the group with `duplicate(title = ...,
      description = ...)`. A provided run gets none (an implicit is used as it is, as for a `Provided` member).
   c. The final `schemas match` (one enum / singletons-only `oneof` / `anyof` / `oneof`) applies to the top run, as
      today. `singletonsOnly` still reads all `f.members` (the leaves). An enum family with a provided run gives
      `List(enum, provided)`, hence the singletons-only `oneof`, as intended (R2: an `enum` is split only for a schema).
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
      leaf: Scala 2's `UImplicits` re-search that shadows `json.Json.auto.derived` (~L64-70), so `Json.auto` never
      counts. Test it.
   d. Evaluate each `provided` once (R5): the family's tree binds every distinct parent to a local `val` and the
      members' `parents` lists refer to it:
      `{ val p0 = $F.Parent(..., Some(<implicit>)); $F.family[T](List($F.Member(..., parents = List(p0)), ...), ..) }`.
      N leaves then cost one evaluation of `implicit def s = Json.schema[Sub]`, not N.
   e. Warning (R6): when the root has `@discriminator` and some parent has `provided`, `c.warning(c.enclosingPosition,
      "the implicit Schema[Sub] is not used inside Fam, which has @discriminator: ...")`.
   f. Recursion (R3): the recursion pass over the members' schemas is unchanged. A recursion definition of a
      sub-trait is never a `provided`; the parent keeps only its texts.
   g. `Either` (`eitherOneOf`) is unchanged.
4. **Scala 3 derivation** (`core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/`):
   a. `Sums.member`, `isSealed` branch: also `lookupImplicit(c.tpe)` → a new `SumMember.provided: Option[Node]`. The
      nested `Sum` is still built, for the leaves. `lookupImplicit` runs under `AutoGuard.searching`, so a nested
      `Json.auto.derived` declines (AutoGuard.scala). A `derives json.Schema` on the sub-trait is a real given and
      counts (R4).
   b. `Sum.leaves` → `leavesWithParents: List[(SumMember, List[SumMember])]` (the enclosing `NestedFamily` members).
      It looks through `nestedSum`'s `Def(_, _, s: Sum, _)` (a recursive sub-trait, R3), as `leaves` does; that
      parent has no `provided`. `validateDiscriminator` keeps using the leaves.
   c. `Sum.children` / `mapChildren` include the `provided` nodes, so the recursion pass sees them.
   d. `FamilyEmit.emitSumFamily` emits `Schema.family.Parent(name, scala2FullName, sig, title, description,
      provided)` per enclosing member (`emit(provided)`), the same values Scala 2 gives (`name` = simple name, `sig` =
      `signature(tpe, dealiased = true)`). Evaluated once (R5): bind each distinct `Parent` in a `Block` of `ValDef`s
      around the `family` expression and refer to the vals from the members.
   e. Warning (R6): as Scala 2, `report.warning(..., Position.ofMacroExpansion)` when the root has `@discriminator`
      and a parent has `provided`.
   f. `Enums.singletonLeaves` returns the parents too, `EnumFamily` items carry them, and `emitEnumFamily` emits them
      (the `En` case). Scala 3 `enum`s have no nested sealed types: `Nil`.
   g. Texts: `typeAnnotations(c.tpe).texts`, as `NestedFamily` already records. Check `docsFromClasspath` matches what
      B12 picked for nested types (B12-R6).
5. **Core utilities**:
   a. `AsTree` (family case): print `parents` per member.
   b. `DefinitionNames`: no change (R7). It runs on the lowered tree (each `AsDraftXX.render` lowers before
      `withDefinitionNames`), where `family` and its `parents` are gone and a used `provided` is an ordinary `def`.
      Collecting `provided` from an unlowered tree would reserve names of definitions that aren't rendered (the
      discriminator fallback).
   c. `SchemaEquality` (family case): it compares `lm.copy(schema = rm.schema) != rm`, so `parents` takes part. Add
      the `provided` schemas to the structural compare like member schemas.
6. **Tests**:
   a. New shared `api/src/test/scala/com/github/andyglow/jsonschema/SubFamilySpec.scala`, models in its companion:
      - `Fam` (titled + described sub-trait) and `DocFam` (scaladoc only; Scala 2 reads it via the sbt-scaladoc plugin
        the api tests use);
      - `En` (all singletons: stays one `enum`, the texts dropped), `En` under `EnumsAsOneOf` (a titled `oneOf` of
        `const`s), and `En` with an implicit `Schema[G]` (`oneOf[enum[Z], $ref]`);
      - a mixed family with a case-object sub-trait (`OA` at the root, `OB` in a titled `OSub`, `OC`);
      - two levels (`S1 > S2` both titled; untitled `S1` > titled `S2`);
      - the recursive sub-trait `RFam`/`RSub` (R3): an inline titled group, the definition in `RC.s` unchanged, on
        both versions;
      - `Imp` (implicit `Schema`), an implicit `Predef[Sub]` (inlined), and a hand-written `Schema[Sub]` of another
        shape (a `string`). That one proves the family now accepts what a custom codec writes (R1), under Neutral and
        Legacy;
      - `Imp` with `@discriminator` on the root: falls back to the leaves and warns. The model sits under
        `@nowarn("msg=is not used inside")`, so the build stays warning-free. Check on 2.12, 2.13 and 3.3 that the
        filter catches the macro's warning;
      - an implicit `Schema[Sub]` defined as `implicit def` (evaluated once: a counter in the def, R5);
      - a diamond leaf, an interleaved declaration order, and `import Json.auto._` around an untitled nested family
        (output unchanged).

      Assert per model:
      - the tree (`family` members with their `parents`) on both versions;
      - Neutral draft 07 JSON (whole document);
      - `JsonSchemaProfile.Legacy`: today's flat output for texts; the `$ref` for an implicit (R1);
      - a `WrapperObject` profile and a `Discriminator("type")` profile (`Neutral.withSum(..)`): the group holds the
        wrapped / tagged leaves;
      - validity via `petshop.JsonValidator` (networknt; canceled on JS): sample documents valid
        (`{"c":1}`, `{"a":true}`, `"X"`), invalid (`{"x":1}`, `"Q"`) under Neutral, Legacy and the two styles;
      - `withSubFamilies(Flattened)` on Neutral equals today's Neutral output for the texts models.
   a'. Scala 3 only, `api/src/test/scala-3/.../SubFamilyDerivesSpec.scala` (R4): `sealed trait Sub extends Fam derives
      json.Schema` (and `Fam derives json.Schema` too): `Fam`'s schema has a `$ref` to `Sub`'s definition, Neutral
      and Legacy.
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
     `@title`/`@description`/scaladoc give its alternatives a nested group, except in a family of case objects only
     (one `enum`: the sub-trait's texts are dropped). An implicit `Schema` of a sub-trait (also `derives json.Schema`
     on Scala 3) is used in the family (definition + `$ref`), except under the root's `@discriminator` (a warning).
     Include the Neutral `Fam` example.
   - `docs/src/main/paradox/rendering.md` (profiles): the `subFamilies` knob (texts only), Legacy = `Flattened`.
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
  - in a mixed family, a titled sub-trait of case objects becomes its own titled `enum`. Its values leave the
    root-level singletons' `enum` (`oneOf[enum[OA, OB], OC]` → `oneOf[enum[OA], {title, enum[OB]}, OC]`). The same
    values validate. Families of case objects only stay one `enum` (R2);
  - a sub-trait with an implicit `Schema` in scope: a `$ref` to its definition in place of its leaves, unless the root
    has `@discriminator`. The outer group may become an `anyOf` (B3: a `$ref` isn't provably disjoint). A hand-written
    implicit of another shape now makes the family accept what the codec writes (a fix);
  - **Scala 3 `derives json.Schema` on a sub-trait** (R4): that's a given `Schema[Sub]`, so every such sub-trait turns
    from inline leaves into a `$ref` + definition. This is likely the most common change of the item, because Scala 3
    users often write `derives` on every type of a hierarchy. It's correct by the core rule (leaves with `derives` are
    already `$ref`s today);
  - a new compile-time warning under a root `@discriminator` with an implicit sub-trait `Schema`.
- **Legacy**: texts unchanged (`subFamilies = Flattened`). An implicit sub-trait `Schema` (also `derives`) is honored,
  a bug fix under the lead's Legacy policy (R1). No golden-legacy file changes. Alternatives L-b and L-c above.
- **Core rule**: restored for sub-traits (an implicit `Schema` of a sub-trait becomes definition + `$ref`), with one
  documented exception (an honored root `@discriminator`).

## Open questions for the owner
1. Ship in 0.10.0 with B3 (recommended), or docs-only (D) for 0.9.x? Either way the implementation needs B1 (#469) and
   B3 (#470) merged first.
2. Legacy: flat texts + honored implicit (L-a, recommended), group too (L-b), or 0.7 for both (L-c)?
3. Implicit: P1 (recommended) or P2 (rescue underivable sub-trees, but consumers see a non-leaf)?
4. Should scaladoc alone (no annotation) trigger a group? Recommended yes: a type's scaladoc is its description
   everywhere else (B12). The alternative is "annotations only", which would make scaladoc on a sub-trait the one
   place it doesn't count.
5. A family of case objects only with a titled sub-trait: stay one `enum` and drop the texts (recommended: codegen and
   form tools map an `enum` to an enum type), or split it into a `oneOf` of a titled `enum` and the rest (texts kept)?
   `EnumsAsOneOf` families group either way.
6. A recursive sub-trait: inline titled group (recommended, simple) or a `$ref` to its recursion definition?

## Review log
Round 1 (r-b28, on 4e311c4):
- B28-R1 (must) Legacy should honor the implicit (a hand-written one makes today's schema reject real data) → accepted:
  "Why" corrected; the implicit is honored under every profile, Legacy included; `subFamilies` governs only texts;
  alternative L-c; Legacy/Output sections, plan 2a/2b, a different-shape implicit test.
- B28-R2 (must) singleton groups turn an `enum` into a `oneOf` → accepted: an enum family stays one `enum` (texts
  dropped); `EnumsAsOneOf` and mixed families group; an implicit still splits; Output lists the mixed-family change; Q5.
- B28-R3 (must) recursive sub-trait → accepted: inline titled group (definition unchanged; B12 titles it later),
  `$ref` to the definition as the alternative (Q6); plan 3f/4b; test on both versions.
- B28-R4 (must) `derives json.Schema` on a sub-trait → accepted: listed as the main output change (also under
  Legacy); Scala 3 test 6a'; guards named (Scala 2 `UImplicits` auto shadowing, Scala 3 `AutoGuard.searching`).
- B28-R5 (nit) evaluate `provided` once → accepted: a local `val` per parent on both versions (3d, 4d), counter test.
- B28-R6 (nit) silent discriminator fallback; tag field never `anyof` → accepted: a macro warning (3e, 4e), test
  under `@nowarn`; stated that a tagged group is always `oneof(_, tagField)`.
- B28-R7 (nit) `DefinitionNames` runs on lowered trees → accepted: 5b is now "no change", with the reason.
- B28-R8 (nit) json4s `PerFamily` on `Sub` alone → accepted: a paragraph under the sum styles.
- B28-R9 (nit) owner questions → accepted: Q5 (singleton groups), Q6 (recursive), prerequisites repeated in Q1.
