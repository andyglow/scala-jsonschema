# Same-shaped sealed members and the order of a family's members  (issue #461, item B3)
Status: draft

## Problem

A sealed family is lowered by the profile (`JsonSchemaProfile.Lowering.familyAt`) to a `Schema.oneof`, whose members
are a `Set[Schema[_]]` (`Schema.scala:473`). The family's members arrive as a `List` in declaration order; the
lowering ends with `oneof(schemas.toSet, tagField)` (`JsonSchemaProfile.scala:348-349`). The renderer
(`AsDraftSupport.mkPlainOneOf`) writes `subTypes.toSeq` as `"oneOf"`. Three defects come out of that.

Repro: `api/src/test/scala/com/github/andyglow/jsonschema/SameShapeFamilySpec.scala` (committed, fails on 2.12.21,
2.13.18 and 3.3.9; validation by networknt, draft-04/07/2020-12). Models:

```scala
sealed trait Ev;    case class Created(id: String) extends Ev;  case class Deleted(id: String) extends Ev
sealed trait DocEv; @description("created") case class DCreated(id: String) extends DocEv
                    @description("deleted") case class DDeleted(id: String) extends DocEv
sealed trait RefEv; case class RCreated(id: String) extends RefEv   // each with an implicit Schema in its companion
                    case class RDeleted(id: String) extends RefEv
sealed trait OvEv;  case class OCreated(id: String) extends OvEv;  case class OUpdated(id: String, name: Option[String]) extends OvEv
sealed trait Five;  case class E1(a: Int), D2(b: Int), C3(c: Int), B4(d: Int), A5(e: Int)   // all extend Five
```

Default profile (Neutral), draft-07; Legacy gives the same results (the same bytes, except that its `Option`
field `name` isn't nullable):

| model | rendered | `{"id":"x"}` (networknt) |
|---|---|---|
| `Ev` | `oneOf` with **one** member `{id: string}`: the two equal schemas collapse in the `Set` | valid |
| `DocEv` | `oneOf` of two members that differ only in `description` | **invalid**: "must be valid to one and only one schema, but 2 are valid" |
| `RefEv` | `oneOf: [{$ref RCreated}, {$ref RDeleted}]`, equal bodies | **invalid**, same message |
| `OvEv` | `oneOf: [{id}, {id, name?}]` | **invalid**, same message |
| `Five` | members in hash order: `D2, B4, C3, E1, A5` (2.13, 3); `B4, D2, C3, E1, A5` (2.12) | n/a |

1. **Collapse (lossy).** Equal member schemas collapse into one `oneOf` entry. `Schema.equals` includes title,
   description and discriminationKey, so this only happens when the members are equal in every respect.
2. **Unsatisfiable `oneOf` (invalid).** `oneOf` requires that *exactly one* member matches. Under
   `SumStyle.Untagged` (Neutral, Legacy, json4s without type hints) nothing tells the members apart in the JSON, so
   whenever two members accept the same document, that document is rejected. Same-shaped members that are *not*
   equal (texts, scaladoc, `@definition`, an implicit `Schema` per member) hit it, and so do members that merely
   overlap (`OvEv`). It also happens in the README's own example (`Created(id)`, `Deleted(id, reason: Option[String])`)
   and in an existing golden model, `SumTypeModels.FooBarInsideCompanionWithAnnotations`: its members include an
   `enum ["M4","M5"]` and a value class of `String` (`{"type": "string"}`), so `"M4"` is rejected today.
3. **Order.** With 5 or more members the `Set` is a `HashSet`: hash order, different on 2.12 than on 2.13/3. The
   derivations already deliver declaration order (Scala 2 sorts `knownDirectSubclasses` by source position,
   `UCommons.inDeclarationOrder`); only the `Set` loses it. The README documents the hash order as a known limit.

Tagged styles (`WrapperObject`, `Discriminator`) are not affected by 1 and 2: every member carries a distinct tag
(probe: both twins kept, each member's documents valid). They are affected by 3.

The golden harness sorts arrays (`harness/Golden.scala:19-22`), so no golden file sees the order; order needs
order-sensitive asserts.

## Why it is a problem

- 2 is an invalid schema: a validator built from it rejects real documents of the family. It hits anyone whose
  members share a shape or overlap under an untagged style, which is common for event/command families
  (`Created(id)`, `Deleted(id)`, `Updated(id, ...)`).
- 1 hides a member from anyone reading or generating code from the schema. It is not a validation problem, but
  whether it happens depends on unrelated details (add a scaladoc to one member and the family becomes 2).
- 3 makes the output order depend on the member count and the Scala version.

The obvious fix for 1 ("keep both: make `subTypes` a `Seq`") turns every `Ev`-like family from 1 into 2: valid
documents get rejected. So 1 and 2 have to be fixed together.

## Options

The order fix (3) is the same in every option except D: build the member set as an insertion-ordered
`immutable.ListSet` (a `Set`, so no signature change; insertion order on 2.12, 2.13 and 3). This is the choice B7
(#458, `enum` values) made for the same problem in `enum.values`; both items should use the same collection.

### A. Ordered members; `anyOf` when the family's members may overlap  — WON

The lowering keeps `oneOf` when it can prove that no document matches two members (the usual case, and always the
case for tagged styles), and emits **`anyOf`** otherwise. `anyOf` means "at least one member matches", which is what
an untagged sum really is. Every member is kept (each with its texts, its `$ref` and its definition), and every
document of every member is valid.

- Model: a new node `Schema.anyof[T](subTypes: Set[Schema[_]])`, a sibling of `allof` (with `anyof.of(x, xs*)`).
  It is rendered as `"anyOf"` on every draft (draft-04 has it), with the same shared-`type` hoisting as `oneOf`.
- Profile: a new knob `JsonSchemaProfile.overlappingMembers: OverlappingMembers` (`AnyOf`: the default; `OneOf`:
  always `oneOf`, the 0.7 output), with `withOverlappingMembers(...)`. `Legacy` sets `OneOf`; Neutral and the
  bridge profiles keep the default.
- The disjointness check (`Lowering`, after the members are built; conservative: "can't prove it" means overlap).
  Two members are disjoint when, after looking through `def` and `value-class` wrappers:
  - (tag) both are objects with different `discriminationKey`s and the `oneof` has a discriminator field;
  - (type) their JSON types can't meet: object (`object`, `dictionary`), array, string, boolean, null, integer,
    number (integer ⊂ number); an `enum` has its value schema's type, a `const` its value's type; a `ref` or a
    nested `oneof`/`allof`/`anyof` member is "any type";
  - (key) both are objects and one requires a field the other doesn't have and the other is closed (not `Free`);
    this covers `WrapperObject` (one required key per member) and most untagged families (distinct field names);
  - (field) both are objects and a field required by both has types that can't meet by the (type) rule;
  - (values) both are `enum`/`const` with no value in common.
  A family with a single member, or whose members are pairwise disjoint, stays `oneOf`.
- Exact twins (`Ev`) stay one entry: their schemas are equal, the `Set` keeps one, and one member is trivially
  disjoint: output unchanged. Both entries would be identical (no title, no name), so keeping two would add
  nothing a reader could use; it would need `subTypes: Seq` (see D).

Sample (Neutral, draft-07):

```json
// DocEv
{"anyOf": [{"description": "created", "additionalProperties": false, "properties": {"id": {"type": "string"}}, "required": ["id"]},
           {"description": "deleted", "additionalProperties": false, "properties": {"id": {"type": "string"}}, "required": ["id"]}]}
// OvEv
{"anyOf": [{"additionalProperties": false, "properties": {"id": {"type": "string"}}, "required": ["id"]},
           {"additionalProperties": false, "properties": {"id": {"type": "string"}, "name": {"type": ["string", "null"]}}, "required": ["id"]}]}
// Five: oneOf, members E1, D2, C3, B4, A5 (declaration order)
```

Pros: fixes 1, 2 and 3; every valid document validates; nothing is dropped except exact duplicates; output changes
only for families that are invalid today (in our goldens: `FooBarInsideCompanionWithAnnotations` and its
`EnumsAsOneOf` variant) and for the order of families of 5+ members. No signature changes.
Cons: a new model node (code that matches on every `Schema` case gets a non-exhaustive-match warning); a new knob;
the check is a heuristic (conservative: a false "overlap" gives `anyOf`, which still validates the same documents
as a correct `oneOf` would; it can never turn a valid schema invalid).

### B. Ordered members; merge same-shaped members into one entry  — LOST

Under `Untagged`, members whose schemas are equal once title/description/`def` are stripped are merged into one
entry (the first one), on purpose. Fixes `DocEv` and `RefEv` (one `$ref` left; the other definition disappears
unless used elsewhere) and 3.
Lost: drops texts, a `$ref` and a definition silently (makes 1 worse, on purpose); doesn't fix overlapping members
(`OvEv`, the README example, `FooBarInsideCompanionWithAnnotations`); which member "wins" is arbitrary.

### C. Ordered members only (ListSet), keep everything else  — LOST

Fixes 3, no output change otherwise. Smallest change, a patch.
Lost: leaves 2 (invalid schemas) as it is; 1 stays as documented behavior. It is the fallback if the owner doesn't
want a new node: it can ship alone and A can follow.

### D. `subTypes: Seq`, no dedupe  — LOST

What the gap note suggested ("ordered, non-deduplicated members"). Fixes 3 and keeps exact twins as two entries.
Lost: under `oneOf` it turns every exact-twin family (valid today) into an unsatisfiable one, i.e. 1 becomes 2.
Only sensible combined with A, and then it buys two identical entries for a source- and binary-breaking change of a
public case-class field (PVP: 0.10.0).

### E. `anyOf` for every untagged family  — LOST

No check: `Untagged` always renders `anyOf`. Simplest rule, fixes 1 (with D) and 2.
Lost: changes every Neutral/Legacy-like output with a sealed family (the large majority of families are disjoint
and correct today), and drops the "exactly one" information that `oneOf` gives readers and code generators.

## Decision

A: build every family's members as an insertion-ordered set (declaration order, all profiles), and lower a family to
`anyOf` instead of `oneOf` when its members can't be proven disjoint, controlled by a new profile knob
(`OverlappingMembers.AnyOf` by default, `OneOf` for Legacy). If the owner rejects the new node, C is the fallback
(order only), with A's check kept for a later release.

## Implementation plan

1. `core/src/main/scala/json/Schema.scala`
   - new `final case class anyof[T](subTypes: Set[Schema[_]])` (mirrors `allof`: `mkCopy`, `equals`, `jsonType = "anyof"`,
     `toString`), `object anyof { def of(x, xs*) }`; `deepCopy` handles it;
   - `oneof.of`, `allof.of`, `anyof.of`: `ListSet((x +: xs): _*)` instead of `.toSet`; `deepCopy` keeps the order
     (`ListSet(ys.toSeq.map(deepCopy): _*)`, not `ys map`, which on 2.12 may rebuild a hash set).
2. `core/src/main/scala/json/schema/JsonSchemaProfile.scala`
   - `sealed trait OverlappingMembers { AnyOf, OneOf }`, `def overlappingMembers = AnyOf`, `withOverlappingMembers`, the
     `Knobs` field; `Legacy` = `Knobs("legacy", OptionStyle.Legacy, overlappingMembers = OneOf)`;
   - `familyAt`: `ListSet(schemas: _*)`; when `p.overlappingMembers == AnyOf` and some pair isn't disjoint,
     `anyof(ListSet(schemas: _*))` (no discriminator field; tagged families never get here). A private `Disjoint`
     helper with the rules above; scaladoc on the knob states them;
   - `Lowering.apply`: `oneof`/`allof`/`anyof` map their members into a `ListSet` in order; `optionAt` builds `ListSet`s
     (2 members: order unchanged);
   - update the comment at the top of the sealed-families section (Sets and insertion order).
3. `core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala`: `mkAnyOf` (factor `mkPlainOneOf`'s body into
   one helper taking the keyword); dispatch; `extractDefs`; the no-validations case at line 57.
   `AsTree`, `SchemaEquality`, `DefinitionNames`: the `anyof` case, as for `allof`.
4. `core/src/main/scala-3/.../derivation/Enums.scala` `emitOneOf`: `ListSet.from($ms)` (Either, 2 members: no output
   change, for consistency). The Scala 2 macro's `OneOf` (Either only) and refined's `Or` build 2-element `Set`s, which
   keep order: unchanged. No macro change otherwise (the derived `family` tree is unchanged).
5. Tests
   - `SameShapeFamilySpec` (already committed, failing): order under Neutral, a library-like untagged profile,
     WrapperObject, Discriminator, plus Legacy (order only: Legacy keeps `oneOf`); validity of `Ev`, `DocEv`, `RefEv`,
     `OvEv` under untagged profiles; tagged twins kept and valid. Add: `withOverlappingMembers(OneOf)` gives the
     old `oneOf`; Legacy's same-shape output equals today's bytes; `Option[OvEv]` (anyOf inside a nullable oneOf);
     the README `Event` example validates.
   - core `FamilyLoweringSpec`: one tree case per disjointness rule (tag, type, key, field, values) and its negative.
   - `AsDraft04/06/07/09/12Spec`: render an `anyof.of(...)` (full JSON per draft).
   - `SchemaCopySpec` / `SchemaEqualsHashCodeSpec`: `anyof`.
   - harness: `Golden` tree printer and `Families.as07` get the `anyof` case (sorted members, like `allof`).
   - goldens (Neutral `golden/` only): `FooBarInsideCompanionWithAnnotations` and
     `...WithAnnotations.EnumsAsOneOf` switch `oneOf` → `anyOf` (`GOLDEN_UPDATE=1 sbt clean api/test`, review the
     diff); `golden-legacy/` unchanged (the harness sorts arrays, and Legacy keeps `oneOf`).
   - Scala.js: order tests run on JS; validation tests cancel on JS (existing harness).
6. Docs (README and paradox `types.md` / `rendering.md`, the same words): sealed families render as `oneOf`, or
   `anyOf` when members may match the same document; members in declaration order (remove "keeps that order up to
   4 entries ... hash order"); the README `Event` example's comment becomes `anyOf`; the new knob in the profiles
   section. Release notes are the lead's (no CHANGELOG edit).

## Compatibility

- Source: one new case class in the sealed `Schema` ADT. Code that pattern-matches on every `Schema` case gets a
  non-exhaustive-match warning (an error under `-Xfatal-warnings`). New trait method with a default on
  `JsonSchemaProfile` (user implementations compile unchanged). No signature changes: `oneof.subTypes` stays a `Set`.
- Binary: additions only (a new class, a new default method on a trait, a new `Knobs` field in a private class).
- Output:
  - all profiles, Legacy included: families of 5+ members in declaration order (today: hash order); also hand-built
    `oneof.of`/`allof.of` of 5+ members. Same sub-decision as B7: Legacy gets declaration order too, because 0.7's
    order was hash order that differed between 2.12 and 2.13 (no bytes a user could rely on across versions). The
    alternative (Legacy keeps hash order: build a `HashSet` under Legacy) is a one-line change if the owner wants it.
  - Neutral and the bridge profiles: an untagged family whose members may overlap renders `anyOf` instead of `oneOf`
    (today such a schema rejects valid documents). Tagged families: unchanged except for order.
  - Legacy: byte-identical to today except the order above (it keeps `oneOf`, collapse included).
- PVP: by the letter, adding a case to a sealed ADT can break exhaustive matches; I'd ship it in 0.9.x as an
  addition, but it's the owner's call whether it waits for 0.10.0. Option C alone is a pure patch.

## Review log
