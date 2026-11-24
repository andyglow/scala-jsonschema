# Same-shaped sealed members and the order of a family's members  (issue #461, item B3)
Status: approved (r-b3, 2026-11-23)

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
  It is rendered as `"anyOf"` on every draft (draft-04 has it). Unlike `oneOf`, it **always states its type**: when the
  members share one JSON type it is hoisted, at the root too (`oneOf` leaves it out at the root "as in 0.7"); otherwise
  each member keeps its own. Without that, a root `anyOf` of objects accepts `5` (r-b3's probe), because a non-object
  document satisfies every object member; `oneOf` only rejects it by accident ("2 are valid").
- Profile: a new knob `JsonSchemaProfile.overlappingMembers: OverlappingMembers` (`AnyOf`: the default; `OneOf`:
  always `oneOf`, the 0.7 output), with `withOverlappingMembers(...)`. `Legacy` sets `OneOf`; Neutral and the
  bridge profiles keep the default.
- The disjointness check (`Lowering`, after the members are built; conservative: "can't prove it" means overlap).
  Two members are disjoint when, after looking through `def` and `value-class` wrappers:
  - (tag) both are objects with different `discriminationKey`s and the `oneof` has a discriminator field;
  - (type) their JSON types can't meet: object (`object`, `dictionary`), array, string, boolean, null, integer,
    number (integer ⊂ number); an `enum` has its value schema's type (never `jsonType`, which is `"enum"`; an enum of
    numbers next to an `integer` member is an overlap), a `const` its value's type; a `ref` or a
    nested `oneof`/`allof`/`anyof` member is "any type";
  - (key) both are objects and one requires a field that is not among the other's properties at all (an optional
    property still counts as "has"), and the other is closed: a non-`Free` `object` (not a `dictionary`, not `Free`,
    not a `ref`). This covers `WrapperObject` (one required key per member; wrapper objects are built non-`Free` by
    `object.fromList`) and most untagged families (distinct field names);
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
`EnumsAsOneOf` variant) and for the order of families of 5+ members. No signature changes. The model says what the
output says (`anyof` renders `anyOf`), and a hand-written `Schema.anyof.of(...)` comes with it (refined's `Or`, which is
inclusive, can use it later).
Cons: a new case in the sealed `Schema` ADT is **breaking under PVP** (user code that matches on every case, e.g. a
custom renderer, is no longer exhaustive): 0.10.0. A new knob. The check is a heuristic (conservative: a false
"overlap" gives `anyOf`, which validates the same documents as a correct `oneOf` would; it can never make a valid
schema invalid).

### F. Like A, but an inclusive marker on `oneof` instead of a new case  — LOST (close second)

The codebase already marks open objects this way: `object[T] with Free` (Schema.scala:321-337, `isFree` in `equals`,
its own `mkCopy`). Here: `sealed trait Inclusive { this: oneof[_] => }`, built by `oneof.inclusive(...)` and by the
lowering, rendered as `"anyOf"`. Everything else (order, the disjointness check, the knob, the root `type`) as in A.
Pros: no new ADT case, so user matches stay exhaustive; a third-party renderer that ignores the marker prints today's
`oneOf`; source and binary compatible: a patch (0.9.x).
Cons: every rebuild must keep the marker (`mkCopy`, `duplicate`/`withExtraFrom`, `discriminatedBy`, `deepCopy`,
`Lowering.apply`, `optionAt`), the same discipline `Free` needs; `oneof` then doesn't always mean `oneOf` (a
permanent naming wart in the public model, and walking code that matches `oneof` silently treats an `anyOf` as
exclusive); a third-party renderer keeps producing the invalid schema without any signal.
Why it lost: it trades a one-time, visible break for a permanent, silent one. And the next release is likely 0.10.0
anyway: E2 (#295/#289/#286) adds a new model node ("any JSON value") as well. **If the owner wants B3 in a 0.9.x
patch, F is the choice**: it changes only steps 1 and 3 of the plan (a marker instead of a class; the renderer checks
the marker), everything else stays.

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
(`OverlappingMembers.AnyOf` by default, `OneOf` for Legacy). A is breaking under PVP (0.10.0). For the owner:
- A in 0.10.0 (recommended; E2 adds a model node too);
- F if B3 must ship in a 0.9.x patch (same behavior, a marker on `oneof` instead of a new case);
- C alone (order only) if neither: a pure patch, and A or F can follow.

## Not in scope (same kind of bug, elsewhere; reported to the lead)
- `Either[Int, Double]` → `oneOf [{value: integer}, {value: number}]`: `{"value": 1}` is rejected; `Either[String, String]`
  collapses. Either isn't a family (the macros build its `oneof` directly): B14.
- refined `A Or B` is inclusive but renders `oneOf`: `String Refined (StartsWith["a"] Or EndsWith["b"])` rejects `"ab"`
  (RefinedMacro.scala:378, AST.scala:192). With `anyof` in place it's a one-line follow-up: B14.
- A root `oneOf` states no `type` (AsDraftSupport.scala:180, "as in 0.7"): root `Ev` (one member) accepts `5` and `"s"`
  today. A renderer bug of `oneOf`, not changed here (the new `anyOf` doesn't have it, see A): B15.

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
3. `core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala`: `mkAnyOf` (share `mkPlainOneOf`'s member
   typing in a helper taking the keyword and a `typeAtRoot` flag: `false` for `oneOf`, unchanged; `true` for `anyOf`);
   `mkNullable` treats an `anyof` X like a `oneof` X (not a single type: `oneOf [anyOf[...], {"type": "null"}]`);
   dispatch; `extractDefs`; the no-validations case at line 57.
   `AsTree`, `SchemaEquality`, `DefinitionNames`: the `anyof` case, as for `allof`.
4. `core/src/main/scala-3/.../derivation/Enums.scala` `emitOneOf`: `ListSet.from($ms)` (Either, 2 members: no output
   change, for consistency). The Scala 2 macro's `OneOf` (Either only) and refined's `Or` build 2-element `Set`s, which
   keep order: unchanged. No macro change otherwise (the derived `family` tree is unchanged).
5. Tests
   - `SameShapeFamilySpec` (already committed, failing): order under Neutral, a library-like untagged profile,
     WrapperObject, Discriminator, plus Legacy (order only: Legacy keeps `oneOf`); validity of `Ev`, `DocEv`, `RefEv`,
     `OvEv` under untagged profiles; tagged twins kept and valid. Add: `withOverlappingMembers(OneOf)` gives the
     old `oneOf`; Legacy's same-shape output equals today's bytes; the README `Event` example validates;
     `Option[OvEv]` as a field: `null`, `{"id":"x"}` valid, `5` invalid; root and nested `DocEv`/`OvEv` reject `5`,
     `"s"`, `[]` and (not an Option) `null`.
   - order-sensitive Legacy assert on one of the 5-member golden-legacy models (`MultiLevelSealedTraitRoot`), whose
     expected order is declaration order (the lead's answer to R4).
   - guard tests that ordinary families stay `oneOf`: a WrapperObject family, an untagged json4s-like
     (`honorsSchemaAnnotations = false`) family of distinct shapes, a recursive family (`Leaf(v)`, `Node(l, r)` with
     `ref` fields).
   - core `FamilyLoweringSpec`: one tree case per disjointness rule (tag, type, key, field, values) and its negative,
     including an enum of numbers next to an `integer` member (overlap).
   - `AsDraft04/06/07/09/12Spec`: render an `anyof.of(...)` (full JSON per draft).
   - `SchemaCopySpec` / `SchemaEqualsHashCodeSpec`: `anyof`.
   - harness: `Golden` tree printer and `Families.as07` get the `anyof` case (sorted members, like `allof`).
   - goldens (Neutral `golden/` only): `FooBarInsideCompanionWithAnnotations` and
     `...WithAnnotations.EnumsAsOneOf` switch `oneOf` → `anyOf` (`GOLDEN_UPDATE=1 sbt clean api/test`, review the
     diff); `golden-legacy/` unchanged (the harness sorts arrays, and Legacy keeps `oneOf`).
   - Scala.js: order tests run on JS; validation tests cancel on JS (existing harness).
   - bridges: re-run the json4s petshop acceptance (Untagged, real codec output) and the other bridges' suites; the
     PR lists the result.
6. Docs (README and paradox `types.md` / `rendering.md`, the same words): sealed families render as `oneOf`, or
   `anyOf` when members may match the same document; members with identical schemas appear once (the one remaining
   collapse); members in declaration order (remove "keeps that order up to 4 entries ... hash order"); only `.of(...)`
   and derived families are ordered, a hand-built `oneof(Set(...))` keeps its `Set`'s order; the README `Event`
   example's comment becomes `anyOf`; the new knob in the profiles section. Release notes are the lead's (no
   CHANGELOG edit).

## Compatibility

- Source: one new case class in the sealed `Schema` ADT. Code that pattern-matches on every `Schema` case gets a
  non-exhaustive-match warning (an error under `-Xfatal-warnings`). New trait method with a default on
  `JsonSchemaProfile` (user implementations compile unchanged). No signature changes: `oneof.subTypes` stays a `Set`.
- Binary: additions only (a new class, a new default method on a trait, a new `Knobs` field in a private class).
- Output:
  - all profiles, Legacy included: families of 5+ members in declaration order (today: hash order); also hand-built
    `oneof.of`/`allof.of` of 5+ members. Legacy gets declaration order too, as in B7/B2 (**the lead's decision,
    2026-11-23**): 0.7's order was hash order that differed between 2.12 and 2.13 (no bytes a user could rely on
    across versions). The PR's output-change section calls it out.
  - Neutral and the bridge profiles: an untagged family whose members may overlap renders `anyOf` instead of `oneOf`
    (today such a schema rejects valid documents). Tagged families: unchanged except for order.
  - Legacy: byte-identical to today except the order above (it keeps `oneOf`, collapse included).
- PVP: A is **breaking** (a new case in an exported sealed ADT): 0.10.0. F is the same behavior as a patch (0.9.x).
  C alone is a pure patch. The owner picks (see Decision).

## Review log
Round 1 (r-b3, design at eb0306e):
- B3-R1 (must) straight PVP verdict → accepted: A is breaking → 0.10.0; Decision and Compatibility lay out A (0.10.0) /
  F (0.9.x) / C (patch).
- B3-R2 (must) option F, an inclusive marker on `oneof` → accepted: added as F, "close second", with the conditions under
  which it wins (B3 in a 0.9.x patch). A stays the winner: a visible one-time break beats a permanent silent wart, and
  E2 adds a model node anyway.
- B3-R3 (must) `anyOf` must state its type at the root → accepted: `anyOf` always states its type (hoisted at the root
  too, or on each member); plan step 3 and tests (root/nested reject `5`, `"s"`, `[]`, `null`) updated. The root
  `oneOf` hole is listed under "Not in scope".
- B3-R4 (must) Legacy order needs the lead's OK → accepted: asked the lead; answer (2026-11-23): Legacy gets declaration
  order, called out in the PR. An order-sensitive Legacy test (declaration order) is in the plan.
- B3-R5 (nit) rule wording + guard tests → accepted: (key) and (type) reworded; guard tests (WrapperObject, json4s-like,
  recursive) and the enum-of-numbers case added.
- B3-R6 (nit) docs on what still collapses → accepted (plan step 6).
- B3-R7 (nit) "Not in scope" list → accepted: new section.
- B3-R8 (nit) `Option[OvEv]` and bridges → accepted: `mkNullable` handling in step 3, assertions and the bridge
  acceptance re-run in step 5.
