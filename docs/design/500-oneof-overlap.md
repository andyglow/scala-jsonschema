# `oneOf` for alternatives that can overlap: `Either` and refined `Or`  (issue #500, item B14)
Status: approved (r-b14, 2026-11-23)

## Problem

`oneOf` means "exactly one member matches". Two derived schemas use it for alternatives that are not exclusive, so a
document that matches two members is rejected, although it is a valid value of the Scala type.

Repro: `notes/scratch/c-b14/OneOfOverlapReproSpec.scala` (an api test, not committed) and
`notes/scratch/c-b14/OrProbeSpec.scala` (a refined test printing refined's trees). Validation by networknt
(draft-04, 07, 2019-09, 2020-12). Logs: `notes/scratch/c-b14/repro-*.log`, `refined-probe.log`. Same output and
results on 2.12.21, 2.13.18 and 3.3.9 (JVM; the schema is platform-independent).

```scala
case class OCreated(id: String)
case class OUpdated(id: String, name: Option[String])
case class HasNum(e: Either[Int, Double])
case class HasOv(e: Either[OCreated, OUpdated])

String Refined (StartsWith["a"] Or EndsWith["b"])
Int    Refined (Divisible[2] Or Divisible[3])
String Refined (IPv4 Or IPv6)            // refined has no IP predicate: users alias this (RefinedAliasedSpec)
String Refined Not[StartsWith["a"] Or EndsWith["b"]]
```

| model | rendered (draft-07, Neutral; Legacy the same) | document | networknt |
|---|---|---|---|
| `HasNum.e` | `{"type":"object","oneOf":[{…"value":{"type":"integer"}…},{…"value":{"type":"number"}…}]}` | `{"value":1}` | **invalid**, every draft: "must be valid to one and only one schema, but 2 are valid" |
| `HasOv.e` | `oneOf[{value: {id}}, {value: {id, name?}}]` | `{"value":{"id":"x"}}` | **invalid**, every draft |
| `StartsWith Or EndsWith` | `{"oneOf":[{"pattern":"^a.*$"},{"pattern":"^.*b$"}]}` | `"ab"` | **invalid**, every draft |
| `Divisible Or Divisible` | `{"oneOf":[{"multipleOf":2.0},{"multipleOf":3.0}]}` | `6` | **invalid**, every draft |
| `IPv4 Or IPv6` | `{"oneOf":[{"format":"ipv4"},{"format":"ipv6"}]}` | `"1.2.3.4"`, `"::1"` | **invalid** on 2019-09/2020-12 (`format` is an annotation there, so both members match); valid on 04/07 |
| `Not[… Or …]` | `{"type":"not","not":{"type":"string","oneOf":[…]}}` | `"ab"` | today every value is invalid (B22's `"type":"not"`); without it, `"ab"` is **valid**, but refined rejects it: `not(oneOf(a, b))` is not `not(a or b)` |
| guard: `Either[Int, String]` | `oneOf[{value: integer}, {value: string}]` | `{"value":1}`, `{"value":"a"}` | valid |

Where the `oneof`s come from:
- `Either`: both macros build it directly, not as a `family`: Scala 2 `USumTypes.eitherOneOf`
  (`macros/.../USumTypes.scala:104`), Scala 3 `Sums.emitSum` → `Enums.emitOneOf` (`derivation/Enums.scala:118`).
  `JsonSchemaProfile` never sees it, so B3's overlap check (#461, #470) doesn't apply. E7 (#220, design approved)
  replaces it with an `either` fact node lowered by a new `eitherStyle` knob; E7 runs B3's check for every style
  except `ValueWrapped` (the default: Neutral, Legacy, play), which it leaves to B14 (E7 doc, A5).
- refined `Or`: `Or.norm` falls back to `OneOf(t, l :: r :: Nil)`, which emits `` `oneof`[T](Set(...)) ``
  (Scala 2 `refined/AST.scala:191-213`, Scala 3 `refined/RefinedMacro.scala:375-400`). Numeric `Or`s that normalize to
  a range (`Less Or Greater` → `not(bounded)`, `Greater[5] Or Less[10]` → `integer`) don't reach it; every other pair
  does (strings always; `Divisible`, `Greater[5] Or Divisible[2]`). A nested `A Or (B Or C)` is a nested `oneof`
  (`norm` doesn't flatten it in practice: probe output).

Items folded into B14 by the lead, checked against B3's branch (`origin/gap/b3-sealed-collapse`, d97e12e):
- `SingletonStyle.AsTagged` + `SumStyle.Untagged` (json4s without type hints; r-d5 N1): case objects with texts render
  as `{"description": …, "additionalProperties": false}` each, so `{}` matches several members. On master: invalid
  ("2 are valid"). On B3's branch: `anyOf`, `{}` valid. **Already fixed by B3** (it is a family); B14 adds a regression
  test only. (Without texts the members are equal and appear once, valid on both.)
- E2's `Schema.any` (`{}`, #486; r-e2 R4) overlaps every member. B3's `Disjoint.types` treats every node it doesn't
  know as "any type", so an `any` member is already an overlap. B14 makes that case explicit and tests it.

Found in review (r-b14, B14-R1): `Option[X]` where X already admits `null`. `case class VOpt(x: Option[Int]) extends AnyVal`,
`case class HasVOpt(v: Option[VOpt])` renders `"v": {"oneOf": [{"type": ["integer","null"]}, {"type": "null"}]}`, and
`{"v": null}` is **rejected** ("2 are valid"), on master and on B3's branch, 2.13 and 3. `optionAt`'s `flatten` drops only a
top-level `option`, not one inside a `value-class`, and `mkNullable`'s single-type short-cut doesn't apply to
`value-class(oneof)`. The same for `Option[D]` where `D` has an implicit `Schema` whose body admits `null` (a `def` of a
`oneof{…, null}`). E2 fixes only the X-admits-*everything* case (`Option[JsValue]`).

### Every derived `oneof`, and what happens to it

| source | members can overlap? | verdict |
|---|---|---|
| `Either` (both macros today; E7's `eitherAt` later) | yes | B14 (step 2) |
| refined `Or` (both refined macros) | yes | B14 (step 1) |
| `optionAt`, `OptionStyle.Nullable`: `oneof{X, null}` | when X admits `null` | B14 (step 3, R1) |
| `optionAt`, `OptionStyle.Legacy`: `enum["None"]` vs `{value: X}` | never (a string vs an object) | nothing to do |
| `familyAt` (sealed families, Scala 3 enums, `Flag.EnumsAsOneOf` on both versions: they all go through `family`) | yes | B3 (#470); `AsTagged` + `any` members: tests in B14 (step 4) |
| a hand-built `Schema.oneof` | the user's choice | rendered as written ("exactly one" may be the point: an XOR); option B |
| the parser (`oneOf` in, `oneof` out; `[X, "null"]` → `oneof{X, null}`) | as written in the input | nothing to do |

### Not in scope (neighbours)
- B15: a root `oneOf` states no `type` (a root refined `oneOf` rejects `5` only because both members match it).
- B22: `Schema.not` renders `"type": "not"`, which makes `Not[...]` reject every value.
- refined numeric `Or` normalization (r-b14, sent to the lead): `Greater[7] Or Less[2]` / `Positive Or Less[-5]` become a
  `not{exclusiveMinimum a, exclusiveMaximum b}` that accepts every integer, and `Less[2] Or Greater[7]` keeps 2 and 7 as
  exclusive bounds, so it accepts 2. Those `Or`s never reach the `oneof` that B14 changes.

## Why it is a problem

- The schema rejects valid values: a service validating its output against it fails on `Left(1)` of an
  `Either[Int, Double]`, on `"ab"` of a `StartsWith["a"] Or EndsWith["b"]`, and on **every** value of the common
  `IPv4 Or IPv6` alias under 2019-09/2020-12 (r-e5's finding).
- Overlapping `Either` sides are common (E7-R1's probes, also invalid today: `Either[String, UUID]`,
  `Either[String, LocalDate]`, `Either[Int, Long]`, `Either[String, Color]`).
- refined's `Or` is a logical inclusive or; `oneOf` is xor (for two members). Under `Not` it inverts to the wrong set.
- After E7 lands without B14, the same `Either` would be `anyOf` under `AsFamily` + `Untagged` and `oneOf` under
  `ValueWrapped`, although both describe the same JSON (E7 asks B14 to align them).

## Options

### A. Inclusive where the source is inclusive: refined `Or` → `anyof`; `Either` through B3's overlap check  — WON

Built on B3's `Schema.anyof` node and its `Disjoint` check / `OverlappingMembers` knob (#470), and, for `Either`, on
E7's `either` node (`eitherAt`).

1. **refined `Or` → always `anyof`.** The refined macros' `OneOf` predicate becomes `AnyOf` and emits
   `` `anyof`.of[T](p1, p2, …) `` (insertion-ordered, B3's `of`). No overlap check: `anyOf` is exactly what `Or`
   means, whether or not the operands overlap (when they don't, it validates the same documents as `oneOf`).
   `Not[A Or B]` becomes `not(anyOf)`, which is `not(a or b)`. A nested `Or` stays nested, which is correct for
   `anyOf` (associative). Rendered (B3's `anyOf` always states its type, at the root too):
   ```json
   {"type": "string",  "anyOf": [{"pattern": "^a.*$"}, {"pattern": "^.*b$"}]}
   {"type": "integer", "anyOf": [{"multipleOf": 2.0}, {"multipleOf": 3.0}]}
   {"type": "string",  "anyOf": [{"format": "ipv4"}, {"format": "ipv6"}]}
   ```
   Not profile-dependent: the knob is about how a JSON library tells members apart; refined's `Or` is a predicate,
   not an encoding. (The IP schema accepts any string under 2019-09/2020-12, because `format` is an annotation there.
   That is the draft's meaning, and it no longer rejects anything.)
2. **`Either` (`ValueWrapped`) through the check.** In E7's `eitherAt`, the `ValueWrapped` branch builds its two
   lowered `{"value": side}` objects as E7 specifies and then, like `familyAt` and E7's other styles, emits
   `anyof(members)` when `p.overlappingMembers == AnyOf` and `Disjoint.overlapping(members)`, else today's `oneof`.
   The (field) rule decides it: both objects require `value`, so they are disjoint when the two sides are.
   **B3's (field) rule must recurse for this**: it compares the property schemas with `leaves` (JSON types and enum
   values only), so two object sides always "overlap" and `Either[Foo, Bar]` (distinct required fields) would become
   `anyOf` needlessly, while E7's `Untagged` (no wrapper, B3's (key) rule) gives `oneOf` for the same pair. B14 changes
   `requiredFieldsDisjoint` to use `disjoint` (the whole rule set) on the two property schemas. That is sound (no
   object can match both members if no value of the shared required property can) and terminates (the trees are
   finite; a `$ref` is "any type"). It also makes B3's family check more precise: members `A(x: Foo)`, `B(x: Bar)`
   with disjoint `Foo`/`Bar` become `oneOf` instead of `anyOf` (both are valid; `oneOf` says more).
   `Either[Int, String]`, `Either[Foo, Bar]`: `oneOf`, unchanged. `Either[Int, Double]`:
   ```json
   {"type": "object", "anyOf": [
     {"additionalProperties": false, "properties": {"value": {"type": "integer"}}, "required": ["value"]},
     {"additionalProperties": false, "properties": {"value": {"type": "number"}},  "required": ["value"]}]}
   ```
   `withOverlappingMembers(OneOf)` keeps `oneOf` for `Either` as for families. With this, every `eitherStyle` goes
   through the same check, and `ValueWrapped` equals `AsFamily` under an untagged `sumStyle` (E7's request).
   Two more sound refinements of `Disjoint` (B14-R3), so that common `Either`s stay `oneOf`:
   - (type) of a nested alternative: `types(oneof(ms))` and `types(anyof(ms))` are the union of the members' types (a
     document of either node matches some member). `Either[Int, SealedFamily]` and an `Option` side
     (`oneof{X, null}`) then get a real answer. `allof`, `not`, `ref` stay "any type".
   - (field) with one side optional: a property required by one object and *declared* (even optional) by the other,
     with disjoint schemas, makes the two disjoint: a document without it fails the first, one with it can't satisfy
     both property schemas. Closedness doesn't matter. `Either[Option[Int], String]` (E7's A3: `value` optional on the
     `Option` side) is then `oneOf`.
   Conservative cases left (`anyOf`, which still validates exactly the `Either`'s values): a `$ref` side, an `allOf` or
   `not` side.
3. **`Option[X]` where X admits `null` (R1) → X itself.** `optionAt` (`Nullable`) builds `oneof{X, null}` only when the
   lowered X doesn't already admit `null`; otherwise `Option[X]` adds nothing, and it lowers to X (its extras kept by
   `apply`'s `withExtraFrom`, as today). One predicate `admitsNull(s)`, true only when the *rendered* `s` accepts `null`:
   `null`; a `const(null)` (renders `{"const": null}`, no `type`); an `enum` with a `null` value **whose `tpe` admits
   `null`** (an `enum` renders `"type"` from its `tpe`, so `{"type":"string","enum":["a",null]}` rejects `null`; only an
   `enum` over E2's `any`, rendered without a type, qualifies); a `oneof` with **exactly one** member that admits `null`
   (two would make `null` match twice); an `anyof` with at least one; an `allof` whose members all do; a `def` or
   `value-class` of one; E2's `any` (it generalises E2's "an `Option` of an `any` is the `any`"). A `ref`, `not` or
   anything else: no (today's output, which is correct for them: `null` then matches only the `null` member).
   `Option[VOpt]`:
   ```json
   "v": {"type": ["integer", "null"]}
   ```
   No overlap check is needed: the two members overlap exactly in `null`, so "admits `null`" is the whole question.
   `Legacy` (`enum["None"]` vs `{value: X}`) never overlaps and is unchanged.
4. **`any` and `AsTagged` singletons.** `Disjoint.types` gets an explicit `` case _: `any`[_] => anyType `` (today's
   fall-through gives the same answer; the case documents it and survives a later change of the default). Tests for
   both; no output change beyond B3/E2.

Pros: every valid value validates; output changes only where the schema rejects valid values today (plus refined
`Or` with disjoint operands: `oneOf` → `anyOf`, same documents); the "exactly one" of `oneOf` stays wherever it is true
(`Either` with disjoint sides, tagged styles); one rule (B3's) for families and `Either`; no new public API.
Cons: depends on B3 (the `anyof` node; breaking under PVP, see Compatibility) and, for `Either`, on E7; refined output
changes under every profile, Legacy included, and no knob brings back refined's `oneOf`.

### B. The overlap check on every untagged `oneof`, hand-built ones included  — LOST

`Lowering.apply` runs `Disjoint` on every `oneof` without a discriminator and turns the overlapping ones into
`anyof`. One place, no dependency on E7 (it catches the macros' `Either` `oneof` as it is today).
Lost: it rewrites a user's explicit `Schema.oneof`, whose "exactly one" may be the point (an xor of two patterns), and
the check is conservative, so it would also flip disjoint user schemas it can't prove (`$ref`s, nested `oneof`s). The
lowering can't tell a derived `oneof` from a hand-built one. B3 rejected the same idea for families (its option E).

### C. refined `Or` through the overlap check too (`oneOf` when provably disjoint)  — LOST

Consistent with families. But both operands of a refined `Or` refine the same base type, so B3's (type) rule never
proves them disjoint; only its (values) rule could, and refined has no `Equal`/enum predicate the macros support. The
result would be `anyOf` in practice, through a runtime check the refined macro would have to emit (it's private to the
lowering today). And `anyOf` is the exact meaning of `Or` anyway.

### D. `Either` without E7: the macros pick `anyof`/`oneof` when the tree is built  — LOST (fallback if E7 is dropped)

Both macros emit a call to a new public helper, e.g. `` json.Schema.`oneof`.unlessOverlapping[T](l, r) ``, that runs the
overlap check on the two `{"value": side}` objects when the tree is built.
Lost while E7 is planned: the sides aren't lowered yet (a `family` or `option` side is "any type" to the check, so
`Either[SealedA, SealedB]` would become `anyOf` even when disjoint); the profile and its knob can't change it at render
time; it adds public API that E7 replaces. **If the owner drops E7**, this is the way to fix `Either`: plan step 2
becomes "move `Disjoint` to a core object the helper can call; both macros emit the helper; tree tests expect
`anyof`/`oneof` per pair". The other steps don't change.

### E. Keep `Either`'s `ValueWrapped` as `oneOf` (E7 as approved), document it  — LOST

No change for the default style. Lost: Neutral and Legacy keep rejecting `Left(1)` of `Either[Int, Double]`, and
`ValueWrapped` and `AsFamily` + `Untagged` disagree about the same JSON.

## Decision

A: refined's `Or` emits `anyof` (always; it is an inclusive or), and `Either`'s default `ValueWrapped` style goes
through B3's overlap check in E7's `eitherAt`, so overlapping sides render `anyOf` (knob: B3's
`overlappingMembers`), with B3's (field) rule made recursive (plus two more sound refinements) so that object sides
with distinct fields stay `oneOf`. An `Option[X]` whose X already admits `null` lowers to X instead of
`oneOf[X, null]`. B14 adds tests for the `any` and `AsTagged` cases B3 already covers. Implement after B3 (#470)
and E7 (#220); the refined and `Option` parts need only B3 (the `Option` part not even that). If E7 is dropped, D
fixes `Either`.

## Implementation plan

Order: after B3 (#470) is merged; step 2 after E7's implementation; the other steps can go first (step 3 needs
neither B3 nor E7).
If the owner picks B3's option F (an `Inclusive` marker on `oneof` instead of a new `anyof` case), read
`` `oneof`.inclusive(...) `` for every `` `anyof` `` below; nothing else changes.

1. **refined `Or` → `anyof`** (both macros; `sbt clean` after editing, macro call sites):
   - Scala 2 `modules/refined/src/main/scala-2/com/github/andyglow/jsonschema/refined/AST.scala`: rename
     `case class OneOf(t, preds)` → `AnyOf`; `tree = q"`anyof`.of[$t](${preds.head.tree}, ..${preds.tail.map(_.tree)})"`;
     `norm`'s flattening case matches `AnyOf`; `Or.norm`'s fallback `(l, r) => AnyOf(t, ::(l, r :: Nil))`. Check the
     `Schema._` import the trees rely on covers `` `anyof` ``.
   - Scala 3 `modules/refined/src/main/scala-3/com/github/andyglow/jsonschema/refined/RefinedMacro.scala`: the same
     rename (~L375-400); `tree` = `'{ Schema.`anyof`.of[x](${head}, ${Varargs(tail)}*) }` (or `ListSet` via `setOf`;
     two members keep their order either way). Update the scaladoc at L14 ("`And`/`Or` ... nested in `allof`/`anyof`").
   - Tests: `RefinedQuirksSpec` L78 ("Or of string predicates is an anyof"), `RefinedAliasedSpec` L18/L28
     (`` `anyof`.of(...) ``), 2.12's copies under `src/test/scala-2.12-` if they pin `oneof` (grep). New cases in
     `RefinedQuirksSpec` (and its 2.12 twin with witnesses): `Divisible[2] Or Divisible[3]`,
     `Greater[5] Or Divisible[2]` → `anyof`; `Not[StartsWith["a"] Or EndsWith["b"]]` → `not(anyof(...))`;
     `StartsWith["a"] Or (EndsWith["b"] Or Uuid)` → nested `anyof`; numeric `Or`s that normalize to a range unchanged.
   - Validation: `build.sbt` refined `dependsOn(core % "compile->compile;test->test", api % "compile->compile;test->test")`
     (as play-json etc.) for `petshop.Acceptance`; a new `RefinedOrValiditySpec` (JVM; cancels on JS): `"ab"`, `"a"`,
     `"b"` valid and `"c"` invalid for `StartsWith Or EndsWith`; `6`, `4`, `9` valid, `5` invalid for
     `Divisible Or Divisible`; `"1.2.3.4"`, `"::1"` valid for the IP alias on every draft, `"x"` invalid on 04/07;
     root `5` invalid (the `anyOf`'s root `type`). `Not[…Or…]`'s validity waits for B22 (`"type": "not"`): assert the
     tree only, and add the `"ab"`-invalid case to B22's tests (note for B22).
2. **`Either` `ValueWrapped` through the check** (`core/src/main/scala/json/schema/JsonSchemaProfile.scala`,
   E7's `eitherAt`): after building the two lowered members, `if (p.overlappingMembers == OverlappingMembers.AnyOf &&
   Disjoint.overlapping(members.toVector)) `anyof`[Any](members) else `oneof`[Any](members, None)`, the same expression
   E7 uses for its other styles (factor one private `alternatives(members)` helper for `familyAt`'s untagged branch and
   every `eitherAt` style; `optionAt` is step 3). An `Either[X, X]` (one member) stays a one-member `oneof` (one member
   is never an overlap).
   - `Disjoint` refinements, each with a positive and a negative tree case in `FamilyLoweringSpec`:
     - (field) recursive: `requiredFieldsDisjoint` compares with `disjoint(f.tpe, _)` instead of `leaves(f.tpe, _)`.
       `{value: Foo}` vs `{value: Bar}` (distinct required fields) disjoint; `{value: OCreated}` vs `{value: OUpdated}`
       overlap; a family `A(x: Foo)`, `B(x: Bar)` → `oneof`.
     - (field) one side optional: a property required by `x` and declared (required or not) by `y`, with
       `disjoint` schemas → disjoint (either direction). `{value?: [integer, null]}` vs `{value: string}` disjoint;
       `{value?: integer}` vs `{value: number}` overlap.
     - (type) of `oneof`/`anyof`: the union of the members' `types`. `{value: integer}` vs `{value: oneof{obj, obj}}`
       disjoint; `{value: string}` vs `{value: oneof{string, null}}` overlap.
   - Before changing them, grep B3's `FamilyLoweringSpec`/`SameShapeFamilySpec` for a case that pins `anyof` through the
     (field) rule with object-typed or alternative-typed properties (r-b14 found none at d97e12e); such a case would
     flip to `oneof` and must be updated on purpose.
   - Scaladoc of `OverlappingMembers` and `overlappingMembers`: "an untagged sealed family **or an `Either`** ...";
     the (field) bullet: "a property one requires and the other declares has schemas that are disjoint by these
     rules"; the (type) bullet: "a nested `oneOf`/`anyOf` has its members' types".
   - Tests (api, new `OverlappingAlternativesSpec`, shared, validation cancels on JS): under Neutral and Legacy,
     `Either[Int, Double]`, `Either[Int, Long]`, `Either[String, java.util.UUID]`, `Either[String, LocalDate]`,
     `Either[String, Color]` (a case-object enum), `Either[OCreated, OUpdated]` → `anyOf`, their overlapping documents
     valid, a wrong-typed `{"value": true}` invalid; guards `Either[Int, String]`, `Either[Foo, Bar]` (distinct required
     fields), `Either[Int, Either[String, Boolean]]` and `Either[Option[Int], String]` (R3) → `oneOf`, every side's
     documents valid; `withOverlappingMembers(OneOf)` → `oneOf`; root
     `Either[Int, Double]` states `"type": "object"` and rejects `5`; `Option[Either[Int, Double]]` field: `null` and
     `{"value": 1}` valid; `ValueWrapped` equals `AsFamily` under an untagged profile for an overlapping pair (compared
     as JSON). E7's `EitherLoweringSpec` (or `FamilyLoweringSpec`): one tree case `ValueWrapped` overlap → `anyof`,
     disjoint → `oneof`.
   - No macro change (E7 owns the `either` node in both macros).
3. **`Option[X]` where X admits `null`** (`JsonSchemaProfile.scala`, `optionAt`, `OptionStyle.Nullable` branch):
   `val x = apply(flatten(inner)); if (admitsNull(x)) x else `oneof`[Any](Set(x, Schema.`null`))`, with a private
   `admitsNull` (rules in option A, item 3) next to `Disjoint`. If E2 (#486) lands first, its `any` short-cut in
   `optionAt` becomes one case of `admitsNull` (merge, don't duplicate); if B14 lands first, E2 adds `any` to
   `admitsNull`. The renderer's `mkNullable` is unchanged (E2's `anyOrNull` there handles hand-built trees).
   - Tests: `FamilyLoweringSpec` (or a new `OptionLoweringSpec`): `option(value-class(option(integer)))` lowers to the
     value class unchanged; `option(def(sig, oneof{string, null}))` → the `def`; `option(const(null))` → the `const`;
     `option(anyof{string, null})` → the `anyof`; stays `oneof{X, null}` as today (negative cases):
     `option(integer)`, `option(ref(sig))`, `option(enum(string, {"a", null}))` (renders with `"type":"string"`, so it
     rejects `null`; api: `null` and `"a"` valid), `option(oneof{oneof{string, null}, oneof{integer, null}})` (two
     null-admitting members of an exclusive `oneof`). With E2: `option(enum(any, {"a", null}))` → the enum.
     api (validation cancels on JS):
     `HasVOpt(v: Option[VOpt])`, `VOpt(x: Option[Int]) extends AnyVal`: `{"v": null}`, `{"v": 1}`, `{}` valid,
     `{"v": "s"}` invalid; `Option[D]` with an implicit `Schema[D]` of a nullable body: `null` valid. Under Legacy:
     unchanged (`Option` fields drop a layer; elsewhere `enum["None"]` vs `{value}`), asserted on `HasVOpt`.
4. **`any` and `AsTagged`** (`JsonSchemaProfile.scala` `Disjoint.types`): add `` case _: `any`[_] => anyType `` with a
   comment (needs E2 #486). Tests: `FamilyLoweringSpec`: a family with an `any` member (a value class over
   `com.github.andyglow.json.Value`) → `anyof`; api: `Either[Value, Int]` → `anyOf`, `{"value": 1}` valid.
   `SameShapeFamilySpec` (B3's): `DocShape` (documented case objects + a case class) under
   `Neutral.withSingleton(AsTagged)` and the json4s-like profile → `anyOf`, `{}` and `{"r": 1}` valid.
5. **Goldens**: none expected. The golden `Either` models (`groupC/Either[Int,String]`, `groupC/HasEither`, golden and
   golden-legacy) have disjoint sides; no golden uses refined; `grep` the golden models for an `Option` of a value
   class over an `Option` (none expected). Run `GOLDEN_UPDATE=1 sbt clean api/test` on each version to confirm an
   empty diff. (The harness sorts arrays; keyword changes would still show.)
6. **Docs** (README and paradox, the same words):
   - refined, one more sentence: `format` (and so `IPv4`, `IPv6`, `Uri`, `Url`, ...) is an annotation from 2019-09 on
     unless the validator is set to assert formats, so `IPv4 Or IPv6` accepts any string there (R5).
   - `Option` (README ~L114 / "Option" section, `rendering.md` ~L53): "an `Option[X]` whose X already admits `null`
     (e.g. a value class over an `Option`) is X".
   - refined (README "Refined" ~L766, `docs/src/main/paradox/modules/refined.md` L25): "boolean: `And` (`allOf`), `Or`
     (`anyOf`: at least one holds), `Not` (`not`)".
   - `Either` (README ~L719 profiles, ~L966 known issues; `rendering.md` ~L180): E7 rewrites these lines; add "sides
     that one document can match are an `anyOf`, as for sealed families" next to B3's sentence, and mention `Either`
     in the `overlappingMembers` row of the profile-knob table.
   - A note in the B3 section: a hand-built `Schema.oneof` is rendered as written ("exactly one"); use `Schema.anyof`
     for inclusive alternatives.
7. **Gates** (when implemented): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on all three (core and
   refined change); `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`; `scalafmtSbtCheck` (build.sbt change);
   `makeDocs`.

Risks: the refined Scala 3 macro's `Varargs` splice under `-Xcheck-macros` (keep `setOf` + `ListSet` if it complains);
the refined module's new `api` test dependency on JS (api's test shims are cross-built, as for play-json); E7's final
`eitherAt` shape (step 2 is one expression; adapt to whatever E7 ships).

## Compatibility

- Source/binary: no new public API in B14 (the `anyof` node and the knob are B3's; the `either` node is E7's). The
  refined macros emit `anyof` instead of `oneof`: code that pattern-matches a refined schema as `oneof` (only tests,
  in our repo) sees `anyof`. Option D (fallback) would add one public helper.
- Output (every profile, Legacy included, by the lead's Legacy policy: these are validity fixes):
  - refined `A Or B` that doesn't normalize to a range: `oneOf` → `anyOf`, with `"type"` stated at the root too
    (today a root refined `oneOf` states none, B15; `5` is rejected only because it matches both members). For
    disjoint operands the same documents validate.
  - step 2's `Disjoint` refinements, relative to **0.9.0**: no change (a family that was `oneOf` in 0.9.0 is `oneOf`
    or B3's `anyOf`; the refinements only keep more of them `oneOf`, where the members are provably disjoint).
    Relative to B3's unreleased branch: such families (`A(x: Foo)`, `B(x: Bar)`; members whose shared property is an
    `Option` or a nested family) go back from `anyOf` to `oneOf`.
  - `Either` (`ValueWrapped`) with sides that can match one document: `oneOf` → `anyOf`. Disjoint sides unchanged.
  - `Option[X]` (`Nullable`: Neutral and the bridge profiles) where X already admits `null`: `oneOf[X, {"type":"null"}]`
    → X. Legacy unchanged.
  - golden and golden-legacy files: none change (step 5).
  - Alternative "Legacy keeps 0.7's `oneOf`": for `Either`, `Legacy` with `overlappingMembers = OneOf` (one line, but
    it would also undo B3's family fix under Legacy); for refined, not expressible without a fact node. Not proposed.
- PVP: B14 adds nothing breaking of its own. It ships in the release that has B3's `anyof` (0.10.0 with B3's option A;
  a 0.9.x patch with B3's option F). The output changes are bug fixes.

## Review log
Round 1 (r-b14, design at a4981cb):
- B14-R1 (must) `Option[X]` with a null-admitting X (a value class over `Option`, a nullable definition) is a derived
  overlapping `oneOf` neither B3 nor E2 fixes → accepted, folded in: Problem (repro), option A item 3, plan step 3
  (`optionAt` lowers to X when `admitsNull(X)`; one predicate shared with E2's `any` short-cut, whoever lands second
  merges), tests, docs, Compatibility. Issue #500's body updated with the case.
- B14-R2 (nit) list every derived `oneof` source; "Not in scope" → accepted: a table of sources with verdicts
  (Either, refined, `optionAt` Nullable/Legacy, `familyAt` incl. `EnumsAsOneOf`, hand-built, parser) and a Not-in-scope
  list (B15, B22, refined numeric `Or` normalization).
- B14-R3 (nit) two sound `Disjoint` refinements → accepted both: (type) of a nested `oneof`/`anyof` is the union of
  its members' types; (field) a property required by one and declared by the other with disjoint schemas. Plan step 2
  has a positive and a negative tree case for each; `Either[Int, Either[String, Boolean]]` and
  `Either[Option[Int], String]` now stay `oneOf` (moved from "conservative" to the guards).
- B14-R4 (nit) compatibility relative to 0.9.0; grep B3's specs for pinned cases → accepted: Compatibility states
  "no change vs 0.9.0, some anyOf → oneOf vs B3's branch"; plan step 2 has the grep.
- B14-R5 (nit) docs: `format` is annotation-only from 2019-09 → accepted: plan step 6, refined section.

Round 2 (r-b14, design at b231ed6; R1-R5 OK):
- B14-R6 (must) `admitsNull`'s `enum` rule is wrong: `mkEnum` renders `"type"` from the enum's `tpe`, so
  `{"type":"string","enum":["a",null]}` rejects `null`, and `Option[enum-with-null]` → enum would regress → accepted:
  an `enum` admits `null` only when it has a `null` value **and** its `tpe` admits `null` (in practice E2's `any`). The
  planned test is now a negative case (stays `oneof{enum, null}`, `null` valid); the `any` variant is the positive one.
  `const(null)` stays (rendered without a type).
- B14-R7 (nit) an exclusive `oneof` admits `null` only when exactly one member does (`anyof`: at least one) →
  accepted: rule and a negative test (two null-admitting members).

Approved by r-b14 at 35ab3fd.
