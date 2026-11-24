# `oneOf` for alternatives that can overlap: `Either` and refined `Or`  (issue #500, item B14)
Status: draft

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
   Conservative cases (the check can't prove them, so `anyOf`, which still validates exactly the `Either`'s values):
   a side that is itself a `oneOf`/`anyOf` (`Either[Int, Either[String, Boolean]]`, `Either[Int, SealedFamily]` when
   the family is a `oneOf`), a `$ref` side, an `Option` side (lowered to `oneOf[X, null]`).
3. **`any` and `AsTagged` singletons.** `Disjoint.types` gets an explicit `` case _: `any`[_] => anyType `` (today's
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
`anyof`/`oneof` per pair". Steps 1, 3 and 4 don't change.

### E. Keep `Either`'s `ValueWrapped` as `oneOf` (E7 as approved), document it  — LOST

No change for the default style. Lost: Neutral and Legacy keep rejecting `Left(1)` of `Either[Int, Double]`, and
`ValueWrapped` and `AsFamily` + `Untagged` disagree about the same JSON.

## Decision

A: refined's `Or` emits `anyof` (always; it is an inclusive or), and `Either`'s default `ValueWrapped` style goes
through B3's overlap check in E7's `eitherAt`, so overlapping sides render `anyOf` (knob: B3's
`overlappingMembers`), with B3's (field) rule made recursive so that object sides with distinct fields stay `oneOf`.
B14 adds tests for the `any` and `AsTagged` cases B3 already covers. Implement after B3 (#470)
and E7 (#220); the refined part needs only B3. If E7 is dropped, D fixes `Either`.

## Implementation plan

Order: after B3 (#470) is merged; step 2 after E7's implementation; steps 1, 3 and 4 can go first.
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
   every `eitherAt` style; `optionAt` stays as it is: `X | null` is B3's and E2's). An `Either[X, X]` (one member) stays a
   one-member `oneof` (one member is never an overlap).
   - `Disjoint.requiredFieldsDisjoint`: `ys.get(f.name).exists(disjoint(f.tpe, _))` instead of `leaves(f.tpe, _)`.
     Tests in `FamilyLoweringSpec`: `{value: Foo}` vs `{value: Bar}` (distinct required fields) disjoint;
     `{value: OCreated}` vs `{value: OUpdated}` overlap; a family `A(x: Foo)`, `B(x: Bar)` → `oneof`.
   - Scaladoc of `OverlappingMembers` and `overlappingMembers`: "an untagged sealed family **or an `Either`** ...";
     the (field) bullet: "a property both require has schemas that are disjoint by these rules".
   - Tests (api, new `OverlappingAlternativesSpec`, shared, validation cancels on JS): under Neutral and Legacy,
     `Either[Int, Double]`, `Either[Int, Long]`, `Either[String, java.util.UUID]`, `Either[String, LocalDate]`,
     `Either[String, Color]` (a case-object enum), `Either[OCreated, OUpdated]` → `anyOf`, their overlapping documents
     valid, a wrong-typed `{"value": true}` invalid; guards `Either[Int, String]`, `Either[Foo, Bar]` (distinct required
     fields) → `oneOf`; `Either[Int, Either[String, Boolean]]` → `anyOf` (conservative) with every side's document
     valid; `withOverlappingMembers(OneOf)` → `oneOf`; root
     `Either[Int, Double]` states `"type": "object"` and rejects `5`; `Option[Either[Int, Double]]` field: `null` and
     `{"value": 1}` valid; `ValueWrapped` equals `AsFamily` under an untagged profile for an overlapping pair (compared
     as JSON). E7's `EitherLoweringSpec` (or `FamilyLoweringSpec`): one tree case `ValueWrapped` overlap → `anyof`,
     disjoint → `oneof`.
   - No macro change (E7 owns the `either` node in both macros).
3. **`any` and `AsTagged`** (`JsonSchemaProfile.scala` `Disjoint.types`): add `` case _: `any`[_] => anyType `` with a
   comment (needs E2 #486). Tests: `FamilyLoweringSpec`: a family with an `any` member (a value class over
   `com.github.andyglow.json.Value`) → `anyof`; api: `Either[Value, Int]` → `anyOf`, `{"value": 1}` valid.
   `SameShapeFamilySpec` (B3's): `DocShape` (documented case objects + a case class) under
   `Neutral.withSingleton(AsTagged)` and the json4s-like profile → `anyOf`, `{}` and `{"r": 1}` valid.
4. **Goldens**: none expected. The golden `Either` models (`groupC/Either[Int,String]`, `groupC/HasEither`, golden and
   golden-legacy) have disjoint sides; no golden uses refined. Run `GOLDEN_UPDATE=1 sbt clean api/test` on each
   version to confirm an empty diff. (The harness sorts arrays; keyword changes would still show.)
5. **Docs** (README and paradox, the same words):
   - refined (README "Refined" ~L766, `docs/src/main/paradox/modules/refined.md` L25): "boolean: `And` (`allOf`), `Or`
     (`anyOf`: at least one holds), `Not` (`not`)".
   - `Either` (README ~L719 profiles, ~L966 known issues; `rendering.md` ~L180): E7 rewrites these lines; add "sides
     that one document can match are an `anyOf`, as for sealed families" next to B3's sentence, and mention `Either`
     in the `overlappingMembers` row of the profile-knob table.
   - A note in the B3 section: a hand-built `Schema.oneof` is rendered as written ("exactly one"); use `Schema.anyof`
     for inclusive alternatives.
6. **Gates** (when implemented): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on all three (core and
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
  - with step 2's recursive (field) rule, a sealed family whose members differ only inside a shared required
    property (`A(x: Foo)`, `B(x: Bar)`) goes back from B3's `anyOf` to `oneOf` (both valid; B3 is unreleased).
  - `Either` (`ValueWrapped`) with sides that can match one document: `oneOf` → `anyOf`. Disjoint sides unchanged.
  - golden and golden-legacy files: none change (step 4).
  - Alternative "Legacy keeps 0.7's `oneOf`": for `Either`, `Legacy` with `overlappingMembers = OneOf` (one line, but
    it would also undo B3's family fix under Legacy); for refined, not expressible without a fact node. Not proposed.
- PVP: B14 adds nothing breaking of its own. It ships in the release that has B3's `anyof` (0.10.0 with B3's option A;
  a 0.9.x patch with B3's option F). The output changes are bug fixes.

## Review log
