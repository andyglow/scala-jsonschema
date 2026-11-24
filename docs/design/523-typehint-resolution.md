# `@typeHint[T]` must name a JSON type, not a derived structure  (issue #523, item B26)
Status: draft

## Problem
`@typeHint[T]` (on a sealed root or a member) gives the JSON type of enum values that come from an implicit `ToValue`.
Both macros resolve `T` like any other type: an implicit `json.Schema[T]`, else an implicit `json.schema.Predef[T]`,
else **structural derivation** (Scala 2 `UEnums.EnumFamily.schemas` → `resolve(hint, ctx)`; Scala 3
`Enums.valueSchemaOf` → `resolve(h, ctx)`). The family is then lowered to `enum(<hint schema>, values)`, and
`AsDraftSupport.mkEnum` writes `"type" -> <hint schema>.jsonType` and nothing else from the hint schema, without
looking at the values.

So a hint with no implicit is derived as a case class (`"type": "object"`) or a sealed family / enum
(`"type": "enum"`, not a JSON Schema type), and the schema rejects every value of the enum. A hint that *does* have an
implicit `Schema` resolves to a `def` node, whose `jsonType` is `???`: rendering throws. And a hint that resolves fine
but doesn't match the values (`@typeHint[String]` over numbers) is written as is.

```scala
import com.github.andyglow.json.{ToValue, Value}
import json._, json.schema._

@typeHint[Value.num]
sealed trait N
object N {
  case object A extends N
  case object B extends N
  implicit val tv: ToValue[N] = ToValue mk { case A => Value.num(1.5); case B => Value.num(2) }
}
Json.schema[N]   // tree: family(A: Singleton(object(value: number) = 1.5), B: Singleton(object(value: number) = 2))
```

Draft 2020-12, default profile; Neutral and Legacy give the same. Repro: `notes/scratch/c-b26/B26*Spec.scala` (not
committed) and r-b26's `RB26Round1Spec` (rows marked r).

| hint (ToValue writes) | master 0.9.1-SNAPSHOT, 2.12.21 / 2.13.18 / 3.3.9 JVM | on E2's branch (#486 at c74d4bc), 2.13 / 3 |
|---|---|---|
| `Value.num` (1.5, 2), root or member | `{"type": "object", "enum": [1.5, 2]}` | `{"type": "number", "enum": [1.5, 2]}` |
| `Value.str` ("a", "b") | `{"type": "object", "enum": ["a", "b"]}` | `{"type": "string", ...}` |
| `Value.bool` (true, false) | `{"type": "enum", "enum": [true, false]}` | `{"type": "boolean", ...}` |
| `Value` (1.5, "b"), `Value.arr` | compile error: `Can't infer json value for default value of Value.arr(..., value: Seq[Value])` | `{"enum": [1.5, "b"]}`, `{"type": "array", "enum": [[1], []]}` |
| play `JsNumber` (1.5, 2) | `{"type": "object", "enum": [1.5, 2]}` | same; `{"type": "number", ...}` only with `import AsPlay.predef._` |
| `final case class W(v: Int)` (1, 2) | `{"type": "object", "enum": [1, 2]}` | same |
| `W` over `{"v": 1}`, `{"v": 2}` (r) | `{"type": "object", "enum": [{"v": 1}, {"v": 2}]}` (valid, right) | same |
| case-object family `Color` (r) | `{"type": "enum", "enum": ["Red", "Blue"]}` | same |
| value class `Id(v: String)` **with** `implicit val s: Schema[Id] = Json.schema[Id]` (r; 2.13 / 3) | tree `Singleton(def(Id, string))`; **rendering throws `scala.NotImplementedError`** (`def.jsonType`, Schema.scala L554) | same |
| value class `Raw(v: String)`, no implicit (r) | `{"type": "string", "enum": ["a", "b"]}` (right) | same |
| `String` over `1`, `2` (r) | `{"type": "string", "enum": [1, 2]}` (accepts nothing) | same |
| `BigDecimal`, `Double` (control) | `{"type": "number", "enum": [1.5, 2]}` | same |
| no hint (1.5, 2) | `{"oneOf": [{"const": 1.5}, {"const": 2}]}` | same |

No warning is reported in any row. Same output on 2.12, 2.13 and 3.3; the derivation is shared code, so Scala.js is
the same. 0.7 had the same path (r-b26: v0.7.8 `UEnums`), so this isn't a port regression.

A side observation: the hint is resolved even when it is ignored (a family without a `ToValue`: the values are the
names, a warning says the hint is ignored). So `@typeHint[Value]` on such a family is the same compile error as above
instead of the warning, on 2.13.18 and 3.3.9 (both probed on master).

## Why it is a problem
- The natural hint for a `ToValue` is the type a `ToValue` returns: `Value.num`, `Value.str`, ... (`ToValue[T]` is
  `T => Value`). The same holds for the JSON libraries' AST types (`JsNumber`, circe `Json`) when the `ToValue` comes
  from a bridge. Today all of them silently give a schema that accepts nothing (`1.5` is not an object) or carries an
  invalid `"type": "enum"`.
- A user `Schema` for a value class (`UserId`) is the most natural non-built-in hint, and it crashes at render time.
- "`Value.num` means number" is already the established intent elsewhere: Scala 2's `JsonValueType` reads a
  `Value.num(...)` tree as a number schema, and core's `ValueSchema` maps `Value.num` to `number` for `enum.of`.
- Nothing tells the user: no warning, the code compiles, and the wrong `type` sits next to the right `enum` values.

## Neighbours
- **E2 (#486)**: approved by r-e2 at c74d4bc, open (checked 2026-11-23). It adds core Predefs for `Value`,
  `Value.{obj, arr, str, num, bool, null}` and opt-in `AsX.predef` objects in the bridges. With them every `Value.*` row
  above is right (verified, table). E2 also edits `mkEnum` (no `"type"` for an `any`). B26 builds on E2: it is
  implemented after #486 merges.
- **D2 (#491/#496, design)** adds a whole-values guard where `familyAt` builds the enum of an `integer` value schema
  (it falls back to `number` when a value isn't whole). B26's render check (option F) is the general form at the last
  step (`mkEnum`). Ownership: D2 owns *choosing* `integer` vs `number` in `familyAt` (it keeps a type); F only *drops* a
  type that the values contradict. The per-value predicate ("is this `Value` of JSON type t", with `integer` = a whole
  `num`) is one core helper; whichever of B26 / D2 lands first adds it, the other reuses it. With D2's guard in place F
  never sees an `integer` family enum over fractions; F still covers hand-built enums and every other type.
- **B22** (`"type": "not"`) is the same class of bug for `not`; B26 only touches `mkEnum`.

## Options

### A. A hint resolves through an implicit; a hint derived as an object, family or enum is reported — WON (as A-warn, with F)
`@typeHint[T]` names a JSON type, so `T` should have an implicit `Schema` or `Predef` (all built-ins do, and with E2 so
do our `Value` types), or be something whose derived schema *is* a JSON type: a value class (its underlying type), a
collection, an `Option`. When `T` has no implicit and would be derived as

- a case class or a tuple (`object`),
- a sealed family, a Scala 3 `enum`, `Either` (`family` / `oneOf`),
- a Java enum or a `scala.Enumeration#Value` (`enum`),

the macro reports it with a message that names the fix. A hint that has an implicit `Schema` (e.g. `Schema[Id]` for a
value class) or `Predef` is found first and never reported. The check runs only for values that come from a `ToValue`.
For a family whose values are the names, the hint isn't resolved at all any more (the schema is `string` whatever the
hint, and the existing "ignored" warning stays), which removes the `@typeHint[Value]` compile error there.

Two variants:

- **A-warn (wins):** a warning, and the hint is still resolved as today; F then leaves `"type"` out where the derived
  type doesn't match the values (`{"enum": [1.5, 2]}` for `JsNumber`), and keeps `"type": "object"` for a case-class
  hint over objects. No source break.
  ```
  @typeHint[play.api.libs.json.JsNumber] on N: JsNumber has no implicit json.Schema or json.schema.Predef, so its schema
  is derived from its structure (an object), which is unlikely to be the JSON type of the values the family's ToValue
  writes; the enum's "type" is left out where the values don't match it. Name a type with a Predef: String, Int, Long,
  Double, BigDecimal, Boolean, ... or the JSON AST types Value.str, Value.num, Value.bool, Value.arr, Value.obj, Value
  (any value). For a JSON library's AST type import its Predefs (e.g. `import com.github.andyglow.jsonschema.AsPlay.predef._`),
  or provide an implicit json.schema.Predef[JsNumber].
  ```
- **A-error:** the same text as a compile error (`c.abort` / `report.errorAndAbort`, first sentence "... would be derived
  ... which is not the JSON type ..."). Stronger: the mistake can't ship. But code that compiles today stops compiling
  (only these hints), so it's breaking (minor bump), and it rejects the one valid case (a case-class hint over object
  values; migration `@typeHint[Value.obj]`, same output).

Why A-warn wins: with F in the plan the output is valid in every row either way, so the error buys only enforcement,
at the price of a source break and a minor bump. A warning is also how the macro already treats a hint it can't use
("`@typeHint[...]` is ignored for ..."), and builds with fatal warnings still stop there. The owner can pick A-error:
in the plan, step 2/3's `warn` becomes `abort`, the old resolution after the warning goes away, and the PVP line
becomes "minor".

- Pros: the mistake is reported where it is made, with the fix in the message; no output change for any hint that
  works today; the check sits at the two choke points that already resolve hints.
- Cons: a warning can be ignored (but the schema is valid thanks to F).

### B. Hard-code the JSON AST types in the hint resolution — LOST
Map `Value.num` → `number`, `Value.str` → `string`, ... (and the bridges' AST types by name) inside both macros'
hint resolution, independent of E2.
- Pros: works without E2.
- Cons: duplicates E2's Predefs (which also serve `Json.schema[Value.num]` and fields); naming bridge types in the core
  macro couples core to every library; a case-class or family hint stays silently wrong.

### C. `mkEnum` writes `"type"` only when it is a JSON type, seeing through definitions — WON (as part of the render step)
Before asking any `jsonType`, unwrap the value schema structurally: `def(_, inner)` → inner, `value-class(inner)` →
inner, recursively; a bare `ref` (no inner schema) and an `any` (E2's `` `any`.admitsEverything ``, which this
subsumes) give no type; otherwise write `"type"` only if the `jsonType` is one of `string`, `number`, `integer`,
`boolean`, `array`, `object`, `null`.
- Pros: fixes the `NotImplementedError` of an implicit-`Schema` hint (`@typeHint[Id]` → `{"type": "string", ...}`,
  the definition's type is kept, not dropped); turns `"type": "enum"` (family/enum hints with an implicit, hand-built
  enums) into no `"type"`; small.
- Cons alone: `"type": "object"` is a valid type, so `JsNumber` / case-class hints over numbers stay wrong; silent.

### F. Check the hint's type against the values at render time — WON (as part of the render step) (r-b26, B26-R1)
`mkEnum` has the values (`enum.values`). After C's unwrapping, keep `"type"` only when every value is of that JSON
type (`string` ↔ all `str`, `number` ↔ all `num`, `integer` ↔ all whole `num`, `boolean` ↔ all `bool`, `object` ↔ all
`obj`, `array` ↔ all `arr`, `null` ↔ all `null`); otherwise leave `"type"` out (the values carry their types).
- Pros: valid and right in every row, including `@typeHint[String]` over numbers (which has a Predef, so A can't see
  it) and `JsNumber` without the import; no compiling code changes; no golden file changes (every hint that works
  today already matches its values), so it's a patch. Differs from D: the hint's type is kept whenever it is consistent
  (so `integer` vs `number` stays the hint's/D2's choice).
- Cons alone: silent; a wrong hint is just dropped. Hence A-warn next to it.

### D. Derive `"type"` from the values at render time, ignore the hint's type — LOST
The family carries its values, so `familyAt`/`mkEnum` could compute the JSON type(s) from them (`number` for
`Value.num`, ...) and only use the hint to group values at compile time.
- Pros: always right, no compile error.
- Cons: changes what `@typeHint` means (it would no longer pick `integer` vs `number`, which D2 relies on); F gets the
  same validity while keeping the hint whenever it is consistent.

### E. Nothing beyond E2: tests and docs — LOST
After #486 the `Value.*` hints work. B26 would add regression tests and document "the hint needs a Predef".
- Pros: no code change, no compatibility question.
- Cons: `JsNumber` without the opt-in import, case classes and families stay silently wrong; `"type": "enum"` stays;
  the implicit-`Schema` hint keeps crashing.

## Decision
A-warn + C + F, after E2 (#486) merges. A hint that would be derived as an object, a family or an enum gets a warning
naming the fix; `mkEnum` sees through definitions and value classes (no more `NotImplementedError`), and writes
`"type"` only when it is a JSON type that every value matches. A patch release; A-error is the drop-in stricter pick.

## Implementation plan
0. **Prerequisite:** E2 (#486) merged; rebase on `origin/master`. B26 waits for E2 rather than copying its Predefs:
   adding them here would also change `Json.schema[Value.num]` and every field of a `Value.*` type
   (`{"type": "object", "properties": {"value": ...}}` → `{"type": "number"}`), which is E2's scope. If E2 is dropped,
   mail the lead before implementing (and leave the `AsX.predef` hint out of the message).
1. **Message** (both versions, same text, as in option A). The owner `N` is the type that carries the annotation (the
   root, or the member for a member hint); the shape is "an object" / "a sealed family" / "an enum".
2. **Scala 2, `macros/src/main/scala-2/com/github/andyglow/jsonschema/UEnums.scala`:**
   - new `resolveHint(hint: Type, owner: Type)(implicit ctx: ResolutionContext): SchemaType`:
     `Implicit.get(hint.dealias)` (UImplicits L81, which searches `Schema` then `Predef`; as `USumTypes` L58 uses it) →
     that schema; else if `derivedShape(hint)` is defined → `warn(message)` (A-error: `c.abort(c.enclosingPosition, message)`);
     then `resolve(hint, ctx)` as today.
   - `derivedShape(t: Type): Option[String]`: on `t.dealias.typeSymbol`: a case class that isn't a value class
     (`isCaseClass && !(t <:< AnyVal)`; tuples are case classes) → "an object"; `isSealed(t)` (`UCommons` L125) →
     "a sealed family"; `sym.isJavaEnum` or `t <:< typeOf[Enumeration#Value]` → "an enum"; else `None`.
   - `EnumFamily.schemas` (L50-57): call `resolveHint` instead of `resolve`, and only for `EnumItem.FromToValue` items;
     `FromCaseObject` items get `SchemaType.Str` without resolving the hint (the warning in `EnumExtractor.fromSumType`
     L151-165 stays as is). `USumTypes` L67 (singleton members of a mixed family) goes through `schemaOf` → covered.
     A `ToValue` is per root, so a family's items are all `FromToValue` or all `FromCaseObject`.
   - The warning must be emitted once per hint, not once per item (dedupe by hint type + owner, or check the root hint
     once and each member hint once).
   - `clean` before testing (macro change).
3. **Scala 3, `core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/`:**
   - `Enums.scala` `valueSchemaOf` (L247): `case Some(h) => resolveHint(h, owner, ctx)`, owner = `item.tpe` if the
     member carries the hint, else the root (`valueSchemaOf` gets the root `TypeRepr`; callers: `singleValueSchema`
     L268, `Sums.scala` L127, which has `root.tpe`).
   - `singletonFamily` (L180): when no item is `fromToValue`, don't call `singleValueSchema` with the hints (use
     `Str(tpe)` directly, keep the `ignored` warnings).
   - `resolveHint` next to `resolve` in `Resolve.scala`: `lookupImplicit(h.dealias)` getOrElse
     { `derivedShape(h).foreach(s => report.warning(message))` (A-error: `report.errorAndAbort`); `structural(h.dealias, ctx)` }.
     `derivedShape`: `isCaseClass` (Resolve.scala; excludes value classes, includes tuples) → "an object"; `isSealed`
     (Sums.scala L241; Scala 3 enums and `Either` are sealed) → "a sealed family";
     `classSymbol(h).flags.is(Flags.JavaEnum)` or `h <:< TypeRepr.of[Enumeration#Value]` → "an enum". Same once-per-hint
     rule as Scala 2. The warning's position: the macro call site (as the existing "ignored" warning).
   - `clean` before testing.
4. **Render step, `core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala` `mkEnum`** (on top of E2's
   version; options C + F):
   - `enumType(s: Schema[_]): Option[String]`, matching structurally before any `jsonType` call: `` `def`(_, inner) `` →
     `enumType(inner)`; `` `value-class`(inner) `` → `enumType(inner)`; `` `ref` `` → `None`; `` `any`.admitsEverything(s) ``
     → `None`; else `Some(s.jsonType).filter(JsonTypes)` with `JsonTypes` = the seven JSON type names.
   - `"type"` is written iff `enumType(x.tpe)` is `Some(t)` and every value matches `t` (the shared predicate, see
     Neighbours/D2: `str`, `num`, whole `num` for `integer`, `bool` (`true`/`false`), `obj`, `arr`, `null`).
   - Check every other place that asks an enum's value schema for `jsonType` the same way (e.g. `mkPlainOneOf`'s
     same-type test over `enum` members, the `Option[enum]` type-array path in `mkNullable`): they must use `enumType`
     too, or they still throw on a `def`. List each one in the PR.
5. **Tests** (shared `api/src/test/scala`, run on JVM and JS):
   - new `TypeHintSpec.scala`: hints `Value.num`, `Value.str`, `Value.bool`, `Value.arr`, `Value.obj`, `Value`,
     `` Value.`null`.type `` on the root and per member, with a `ToValue`: the derived tree and the rendered draft-07
     and 2020-12 JSON (`{"type": "number", "enum": [1.5, 2]}`, ..., `{"enum": [1.5, "b"]}` for `Value`); a mixed family
     (a case class + hinted case objects with a `ToValue`): the singleton's value schema is `number`; controls
     (`BigDecimal`, `Double`, a value class without an implicit → `string`).
   - implicit-`Schema` hint: `@typeHint[Id]` with `implicit val s: Schema[Id] = Json.schema[Id]` renders
     `{"type": "string", "enum": ["a", "b"]}` and doesn't throw; also as an `Option` field and in a mixed `oneOf`
     (the step 4 call sites). An implicit `Schema[Color]` (case-object family) hint → `{"enum": ["Red", "Blue"]}`.
   - render check: `@typeHint[String]` over `1`, `2` → `{"enum": [1, 2]}`; `@typeHint[W]` over numbers →
     `{"enum": [1, 2]}`; `@typeHint[W]` over objects → `{"type": "object", "enum": [{"v": 1}, {"v": 2}]}` (unchanged).
   - warnings: hints of a case class, a tuple `(Int, String)`, a sealed case-object family, a Java enum
     (`java.time.DayOfWeek`), an `Enumeration#Value` compile (A-warn) and render without a contradicted `"type"`.
     Each call is wrapped as EnumSpec L166 does for the existing warning: `(Json.schema[X]: @nowarn("msg=has no implicit json.Schema"))`,
     which keeps the build warning-free on 2.12/2.13/3 and fails the build (unused `@nowarn` is reported with
     `-Wunused:nowarn`, if enabled; otherwise check once by hand that removing it warns) if the warning disappears.
     If the owner picks A-error, these become `harness.CompileErrors.errorOf(assertCompiles(...))` asserting `@typeHint[`,
     `has no implicit json.Schema or json.schema.Predef` and the shape. Scala 3 enum hint in
     `api/src/test/scala-3/.../sums/Scala3EnumsSpec.scala`.
   - escape hatch: a user `implicit val p: Predef[W] = Predef(...)` makes `@typeHint[W]` resolve without a warning.
   - a family without `ToValue` and `@typeHint[Value]` / `@typeHint[W]`: compiles (was an error on 2.13 and 3 for
     `Value`), string enum, the "ignored" warning.
   - `AsDraft07Spec` (+ 04/12 as they pin `enum`): hand-built `enum`s over a `def`, a `value-class`, a `ref` and a
     `family`, and an `enum` whose values contradict its type.
   - `modules/play-json/src/test/.../TypeHintPlaySpec.scala` (play is JVM + JS): `@typeHint[JsNumber]` without the
     import → `{"enum": [1.5, 2]}` (+ the warning); with `import AsPlay.predef._` → `{"type": "number", ...}`.
   - Golden files: none expected to change (no golden has an `enum` with a non-JSON `"type"` (grep), and every hinted
     model's values match its hint). Run the golden suite on all three versions; any diff is an enum whose type its
     values contradict: list it in the PR. Legacy goldens: same rule.
6. **Docs** (README + paradox, same words): README L332-334 and the annotation table L486, paradox `types.md` L72 and
   `descriptions.md` L85: "`T` should have a `Predef` or an implicit `Schema` (all built-in types have one, and so do
   the JSON AST types `Value.num`, `Value.str`, `Value.bool`, `Value.arr`, `Value.obj` and `Value`; for a JSON
   library's AST types import `AsX.predef._`). A hint that would be derived as a case class, a sealed family or an enum
   gets a warning. The rendered `type` is the hint's, as long as every value is of that type; otherwise the enum has no
   `type`." Plus a `Value.num` example next to the `ToValue` paragraph (`mdoc:compile-only`: a sealed family in an
   object; or plain `scala`). `makeDocs`. No CHANGELOG edit.
7. **Gates** as in the protocol (all three Scala versions JVM, `rootJS/test` on all three: core + macros + api touched).

Scala 2 vs 3: same rule and message; the shape detection uses each macro's own helpers. JS: nothing platform-specific
(compile-time checks and the renderer in shared code).

Risks: (a) `Implicit.get` on Scala 2 has the self-reference special case (`isSelfRef`); a hint type isn't the type
being defined, so it doesn't apply, but the implicit-`Schema` hint test covers it. (b) A hint the check misses (e.g.
a plain non-case class) still goes to structural derivation and fails there as today. (c) `Option[X]` hints stay as
today (not reported); nobody is known to use them. (d) F's cost: one pass over the values per rendered enum (small).

## Compatibility
- Output: no change for any hint whose type matches its values today. Changes, all from schemas that accepted no value
  or were invalid: `"type": "enum"` / `"oneof"` / ... next to an `enum` disappear; a `"type"` the values contradict
  disappears (`{"type": "object", "enum": [1.5, 2]}` → `{"enum": [1.5, 2]}`, `{"type": "string", "enum": [1, 2]}` →
  `{"enum": [1, 2]}`); an implicit-`Schema` hint renders instead of throwing. These are bug fixes, so they apply under
  Legacy too (lead policy 2026-11-23); no golden-legacy file is expected to change (step 5). With E2, the `Value.*`
  hints render their JSON type (E2's change, listed here because it's the point of the item).
- Source: nothing that compiles stops compiling (A-warn). New warnings for hints of a case class, tuple, sealed family,
  Scala 3 enum, Java enum or `Enumeration#Value` without an implicit, on a family with a `ToValue` (a build with fatal
  warnings stops there). A family without a `ToValue` compiles in more cases than before (`@typeHint[Value]` was a
  compile error on 2.13 and 3; the hint isn't resolved any more).
- Binary: no public signature changes (new private macro / renderer helpers only).
- PVP: patch. With A-error: breaking → minor bump (0.10.0), as B3 (#470) treats its change.
- Core design rule: unchanged; it is applied to hints more strictly (implicit first, derivation reported unless it
  gives a JSON type).

## Review log
B26-R1 (must) missed option F, render-time hint-vs-values check → accepted: F added as an option and made part of the
winner (step 4, tests in step 5); `@typeHint[String]` over numbers added to the Problem table and listed as fixed.
Re-argued A-error vs A-warn with F available: the winner is now **A-warn + C + F (patch)**; A-error stays the drop-in
stricter pick (what changes: `warn` → `abort` in steps 2/3, the warning tests become `CompileErrors` tests, PVP minor).
D2 coordination in Neighbours: D2 owns choosing `integer` vs `number` in `familyAt`; F only drops a contradicted type;
the per-value predicate is one shared helper, added by whichever lands first.
B26-R2 (must) implicit-`Schema` hint crashes (`def.jsonType` is `???`) → accepted: row added to the Problem table
(2.13 + 3, r-b26's probe); step 4 now matches `def` / `value-class` / `ref` structurally before any `jsonType` and
unwraps `def` and `value-class` to the inner type (`{"type": "string", ...}` for `Id`; only a bare `ref` has no type);
step 4 also lists the other `jsonType` call sites over an enum's value schema; tests added (enum over a `def` in
`AsDraft07Spec`, `@typeHint[Id]` with an implicit `Schema` as root, `Option` field and `oneOf` member). Option A states
that an implicit-backed hint is found first and never reported.
B26-R3 (nit) step 0's fallback changes `Json.schema[Value.num]` and fields → accepted: step 0 now waits for E2 and says
why; if E2 is dropped, mail the lead.
B26-R4 (nit) Scala 3 status of the side observation → accepted: probed `@typeHint[Value]` without a `ToValue` on master,
2.13.18 and 3.3.9: the same compile error on both; Problem and Compatibility say so.
B26-R5 (nit) wording → accepted: E2's state with sha and date (c74d4bc, 2026-11-23); the message says "the values the
family's ToValue writes".
