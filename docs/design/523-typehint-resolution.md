# `@typeHint[T]` must name a JSON type, not a derived structure  (issue #523, item B26)
Status: draft

## Problem
`@typeHint[T]` (on a sealed root or a member) gives the JSON type of enum values that come from an implicit `ToValue`.
Both macros resolve `T` like any other type: an implicit `json.Schema[T]`, else an implicit `json.schema.Predef[T]`,
else **structural derivation** (Scala 2 `UEnums.EnumFamily.schemas` → `resolve(hint, ctx)`; Scala 3
`Enums.valueSchemaOf` → `resolve(h, ctx)`). The family is then lowered to `enum(<hint schema>, values)`, and
`AsDraftSupport.mkEnum` writes `"type" -> <hint schema>.jsonType` and nothing else from the hint schema.

So a hint with no implicit is derived as a case class (`"type": "object"`), a sealed family / enum (`"type": "enum"`,
not a JSON Schema type), and the schema rejects every value of the enum.

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

Draft 2020-12, default profile; Neutral and Legacy give the same. Repro: `notes/scratch/c-b26/B26*Spec.scala` (not committed).

| hint (ToValue writes) | master 0.9.1-SNAPSHOT, 2.12.21 / 2.13.18 / 3.3.9 JVM | on E2's branch (#486, c74d4bc), 2.13 / 3 |
|---|---|---|
| `Value.num` (1.5, 2), root or member | `{"type": "object", "enum": [1.5, 2]}` | `{"type": "number", "enum": [1.5, 2]}` |
| `Value.str` ("a", "b") | `{"type": "object", "enum": ["a", "b"]}` | `{"type": "string", ...}` |
| `Value.bool` (true, false) | `{"type": "enum", "enum": [true, false]}` | `{"type": "boolean", ...}` |
| `Value` (1.5, "b"), `Value.arr` | compile error: `Can't infer json value for default value of Value.arr(..., value: Seq[Value])` | `{"enum": [1.5, "b"]}`, `{"type": "array", "enum": [[1], []]}` |
| play `JsNumber` (1.5, 2) | `{"type": "object", "enum": [1.5, 2]}` | same; `{"type": "number", ...}` only with `import AsPlay.predef._` |
| `final case class W(v: Int)` (1, 2) | `{"type": "object", "enum": [1, 2]}` | same |
| case-object family `Color` (r-b26) | `{"type": "enum", "enum": ["Red", "Blue"]}` | same (E2 doesn't touch it) |
| `BigDecimal`, `Double` (control) | `{"type": "number", "enum": [1.5, 2]}` | same |
| no hint (1.5, 2) | `{"oneOf": [{"const": 1.5}, {"const": 2}]}` | same |

No warning is reported in any row. Same output on 2.12, 2.13 and 3.3; the derivation is shared code, so Scala.js is
the same. 0.7 had the same path (r-b26: v0.7.8 `UEnums`), so this isn't a port regression.

A side observation: the hint is resolved even when it is ignored (a family without a `ToValue`: the values are the
names, a warning says the hint is ignored). On Scala 2, `@typeHint[Value]` on such a family is therefore the same
compile error as above instead of the warning.

## Why it is a problem
- The natural hint for a `ToValue` is the type a `ToValue` returns: `Value.num`, `Value.str`, ... (`ToValue[T]` is
  `T => Value`). The same holds for the JSON libraries' AST types (`JsNumber`, circe `Json`) when the `ToValue` comes
  from a bridge. Today all of them silently give a schema that accepts nothing (`1.5` is not an object) or carries an
  invalid `"type": "enum"`.
- "`Value.num` means number" is already the established intent elsewhere: Scala 2's `JsonValueType` reads a
  `Value.num(...)` tree as a number schema, and core's `ValueSchema` maps `Value.num` to `number` for `enum.of`.
- Nothing tells the user: no warning, the code compiles, and the wrong `type` sits next to the right `enum` values.

## Neighbours
- **E2 (#486, approved, not merged)** adds core Predefs for `Value`, `Value.{obj, arr, str, num, bool, null}` and opt-in
  `AsX.predef` objects in the bridges. With them every `Value.*` row above is already right (verified, table). E2 also
  edits `mkEnum` (no `"type"` for an `any`). B26 builds on E2: it is implemented after #486 merges.
- **D2 (#491/#496, design)** adds a whole-values guard where `familyAt` builds the integer enum. Different lines
  (`JsonSchemaProfile.familyAt` vs `AsDraftSupport.mkEnum`); no overlap in behavior.
- **B22** (`"type": "not"`) is the same class of bug for `not`; B26 only touches `mkEnum`.

## Options

### A. A hint resolves through an implicit; deriving it as an object, family or enum is a compile error — WON
(together with C's render guard)

`@typeHint[T]` names a JSON type, so `T` must have an implicit `Schema` or `Predef` (all built-ins do, and with E2 so
do our `Value` types), or be something whose derived schema *is* a JSON type: a value class (its underlying type), a
collection, an `Option`. When `T` has no implicit and would be derived as

- a case class or a tuple (`object`),
- a sealed family, a Scala 3 `enum`, `Either` (`family` / `oneOf`),
- a Java enum or a `scala.Enumeration#Value` (`enum`),

the macro stops with a message that names the fix:

```
@typeHint[play.api.libs.json.JsNumber] on N: JsNumber has no implicit json.Schema or json.schema.Predef, so its schema
would be derived from its structure (an object), which is not the JSON type of the values N's ToValue writes.
Name a type with a Predef: String, Int, Long, Double, BigDecimal, Boolean, ... or the JSON AST types Value.str,
Value.num, Value.bool, Value.arr, Value.obj, Value (any value). For a JSON library's AST type import its Predefs
(e.g. `import com.github.andyglow.jsonschema.AsPlay.predef._`), or provide an implicit json.schema.Predef[JsNumber].
```

The check runs only for values that come from a `ToValue`. For a family whose values are the names, the hint isn't
resolved at all any more (the schema is `string` whatever the hint, and the existing "ignored" warning stays), which also
removes the Scala 2 compile error of `@typeHint[Value]` there.

- Pros: the mistake is caught where it is made, with the fix in the message; the hint keeps meaning what the docs say;
  no output change for any hint that works today; the check sits at the two choke points that already resolve hints.
- Cons: code that compiled before stops compiling (only hints of a case class / family / enum, see Compatibility).
  A case-class hint with object values renders a *valid* `{"type": "object", "enum": [{...}]}` today; that user moves
  to `@typeHint[Value.obj]` (identical output, said in the message).

Sub-decision **A-error vs A-warn**: a warning instead of the error keeps all code compiling (patch release), but the
schema stays wrong and builds with `-Xfatal-warnings` break anyway. Error wins: the output it would let through is
wrong in every case but the case-class one, which has a one-word migration. The owner can pick A-warn; the plan is the
same with `warn` for `abort` and the old resolution kept after the warning.

### B. Hard-code the JSON AST types in the hint resolution — LOST
Map `Value.num` → `number`, `Value.str` → `string`, ... (and the bridges' AST types by name) inside both macros'
hint resolution, independent of E2.
- Pros: works without E2.
- Cons: duplicates E2's Predefs (which also serve `Json.schema[Value.num]` and fields); naming bridge types in the core
  macro couples core to every library; a case-class or family hint stays silently wrong.

### C. Fix it at render time: `mkEnum` writes `"type"` only when it is a JSON type — part of the winner, LOST alone
`mkEnum` writes `"type"` only if the value schema's `jsonType` is one of `string`, `number`, `integer`, `boolean`,
`array`, `object`, `null`, and never asks a `ref` (its `jsonType` is `???`); otherwise the `enum` has no `"type"`
(the values carry their types).
- Pros: small; covers hints that *do* have an implicit but aren't scalars (`implicit val s: Schema[Color] = ...` then
  `@typeHint[Color]` → today `"type": "enum"`), and hand-built `enum` schemas; can't crash on a `ref`.
- Cons alone: `"type": "object"` is a valid type, so `JsNumber` / case-class hints stay wrong; silent.
- Kept as step 4 of the plan: A prevents the derived cases, C makes the implicit-backed ones valid.

### D. Derive `"type"` from the values at render time, ignore the hint's type — LOST
The family carries its values, so `familyAt`/`mkEnum` could compute the JSON type(s) from them (`number` for
`Value.num`, ...) and only use the hint to group values at compile time.
- Pros: always right, no compile error.
- Cons: changes what `@typeHint` means (it would no longer pick `integer` vs `number`, which D2 relies on); silently
  ignores a wrong hint (`@typeHint[String]` with numbers); render-time output change on every profile, Legacy included.

### E. Nothing beyond E2: tests and docs — LOST
After #486 the `Value.*` hints work. B26 would add regression tests and document "the hint needs a Predef".
- Pros: no code change, no compatibility question.
- Cons: `JsNumber` without the opt-in import, case classes and families stay silently wrong; `"type": "enum"` stays.

## Decision
A (error) plus C's render guard, after E2 (#486) merges. A hint is resolved through an implicit `Schema`/`Predef`, or
derived only if the result is a JSON type (value class, collection, `Option`); a hint that would be derived as an
object, a family or an enum is a compile error naming the fix. `mkEnum` writes only real JSON types.

## Implementation plan
0. **Prerequisite:** E2 (#486) merged; rebase on `origin/master`. If E2 were dropped, add its seven core Predefs
   (`core/src/main/scala/json/schema/Predef.scala`, `jsonValueS` ... `jsonNullS`) in this PR instead, and leave the
   `AsX.predef` hint out of the message.
1. **Message** (both versions, same text, as in option A). Owner `N` is the type that carries the annotation (the root,
   or the member for a member hint); the shape is "an object" / "a sealed family" / "an enum".
2. **Scala 2, `macros/src/main/scala-2/com/github/andyglow/jsonschema/UEnums.scala`:**
   - new `resolveHint(hint: Type, owner: Type)(implicit ctx: ResolutionContext): SchemaType`:
     `Implicit.get(hint.dealias)` (as `USumTypes` L58 uses it) → that schema; else if `derivedShape(hint)` is defined →
     `c.abort(c.enclosingPosition, message)`; else `resolve(hint, ctx)`.
   - `derivedShape(t: Type): Option[String]`: on `t.dealias.typeSymbol`: a case class that isn't a value class
     (`isCaseClass && !(t <:< AnyVal)`; tuples are case classes) → "an object"; `isSealed(t)` (`UCommons` L125) →
     "a sealed family"; `sym.isJavaEnum` or `t <:< typeOf[Enumeration#Value]` → "an enum"; else `None`.
   - `EnumFamily.schemas` (L50-57): call `resolveHint` instead of `resolve`, and only for `EnumItem.FromToValue` items;
     `FromCaseObject` items get `SchemaType.Str` without resolving the hint (the warning in `EnumExtractor.fromSumType`
     L151-165 stays as is). `USumTypes` L67 (singleton members of a mixed family) goes through `schemaOf` → covered.
   - `clean` before testing (macro change).
3. **Scala 3, `core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/`:**
   - `Enums.scala` `valueSchemaOf` (L247): `case Some(h) => resolveHint(h, owner, ctx)`, owner = `item.tpe` if the
     member carries the hint, else the root (`valueSchemaOf` gets the root `TypeRepr`; callers: `singleValueSchema`
     L268, `Sums.scala` L127, which has `root.tpe`).
   - `singletonFamily` (L180): when no item is `fromToValue`, don't call `singleValueSchema` with the hints (use
     `Str(tpe)` directly, keep the `ignored` warnings).
   - `resolveHint` next to `resolve` in `Resolve.scala`: `lookupImplicit(h.dealias)` getOrElse
     { `derivedShape(h).foreach(s => report.errorAndAbort(message))`; `structural(h.dealias, ctx)` }. `derivedShape`:
     `isCaseClass` (Resolve.scala; excludes value classes, includes tuples) → "an object"; `isSealed` (Sums.scala L241;
     Scala 3 enums and `Either` are sealed) → "a sealed family"; `classSymbol(h).flags.is(Flags.JavaEnum)` or
     `h <:< TypeRepr.of[Enumeration#Value]` → "an enum".
   - `clean` before testing.
4. **Render guard, `core/src/main/scala/com/github/andyglow/jsonschema/AsDraftSupport.scala` `mkEnum`** (on top of
   E2's version): `"type"` only when `enumType(x.tpe)` is defined, `enumType` = `None` for a `ref`, for an `any`
   (E2's `` `any`.admitsEverything ``, which this subsumes), and for a `jsonType` outside the seven JSON types.
5. **Tests** (shared `api/src/test/scala`, run on JVM and JS):
   - new `TypeHintSpec.scala`: hints `Value.num`, `Value.str`, `Value.bool`, `Value.arr`, `Value.obj`, `Value`,
     `` Value.`null`.type `` on the root and per member, with a `ToValue`: the derived tree and the rendered draft-07
     and 2020-12 JSON (`{"type": "number", "enum": [1.5, 2]}`, ..., `{"enum": [1.5, "b"]}` for `Value`); a mixed family
     (a case class + hinted case objects with a `ToValue`): the singleton's value schema is `number`; controls
     (`BigDecimal`, `Double`, a value class `case class Id(v: String) extends AnyVal` → `string`).
   - compile errors with `harness.CompileErrors.errorOf(assertCompiles(...))`: hint of a case class, a tuple
     `(Int, String)`, a sealed case-object family, a Java enum (`java.time.DayOfWeek`), an `Enumeration#Value`; assert
     the message contains `@typeHint[`, `has no implicit json.Schema or json.schema.Predef` and the shape. Scala 3 enum
     hint in `api/src/test/scala-3/.../sums/Scala3EnumsSpec.scala`.
   - escape hatches compile: a user `implicit val p: Predef[W] = Predef(...)` makes `@typeHint[W]` work; an implicit
     `Schema[Color]` hint renders `{"enum": ["Red", "Blue"]}` (no `"type"`, step 4).
   - a family without `ToValue` and `@typeHint[Value]` / `@typeHint[W]`: compiles (Scala 2: was an error), string enum.
   - `AsDraft07Spec` (+ 04/12 as they pin `enum`): a hand-built `enum` over a `ref` / a `family` renders without `"type"`.
   - `modules/play-json/src/test/.../TypeHintPlaySpec.scala` (play is JVM + JS): `@typeHint[JsNumber]` without the
     import → compile error mentioning `AsPlay.predef`; with `import AsPlay.predef._` → `{"type": "number", ...}`.
   - Golden files: none change (no golden has an `enum` with a non-JSON `"type"`; checked with grep; no model uses a
     non-scalar hint). Legacy goldens untouched.
6. **Docs** (README + paradox, same words): README L332-334 and the annotation table L486, paradox `types.md` L72 and
   `descriptions.md` L85: "`T` needs a `Predef` or an implicit `Schema` (all built-in types have one, and so do the JSON
   AST types `Value.num`, `Value.str`, `Value.bool`, `Value.arr`, `Value.obj` and `Value`; for a JSON library's AST
   types import `AsX.predef._`). A hint that would be derived as a case class, a sealed family or an enum is a compile
   error." Plus a `Value.num` example next to the `ToValue` paragraph (plain `scala` or `mdoc:compile-only`; it's a
   sealed family in an object, so mdoc can take it). `makeDocs`. No CHANGELOG edit.
7. **Gates** as in the protocol (all three Scala versions JVM, `rootJS/test` on all three: core + macros + api touched).

Scala 2 vs 3: same rule and message; the shape detection uses each macro's own helpers. JS: nothing platform-specific
(compile-time checks and one renderer line in shared code).

Risks: (a) `Implicit.get` on Scala 2 has the self-reference special case (`isSelfRef`); a hint type isn't the type
being defined, so it doesn't apply, but a test with an implicit `Schema` hint covers it. (b) A hint the check misses
(e.g. a plain non-case class) still goes to structural derivation and fails there as today. (c) `Option[X]` hints stay
as today (not rejected); nobody is known to use them.

## Compatibility
- Output: no change for any hint that compiles today and has an implicit or derives to a JSON type. `"type": "enum"` /
  `"oneof"` / ... next to an `enum` disappear (step 4): invalid before, so a bug fix; it applies under Legacy too (lead
  policy 2026-11-23), and no golden-legacy file changes.
- Source: `@typeHint[T]` with `T` a case class, tuple, sealed family, Scala 3 enum, Java enum or `Enumeration#Value`
  without an implicit stops compiling, on a family with a `ToValue`. All of them render a wrong schema today except a
  case-class hint over object values (migration: `@typeHint[Value.obj]`, same output). A family without a `ToValue`
  compiles in more cases than before (the hint isn't resolved).
- Binary: no public signature changes (new private macro helpers only).
- PVP: the new compile error is breaking → minor bump (0.10.0), as B3 (#470) treats its change. With A-warn instead,
  a patch.
- Core design rule: unchanged; it is applied to hints more strictly (implicit first, derivation only to JSON types).

## Review log
