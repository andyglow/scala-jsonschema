# A nested derived type keeps its own `@title` / `@description` / scaladoc; annotations win over scaladoc  (issue #501, item B12)
Status: draft

Builds on B1 (#469, `gap/b1-title-desc`): B1 makes the renderers emit `title`/`description` on every node kind, which is
what makes a nested value class's or enum's texts visible at all. This item is about what the **derivation** puts in the
tree. The precedence question in part 2 was folded into B12 by the lead (finding of r-b12).

## Problem

### 1. Nested types get no texts
Both macros apply a type's own `@title`, `@description` and scaladoc description only in these places:
- the **root** of the expansion: Scala 2 `Macroses.deriveInternal` (L80, L96-101), Scala 3 `Derivation.build` (L37-43,
  comment "R8: nested types don't get them");
- hence also the body of an implicit `Schema` written as `implicit val s: Schema[X] = Json.schema[X]` (it was the root of
  its own expansion), and `derives json.Schema`;
- **members of a sealed family** (member texts, applied at lowering).

Every other place where a type is derived from its structure gets nothing: a field's type, a `List`/`Set`/`Map`/`Option`
element, a nested sealed family or case-object enum, a Scala 3 `enum`, a nested value class, the body of a recursive
definition. A nested type's scaladoc `@param` tags *are* used (its fields get their descriptions), only the type's own
description is lost.

Repro (`notes/scratch/c-b12/ZzB12ProbeSpec.scala`, `ZzB12Probe3Spec.scala`, not committed; logs `repro-{212,213,3}.log`):

```scala
@title("Inner T") @description("Inner D") case class B12Inner(a: Int)
/** Doc of B12DocInner.
  * @param a the a */                     case class B12DocInner(a: Int)
@title("VC T") @description("VC D")       case class B12VC(v: String) extends AnyVal
@title("Color T") @description("Color D") sealed trait B12Color   // case objects Red, Blue
@title("Shape T") @description("Shape D") sealed trait B12Shape   // case classes Circle, Square
@title("Tree T") @description("Tree D")   case class B12Tree(kids: List[B12Tree])
@title("Level T") @description("Level D") enum B12Level { case Low, High }          // Scala 3

case class B12Outer(i: B12Inner, doc: B12DocInner, vc: B12VC, color: B12Color, shape: B12Shape,
                    @description("field D") fi: B12Inner, @title("field T") ft: B12Inner,
                    li: List[B12Inner], oi: Option[B12Inner], mi: Map[String, B12Inner])
case class B12Forest(t: B12Tree)
```

Tree of `Json.schema[B12Outer]`, identical on 2.12.21, 2.13.18 and 3.3.9:

```
object(i: object(a: integer), doc: object(a: integer description=`the a`), vc: value-class(string),
       color: family(B12Red:Singleton(..), B12Blue:Singleton(..)), shape: family(B12Circle:Product(..), B12Square:Product(..)),
       fi: object(a: integer) description=`field D`, ft: object(a: integer), li: array(object(a: integer)),
       oi: option(object(a: integer)), mi: dictionary(object(a: integer)))
```

No title or description anywhere. `Json.schema[B12Inner]`, `Json.schema[B12Color]`, `Json.schema[B12DocInner]` as roots do
have them. `B12Forest.t` is `def(B12Tree, object(kids: ..))` with no texts on either node, so the definition body renders
without them. With `import Json.auto._` the nested type is still derived structurally and gets nothing (both versions).
Draft-07 for `i` today: `{"type": "object", "additionalProperties": false, "properties": {"a": {"type": "integer"}}, "required": ["a"]}`.
The Scala 3 `enum B12Level` as a field: `family(Low, High)`, no texts.

Probe result: 12 of 16 shared cases fail on each Scala version (plus the Scala 3 `enum` case on 3.3.9); the 4 that pass are
the printout, the root controls, the nested `@param` and an implicit-`Predef` root (which keeps the Predef's own title).
r-b12 reproduced it independently on B1's branch (`notes/reports/r-b12.md`). Pinned on purpose today by characterization tests:
`EntryPointsCharSpec` "R8" (`char/entry/HasTitled.json`) and `ProductsCharSpec` "T9" (`char/products/HasId.json`).

History: releases **0.3.0 to 0.5.1** (Oct-Nov 2020; `SchemaMacro.CaseClass.gen`, checked in the published
`scala-jsonschema-macros_2.13-0.5.1-sources.jar`) applied a case class's title/description wherever it was derived,
nested included. The 0.6.0 refactor (`d29c3f6` "refactor Macroses") commented that code out and moved the texts to the
root only; no changelog entry, no test. 0.7's README then described the result ("3 places ...: root model level,
definition level, one-of level"), and the Scala 3 port kept it for parity.

### 2. Scaladoc wins over `@description`; the docs say the reverse
README L447 and paradox `descriptions.md` L44: "When several sources are present, the order is: decorations, then
annotations, then scaladoc." 0.7's README had the same table (Config -> Annotation -> Scaladoc), written the day the
feature landed (`9e8cc11`, 2020-10-03). The code, since that same release, does decorations -> **scaladoc** -> annotations,
for the type's description and for field descriptions:
- type: Scala 2 `UTypeAnnotations.Texts.apply` (`fromSD.fold(fromAN) { _ orElse fromAN }`), Scala 3
  `Annotations.symbolAnnotations` (`fromScaladoc orElse fromAnnotation`); used for roots, sealed members, case objects;
- field: Scala 2 `UProductTypes` (`fromFieldAnnotations ++ scaladocFD ++ specFD`, later wins), Scala 3
  `Products.fieldDescriptions` (`fromAnnotations ++ fromScaladoc ++ fromSpec`).

```scala
/** Type scaladoc
  * @param a from scaladoc */
@description("type annotation")
case class Precedence(@description("from annotation") a: Int, @description("only annotation") b: Int, c: Int)
```
→ `"description": "Type scaladoc"`, `a.description = "from scaladoc"` (both versions; pinned by `ScaladocCharSpec` "F7",
`char/scaladoc/Precedence{,.objectSchema}.json`). `@title` has no scaladoc source, so it isn't affected.

## Why it is a problem
- Silent loss: a user who annotates or documents a model and uses it as a field (the normal case: only the top-level type
  is a root) gets no title/description for it. Form generators and doc tools read exactly these. README's annotation
  table says `@title`/`@description` apply to a "type", with no "root only" restriction (B1 adds one, as a stopgap).
- It's inconsistent within one type: the nested type's scaladoc `@param`s apply, its scaladoc description doesn't; a
  value class's texts apply as the root (golden `char/products/Id.json`) but not as a field (`HasId.json`).
- Part 2: the documented order is the useful one. `@description` is written for the schema; scaladoc is developer
  documentation that may say things not meant for API consumers. Today an annotation can't override a doc comment, so
  the only way to give a documented class another schema description is a runtime `.withDescription` at every root.
- With part 1, scaladoc applies to many more types, so part 2 matters more.

## Options

### Part 1: where nested types get their texts

#### A. At resolve time, on every structurally derived node — WON
When `resolve` derives a node from the type's structure (step 3 of the core rule), the node takes the type's own
title/description, its existing texts first. Not when the node comes from an implicit `Schema`/`Predef` (the implicit's
schema is the user's whole answer, core rule) and not for `Option`/collection/map nodes (library types: nothing to read).

- Scala 3, `derivation/Resolve.scala`: `searched getOrElse withTypeTexts(tpe, structural(tpe, ctx))`, with
  ```scala
  private def withTypeTexts(tpe: TypeRepr, n: Node): Node = n match {
    case _: Opt | _: Arr | _: Dict => n
    case _ =>
      val ta = typeAnnotations(tpe)
      if (ta.title.isEmpty && ta.description.isEmpty) n
      else n.withExtra(n.extra.copy(title = n.extra.title orElse ta.title, description = n.extra.description orElse ta.description))
  }
  ```
- Scala 2, `Macroses.resolve.genTree`: `Enum`/`SumType`/`CaseClass`/`ValueClass` results go through the same helper,
  built on `Texts(tpe)` (not `withTypeAnnotations`, which would also copy `@discriminatorKey` onto a non-member).
- The root steps (`deriveInternal`, `Derivation.build`) stay as they are, because an implicit root still takes its type's
  texts there (parity decision 2026-11-20). They skip a root that is a `def` (a recursive root), so its texts stay on the
  definition body that `resolve` put them on and don't move onto the `def` node, where B1 would render them as `$ref`
  siblings at the root.
- Sealed-family members are built directly (`CaseClass(st)` / `product(c.tpe, ..)`, not through `resolve`), so their
  schemas and the member texts are unchanged. The types of their fields go through `resolve` and gain texts like any field's.
- A recursive type: `substituteRecursive` wraps the already-texted node in a `def`. Every use is a `$ref` without texts,
  and `definitions.X` has them once.
- At a field: a field's `@description` / scaladoc `@param` (on `Field.description`) and a field's `@title` (B1: on the
  field's schema, `fold` over the type's) still win, so only fields without their own texts change.

Prototype (`notes/scratch/c-b12/proto-nested.patch`, on B1's branch at 33987cf): all probe cases pass, and every JVM
test passes after regenerating exactly 6 golden files (3 neutral + the same 3 legacy, see "Output changes"):
`clean test` on 2.13.18 and 3.3.9 with this patch, and on 2.12.21 with the combined patch (`proto-all.patch`, parts 1+2). Bridge petshop
suites didn't change: texts don't affect validation.

Pros: one rule that covers every derived kind (case class, value class, sealed family, case-object enum, Scala 3 enum,
generic case class), every position (field, element, map value, `Option`, recursive definition; an `Either` side or a
tuple element by the same path, to be tested), on both versions, in about 15 lines per macro. Families added later (E6's unions/opaque types) go through
`structural` and are covered too. Cons: an output change for every annotated or documented nested type (that's the fix);
scaladoc is read once more per derived product (it's already parsed for `@param`s; a per-symbol cache is optional).

#### B. Case classes only (restore 0.3-0.5.1) — LOST
Same mechanism, only for `Obj` nodes. That was the old behavior, but nested value classes, families and enums (which B1
just made renderable) would keep losing their texts, and we'd need to explain why.

#### C. Keep root-only, document it — LOST
B1's README wording already says so. The cost: the most common way of using an annotated model drops its texts silently,
and the README table would need a "root only" footnote. It's also half-applied, since `@param`s of nested types apply.

#### D. A compile-time switch (`json.schema.Flag`, like `Flag.EnumsAsOneOf`) — LOST as the default, available as an escape hatch
`implicit val f: Flag.RootTextsOnly = ...` in scope at the `Json.schema` call would restore 0.7's trees (or the reverse
for an opt-in). Both macros already read flags (`hasFlag`). It isn't a profile knob because texts are added at compile
time and a profile works at render time (see L2). I'd add it only if the owner wants a way back to 0.7 output that
doesn't depend on the profile.

### Part 2: scaladoc vs `@description`

#### P1. Annotations over scaladoc (the docs are right, change the code) — WON
Reorder the four places above: type `fromAnnotation orElse fromScaladoc` (Scala 2 `Texts.apply`: `fromSD.fold(fromAN)(fromAN orElse _)`),
field `scaladoc ++ annotations ++ objectSchema decorations` (later wins). The full order is then: runtime `.withTitle`/
`.withDescription` > `Json.objectSchema` decorations > `@description` > scaladoc, and at a field: the field's own
(decoration, `@description`, `@param`) > the field type's (annotation, scaladoc). Because it's done in the shared readers,
it also applies to sealed members and case objects (Scala 2 `TypeAnnotations`, Scala 3 `symbolAnnotations`).
Prototype (`proto-all.patch`): only `char/scaladoc/Precedence{,.objectSchema}.json` change (neutral and legacy).
Pros: matches the docs (0.7 and 0.9), and the explicit, schema-specific source wins, so one annotation can override a
doc comment. Cons: an output change for types/fields that have both (rare: only that test model in our suites).

#### P2. Keep the code, fix the docs ("decorations, then scaladoc, then annotations") — LOST
No output change. But the order makes the annotation useless whenever there's a doc comment. The docs have described the
other order since the feature was added, so users who relied on it got the wrong text all along.

### Legacy profile

#### L1. The changes apply under Legacy too — WON (the lead must confirm: is this a bug or 0.7 style?)
Under the lead's Legacy policy (style frozen, bugs fixed), both parts are bugs: part 1 is a silent regression of
0.3-0.5.1 behavior, which README's annotation table never limited to the root; part 2 is code contradicting the docs.
golden-legacy gets the same 5 changes as golden (listed below). Counter-argument, stated fairly: 0.7's README did describe
part 1's result ("root model level, definition level, one-of level"), so it can be read as documented 0.7 style. If the
lead reads it that way, the choice is L2 (costly) or D (a flag, profile-independent).

#### L2. Legacy keeps 0.7's texts — LOST
Legacy is a render-time profile, and by then a nested type's texts look the same as a user's `.withTitle`. Stripping
them needs a hidden marker on each node ("texts from the type, nested"), set by both macros, kept by `duplicate`/copies,
excluded from equality, and cleared by `Legacy.lower`. That's a lot of hidden ADT state for doc keywords. Part 2 can't be
undone at render time at all (the scaladoc text isn't in the tree).

## Decision
A: every structurally derived node takes its type's own title/description at resolve time, on both macros (implicits,
collections and `Option` excluded; a recursive type's texts go on the definition body). P1: `@description` wins over
scaladoc, for type and field descriptions, as the docs say. L1: under every profile, Legacy included (lead to confirm).

## Implementation plan
0. Prerequisite: B1 (#469) merged; rebase on master. Without B1, the new texts on non-object nodes (value class, enum,
   family) are in the tree but not rendered.
1. Scala 3, `core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/`:
   - `Resolve.scala`: `withTypeTexts` as in option A; `resolve` applies it to `structural(...)` results only (not to
     `lookupImplicit` results, not to `onCycle` refs). Update the trait's scaladoc (step 4 now also "plus the type's texts").
   - `Derivation.build`: skip `Def` in the root step (`case _: Def => node`); rewrite the "R8" comment: the root step is
     for an implicit root (parity) and, for a structural root, a no-op repeat.
   - `Annotations.scala`: `symbolAnnotations` → `description = fromAnnotation orElse fromScaladoc`; fix the doc line on
     `TypeAnnotations` ("nothing here is applied automatically except the root's texts").
   - `Products.scala`: `fieldDescriptions` → `fromScaladoc ++ fromAnnotations ++ fromSpec`, and its scaladoc
     ("objectSchema config > `@description` > scaladoc `@param`").
2. Scala 2, `macros/src/main/scala-2/com/github/andyglow/jsonschema/`:
   - `Macroses.scala`: in `resolve`, a local `withTypeTexts(x: SchemaType)` from `Texts(tpe)` (existing extra first),
     applied to the `Enum`, `SumType`, `CaseClass`, `ValueClass` cases of `genTree` (not `Opt`, `Dict`, `Arr`, not the
     implicit). `deriveInternal`: skip `U.Def` in the root step.
   - `UTypeAnnotations.scala`: `Texts.apply` → `fromSD.fold(fromAN)(fromAN orElse _)`.
   - `UProductTypes.scala`: `val fd = scaladocFD ++ FieldDecorations.fromFieldAnnotations(resolvedFields) ++ specFD`.
     `UFieldDecorations.scala`: the order in its scaladoc.
   - `clean` before testing (macro changes).
3. Tests (shared unless noted; both versions must give the same trees):
   - New `api/src/test/scala/com/github/andyglow/jsonschema/NestedTextsSpec.scala`, from the probe: nested case class,
     scaladoc-only nested type, value class, case-object family, case-class family, `List`/`Option`/`Map` element, a
     generic `Gen[Int]`, an `Either[Inner, Int]` side, a tuple element, a member's field type (`Fam.A(i: Inner)`), the
     same type in two fields (inlined twice, both with texts), `import Json.auto._`. Tree assertions plus rendered
     draft-04/07/2020-12 for a few.
   - Precedence at a field: `@description` on the field beats the type's description; field `@title` (B1) beats the
     type's title; a field scaladoc `@param` beats the type's description.
   - Recursion: `case class Forest(t: Tree)` → `definitions.Tree` has the texts, the `$ref` has none; a titled recursive
     **root** `Json.schema[Tree]` → texts in the definition body, not as `$ref` siblings at the root.
   - Implicits untouched: a nested type with an implicit `Schema` built by hand without texts stays without; with an
     implicit `Predef` likewise; an implicit-provided sealed member still gets none (Scala 2 quirk, kept).
   - `Option` field under Neutral (texts on the nullable `oneOf`/type array) and under Legacy (unwrapped, texts kept).
   - Scala 3 only (`api/src/test/scala-3/...`): a nested Scala 3 `enum`; `derives json.Schema` unchanged.
   - Cross-module: in `CrossModuleScaladocSpec`, `case class HoldsOther(f: FromOtherModule)` → `f` has "A model from
     another module" (Scala 2 via the scaladoc plugin, Scala 3 via `-Yread-docs`).
   - Part 2: a type and a field with both a scaladoc and `@description` → the annotation; a sealed member and a case
     object with both → the annotation.
   - Characterization tests change meaning: rename `EntryPointsCharSpec` "R8" ("a nested case class reached via a field
     gets its own title/description"), `ProductsCharSpec` "T9" ("a value class gets its type texts as the root and as a
     field"), `ScaladocCharSpec` "F7" ("`@description` beats scaladoc").
4. Golden files: `GOLDEN_UPDATE=1 GOLDEN_UPDATE_LEGACY=1 sbt clean api/test` (Legacy rewrite only because of L1), then
   check that the diff is exactly these files:
   `golden{,-legacy}/char/entry/HasTitled.json`, `char/products/HasId.json`, `shared/DocumentationSpec.Outer.json`
   (tree only), `char/scaladoc/Precedence.json`, `char/scaladoc/Precedence.objectSchema.json`. Any other change is a bug.
5. Docs (README and paradox `descriptions.md`, same words):
   - Replace B1's sentence "A type's own annotations and scaladoc apply where it is the root, ... a type derived inline as
     a field's (or an element's) type doesn't get them" with: a type's own `@title`/`@description`/scaladoc apply
     wherever it's derived (root, field, element, member, definition); not where an implicit `Schema`/`Predef` is used
     (the implicit's own texts count); a field's own texts win over its type's.
   - Below the annotation table: drop B1's "A type's `@title`/`@description` apply where it is the root, ...".
   - The precedence sentence (L447) stays, now true; add the field rule ("a field's description: decorations, then
     `@description`, then the scaladoc `@param`; then the field type's own").
   - `makeDocs`. No CHANGELOG (lead).
6. Gates (core + macros + api): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on all three (no platform code,
   but the derivation is shared); `scalafmtCheckAll` + `rootJS/scalafmtCheckAll` (touch the sources first).

Risks / notes for the implementer:
- Scala 3 with `-Yread-docs`: a case class from a library jar with TASTy docs, derived structurally as a field, now
  gets its scaladoc as description, as it already does as a root. Scala 2 only sees scaladoc of sources in the current
  run or embedded by the plugin. Same as the root today; worth a line in the docs.
- Scala 2 `SchemaType.equals` compares trees, texts included. `EnumFamily` groups by value schema (primitives): unaffected.
- `scala.Enumeration` values and Java enums can't carry our annotations (`Color.Value` is the library's class); unchanged.
- Neighbours: B1 (prerequisite); B13/#498 (value class `toDefinition`/`withValidationsAddedFrom` dropping texts:
  separate, untouched here); B3/#461 (members are not touched here, so no overlap); E6 (new structural families get the
  texts for free through `structural`).

## Output changes (prototype on B1's branch, Neutral and Legacy alike)
- `char/entry/HasTitled.json`: `properties.t` gains `"title": "Nested Title", "description": "Nested Description"` (and the tree dump).
- `char/products/HasId.json`: `properties.id` gains `"title": "Id", "description": "A wrapper"` (value class as a field).
- `shared/DocumentationSpec.Outer.json`: tree dump only (the nested `inner` gets "My perfect class"); rendered output
  unchanged because the field's description "Inner Param" wins.
- `char/scaladoc/Precedence.json`: root `"description": "Type scaladoc"` → `"type annotation"`; `a`: `"from scaladoc"` →
  `"from annotation"`. `Precedence.objectSchema.json`: root description likewise (field `a` keeps the decoration).
- In user code: new `title`/`description` keys on every annotated/documented nested derived type; a different
  description where a type or field has both a scaladoc and an `@description`; a titled recursive root's texts move from
  next to the root `$ref` (B1) into `definitions`.

## Compatibility
- Source/binary: no public signature changes (macro internals only). Patch under PVP (0.9.x), like B1: output-only fixes.
- Output: additive for part 1 (new keys, nothing removed); part 2 changes a description's value where both sources
  exist. Both are listed for the lead's CHANGELOG.
- Legacy: changes too (L1, needs the lead's OK); the alternative is L2 or the flag D.

## Open questions for the owner/lead
1. L1: is root-only texts a 0.7 bug (fix under Legacy) or 0.7 style (keep under Legacy, via L2 or D)?
2. Want flag D as an escape hatch anyway?

## Review log
