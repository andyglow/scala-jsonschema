# Generic implicit `def`s: one shared definition for the type parameter on Scala 2  (issue #297, item B8)
Status: draft

## Problem
A generic implicit that derives a schema for a generic class:

```scala
case class Foo[A](data: A)
implicit def jsonSchemaFoo[A: Schema]: Schema[Foo[A]] = Json.schema[Foo[A]]

case class Bar(x: Int)
implicit val jsonSchemaBar: Schema[Bar] = Json.schema[Bar]
case class Baz(y: String)
implicit val jsonSchemaBaz: Schema[Baz] = Json.schema[Baz]

case class Quux(foobar: Foo[Bar], foobaz: Foo[Baz])
Json.stringify(Json.schema[Quux], Version.Draft07("Quux"))
```

Inside `jsonSchemaFoo`, the macro expands `Json.schema[Foo[A]]` once, for the type parameter `A`. For the field `data: A`
it finds the evidence `Schema[A]` (an implicit `Schema`, so by the core rule a definition + `$ref`) and names the
definition at compile time: `UImplicits.get` emits `` Schema.`def`.adapt[A](evidence, signature(A)) ``, and
`signature(A)` is the type parameter's full name, `b8tmp.M.A`. Every instantiation (`Foo[Bar]`, `Foo[Baz]`) produces a
definition with that one name and a different body.

Actual output, 2.12.21 and 2.13.18 (JVM; JS is the same macro):

```json
"properties": {
  "foobar": { "$ref": "#b8tmp.M.Foo_b8tmp.M.Bar_" },
  "foobaz": { "$ref": "#b8tmp.M.Foo_b8tmp.M.Baz_" }
},
"definitions": {
  "b8tmp.M.A": { "$id": "#b8tmp.M.A", "type": "object", "properties": { "y": { "type": "string" } }, ... },
  "b8tmp.M.Foo_b8tmp.M.Bar_": { ..., "properties": { "data": { "$ref": "#b8tmp.M.A" } } },
  "b8tmp.M.Foo_b8tmp.M.Baz_": { ..., "properties": { "data": { "$ref": "#b8tmp.M.A" } } }
}
```

`b8tmp.M.A` holds only Baz's body (the renderer keeps one definition per name), so `foobar.data` must be a `Baz`.

3.3.9 is right: `foobar.data` is Bar's object and `foobaz.data` Baz's, inlined in the two `Foo` definitions. The Scala 3
derivation has the rule this item ports (lead decision 2026-11-20, `Nodes.FromImplicit` / `Implicits.definitionName`):
an implicit `Schema` for a type that *mentions an abstract type* has no compile-time name, so the instance is used as
it is at runtime: an instance that is already a `def`/`ref` keeps its own name, anything else is inlined.

A second shape fails the same way, a recursive generic class:

```scala
case class Tree[A](v: A, kids: List[Tree[A]])
implicit def jsonSchemaTree[A: Schema]: Schema[Tree[A]] = Json.schema[Tree[A]]
case class Forest(a: Tree[Bar], b: Tree[Baz])
```

Inside `jsonSchemaTree`, the recursion makes `Tree[A]` a definition named `pkg.Tree[pkg.Plain.A]`, and its kids refer to
it. The consumer (`Forest`) wraps the result with `` `def`.adapt[Tree[Bar]](x, "pkg.Tree[pkg.Bar]") ``, but `adapt`
keeps an existing definition's own name, so `Tree[Bar]` and `Tree[Baz]` are both `pkg.Tree[pkg.Plain.A]` (and `v` is
again the shared `pkg.Plain.A`). Scala 3 renames: when the implicit it found is *generic* (a given/def applied to type
arguments) and returns a definition, it renames that definition and its refs to the concrete name with
`def.toDefinition(sig)` (BACKLOG #9, `FromImplicit.generic`).

Repro: `api/src/test/scala/com/github/andyglow/jsonschema/GenericImplicitDefsSpec.scala` (shared, 14 tests).
2.13.18 today: 4 passed, 10 failed; 3.3.9: 14 passed. Failing on Scala 2 (each goes through a different path for `A`):
- `Foo[Bar]`/`Foo[Baz]` through a generic def (#297 itself): `data` is `def(pkg.Plain.A, ..)` in both; also the
  rendered draft-07 check (a `pkg.Plain.A` definition);
- recursive `Tree[A]` through a generic def: both are `def(pkg.Tree[pkg.Plain.A], ..)`;
- a generic def that names its result itself (`Json.schema[Foo[A]].toDefinition("Foo")`): both are `def(Foo, ..)`;
  Scala 3 renames them to `pkg.Foo[pkg.Bar]` / `pkg.Foo[pkg.Baz]`;
- an abstract type *member* with an implicit `Schema` (`abstract class Members { type T; implicit def tSchema: Schema[T]; ... Json.schema[Foo[T]] }`):
  `data` is `def(pkg.Members.T, ..)`, the same for every subclass;
- two type parameters, `Pair[Bar, Baz]` and `Pair[Baz, Bar]`: both share `pkg.Plain.A` and `pkg.Plain.B` (a method's
  type parameter is named after the enclosing object, so `jsonSchemaFoo[A]` and `jsonSchemaPair[A, B]` share `A` too);
- `A` inside `List[A]`, `Option[A]`, `Map[String, A]`; a generic sealed family `Res[A]` (`Ok[A]`, `Err[A]`); a
  higher-kinded parameter (`[F[_]](implicit s: Schema[F[Bar]])`, `Foo[List[Bar]]` and `Foo[Set[Bar]]`); a generic
  class's parameter (`class FooWrap[T: Schema] { val s = Json.schema[Foo[T]] }`).

Passing on both, before and after (guards): an evidence that is already a definition
(`Json.schema[Bar].toDefinition("Bar")`) keeps its name inside the generic def; an implicit `Schema[Foo[_]]`
(existential) is still a definition. Two more pin shapes that stay invalid on both versions (see Decision, tagged
`Untriaged`).

## Why it is a problem
The schema is wrong without any warning: validating a `Quux` whose `foobar.data` is a real `Bar` fails. Anyone who
writes the natural "one implicit for all `Foo[_]`" hits it (#297, 1 user; the owner's workaround is one `implicit val`
per instantiation). The same model gives a different (correct) schema on Scala 3, which breaks the "same schema on both
versions" promise of the cross-build.

## Options

### A. Port Scala 3's rule to the Scala 2 macro — WON
In `macros/.../UImplicits.scala`, `Implicit.get`, for a `FromSchema(x)` found for `tpe`:
1. if `tpe` mentions an abstract type (it is, or has as a type argument, a type parameter or an abstract type member;
   `mentionsAbstractType`, new, next to `signature` in `USignatures`), emit `x` as is: no `def.adapt`, no compile-time
   name. At runtime a `def`/`ref` instance keeps its name, anything else is inlined;
2. else, if `x` is a generic implicit (the found tree is a method applied to type arguments, `Apply(TypeApply(..), ..)`;
   `isGenericImplicit`, as on Scala 3), emit
   `` x match { case d: Schema.`def`[_] => d.toDefinition[T](sig); case o => Schema.`def`.adapt[T](o, sig) } ``;
3. else as today, `` Schema.`def`.adapt[T](x, sig) ``.

Output of the repro after the fix (both versions): `foobar` = `def(pkg.Foo[pkg.Bar], {data: Bar's object})`,
`foobaz` likewise; `Tree[Bar]` = `def(pkg.Tree[pkg.Bar], {v: Bar's object, kids: [ref(pkg.Tree[pkg.Bar])]})`.

`mentionsAbstractType` on Scala 2: `TypeRef(_, sym, args)` with `sym` a type parameter or abstract type member, or any
arg that mentions one; `RefinedType` parents, `AnnotatedType` underlying; everything else `false`. Applied after
`dealias`, like Scala 3. How a type parameter / abstract member is recognised:
`sym.isType && !sym.isClass && sym.asType.isAbstract && !sym.asType.isExistential`.
- `isAbstract` is true for a type parameter (method or class) and an abstract type member (both DEFERRED), but also for
  traits and abstract classes, hence `!isClass` (without it `Option[Bar]` or a sealed trait counts: a prototype broke 4
  existing tests). Aliases are gone after `dealias`; a concrete alias `type T = Bar` isn't abstract.
- `!isExistential`: `Schema[Foo[_]]` keeps its definition (the existential's `_$1` is abstract too).
- A higher-kinded parameter in constructor position (`F[Bar]`, `TypeRef(_, F, List(Bar))`) counts: `F` is a type
  parameter, as Scala 3's `mentionsAbstractType(tc)`. Pinned by the `HigherKinded` test.

Prototype (uncommitted, ~25 lines in the two files): `GenericImplicitDefsSpec` 14/14 on 2.13.18 and 3.3.9; the full
`clean test` on 2.12.21 and 2.13.18 (run with the first 6 tests of the spec) green in every module, no golden or
golden-legacy file changes.

Pros: Scala 2 and 3 give the same schema for the same model (the cross-build's promise); the Scala 3 rule is already a
decided, documented behaviour (README "Scala 3 notes"), so this is a parity fix, not a new design; small (one file plus
a helper); no change for any type that doesn't mention an abstract type and isn't found through a generic implicit.
Cons: inside the generic def, `Bar` is inlined although it has an implicit `Schema` (the core rule says definition):
the price of not knowing `A` at compile time, already paid on Scala 3. A user who wants `Bar` as a definition gives
it a name (`Json.schema[Bar].toDefinition("Bar")`, or Scala 3 `derives`), which then survives (case 3 of the spec).

### B. Composable rename at the consumer with unique placeholders — LOST (a starting point for a follow-up)
Keep a definition for `A` inside the generic def, but name it with a placeholder unique to its owner:
`pkg.M.jsonSchemaTree.A`, not `pkg.M.A` (a method's type parameter is named after the enclosing object on Scala 2, so
`def f[A]` and `def g[A]` clash), on both versions. At each consumer the macro knows the found method's `typeParams`
and the `TypeApply` arguments, so it emits a runtime rename `placeholder(A) → signature(arg)` of every definition and
ref in the result (segment-wise on signatures: `pkg.M.jsonSchemaTree.A` → `pkg.Bar`, `pkg.Tree[pkg.M.jsonSchemaTree.A]`
→ `pkg.Tree[pkg.Bar]`). When the argument is itself a type parameter of the enclosing generic def (`jsonSchemaTree[A]`
inside `jsonSchemaOuter[A]`), it maps to *that* def's placeholder, and the next consumer up renames it again, so chains
compose. (Suggested by r-b8, B8-R2.)

Pros: keeps the core rule for `Bar` (a definition + `$ref`, `pkg.Bar`, as for a plain `case class X(data: Bar)`); fixes
the nested recursive generic (Q10 below) that A and Scala 3 leave invalid.
Cons: changes Scala 3's output too (Scala 3 inlines today, a lead decision of 2026-11-20), or the two versions differ;
a new runtime walk that rewrites names on every generic-implicit use; a placeholder still leaks when a generic def is
called outside a macro (`implicitly[Schema[Tree[Bar]]]`, as `Tree[A]` does today on 3); the generic-class member case
(Q11) still needs its own mapping (the found tree is `w.s`, no `TypeApply`: the substitution would come from the
prefix's type arguments). Much more machinery than this item; the remaining shapes are documented instead.

### C. Compile error (or warning) for an implicit `Schema` of an abstract type — LOST
Abort with "an implicit Schema for an abstract type parameter can't be named; use one implicit val per type" when the
macro would name a definition after a type parameter.

Pros: no silent wrong schema; trivial.
Cons: breaks code that works today with a single instantiation (and on Scala 3 with any number); Scala 3 already
accepts it, so the versions would still differ; the owner's comment on #297 calls it "a nice problem" to solve, not to
forbid.

## Decision
A: the Scala 2 macro adopts Scala 3's rule. An implicit `Schema` for a type that mentions an abstract type is used as
it is (its own definition name if it has one, else inlined), and a definition returned by a generic implicit is renamed
to the consumer's concrete type, refs included. Both versions then give the same schema for #297's model.

What A leaves invalid, on **both** versions (one definition name, two bodies; today on Scala 3 too), documented as a
known issue and pinned by `Untriaged` tests so a later fix (e.g. option B) shows up as a test change:
- **Q10, a recursive generic nested in another generic schema**: `case class Outer[A](t: Tree[A])` through
  `implicit def jsonSchemaOuter[A: Schema]`, used as `Outer[Bar]` and `Outer[Baz]`: inside `jsonSchemaOuter`, `Tree[A]`
  still mentions `A`, so it is used as it is and keeps the name `jsonSchemaTree` gave it (`pkg.Tree[<A>]`). Only the
  top-level definition of a generic implicit is renamed.
- **Q11, a definition from a generic class's member**: `class TreeWrap[T: Schema] { implicit val s: Schema[Tree[T]] = Json.schema[Tree[T]] }`,
  `new TreeWrap[Bar].s` and `new TreeWrap[Baz].s` as the implicits of one schema: the found tree is a val (`a`, or
  `w.s`), not a type application, so no rename fires and both keep `pkg.Tree[pkg.TreeWrap.T]`.
- Also, as today on Scala 3: a generic implicit called outside a macro (`implicitly[Schema[Tree[Bar]]]`) keeps the
  provisional name (valid on its own, a wrong name).
Workaround for all three: one `implicit val` per instantiation (`implicit val treeBar: Schema[Tree[Bar]] = Json.schema[Tree[Bar]]`).

## Implementation plan
1. `macros/src/main/scala-2/com/github/andyglow/jsonschema/USignatures.scala`: add `mentionsAbstractType(tpe: Type): Boolean`
   (rules in option A).
2. `macros/src/main/scala-2/com/github/andyglow/jsonschema/UImplicits.scala`: `isGenericImplicit(x: Tree)` (strip
   `Typed`/`Block(Nil, _)`, then `Apply(f, _)` recurses on `f`, `TypeApply` → true); the `FromSchema` branch of `get`
   picks one of the three emissions of option A. Scaladoc comments point to the Scala 3 counterparts.
2b. The `isOwnerSelfRef`/`isSelfRef` paths are unchanged (inside `jsonSchemaFoo`, the search for `Schema[Foo[A]]` finds
   `jsonSchemaFoo` itself, is declined, and `Foo[A]` is derived; that part already works).
3. Tests: `GenericImplicitDefsSpec` (shared, the 14 tests above, written: 10 fixed by A, 2 guards, 2 `Untriaged` pins
   for Q10/Q11). Run on 2.12, 2.13, 3 (JVM and JS).
4. Golden files: none expected (no golden model uses a generic implicit def); the full `clean test` on all three
   versions confirms. No Legacy change expected (golden-legacy models don't use one); if one shows up, it falls under
   the lead's Legacy policy (bug fix, applies under Legacy) and is listed in the PR.
5. Docs (README + paradox say the same):
   - README "Definitions and `$ref`s" / paradox `definitions.md`: a short paragraph "Generic implicits" with #297's
     model: `Foo[Bar]` and `Foo[Baz]` are two definitions named after the concrete types; inside
     `implicit def f[A: Schema]: Schema[Foo[A]] = Json.schema[Foo[A]]`, the `Schema[A]` is used as it is, so **`Bar` is
     inlined inside `Foo[Bar]` even though it has an implicit `Schema`** (a plain `case class X(data: Bar)` gives
     `$ref: Bar`): no name for `A` is known at compile time. To keep `Bar` a definition there, give the instance a
     name: `implicit val jsonSchemaBar: Schema[Bar] = Json.schema[Bar].toDefinition("Bar")` (or `derives` on Scala 3).
   - README "Scala 3 notes" and paradox `scala3.md`: drop the "Inside the derived schema, an implicit Schema for an
     abstract type parameter is used as it is" sentence from the deliberate differences (it is shared now; keep the
     `derives`-specific part).
   - Known issues: README's Scala 3-only "recursive generic `derives`" entry becomes one entry for both versions
     ("Recursive generic definitions"): for a recursive `Tree[A]` with a generic implicit (`implicit def [A: Schema]`
     or `derives`), a `Tree[A]` nested inside another generic schema (Q10) and a definition from a generic class's
     member (Q11) keep the type parameter's name, so two instantiations in one schema get **one definition with one
     body: the schema is invalid**. A summoned/`implicitly` `Schema[Tree[X]]` keeps it too (valid on its own).
     Workaround: one `implicit val` per instantiation. Paradox `scala3.md`'s Known issues gets the same wording (it
     links to the README for the rest); the shared part goes into `definitions.md` if paradox has no shared Known issues
     page.
6. Gates per the protocol: `clean test` 2.12.21 / 2.13.18 / 3.3.9, `rootJS/test` on all three (macros touched),
   `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`, `makeDocs`.

## Compatibility
- Source/binary: no public signature changes (the macro's helpers are `private[jsonschema]`). Binary-compatible.
- Output (Scala 2 only; Scala 3 already does this):
  1. an implicit `Schema` for a type mentioning an abstract type that is a plain (non-`def`) instance is now inlined
     instead of a definition named after the type parameter (`pkg.M.A`). That definition was wrong whenever there were
     two instantiations, and a misleading name with one;
  2. a definition returned by a generic implicit is renamed to the consumer's concrete type, refs included (was: the
     name chosen inside the generic def, e.g. `pkg.Tree[pkg.M.A]` or a user's `toDefinition("Foo")`). The user-named
     case is a visible change for someone with a single instantiation who relied on the custom name; it matches
     Scala 3, and the fixed name is wrong as soon as there are two instantiations.
- PVP: a bug fix bringing Scala 2 in line with Scala 3; patch level (0.9.1). The owner may judge 2. as minor-worthy.
- Legacy profile: the change is in the derived tree, not the profile lowering, so Legacy output changes the same way
  for such models (lead policy: a fix that makes a schema correct applies under Legacy). No golden-legacy model is
  affected (to be confirmed by the gates). Alternative kept open: none practical, since Legacy can't recover a name the
  macro no longer emits.
- Core rule: unchanged for every concrete type. For an abstract type the rule can't name a definition at compile time;
  the documented exception (already Scala 3's) now holds on both versions.

## Review log
Round 1 (r-b8, on 5e08d2d):
- B8-R1 (must) list, document and pin the shapes A leaves invalid on both versions → accepted: Decision lists Q10
  (nested recursive generic), Q11 (generic-class member) and the non-macro `implicitly`, with the workaround; the
  Known-issues plan says "one definition with one body: invalid" for both versions; two `Untriaged` tests pin today's
  output (same name, different bodies) on all versions.
- B8-R2 (nit) add a composable-rename option → accepted: option B rewritten with owner-unique placeholders and chained
  renames, still LOST (changes Scala 3, runtime walk, Q11 needs its own mapping), credited.
- B8-R3 (nit) more guard tests → accepted: `Pair[A, B]` both orders, `List`/`Option`/`Map[String, A]`, generic sealed
  `Res[A]` (rendered check: no placeholder names), all fail on 2.13 today and pass with the prototype.
- B8-R4 (nit) spell out `mentionsAbstractType` → accepted: option A lists the predicate and why each part is there
  (`!isClass`, `!isExistential`), and that a higher-kinded `F[Bar]` counts; `HigherKinded` test pins it.
- B8-R5 (nit) docs say `Bar` is inlined → accepted: the "Generic implicits" paragraph states it with the
  `toDefinition("Bar")` workaround.
