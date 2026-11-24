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

Repro: `api/src/test/scala/com/github/andyglow/jsonschema/GenericImplicitDefsSpec.scala` (shared, 6 tests).
2.12.21 and 2.13.18: 2 passed, 4 failed; 3.3.9: 6 passed. Failing on Scala 2:
- `Foo[Bar]`/`Foo[Baz]` through a generic def (#297 itself): `data` is `def(pkg.Plain.A, ..)` in both;
- recursive `Tree[A]` through a generic def: both are `def(pkg.Tree[pkg.Plain.A], ..)`;
- a generic def that names its result itself (`Json.schema[Foo[A]].toDefinition("Foo")`): both are `def(Foo, ..)`;
  Scala 3 renames them to `pkg.Foo[pkg.Bar]` / `pkg.Foo[pkg.Baz]`;
- an abstract type *member* with an implicit `Schema` (`abstract class Members { type T; implicit def tSchema: Schema[T]; ... Json.schema[Foo[T]] }`):
  `data` is `def(pkg.Members.T, ..)`, the same for every subclass.

Passing on both (guards): an evidence that is already a definition (`Json.schema[Bar].toDefinition("Bar")`) keeps its
name inside the generic def; an implicit `Schema[Foo[_]]` (existential) is still a definition.

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

`mentionsAbstractType` on Scala 2: `TypeRef(_, sym, args)` with `sym` a type parameter or abstract type member
(`sym.isType && sym.asType.isAbstract`) that is not existentially bound (`!sym.isExistential`, so `Schema[Foo[_]]`
keeps its definition), or any arg that mentions one; `RefinedType` parents, `AnnotatedType` underlying; everything else
`false`. Applied after `dealias`, like Scala 3.

Pros: Scala 2 and 3 give the same schema for the same model (the cross-build's promise); the Scala 3 rule is already a
decided, documented behaviour (README "Scala 3 notes"), so this is a parity fix, not a new design; small (one file plus
a helper); no change for any type that doesn't mention an abstract type and isn't found through a generic implicit.
Cons: inside the generic def, `Bar` is inlined although it has an implicit `Schema` (the core rule says definition):
the price of not knowing `A` at compile time, already paid on Scala 3. A user who wants `Bar` as a definition gives
it a name (`Json.schema[Bar].toDefinition("Bar")`, or Scala 3 `derives`), which then survives (case 3 of the spec).

### B. Rename at the consumer, keep the type parameter's definition — LOST
Keep `def(pkg.M.A, ...)` inside the generic def. At the consumer (`Quux`), the macro knows the found implicit is
`jsonSchemaFoo[Bar](..)`, so it knows the substitution `jsonSchemaFoo`'s `A` → `Bar`. It would emit a runtime rename of
every definition and ref inside the result whose signature mentions `pkg.M.A` (segment-wise string substitution,
`pkg.M.A` → `pkg.Bar`, `pkg.Tree[pkg.M.A]` → `pkg.Tree[pkg.Bar]`). Chains compose (a generic def calling another
generic def renames at each level).

Pros: keeps the core rule for `Bar` (a definition + `$ref`, `pkg.Bar`); also fixes nested generic definitions, which
A (and Scala 3) only rename at the top level.
Cons: changes Scala 3's output too (Scala 3 inlines today, a lead decision) or the two versions differ; a new
runtime tree walk that rewrites names by string matching (type-parameter full names are not unique by construction:
`def f[A]` and `def g[A]` in one object both give `pkg.M.A`, so a nested schema from another generic def could be
renamed by mistake); a non-macro consumer (`implicitly[Schema[Foo[Bar]]]`) still gets the shared `pkg.M.A` name. Much
more machinery for an edge the README can document.

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

## Implementation plan
1. `macros/src/main/scala-2/com/github/andyglow/jsonschema/USignatures.scala`: add `mentionsAbstractType(tpe: Type): Boolean`
   (rules in option A).
2. `macros/src/main/scala-2/com/github/andyglow/jsonschema/UImplicits.scala`: `isGenericImplicit(x: Tree)` (strip
   `Typed`/`Block(Nil, _)`, then `Apply(f, _)` recurses on `f`, `TypeApply` → true); the `FromSchema` branch of `get`
   picks one of the three emissions of option A. Scaladoc comments point to the Scala 3 counterparts.
2b. The `isOwnerSelfRef`/`isSelfRef` paths are unchanged (inside `jsonSchemaFoo`, the search for `Schema[Foo[A]]` finds
   `jsonSchemaFoo` itself, is declined, and `Foo[A]` is derived; that part already works).
3. Tests: `GenericImplicitDefsSpec` (shared, the 6 repro tests above, written). Add: (d) a rendered draft-07 check
   of #297's model (no `pkg.A` definition, two `Foo` definitions with their own `data`); (e) a class type parameter
   (`class Wrap[T: Schema] { val s = Json.schema[Foo[T]] }`), which goes through the same path as a method's. All
   shared, run on 2.12, 2.13, 3 (JVM and JS).
4. Golden files: none expected (no golden model uses a generic implicit def); the full `clean test` on all three
   versions confirms. No Legacy change expected (golden-legacy models don't use one); if one shows up, it falls under
   the lead's Legacy policy (bug fix, applies under Legacy) and is listed in the PR.
5. Docs (README + paradox say the same):
   - README "Definitions and `$ref`s" / paradox `definitions.md`: a short paragraph "Generic implicits" with #297's
     model: inside `implicit def f[A: Schema]: Schema[Foo[A]] = Json.schema[Foo[A]]`, the `Schema[A]` is used as it
     is (a definition keeps its name, anything else is inlined), and `Foo[Bar]` is a definition named after `Foo[Bar]`.
   - README "Scala 3 notes" and paradox `scala3.md`: drop the "Inside the derived schema, an implicit Schema for an
     abstract type parameter is used as it is" sentence from the deliberate differences (it is shared now; keep the
     `derives`-specific part).
   - Known issues (README; paradox `scala3.md` keeps its Scala 3 entry): the recursive-generic limitation now applies
     to Scala 2 generic implicit defs too: `implicitly[Schema[Tree[X]]]` (not through a macro) and a `Tree[A]` nested
     inside another generic schema keep the type parameter's name. Reword the entry to cover both versions.
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
