# Scala 3: tuple types written with `*:`  (issue #467, item B10)
Status: draft

## Problem
On Scala 3 a tuple type can be spelled as a `*:` chain. `Int *: String *: EmptyTuple` is the same type as `(Int, String)`
(`summon[(Int *: String *: EmptyTuple) =:= (Int, String)]` compiles), and tuple type operations (`Tuple.Concat`,
`Tuple.Map`, `Tuple.Append`, ...) reduce to that spelling. The derivation treats it as an unrelated type:

| type (Scala 3.3.9, JVM and JS) | today |
|---|---|
| `(Int, String)` | `object {_1: integer, _2: string}` |
| `Int *: String *: EmptyTuple` | `Error inferring schema for enum: scala.*:[scala.Int, scala.*:[scala.Predef.String, scala.Tuple$package.EmptyTuple]]. No items defined` |
| field `t: Int *: String *: EmptyTuple`, `List[Int *: String *: EmptyTuple]` | the same error |
| `Tuple.Concat[Int *: EmptyTuple, String *: EmptyTuple]` | the same error (the type reaches the macro already reduced) |
| 23-element `Int *: ... *: EmptyTuple` (no `Tuple23` class exists) | the same error |
| `NonEmptyTuple` | `Error inferring schema for enum: scala.NonEmptyTuple. No items defined` |
| `Int *: Tuple` | `Error inferring schema for enum: scala.*:[scala.Int, scala.Tuple]. No items defined` |
| `Tuple` | **compiles**: `enum ["EmptyTuple"]`, description "Tuple of arbitrary arity" (the stdlib scaladoc) |
| `EmptyTuple` | `schema for scala.Tuple$package.EmptyTuple is not supported, ` |
| `Json.sig[Int *: String *: EmptyTuple]` | `scala.*:[scala.Int,scala.*:[scala.Predef.String,scala.EmptyTuple]]` |

Cause: `families` in `Resolve.scala` tries `deriveEnum` before `deriveProduct`. `*:`, `NonEmptyTuple` and `Tuple` are
sealed in the Scala 3 library; `*:` and `NonEmptyTuple` have no subclasses (the `TupleN` classes are wired in by the
compiler, not declared as children), so `Enums.singletonLeaves` returns `Some(Nil)` and `singleValueSchema` aborts with
"No items defined". `Tuple`'s children are `EmptyTuple` (an object) and `NonEmptyTuple` (no leaves), so it becomes an
enum of the one object. `deriveProduct` would never match anyway: `isCaseClass` looks at the class symbol `*:`, which is
abstract.

Repro: `api/src/test/scala-3/com/github/andyglow/jsonschema/TupleConsSpec.scala` (on the branch; it becomes the real spec).

## Why it is a problem
- Scala 3 code that builds tuple types generically (`Tuple.Concat`, `Tuple.Map[T, F]`, `Tuple.Zip`, match types over
  tuples, `Mirror.ProductOf[X].MirroredElemTypes`) produces the `*:` spelling. Such a type can't get a schema at all,
  although the very same type spelled `(A, B)` does.
- Tuples longer than 22 elements only exist as `*:` chains (`TupleXXL` at run time), so no schema is possible for them.
- The message talks about enums, which sends the user looking in the wrong place.
- `Json.schema[Tuple]` compiles to `enum ["EmptyTuple"]`: a schema that rejects every non-empty tuple, i.e. a wrong schema
  without any warning.

Scala 2 has no `*:` tuples, so Scala 2 isn't affected and doesn't change.

## Options

### A. Normalize known-arity `*:` chains to `TupleN` at the top of `resolve`; build the object directly above 22; reject unknown arity — WON
New cake trait `Tuples.scala` (Scala 3 derivation), with:
- `tupleElements(tpe): TupleShape`, walking the dealiased type: `H *: T` (an `AppliedType` of the class `scala.*:`)
  adds `H` and continues with `T` dealiased; `EmptyTuple` ends the chain; a `TupleN[A1..An]` tail (`Int *: (String, Boolean)`
  is legal) ends it with its arguments. Result: `Known(elems)`, `Empty` (`EmptyTuple` itself), `Unknown` (the chain ends in
  `Tuple`, `NonEmptyTuple`, a type parameter or anything else), or `NotATuple`.
- `normalizeTuple(tpe)`: `Known(elems)` with `1 <= n <= 22` → `defn.TupleClass(n).typeRef.appliedTo(elems)`; anything else is
  returned unchanged.

`resolve` becomes `val tpe = normalizeTuple(tpe0.dealias)`. Everything after it (the cycle check, the implicit search,
definition names, `deriveProduct`) sees `Tuple2[Int, String]`, so the cons spelling and `(Int, String)` give the same
node, the same definition name (`scala.Tuple2[scala.Int,scala.Predef.String]`) under a `given Schema[(Int, String)]`, and
the same output. A field keeps its declared type in `Field.tpe`; the field schema's type is the normalized one (as for
any alias today).

A new family `deriveTuple`, placed **before** `deriveEnum` in `families`, takes what normalization left:
- `Known(elems)` with `n > 22`: an `Obj(tpe, ProductKind.Tuple(n), fields)` with fields `_1 .. _n`, built the way
  `product` builds a tuple's fields (an `Option` element is an `Opt` and not required, like `(Int, Option[String])` today;
  no defaults, no descriptions, read-write). `Obj.emitRaw` already emits `Schema.object.fromList[T]` for any `T`, so the
  cons type works as is. A helper shared with `product` keeps the element → field step in one place.
- `Unknown` (`Tuple`, `NonEmptyTuple`, `Int *: Tuple`, `Int *: T` with `T` abstract): `report.errorAndAbort(s"schema for
  ${tpe.show} is not supported: a tuple type needs a known arity (a TupleN, or a *: chain ending in EmptyTuple), ${ctx.show}")`.
- `Empty` (`EmptyTuple`): `errorAndAbort("schema for EmptyTuple is not supported: an empty tuple has no fields, ...")`
  (see option D for the alternative).

Sample (cons, field, list, `Tuple.Concat`): exactly today's `(Int, String)` output, e.g. Draft 07
`{"type":"object","additionalProperties":false,"properties":{"_1":{"type":"integer"},"_2":{"type":"string"}},"required":["_1","_2"]}`.
23 elements: the same shape with `_1 .. _23`.

Pros: one place, all entry points (`Json.schema`, `objectSchema`, `derives`, `DerivedSchema`, `auto`, nested fields, collection
elements, Option/Either arms) get it because they all go through `resolve`; implicits and definitions stay consistent with
the `TupleN` spelling (core rule untouched: an implicit `Schema[(A, B)]` is still a definition for the cons spelling too).
Cons: a small new file; `>22` needs its own field builder (shared with `product`).

### B. A `deriveTupleCons` family only (no normalization) — LOST
Same family as in A, but it handles every arity and builds the `Obj` from the chain; the type stays `*:[...]` throughout.
Pros: no change to `resolve`. Cons: the implicit search still finds a `given Schema[(Int, String)]` for the cons type (the
types are `=:=`), but the definition name comes from the cons signature (`scala.*:[scala.Int,scala.*:[...]]`), so a model
that uses both spellings gets two definitions with the same body; the recursion check keys on the type too
(`onStack` by `=:=`, so that part would be fine). Lost on the definition names.

### C. Better error message only — LOST
Keep the types unsupported, but give `*:` / `Tuple` a "use `(A, B)`" error. Pros: smallest change. Cons: the cons spelling
is not something the user can always rewrite: it comes out of `Tuple.Concat`/`Tuple.Map`/mirrors, and >22 has no other
spelling. Supporting it costs about as much as rejecting it well.

### D. `EmptyTuple` as an empty object — LOST (alternative for the owner)
`EmptyTuple` → `Obj(tpe, ProductKind.Tuple(0), Nil)`: `{"type":"object","additionalProperties":false}`, which accepts `{}`.
It would make a generic `Box[T <: Tuple](t: T)` with `Box[EmptyTuple]` work. Lost because no JSON library we bridge encodes
`EmptyTuple` as `{}` (circe/upickle/jsoniter don't have a tuple-as-object encoding at all; it's our `_1.._N` convention),
and it's not something anyone asked for. Switching to D is one line
in `deriveTuple`.

## Decision
A: normalize `*:` chains of known arity (1–22) to `TupleN` at the top of `resolve`, so the cons spelling is the same type
everywhere (output, implicits, definition names); build the same `_1 .. _N` object directly for more than 22 elements; reject
tuple types of unknown arity (`Tuple`, `NonEmptyTuple`, `H *: Tuple`) and `EmptyTuple` with an error that says why.

## Implementation plan
1. `core/src/main/scala-3/.../derivation/Tuples.scala` (new): `TupleShape`, `tupleElements`, `normalizeTuple`, `deriveTuple`.
   The `*:` class via `Symbol.requiredClass("scala.*:")` (`TypeRepr.of[*:[Any, Tuple]].typeSymbol`), `EmptyTuple` via
   `=:= TypeRepr.of[EmptyTuple]`, `TupleN` via `defn.isTupleClass` (the arguments of the applied type).
2. `Derivation.scala`: mix in `Tuples`. `Resolve.scala`: `val tpe = normalizeTuple(tpe0.dealias)`; `deriveTuple` first in
   `families` (before `deriveOption`; none of the families before `deriveEnum` match tuple types, so the position is only for
   clarity), with a comment.
3. `Products.scala`: factor the element → `Field` step of `product` (the `optionElement` / `Opt` part) into a helper that
   `deriveTuple` uses for `>22`.
4. Root type texts: `Derivation.build` reads the root's title/description with `typeAnnotations(tpe)` on the **original**
   `TypeRepr.of[T]`, not on the type `resolve` normalized. For a cons root that is `*:[...]`, whose stdlib scaladoc (read from
   TASTy with `-Yread-docs`) would become the description, as `Tuple`'s does today. Fix: `build` reads the texts of
   `normalizeTuple(tpe.dealias)` when that is a `TupleN` (`Tuple2` comes from the Scala 2 library, no TASTy docs: the same empty
   texts as `(Int, String)` today), and none for a `>22` chain. Pinned in the tests (root cons, 2 and 23 elements: no description).
5. Tests: `api/src/test/scala-3/com/github/andyglow/jsonschema/TupleConsSpec.scala` (Scala 3 only, runs on JVM and JS):
   - `Json.schema[Int *: String *: EmptyTuple] shouldEqual Json.schema[(Int, String)]`; the same for a field
     (`HasCons` vs a `HasTuple` case class), a `List` element, an `Option` element (`Int *: Option[String] *: EmptyTuple`:
     `_2` not required), `Tuple.Concat[...]`, `Int *: (String, Boolean)`;
   - rendered Draft 07 JSON of the cons form equals the `(Int, String)` golden output (`groupC/Tuple2[Int,String]`);
   - a `given Schema[(Int, String)]` in scope: the cons-typed field is a `def` named `scala.Tuple2[scala.Int,scala.Predef.String]`;
   - 23 elements: `object` with `_1 .. _23`, all required, in order, no description;
   - `typeCheckErrors` for `Tuple`, `NonEmptyTuple`, `Int *: Tuple`, a generic `def f[T <: Tuple] = Json.schema[Int *: T]`, and
     `EmptyTuple`: each message contains "known arity" / "empty tuple" and not "enum".
   No golden-file changes (no existing output changes; the only change in what compiles is `Json.schema[Tuple]`, see below).
6. Docs: one bullet in README's Scala 3 notes (the list that ends "Compile errors quote Scala 3 code", ~L950) and the same
   bullet in `docs/src/main/paradox/scala3.md`: "**`*:` tuples** (`Int *: String *: EmptyTuple`, what `Tuple.Concat`/`Tuple.Map`
   produce) are the same object as `(Int, String)`, also beyond 22 elements; `Tuple`, `NonEmptyTuple` and `EmptyTuple` are
   compile errors." `makeDocs` gate.
7. Gates per the protocol: `clean test` on 2.12.21 / 2.13.18 / 3.3.9, `rootJS/test` on all three (core and api change),
   `scalafmtCheckAll`, `rootJS/scalafmtCheckAll`, `makeDocs`.

`Json.sig` is not changed: it prints the type as the user wrote it (`scala.*:[...]`), the way it prints any other spelling.
Definition names do change for the cons spelling (it had none: it never compiled).

## Compatibility
- Output: nothing that compiled before changes, except `Json.schema[Tuple]` (and a `Tuple`-typed field), which compiled to
  `enum ["EmptyTuple"]` and becomes a compile error. That schema was wrong (it rejects every non-empty tuple); the lead's
  Legacy policy treats it as a bug, not a style. Legacy profile: no golden-legacy change.
- Source: the cons forms that failed now compile; `Tuple` stops compiling (above). No public signature changes (the new
  trait is internal to the derivation).
- Binary: none (macro-internal). Scala 2: untouched. PVP: patch (0.9.1), unless the owner counts the `Tuple` error as breaking.

## Review log
