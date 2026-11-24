# Scala 2: compiler crash on a case class with a non-public val parameter  (issue #459, item B4)
Status: draft

## Problem
On Scala 2.12 and 2.13 the derivation macro crashes the compiler for a case class whose constructor has a `private val`,
`private[pkg] val` or `protected val` parameter. Scala 3 derives the schema.

```scala
case class PrivateVal(private val a: Int, b: String)
case class PrivatePkgVal(private[characterization] val a: Int, b: String)
case class ProtectedVal(protected val a: Int, b: String)
Json.schema[PrivateVal]
```

Actual, 2.12.21 and 2.13.18 (JVM and JS: it is a compile-time crash), for every one of the three:

```
ProductsCharSpec.scala:48:50: exception during macro expansion:
java.lang.AssertionError: assertion failed:
  method a$access$0
     while compiling: .../ProductsCharSpec.scala
        during phase: typer
```

3.3.9 renders the expected schema: `{"type":"object","additionalProperties":false,"properties":{"a":{"type":"integer"},"b":{"type":"string"}},"required":["a","b"]}`.
Field-targeted annotations on such params work on Scala 3 too: `@(description @field)("from the field") private val a`
gives `a` the description, `@(readOnly @field) protected val c` makes `c` read-only.

The same crash hits library types with such a field. play-json's `case class JsObject(private[json] val underlying: scala.collection.Map[String, JsValue])`:
`AssertionError: method underlying$access$0` on Scala 2, where Scala 3 gives the normal error
`schema for scala.collection.Map[String,JsValue] is not supported: only immutable Maps`.

### Cause
`UProductTypes.fieldAnnotationMap` (macros, Scala 2) reads annotations aimed at the underlying field (`@(x @field)`) by
walking the case accessors and asking each for its field:

```scala
case s: MethodSymbol if s.isCaseAccessor => s.name.decodedName.toString.trim -> s.accessed.annotations
```

What Scala 2 declares for `case class Priv(@(d @field)("f") private val a: Int, @d("p") private[p] val b: String, protected val c: Int, e: Int)`
(runtime reflection, 2.13.18):

| symbol | kind | case accessor | getter | `accessed` |
|---|---|---|---|---|
| `a` | method (private) | **no** | yes | the field `a ` |
| `a ` | field (local name, carries `@(d @field)`) | yes | - | - |
| `a$access$0` | method, synthetic | **yes** | **no** | **assertion fails** |
| `b`, `c` | method (private[p] / protected) | no | yes | the fields |
| `b$access$1`, `c$access$2` | method, synthetic | yes | no | assertion fails |
| `e` | method (public) | yes | yes | the field `e ` |

For a public val the getter is the case accessor. For a non-public one the compiler makes the getter a plain method and
adds a public synthetic case accessor `<name>$access$<i>` (so the pattern matcher and `productElement` can reach the
value). That method is not an accessor, so `accessed` (which asserts the accessor flag) blows up. Even without the crash this
loop would key the entry `a$access$0`, not `a`. The fields themselves come from the primary constructor's parameters
(`resolveFields`), which are right; only the annotation lookup is broken. Nothing else in the Scala 2 macro touches case
accessors (`UImplicits` calls `accessed` only after `isGetter`; value classes read the constructor).

## Why it is a problem
A compiler crash with a stack trace, not an error message, for an ordinary model. Non-public vals are common for
encapsulated models (a value that is serialized but not part of the public API), and every JSON codec serializes them
(play/circe/upickle derive from the constructor). Scala 3 handles them, so this is a Scala 2/3 parity gap, and it blocks
E2 (a `Predef[JsObject]` in the play bridge only helps once deriving through `JsObject` stops crashing; with a Predef
in scope the macro wouldn't reach the case class, but any user without it gets the crash instead of an error).

## Options

### A. Read the annotations from the underlying fields  — WON
Collect the fields instead of the accessor methods: `case s: TermSymbol if !s.isMethod && s.isCaseAccessor`, then
`s.annotations` (forcing them as today with `s.typeSignature` / `_.tree.tpe`). The key is the field's decoded name
trimmed of the local suffix (`a ` → `a`), which the code already trims.
- Pros: one-line change in the pattern; no `accessed` call at all. It reads exactly the data `accessed` returned for
  public vals (`accessed` is `owner.info.decl(name.localName)`), so public fields are unchanged by construction.
  Non-public fields get their `@field` annotations, as on Scala 3. Works for classes from classfiles (the local fields
  and their flags are pickled; covered by a cross-module test).
- Cons: relies on the field carrying the `CASEACCESSOR` flag. It does on 2.12 and 2.13 for every first-list val,
  public or not (table above; and the cross-module test checks it for unpickled classes).

### B. Keep the accessor loop, skip the non-getters  — LOST
`case s: MethodSymbol if s.isCaseAccessor && s.isGetter`.
- Pros: smallest change, no crash.
- Cons: a non-public val then has no case-accessor getter, so its field-targeted annotations are **silently dropped**
  (`@(description @field) private val a` gets no description), where Scala 3 keeps them. Trades a crash for a quiet
  parity bug.

### C. Drive it from the constructor params, via the getter  — LOST
For each first-list param `p`, look up the getter `tpe.decl(p.name)` (every val param has one, whatever its
visibility) and use `getter.asMethod.accessed.annotations`.
- Pros: the parameter list stays the single source of truth for which fields exist; `accessed` is only called on
  getters, so it can't assert.
- Cons: more code for the same result as A; `decl(name)` returns an overloaded symbol when the class also declares a
  method of that name (`def a(x: Int)`), which needs extra handling. No case where it gives a different answer than A.

### D. Catch the AssertionError  — LOST
Wrap `s.accessed` in a `try` and skip on failure. Catching a compiler assertion is fragile (and the entry would still be
keyed `a$access$0`), and it has B's annotation loss.

## Decision
A: read field-targeted annotations from the case fields themselves, not through the accessor methods. It removes the
crash, gives non-public vals the same fields and annotations as public ones, and matches Scala 3's output exactly.
Prototyped (patch: `notes/scratch/c-b4/fix.patch` in the team dir): with it, `ProductsCharSpec` and
`CrossModuleScaladocSpec` pass on clean 2.12.21 and 2.13.18 builds against the goldens generated on 3.3.9, and
play's `JsObject` gets the Scala 3 error text instead of the crash.

## Implementation plan
1. `macros/src/main/scala-2/com/github/andyglow/jsonschema/UProductTypes.scala`, `fieldAnnotationMap.annotations0`:
   collect `TermSymbol`s with `!s.isMethod && s.isCaseAccessor` and read `s.annotations` (keep the forcing workaround),
   with a comment on why not the accessor methods. No other macro changes; Scala 3 derivation untouched.
2. Tests (already on the branch as the repro, failing to compile on 2.12/2.13 before step 1):
   - `api/.../characterization/ProductsCharSpec.scala`: models `PrivateVal`, `PrivatePkgVal`, `ProtectedVal` and
     `PrivateValAnnotated(@(description @field)(...) private val a, @description(...) private val b, @(readOnly @field) protected val c)`;
     two tests checking goldens.
   - New goldens `golden/char/products/{PrivateVal,PrivatePkgVal,ProtectedVal,PrivateValAnnotated}.json` and their
     `golden-legacy/` counterparts (new names, so update mode adds them; their content is identical to the base ones),
     generated on Scala 3 (the target) and checked byte-identical on 2.12/2.13 with the fix.
   - `modules/test-models`: `NonPublicValsInOtherModule(private val a: Int, protected val b: String, c: Int)` with scaladoc
     `@param`s; `CrossModuleScaladocSpec` checks its field names and descriptions (the classfile path on Scala 2, TASTy on 3).
3. JS: no platform code; `rootJS/test` covers the same specs (the crash is at compile time on both platforms).
4. Docs: none. README/paradox say nothing about field visibility; a crash fix needs no docs. (CHANGELOG by the lead.)
5. Not in scope: `private[this] val` (the compiler rejects it for case class params), second-parameter-list vals (not
   case fields, unchanged), a `Predef` for play's `JsObject` (E2).

## Compatibility
- Source/binary: no public API change (the macro's internals only).
- Output: types that compiled before (only public case fields) get exactly the same annotation map, so no existing
  output changes; the existing goldens must stay untouched. Types that crashed now compile.
- PVP: patch (bug fix).
- Legacy profile: unaffected (0.7.11 has the same `s.accessed` loop and crashed on these types too, so there is no 0.7 output to reproduce; the new
  `golden-legacy` files only pin today's Legacy rendering of the new models).

## Review log
