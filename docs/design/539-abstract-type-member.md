# Types declared in a trait, derived through an object that fixes them  (issue #539, item B33)
Status: draft

## Problem
A case class (or a sealed family) declared inside a trait can use an abstract type member of the trait. An object
that extends the trait fixes the type member, and users derive the schema through that object:

```scala
trait Module { type T; case class Item(x: T) }
object M1 extends Module { type T = Int }

trait Zoo { type T; sealed trait Animal; case class Dog(x: T) extends Animal; case object Cat extends Animal }
object Z1 extends Zoo { type T = Long }

Json.schema[M1.Item]
Json.schema[Z1.Animal]
```

Today (0.9.0, 2.12.21 / 2.13.18 / 3.3.9, JVM and JS alike: it is a compile-time failure):

| model | Scala 2.12 / 2.13 | Scala 3.3 |
|---|---|---|
| `M1.Item` | `schema for Module.this.T is not supported, M1.Item` | `{"type":"object",...,"properties":{"x":{"type":"integer"}},"required":["x"]}` |
| `Z1.Animal` | `schema for Zoo.this.T is not supported, Z1.Animal :: Zoo.this.Dog` | the same error |

Two causes, one on each side of the macro:

1. **Scala 2, fields.** `UProductTypes.resolveFields` reads a field's type as declared on the constructor
   parameter (`fieldSym.typeSignature`, `UProductTypes.scala:70`), i.e. `Module.this.T`, and only substitutes the
   class's own type arguments (`resolveGenericType`). The prefix the user wrote (`M1`) is never applied, so `T` stays
   abstract. Scala 3 reads the same field with `owner.memberType(field)` (`Products.scala:109`), which is the type
   *as seen from* `M1.Item`, so `M1.T`, which dealiases to `Int`.
2. **Both versions, family members.** A sealed family's members are listed by symbol (`knownDirectSubclasses` /
   `Symbol.children`) and turned into types with `cs.toType` (Scala 2, `UCommons.resolveSumTypeRecursively`) /
   `cs.typeRef` (Scala 3, `Sums.childType`). That type has the declaring trait's `this` as its prefix
   (`Zoo.this.Dog`), not the parent's (`Z1.Dog`), so the member's fields are seen from `Zoo.this` and `T` is
   abstract again, on Scala 3 too.

The probes (team dir `notes/scratch/c-b33/B33Probe*.scala`, logs `repro-*.log` / `master2-*.log` next to them) cover more shapes. On master:

| # | model | 2.13 | 3.3 |
|---|---|---|---|
| 1 | `M1.Item`, `M2.Item` (`T = Int` / `String`) | error | ok |
| 2 | `W(o: Option[T], l: List[T], m: Map[String, T])` | error | ok |
| 3 | `B(a: A, as: List[A])`, `A(x: T)` both in the trait | error (`... :: ModR.this.A`) | ok |
| 4 | `Z1.Animal` (above); `Z1.Dog` alone | error; error | error; ok |
| 5 | generic member class `G[A](a: A, t: T)`, `MG1.G[Int]` | error | ok |
| 6 | recursive `Node(v: L, kids: List[Node])` | error | ok |
| 8 | `type T = Leaf` (another member class) | error | ok |
| 9 | `D(x: T = dflt)` | error (`Please provide ToValue[ModD.this.T]`) | ok |
| 10 | children in the family's companion inside the trait (`object Pet { case class Fish(x: T) extends Pet }`) | error | error |
| 14 | a family member **with a default**, no type member (`trait ZooD { sealed trait A; case class D(n: Int = 1) extends A }`) | compiler crash: `Internal error: unable to find the outer accessor symbol` | `-Xcheck-macros`: malformed tree `ZooD.this.D.$lessinit$greater$default$1` |

Case 14 has no type member at all; it fails for cause 2 (the default getter is reached through `ZooD.this`, which
doesn't exist at the call site). Concrete aliases (`class H { type T = Int; case class I(x: T) }`, `h.I`) already
work on both versions: `dealias` sees through them without a prefix.

## Why it is a problem
- The "module" pattern (a trait with abstract types, objects that fix them: cake, tagless modules, per-tenant or
  per-version models) can't be used with `Json.schema` on Scala 2 at all, and its sealed families can't be used on
  either version. There is no workaround short of writing the schema by hand: an implicit `Predef[T]` or `Schema[T]`
  can't be provided from outside, because `Module.this.T` isn't a type the call site can name.
- Scala 2 / Scala 3 parity: the same source derives on 3 and fails on 2.
- Case 14 is a compiler crash (Scala 2) and a malformed macro expansion (Scala 3) for a model without anything exotic.

## Options

### A. Read the types as seen from the prefix the user wrote — WON
Do what the compiler does for member selection, at the two places where the macros turn a symbol into a type:

- **Scala 2 fields** (`UProductTypes.resolveFields`):
  `fieldSym.typeSignature.asSeenFrom(tpe, tpe.typeSymbol).dealias`. `asSeenFrom` maps both the class's own `this`
  (and its type parameters, so it subsumes `resolveGenericType`) and every outer `this` along the prefix chain:
  `Module.this` becomes `M1.type` because `Item`'s owner `Module` is a base class of `M1`'s type. This is exactly
  Scala 3's `memberType`.
- **Family members, Scala 2** (`UCommons.resolveSumTypeRecursively`): the child's type is rebased on the parent:
  `stableChild(childTpe).asSeenFrom(tpe, tpe.typeSymbol)`. `stableChild` first rewrites a `ThisType` of a non-static
  object owned by a class (`Pet.this.Fish`, `Fish` declared in `object Pet` inside the trait) into the stable path
  `Zoo2.this.Pet.Fish`, which `asSeenFrom` can then rebase to `Z2.Pet.Fish`. Objects owned by a term (local to a
  block or a method) are left alone: the prototype crashed on them without that guard
  (`ThisType(value <local SchemaMacroSpec>) for sym which is not a class`).
- **Family members, Scala 3** (`Sums.childType`): the quotes API has no `asSeenFrom`, so the child is *selected* on
  the parent's prefix: take the parent's `TypeRef` qualifier (`Z1.type`), walk the child's owners up to the parent's
  owner through objects (`select(o.companionModule)`), then `select(cs)`. If the child's owner isn't reached that way
  (a child declared outside the parent's owner), keep today's `cs.typeRef`. The child's fields are then read with the
  existing `memberType` and come out right.

Prototype (uncommitted, `notes/scratch/c-b33/prototype.diff`, ~30 lines in three files): every probe case derives,
and **2.13.18 and 3.3.9 print byte-identical JSON** for all of them, e.g. (Draft 07, `$schema`/`$id` dropped):

```
M1.Item     {"type":"object","additionalProperties":false,"properties":{"x":{"type":"integer"}},"required":["x"]}
M2.Item     ... "x":{"type":"string"} ...
MR1.B       {"a":{... "x":{"type":"number"} ...},"as":{"type":"array","items":{... "x":{"type":"number"} ...}}}
Z1.Animal   {"oneOf":[{"type":"object",...,"properties":{"x":{"type":"number"}},"required":["x"]},{"type":"string","enum":["Cat"]}]}
MG1.G[Int]  ... "a":{"type":"integer"},"t":{"type":"string"} ...
MD1.D       ... "x":{"default":5,"type":"integer"} ...
Z2.Pet      {"oneOf":[{... "x":{"type":"integer"},"kind":{"enum":["<pkg>.Zoo2.Pet.Fish"]}},{"type":"string","enum":["Rock"]},{... "t":{"type":"array","items":{"type":"integer"}},"kind":{"enum":["<pkg>.Zoo2.Gen"]}}]}
ZD1.A       {"oneOf":[{... "n":{"default":1,"type":"integer"}},{... "s":{"type":"string"} ...}]}    (case 14: no crash)
```

Edges, same on both versions with the prototype: `val r: Module { type T = Int } = M1; Json.schema[r.Item]` and
`val m1 = M1; Json.schema[m1.Item]` derive (`x` integer). A type projection `Module#Item` and a val typed by the bare
trait (`val m: Module = M1; Json.schema[m.Item]`) stay compile errors: `schema for <pkg>.Module#T is not supported,
<pkg>.Module#Item` / `schema for m.T is not supported, m.Item`. That is correct: `T` is unknown there.

`clean test` of the whole JVM build with the prototype: 2.12.21, 2.13.18 and 3.3.9 all pass, 0 failures, no golden
file changes (logs `notes/scratch/c-b33/proto-full-*.log`).

Pros: the compiler's own rule, so it covers every shape at once (nested, wrapped, generic, recursive, defaults);
Scala 2 becomes what Scala 3 already is for fields; small. Cons: it also changes how two models that compile today
are spelled internally, which shows in their output (see Compatibility: two side-effect output changes, both making
Scala 2 and nested models render like Scala 3 / top-level ones).

### B. Resolve the abstract type late, from the resolution context — LOST
Keep reading declared types; when `resolve` meets `C.this.T` with `T` abstract, look up the resolution stack for the
nearest enclosing type whose prefix derives from `C` and dealias `prefix.T`.
Pros: one place in each macro, nothing else changes spelling. Cons: the resolution stack isn't a prefix chain (it
goes through collection elements, implicits, family members); it doesn't fix the family-member prefix at all, so
case 10 and the case-14 crash stay; it re-implements `asSeenFrom` by hand, badly; and the recursion pass still
compares `ZooR.this.Expr` with `ZR1.Expr` as different types. Lost: more code, fewer cases fixed.

### C. Leave derivation alone; give a better compile error — LOST
Detect a field typed by an abstract type member of an outer `this` and abort with "derive it through ... / provide a
hand-written `Schema[M1.Item]`". Pros: no output change anywhere. Cons: Scala 3 derives fields today, so this would
either stay a parity gap or take a working feature away; the case-14 crash needs fixing anyway, and fixing it *is*
option A's family part. Lost.

### Scope variant (within A): Scala 2 fields only — LOST
The item as filed is the Scala 2 field case. Fixing only that leaves `Z1.Animal` failing on both versions and the
case-14 crash. The family part is the same root cause (the prefix is dropped when the macro builds a type from a
symbol) and ~15 more lines, so it's in (the item row says "Scala 3 derives it": true for fields, not for families).

## Decision
A, fields and family members: the Scala 2 macro reads field types with `asSeenFrom` the derived type (as Scala 3's
`memberType` does), and both macros build a sealed family's member types on the parent's prefix (`Z1.Dog`, not
`Zoo.this.Dog`). Member *names* (signatures, discriminator tags, `@definition` names) keep coming from the declaring
class, so no tag changes, now or after B29.

## Implementation plan
1. **Scala 2 fields** — `macros/src/main/scala-2/com/github/andyglow/jsonschema/UProductTypes.scala`, `toField`:
   - `val fieldTpe = fieldSym.typeSignature.asSeenFrom(tpe, tpe.typeSymbol).dealias` (drop the stale `// In(tpe)` comment;
     add one line saying why: the type member / outer `this` seen from the prefix, as Scala 3's `memberType`).
   - `effectiveType`: `asSeenFrom` already substitutes the class's type arguments; keep `resolveGenericType` as is (a
     no-op now, but it carries the "Cannot resolve generic type(s)" abort) or remove it if the tests stay green: the
     implementer's call, note it in the PR.
   - No change in `UValueTypes`: a value class can't be a member of a trait or class (Scala rejects it), so its
     inner type never has an outer `this`.
2. **Scala 2 family members** — `macros/src/main/scala-2/com/github/andyglow/jsonschema/UCommons.scala`:
   - add `private def stablePrefix(pre: Type): Type`: `ThisType(m)` with `m.isModuleClass && !m.isStatic && m.owner.isClass`
     → `internal.singleType(stablePrefix(internal.thisType(m.owner)), m.asClass.module)`; anything else unchanged;
   - add `private def stableChild(t: Type): Type`: `TypeRef(pre, sym, args)` → `internal.typeRef(stablePrefix(pre), sym, args)`;
   - in `resolveSumTypeRecursively`: `val subTpe = stableChild(<today's expression>).asSeenFrom(tpe, tpe.typeSymbol)`.
     It recurses with `subTpe` for nested sealed traits, so nested families are rebased level by level.
3. **Scala 3 family members** — `core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/Sums.scala`:
   - add `private def rebased(cs: Symbol, parent: TypeRepr): TypeRepr`: `po = classSymbol(parent).owner`; the parent's
     prefix from `parent.dealias` (`TypeRef(p, _)` or `AppliedType(TypeRef(p, _), _)`); `via(o, p)` = `Some(p)` if
     `o == po`, `via(o.owner, p).map(_.select(o.companionModule))` if `o` is a (non-package) module class, else `None`;
     result `via(cs.owner, pre).map(_.select(cs)).getOrElse(cs.typeRef)`;
   - `childType`: `cs.typeRef` → `rebased(cs, parent)` in both branches (the generic branch applies the type
     parameters to it, then unifies as today).
   - Singleton children (`c.termRef`, `m.termRef`) stay as they are: case objects in a trait already work (probe 4, 15),
     and their values are emitted by name.
4. **Member names from today's member type** (keeps tags stable; a no-op on master, where signatures drop the prefix,
   but needed once B29 / #529 names classes "reached through an inheriting object" after that object). Keep the member
   type as it is built today (before rebasing) next to the rebased one, and name the member from it:
   - Scala 2: `resolveSumTypeRecursively` returns both types per member (e.g. a small `(rebased, declared)` pair or case
     class; `declared` = today's `cs.toType` / `subclassType(cs, tpe)`, so `Pet.this.Fish` for a child of `object Pet`
     needs no special case). `USumTypes.scala:47` (`sig = signature(subTpe)`) and `UEnums.scala:176`
     (`signature(item.tpe)`) sign the declared type. `UEnums` needs it on Scala 2 because `asSeenFrom` rebases singletons
     too (`Zoo.this.Cat.type` → `Z1.Cat.type`).
   - Scala 3: `SealedChild` gets a `declaredTpe` (today's `cs.typeRef`, type arguments applied as today);
     `Sums.scala:122` (`signature(c.tpe, dealiased = true)`) signs it. Singleton children aren't rebased on Scala 3
     (`c.termRef` unchanged, step 3), so `Enums` needs nothing.
   - Only the member `sig` (default discriminator tag, `@definition` name, member id) uses the declared type; the
     member's schema is derived from the rebased type. `resolveSumTypeRecursively`'s other callers take the rebased type.
5. **Tests** — new shared spec `api/src/test/scala/com/github/andyglow/jsonschema/TypeMemberPrefixSpec.scala`, models
   in its companion object (static paths), tree comparisons (`shouldEqual`) plus Draft 07 JSON where a `$ref` shows:
   - fields: `M1.Item` (integer) and `M2.Item` (string); `W` with `Option`/`List`/`Map` of `T`; `MR1.B` (member class
     field and `List` of it); `MG1.G[Int]`; `type T = Leaf`; `MD1.D` default `5`;
   - more prefixes: an alias root (`type IA = M1.Item`); an anonymous prefix (`val anon = new Module { type T = Long }`,
     `Json.schema[anon.Item]` → number); a higher-kinded member (`type F[_]`, fixed to `List`: array of integer);
     a bounded member (`type T <: AnyVal`, fixed to `Double`);
   - families: `Z1.Animal` (`Dog` number, `Cat`), `Z1.Dog` alone; `Z2.Pet` (children in the companion inside the trait,
     discriminator `kind`); a child in *another* object of the trait (`trait Split { type T; sealed trait P;
     object Sub { case class Q(x: T) extends P } }`); a generic family `Z2.Box[String]` (`Full[A](a: A, t: T)`);
     a family as a field and an `Option` of it (`Wrap(f: Z1.Animal, fs: Option[Z1.Animal])`);
   - case 14 (family member with a default, no type member): `ZD1.A` derives with `"default": 1`;
   - names unchanged (pins, must hold with and without B29): family in a trait with `@discriminator("kind")` and a
     `@definition` member (`Zoo3`/`Z3`): tags `"<pkg>.Zoo3.Dog"`, definition `<pkg>.Zoo3.Cat`; `Json.sig[Z3.Dog]`
     as today; a case-object member's id (`Z1.Cat`) as today. If B29 lands first, these pins are the regression test
     for B29's "family tags don't change" claim;
   - the two side-effect changes (see Compatibility), pinned so they are deliberate, each through an object **and**
     through a val: `T1.Node` (`trait Tree { case class Node(v: Int, kids: List[Node]) }`) and `h.Node`
     (`class HolderR { case class Node(v: Int, kids: List[Node]) }`, `val h = new HolderR`) → `$ref` to the definition
     `<pkg>.Tree.Node` / `<pkg>.HolderR.Node`, same on both versions; a recursive family `ZR1.Expr` (in a trait) and
     `hf.Ex` (`class HolderF { sealed trait Ex; case class L(v: Int) extends Ex; case class N(a: Ex) extends Ex }`,
     `val hf = new HolderF`) → `$ref` + one definition, the shape of a top-level recursive family;
   - a case-object enum in a trait (`ZE1.Color` → `"enum": ["Red","Green"]`): unchanged;
   - a type projection with a class-typed field (`class Outer { class C...; case class In(c: C) }`, `Json.schema[Outer#In]`):
     output unchanged (asSeenFrom turns `Outer.this.C` into `Outer#C`, same signature) — if it changes, stop and report;
   - compile errors (`CompileErrors.errorOf`, phrase `is not supported`): `Module#Item`, and `val m: Module = M1;
     Json.schema[m.Item]`; derives: `val r: Module { type T = Int } = M1; Json.schema[r.Item]`, `val m1 = M1; Json.schema[m1.Item]`;
   - validation (JVM; canceled on JS through `petshop.JsonValidator`): `{"x":1}` valid and `{"x":"a"}` invalid for
     `M1.Item`; `{"x":"a"}` valid for `M2.Item`.
   - JS: the shared spec runs on JS too (compile-time behaviour, no platform code).
6. **No golden-file change**: run `GOLDEN_UPDATE=1 sbt clean api/test` on each version and check `git status` is clean
   under `api/src/test/resources/golden*` (the prototype changed none). Any golden diff means a model outside this
   item changed: stop and report.
7. **Docs** (same words in both): README "Supported types" (after the table, ~L289) and paradox
   `docs/src/main/paradox/types.md`:
   > A case class or sealed family declared in a trait is derived as seen from the path you name: with
   > `trait Module { type T; case class Item(x: T) }` and `object M1 extends Module { type T = Int }`,
   > `Json.schema[M1.Item]` has `x` as an integer. (Before 0.9.1, Scala 2 failed with "schema for Module.this.T is
   > not supported", and sealed families declared in such a trait failed on both versions.)

   No "Scala 3 notes" change (the behaviour becomes shared). `makeDocs`.
8. **Neighbours**:
   - **B29 (#529, design PR #535, design approved)** names a class reached through an inheriting object after that object. Step 4 keeps
     its promise that family members keep the declaring class's name. B29's plan pins Scala 2's `T1.Node` recursion as
     a single `List[<pkg>.Tree.Node]` definition (its step 3, Scala 2-only test); B33 changes that to Scala 3's shape.
     Whichever PR merges second moves that pin into the shared spec with Scala 3's expected names (with B29:
     `<pkg>.T1.Node` / `<pkg>.T2.Node`). Without B29, two prefixes with different `T` in one model (`ForestT(a: TT1.Node,
     b: TT2.Node)`, recursive) now share one definition on Scala 2 as they already do on Scala 3 (B29-R1): B29 fixes it.
     B29's doc (#535, "What does not change: sealed-family members") says tags stay because the members' prefix is
     `Zoo.this`; after B33 the reason is step 4 instead (the lead is asked to add a pointer to #535). If B29 lands
     first, step 5's name pins are the regression test for its tags.
   - **B8 (#297)**: its `mentionsAbstractType` treats an abstract type member with an implicit `Schema` inside the trait.
     Through an object the member is concrete after this change, so B8's rule doesn't apply there; inside the trait it
     does. Different files, no textual conflict.
9. **Gates** (when implemented): `clean test` on 2.12.21, 2.13.18, 3.3.9; `rootJS/test` on all three (macros and core
   change); `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`; `makeDocs`. `clean` after every macro edit.

Risks: `asSeenFrom` with a prefix that isn't stable (a type projection) yields `Outer#X` instead of `Outer.this.X`:
same symbol, same signature, covered by the projection test. `stablePrefix` must keep the `m.owner.isClass` guard.
On Scala 3, `-Xcheck-macros` checks every new tree; the prototype passed it on the whole api suite.

Out of scope: the text of the errors that remain (`Module#Item`, `val m: Module`: `schema for <pkg>.Module#T is not
supported, ...`). A hint such as "an abstract type member: derive it through a path that fixes it (an object that
defines `T`)" when the unsupported type is an abstract type member is cheap on both versions, but compile-error text is
B11's (#471) area; noted for the lead as a follow-up.

## Compatibility
- **Source / binary:** no public API change. Models that failed to compile now compile. PVP: a patch (0.9.1).
- **Output, for models that compile today** (both side effects, verified with the prototype by c-b33 and r-b33; no golden
  model has either). Both hit any class declared in a **trait or class** and reached through a stable path other than
  the declaring class's `this`: an **object** (`T1.Node`) or a **val** (`val h = new HolderR; h.Node`). Path-dependent
  classes in classes are a documented use (`PathDependentDefaultsSpec`), so this is not only the module pattern.
  1. Scala 2 only: a recursive case class declared in a trait or class, reached through an object or val (`T1.Node`,
     `trait Tree { case class Node(v: Int, kids: List[Node]) }`; `h.Node`, `class HolderR { ... }`). Before: the definition is the list,
     `{"type":"object",...,"kids":{"$ref":"#scala.collection.immutable.List_<pkg>.Tree.Node_"}}` with definition
     `scala.collection.immutable.List_<pkg>.Tree.Node_` (an array). After: `{"$ref":"#<pkg>.Tree.Node"}` with definition
     `<pkg>.Tree.Node`, `kids` an array of `$ref`s: what Scala 3 and a top-level recursive class give. Both are valid.
  2. Both versions: a recursive sealed family declared in a trait or class, reached through an object or val
     (`ZR1.Expr`; `hf.Ex`, `class HolderF { sealed trait Ex; ... }`). Before: the
     root is the inlined `oneOf` **and** the same `oneOf` is repeated as the definition `<pkg>.ZooR.Expr` (the root
     `ZR1.Expr` and the inner `ZooR.this.Expr` were different types to the recursion pass). After:
     `{"$ref":"#<pkg>.ZooR.Expr","definitions":{"<pkg>.ZooR.Expr":{"type":"object","oneOf":[...]}}}`, the shape of a
     top-level recursive family. Both are valid.
  No other name, tag or shape changes: definition names and discriminator tags come from the declaring class.
- **Legacy profile:** golden-legacy files don't change. The two changes above apply under Legacy too (they come from
  the derived tree, not the profile). Under the lead's Legacy policy they are not style: (1) is Scala 2 catching up with
  Scala 3, (2) removes a duplicated definition. Alternative if the owner wants 0.7's Scala 2 spelling kept for (1):
  not practical, since it means reading `T1.Node`'s fields without the prefix again, which is the bug.
- **Open question for the owner** (also in the PR body; the lead has been told): accept the two recursion reshapes
  (Scala 2 nested recursive classes; nested recursive families on both versions), also under Legacy? They are side
  effects, not the point of B33, but they can't be avoided without bringing the bug back, and both make the output
  match Scala 3 / top-level types.
- **Core rule:** unchanged (implicit `Schema` → definition, implicit `Predef` → inlined, else derive). An implicit for
  `M1.T` is now searched as `M1.T` / its dealias (`Int`), which users can provide; before, `Module.this.T` couldn't be.

## Review log
Round 1 (r-b33, on 51a824a):
- B33-R1 (must) side effects also hit classes declared in a class and reached through a val (`h.Node` Scala 2, `hf.Ex`
  both) → accepted: Compatibility 1 and 2 reworded ("trait or class", "object or val"), both cases added to step 5's pins.
- B33-R2 (must) side-effect output changes need the lead/owner → accepted: mailed the lead (one line each, incl. the
  class+val reach), added the open question under Compatibility; it goes into the PR body's open questions.
- B33-R3 (nit) name members from today's member type, not a rebuilt `declaredSpelling` → accepted: step 4 keeps the
  pre-rebase type next to the rebased one (Scala 2 pair from `resolveSumTypeRecursively`, Scala 3 `SealedChild.declaredTpe`);
  says why `UEnums` needs it on Scala 2 only (asSeenFrom rebases singletons there, Scala 3 keeps `c.termRef`).
- B33-R4 (nit) B29's tag argument now rests on step 4 → accepted: step 8 says so; lead asked to add a pointer in #535;
  step 5's name pins are B29's tag regression test if B29 lands first.
- B33-R5 (nit) more test shapes → accepted: `Split`/`Sub.Q`, family as a field + `Option`, alias root, anonymous prefix,
  higher-kinded member, bounded member, and the class+val cases (R1) are in step 5.
- B33-R6 (nit) hint in the remaining errors → pushed back on scope only: compile-error text is B11's (#471) area; stated
  as out of scope under Risks with the suggested wording, and passed to the lead as a follow-up.
