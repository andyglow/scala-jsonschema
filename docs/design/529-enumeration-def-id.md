# Definition names of path-dependent classes: `scala.Enumeration.Value` for every Enumeration  (issue #529, item B29)
Status: draft

## Problem
A definition (and `Json.sig`) is named after the **full name of the type's symbol**. For a class that is declared in one
trait/class and reached through another object, that is the declaring class's name, and the path the type was reached
through is dropped. `scala.Enumeration#Value` is the common case: every Enumeration's `Value` is named
`scala.Enumeration.Value`.

```scala
object Colors extends Enumeration {
  val Red, Green = Value
  implicit val schema: Schema[Value] = Json.schema[Value]
}
object Sizes extends Enumeration {
  val S, M, L = Value
  implicit val schema: Schema[Value] = Json.schema[Value]
}
case class UsesTwo(c: Colors.Value, s: Sizes.Value)
```

`Json.schema[UsesTwo]`, Draft 07, the same on 2.12.21, 2.13.18 and 3.3.9 (JVM; the names are compile-time, so JS is the same):

```json
"properties": {
  "c": { "$ref": "#scala.Enumeration.Value" },
  "s": { "$ref": "#scala.Enumeration.Value" }
},
"definitions": {
  "scala.Enumeration.Value": { "$id": "#scala.Enumeration.Value", "type": "string", "enum": ["S", "M", "L"] }
}
```

Draft 04: `"$ref": "#/definitions/scala.Enumeration.Value"` twice, one definition `["S","M","L"]`.
Which definition survives is the last one collected (`AsDraftSupport.inferDefinitions` ends in `.toMap`).

Same cause, run on all three versions (repro: `notes/scratch/c-b29/`):

| case | today |
|---|---|
| implicits outside the Enumerations, alias `type Fruit = Value` (`Fruits.Fruit`, `Tools.Tool`) | both `scala.Enumeration.Value`, one definition `["Saw","Drill"]` |
| `Json.sig[Colors.Value].signature` | `scala.Enumeration.Value` |
| `Json.sig[Fruits.Fruit].signature` (alias kept by `Json.sig`) | `<pkg>.Fruits.Fruit` (right) |
| `trait Outer { case class In(x: Int) }`, `object O1 extends Outer`, `object O2 extends Outer`, each with its own implicit `Schema[In]` (`.withDescription("o1")` / `"o2"`) | `O1.In` and `O2.In` both `<pkg>.Outer.In`; one definition, description `"o2"`, `"o1"` lost |
| an Enumeration declared in a class, `h.E.Value` | `scala.Enumeration.Value` |
| Enumerations without an implicit `Schema` | inlined, not affected |

**Scala 3 only, no implicit at all** (found by r-b29, B29-R1): a recursive class declared in a trait with a type member
becomes a definition through the recursion pass, named the same way, so two objects that fix the member differently share it:

```scala
trait TreeT { type L; case class Node(v: L, kids: List[Node]) }
object TT1 extends TreeT { type L = Int }
object TT2 extends TreeT { type L = String }
case class ForestT(a: TT1.Node, b: TT2.Node)
```

3.3.9: `"a":{"$ref":"#<pkg>.TreeT.Node"},"b":{"$ref":"#<pkg>.TreeT.Node"}`, one definition with `"v":{"type":"string"}`:
`a` rejects every `TT1.Node`. The same with implicits (`trait ModT { type T; case class Item(x: T) }`, `MT1.Item` / `MT2.Item`:
one definition, `x` string). On Scala 2 this model doesn't compile (`schema for Module.this.T is not supported`: a field
type isn't seen from the prefix). That is a separate bug, out of scope here (r-b29 mailed it to the lead).

The code: Scala 2 `USignatures.signature` (`case TypeRef(_, name, Nil) => decodedFullName(name)`), Scala 3
`Signatures.sig` / `tyconName` (`case t: TypeRef => fullName(t.typeSymbol)`). Both ignore the prefix.

## Why it is a problem
- **Invalid schema, silently.** With two Enumerations that have their own implicit `Schema` in one model, one field
  rejects every value of its Enumeration (`{"c":"Red","s":"S"}` fails). No error or warning at compile time or at render.
- An implicit `Schema[Value]` in the Enumeration object is the natural way to share one definition of an Enumeration
  (the README's core rule: implicit `Schema` → definition). It works for one Enumeration and breaks with the second.
- Even with one Enumeration, the name `scala.Enumeration.Value` says nothing about the type (`Json.sig` too).
- The general case (a class declared in a trait, used through several objects: cake patterns, "module" traits) loses one
  of the implicit schemas the same way. Without type members the shapes are equal and only texts/validations that differ
  per implicit are lost. With a type member the objects fix differently, the shapes differ, and on Scala 3 the schema is
  wrong even without any implicit (recursion, above). The cause and the fix are the same.

## Options

### A. Name an inherited class member after the path it is reached through  — WON
When a type is `pre.C` where `pre` is a stable prefix (an object, a stable val, a `this`) whose class is **not** the
class that declares `C` (it inherits `C`), the name is the prefix's class name + `.` + `C`:
`Colors.Value` → `<pkg>.Colors.Value`, `O1.In` → `<pkg>.O1.In`, `h.E.Value` → `<pkg>.Holder.E.Value`.
Everywhere else (the declaring class is the prefix's class, a package prefix, a package object / Scala 3
`<file>$package`, a type projection `Outer#In`, a local class) the name is unchanged: for those the path name and
the symbol's full name are the same string, so nothing else changes.

Mechanism (one rule in each signature function, used by `Json.sig`, definition names, recursive `ref`s and enum-item sigs alike):
- Scala 2 `USignatures.compute`: a new case before the `TypeRef` cases,
  `case TypeRef(pre, sym, args) if inheritedThroughPath(pre, sym) => compute(pre) + "." + decode(sym.name) + args`.
  For the two prefix shapes the guard admits, `compute(pre)` is `decodedFullName(pre.typeSymbol)` (the existing
  `SingleType` / `ThisType` cases), so it names the prefix's class.
  `inheritedThroughPath`: `pre` is a `SingleType` or `ThisType`; `pre.typeSymbol.isClass` (not a type parameter
  `E <: Enumeration` reached through `e.Value`, B29-R5) and not a package class; `sym.isClass`; `sym.owner.isClass`,
  not a package class; `sym.owner != pre.typeSymbol` and `pre.typeSymbol.isSubClass(sym.owner)` (it inherits the
  member; excludes self-types).
- Scala 3 `Signatures`: the same test on `t.qualifier` (`TermRef` or `ThisType`), used by the `TypeRef` case of `sig`
  and by `tyconName` (an applied inherited class, `O1.Box[Int]`). Prefix name `fullName(t.qualifier.typeSymbol)`:
  the prefix's *class*, so a val alias (`val c: Colors.type = Colors; c.Value`) is named like `Colors.Value` on both versions
  (Scala 3's `TermRef` would otherwise print the val's name; Scala 2's `SingleType.typeSymbol` already is the class).

Output for the repro: `c` → `#<pkg>.Colors.Value` (`["Red","Green"]`), `s` → `#<pkg>.Sizes.Value` (`["S","M","L"]`).
Scala 3 `ForestT`: `<pkg>.TT1.Node` (`v` integer) and `<pkg>.TT2.Node` (`v` string), the recursive refs inside each
following their own name (verified by r-b29 with the prototype).

**What does not change: sealed-family members** (B29-R3). A family declared in a trait and used through an object
(`trait Zoo { @discriminator("kind") sealed trait Animal; case class Dog(n: Int) extends Animal; @definition case class Cat(m: Int) extends Animal }`,
`object Z1 extends Zoo`, a field `Z1.Animal`) gets its members from `knownDirectSubclasses` / `children`, whose types
have the prefix `Zoo.this`: the owner is the prefix's class, so the rule doesn't apply. The members' sigs, the default
discriminator tags (`JsonSchemaProfile` uses `m.sig`, "as in 0.7") and `@definition` names stay `<pkg>.Zoo.Dog` /
`<pkg>.Zoo.Cat` (r-b29 verified on 2.13 and 3.3). That is what we want: a tag is the value the user's codec writes, and
changing it would break codecs. The docs call the default tag "its full name" (README L484, paradox `descriptions.md`),
which stays true. What no longer holds for such members is "tag == `Json.sig`": `Json.sig[Z1.Dog]` becomes `<pkg>.Z1.Dog`.
And a field `d: Z1.Cat` with its own implicit `Schema` becomes a definition `<pkg>.Z1.Cat` next to the family's
`<pkg>.Zoo.Cat`. Both definitions have the same shape, so the schema is valid; it's only duplicated. Making members
follow the outer prefix would change tags, so it's rejected.

Prototype (uncommitted, both versions): the repro gets distinct names on 2.12/2.13/3.3, and the whole JVM suite passes with
no other output change. See "Prototype result" below.

Pros: fixes the item and the general case with one rule, both versions alike; the names say what the type is; `Json.sig`
agrees with definitions; no API change; nothing changes for types declared where they are used (the vast majority).
Cons: a model with **one** Enumeration and an implicit `Schema` sees its definition renamed
(`scala.Enumeration.Value` → `<pkg>.Colors.Value`), an output change for a schema that was valid. Instances of a
class are still not told apart (`h1.E.Value` and `h2.E.Value` are both `<pkg>.Holder.E.Value`): see A2. Nor is a
static object declared in a trait (B29-R4): `trait T { object E extends Enumeration {..} }`, `object X extends T`,
`object Y extends T`: `X.E.Value` and `Y.E.Value` both name the prefix class `T.E`, so both are `<pkg>.T.E.Value`.
That's harmless (one declaration, the same values) and pinned by a test. On Scala 3, a recursive class declared in a
trait gets its recursion-pass definition renamed too (`T1.Node` / `T2.Node` instead of one `Tree.Node`), even without
a type member: two definitions of one shape (B29-R2, see Compatibility).

### A2. Like A, but name every stable *term* path (instances too)  — LOST
Use the path itself (`<pkg>.Spec.h1.E.Value`, `<pkg>.Spec.h2.Inner`) whenever the prefix is a term, even when the
declaring class is the prefix's class (`h.Inner` with `class Holder { case class Inner(..) }`).
Pros: two instances of one class get two definitions. Cons: renames definitions of class-nested types reached through
vals, which work today; names depend on how the value was reached (a local val's name, an alias val), so the same type
can get several definitions; Scala 2 and 3 name local vals' owners differently, a parity risk. Instance-level
collisions have never been reported and are between instances of the *same* declaration (same shape). Lost: more
churn for a case nobody hits; A can be widened to A2 later without renaming anything A introduces.

### B. Special-case `scala.Enumeration#Value` only  — LOST
In both signature functions, a `TypeRef` whose symbol is `scala.Enumeration.Value` takes the prefix's class name:
`<pkg>.Colors.Value`. Same output as A for Enumerations.
Pros: the smallest possible change; nothing else can move. Cons: hard-codes one library class into the naming rule;
leaves the trait-member case (`O1.In` / `O2.In`) colliding; A's extra blast radius is already shown to be empty by the
prototype. Lost: A is as small in code (one guard) and fixes the cause, not one instance of it. Easy to switch to: the
guard becomes `sym.fullName == "scala.Enumeration.Value"` (plus `Val`), the rest of the plan is the same.

### C. Detect colliding definitions at render time  — LOST (for B29; possible follow-up)
`inferDefinitions` sees two `def`s with one name and different bodies: rename the later ones (`_2`, like the draft
name sanitizer already does) in `definitions` and in every `$ref`, or throw.
Pros: catches every collision, including ones naming can't fix (user `toDefinition("x")` twice, B8-like generics).
Cons: names stay meaningless (`scala.Enumeration.Value_2`) and depend on traversal order; `Json.sig` stays wrong;
`$ref`s are rendered from each node's own name, and recursive `ref` nodes refer by name, so renaming needs a pre-pass
over the whole tree in every renderer and in the bridges; "throw" turns today's silent bug into a runtime failure of a
schema users may already render. Lost for this item: it treats the symptom. A collision *check* (warn/throw on two
different bodies under one key) is worth its own item; noted for the lead.

### D. Document the workaround only  — LOST
`Json.schema[Value].toDefinition("colors")` (or `@definition` where it applies) already gives distinct names.
Pros: no code change. Cons: users hit the silent invalid schema first and have to know the cause. Lost; the
workaround is still worth a line in the docs for 0.9.0 users.

## Decision
A: when a class is reached through an object/val/`this` whose class inherits it (rather than declares it), the
definition name and `Json.sig` use that prefix's class name: `<pkg>.Colors.Value`, `<pkg>.O1.In`. Same rule in the
Scala 2 macro and the Scala 3 derivation; types used where they are declared keep their names.

## Implementation plan
1. **Scala 2** `macros/src/main/scala-2/com/github/andyglow/jsonschema/USignatures.scala`:
   - add `private def inheritedThroughPath(pre: Type, sym: Symbol): Boolean` (rule in option A);
   - in `compute`, before `case TypeRef(_, name, Nil)`: `case TypeRef(pre, sym, args) if inheritedThroughPath(pre, sym) =>`
     `compute(pre) + "." + NameTransformer.decode(sym.name.toString)` plus `[args]` when non-empty. For the
     `SingleType` / `ThisType` prefixes the guard admits, `compute(pre)` is `decodedFullName(pre.typeSymbol)`, the
     prefix's class (the prototype's spelling)
     (args through `compute`, as the existing cases). The `ExistentialType(_, TypeRef(...))` cases get the same treatment
     (call the shared helper) so `O1.Box[_]` matches.
   - B8 (#297, PR #483) adds `mentionsAbstractType` to the same file: a textual merge only, no interaction (an
     inherited *class* is never abstract in its sense).
2. **Scala 3** `core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/Signatures.scala`:
   - add `private def inheritedThroughPath(t: TypeRef): Boolean` on `t.qualifier` (`TermRef` | `ThisType`; `sym.isClassDef`,
     `sym.owner.isClassDef`, the prefix's `typeSymbol.isClassDef` (not a type parameter, B29-R5), neither the owner nor
     the prefix (`typeSymbol` and `termSymbol`) a package, owner != prefix class, `t.qualifier.derivesFrom(sym.owner)`);
   - `sig`: `case t: TypeRef => if (inheritedThroughPath(t)) pathName(t) else fullName(t.typeSymbol)`;
   - `tyconName`: an inherited `TypeRef` takes `pathName(t)` and skips `Scala2Spelling` (no standard alias is a
     path-dependent class);
   - `pathName(t) = fullName(t.qualifier.typeSymbol) + "." + t.typeSymbol.name` (the class name: `$` of a module class is
     already stripped by `fullName`; the member is a class, no `$`);
   - extend the trait's doc list (the spelling rules) with the new rule.
   - `clean` before testing (macro call sites).
3. **Tests** (shared, `api/src/test/scala/com/github/andyglow/jsonschema/EnumerationDefinitionNameSpec.scala`; models in
   the spec's companion so they're static):
   - two Enumerations with implicits inside → two definitions `...Colors.Value` / `...Sizes.Value`, each with its own enum
     (tree compare, plus Draft 07 and Draft 04 JSON: `$ref`s and `definitions` keys; 2020-12 `$defs` key);
   - implicits outside + aliases (`Fruits.Fruit`, `Tools.Tool`) → `...Fruits.Value`, `...Tools.Value` (the alias is seen
     through for definitions, as today);
   - `Json.sig`: `Colors.Value` → `...Colors.Value`; `Fruits.Fruit` → `...Fruits.Fruit` (unchanged); `O1.In` → `...O1.In`;
     a val alias `c.Value` (`val c: Colors.type = Colors`) → `...Colors.Value`;
   - trait member `O1.In` / `O2.In` with different descriptions → two definitions, both descriptions kept;
     an applied inherited class (`trait Outer2 { case class Box[A](a: A) }`, `O3.Box[Int]` with an implicit) → `...O3.Box[scala.Int]`;
   - Enumeration declared in a class (`h.E.Value`) → `...Holder.E.Value`;
   - a self-type use (`trait T { self: Outer => implicit val s: Schema[In] = ... }`) keeps `...Outer.In` (pinned);
   - no-implicit Enumerations still inlined (unchanged);
   - the type inside the Enumeration and outside get one name. The implicit's own `Json.schema[Value]` sees the type as
     `Colors.this.Value` (prefix `Colors.this`); a field outside sees `Colors.Value` (prefix the module `Colors`). Both
     print `...Colors.Value`, so a model using both has one definition (r-b29 verified it with the prototype);
   - a static object in a trait (`trait T { object E extends Enumeration }`, `X.E.Value` / `Y.E.Value`) → both
     `...T.E.Value` (pinned: B29-R4);
   - a type-parameter prefix (`def f[E <: Enumeration](e: E)(implicit s: Schema[e.Value]) = Json.schema[Box[e.Value]]`)
     keeps today's name (B29-R5);
   - a sealed family declared in a trait, through an object (`Zoo` / `Z1` from option A): the discriminator tags
     (`"kind": {"enum": ["...Zoo.Dog"]}`) and the `@definition` name `...Zoo.Cat` are unchanged on both versions, while
     `Json.sig[Z1.Dog]` is `...Z1.Dog` (B29-R3);
   - validation (JVM; canceled on JS through `petshop.JsonValidator`): `{"c":"Red","s":"S"}` valid, `{"c":"S","s":"Red"}` invalid.
   - `TypeSignatureSpec`: add `Json.sig[Colors.Value]`.
   - **Scala 3 only** (`api/src/test/scala-3/com/github/andyglow/jsonschema/PathDependentDefinitionsSpec.scala`):
     `ForestT(a: TT1.Node, b: TT2.Node)` with type member `L` (B29-R1) → `...TT1.Node` with `v` integer and `...TT2.Node`
     with `v` string, inner refs to their own name, plus validation of `{"a":{"v":1,"kids":[]},"b":{"v":"x","kids":[]}}`;
     `MT1.Item` / `MT2.Item` with implicits and a type member → two definitions (`x` integer / string);
     the rename without a type member (`trait Tree { case class Node(v: Int, kids: List[Node]) }`, `T1.Node` / `T2.Node`
     → two definitions, B29-R2).
   - **Scala 2 only** (`api/src/test/scala-2/com/github/andyglow/jsonschema/PathDependentDefinitionsSpec.scala`; a new dir, sbt's
     built-in `scala-2` source dir, used by 2.12 and 2.13): the same `Tree` / `T1.Node` / `T2.Node` model keeps today's single
     definition `scala.collection.immutable.List[...Tree.Node]`, with `a`/`b` inlined and `"kids"` referring to it
     (Scala 2's recursion names the inner `List[Tree.this.Node]`; r-b29, B29-R8), pinned so a future parity change
     is deliberate. The type-member models don't compile on Scala 2 (neighbour bug, not here).
   Optionally one golden model (`enumeration_two_implicits`) through the golden harness, default profile only.
4. **Guard:** the full JVM suite on 2.12.21, 2.13.18, 3.3.9 (`clean test`) and `rootJS/test` on 2.13 and 3.3 pass with
   **no golden-file change** (`git status` clean under `api/src/test/resources/golden*` after a `GOLDEN_UPDATE=1` run).
   Any other renamed definition means the guard is too wide: fix the guard, don't regenerate.
5. **Docs**: README "Definition names" (the section at L225, after the `toDefinition` paragraph) and paradox
   `docs/src/main/paradox/definitions.md` "Definition names" (L66), the same words:
   > A class declared in a trait or class and reached through an object that inherits it is named after that object:
   > an `Enumeration`'s `Value` is `com.example.Colors.Value`, not `scala.Enumeration.Value` (before 0.9.1, every
   > Enumeration's `Value` had that one name, so two of them with implicit schemas shared a definition; on older
   > versions, give each its own name with `Json.schema[Value].toDefinition("colors")`). Members of a sealed family
   > keep the name of the class that declares them, so their discriminator values don't change.

   Plus one line for the Scala 3 notes (README "Scala 3 notes", L921, and paradox `scala3.md`): a recursive class declared in a
   trait gets one definition per object it's reached through (`T1.Node`, `T2.Node`); Scala 2 keeps its single definition, named after `List[<pkg>.Tree.Node]`.
   A "changes in 0.9.x" line goes wherever the lead puts output changes (no CHANGELOG edit by the item). `makeDocs`.
6. **JS:** nothing platform-specific (compile-time names); the shared spec runs on JS, the validation test is canceled there.

### Prototype result
Before writing the plan, the rule was prototyped uncommitted in both signature functions (diff, repro spec and logs: `notes/scratch/c-b29/`;
the prototype's Scala 3 prefix used `sig(t.qualifier)`, the plan uses the prefix's class, see step 2).
`clean test` of the whole JVM build on 2.12.21, 2.13.18 and 3.3.9: everything passes, 0 failures. The golden harness
compares the output of every golden model, and nothing else was renamed. Repro output with the prototype (all three versions):
`Colors.Value` → `...B29ReproSpec.Colors.Value`, `Sizes.Value` → `...Sizes.Value`, `Fruits.Fruit` → `...Fruits.Value`,
`O1.In` / `O2.In` → `...O1.In` / `...O2.In` (both descriptions kept), `h.E.Value` → `...Holder.E.Value`,
`Json.sig[Fruits.Fruit]` unchanged, no-implicit Enumerations still inlined. Not prototyped (tests in step 3 cover them):
val aliases, applied inherited classes, existentials, JS.

### Risks
- A prefix shape the guard misses would rename a definition that is right today: Scala 3 top-level `<file>$package`,
  package objects, Java statics (Scala 2 `SingleType` of the companion), `Sub.this.In` inside a subclass body (which does get
  the new name `<pkg>.Sub.In`: the same type from outside, `sub.In`, is `<pkg>.Sub.In` too, so they agree). Step 4 is the net.
- Self-types (`trait T { self: Outer => ... In ... }`): the prefix is `T.this`, owner `Outer`, and `T` doesn't
  inherit `Outer`; the `isSubClass` check keeps today's `<pkg>.Outer.In` there. A use through `O1` from outside says
  `<pkg>.O1.In`: two definitions of one shape, a valid schema. A test pins it.
- The same type reached two ways gets two definitions (`Colors.this.Value` inside the object and `Colors.Value` outside
  are one name, both prefix class `Colors`; but `Sub.this.In` vs `O1.In` when `O1 extends Sub` are two). Valid, only duplicated.

## Compatibility
- Source and binary: unchanged (private macro/derivation helpers only).
- Output: definition keys, `$id`/`$anchor`/`$ref` and `Json.sig` change **only** for a class reached through an object/`this`
  that inherits it. In practice:
  - definitions from **implicits**: an Enumeration's `Value` (`scala.Enumeration.Value` → `<pkg>.<Enum>.Value`), a
    class declared in a trait that objects mix in (`<pkg>.Outer.In` → `<pkg>.O1.In`);
  - definitions from **`@definition`** on such a class when it's used directly as a field type (not as a family member, see below);
  - **Scala 3 only**, definitions from the **recursion pass**: a recursive class declared in a trait
    (`<pkg>.Tree.Node` → `<pkg>.T1.Node` and `<pkg>.T2.Node`). Without a type member that's two definitions of one
    shape where there was one (valid before and after). With a type member it fixes a wrong schema (B29-R1). Scala 2
    keeps its single recursion definition `scala.collection.immutable.List[<pkg>.Tree.Node]` (`a`/`b` inlined), so the
    two versions differ there (they already did before): Scala 2 can't derive the type-member case at all, and
    renaming its recursion is not needed to fix a bug.
  - `Json.sig` of all of the above.

  Not changed: sealed-family members' sigs, discriminator tags and `@definition` names (members are seen from the
  declaring class), and types used where they are declared. A model with one such Enumeration had a valid schema and
  sees its definition renamed; anyone pointing at `#/definitions/scala.Enumeration.Value` from outside breaks. No
  existing golden, test or doc pins any of the renamed names (prototype run plus grep).
- PVP: no API change, a bug fix of generated output: patch (0.9.x) like the other gap fixes. If the owner counts the
  rename of a valid definition as breaking, it goes into the next minor instead (open question).
- `JsonSchemaProfile.Legacy`: profiles don't name definitions, so Legacy gets the new names too (0.7 had the same bug).
  Per the lead's Legacy policy this is a correctness fix. "Legacy keeps `scala.Enumeration.Value`" would need the
  compile-time tree to carry both names (a renderer can't recover the old one); not worth it, alternative noted.
  `golden-legacy/` has no model with an Enumeration implicit: no file changes.

## Review log
Round 1 (r-b29, 6c897d9):
- B29-R1 (must) Scala 3 type-member recursion is a wrong schema today with no implicit → accepted: added to Problem
  (Scala 3 only; Scala 2 doesn't compile it, out of scope, neighbour mailed by r-b29). Fixed the "less harmful"
  sentence in Why. Added the Scala 3 spec to step 3 (`ForestT`, `MT1/MT2.Item`).
- B29-R2 (must) Scala 3 recursion renames `T1.Node`/`T2.Node` → accepted: listed in Compatibility (implicits,
  `@definition`, Scala 3 recursion), a Cons line in A, the Scala 3 test and a Scala 2 test pinning `Tree.Node`, and a docs line.
- B29-R3 (must) family members, tags, `@definition` → accepted: "What does not change" paragraph in A (members are seen
  from `Zoo.this`; tags are codec data, so keeping them is right). The `Json.sig` disagreement and the duplicate
  `Z1.Cat` definition are stated there. The docs say "full name", not "`Json.sig`", for the default tag, so no wording
  change is needed beyond the new docs sentence. Pin test added.
- B29-R4 (nit) static `object E` in a trait still collides → accepted: Cons line + pin test.
- B29-R5 (nit) prefix symbol must be a class → accepted: `pre.typeSymbol.isClass` / `isClassDef` in both guards + test.
- B29-R6 (nit) docs: workaround line and places → accepted: step 5 names README L225 / paradox `definitions.md` L66
  (same words, with the `toDefinition` workaround for older versions) and the Scala 3 notes line.
- B29-R7 (nit) plan details → accepted: step 1 and option A use `compute(pre)` and say it equals
  `decodedFullName(pre.typeSymbol)`; the self-reference test is spelled out (`Colors.this.Value` and `Colors.Value` give one name).

Round 2 (r-b29, 5fa6b75):
- B29-R8 (nit) Scala 2's recursion definition is `scala.collection.immutable.List[<pkg>.Tree.Node]`, not
  `<pkg>.Tree.Node` → accepted: fixed the Scala 2 pin in step 3, the Compatibility bullet and the Scala 3 notes docs line.
