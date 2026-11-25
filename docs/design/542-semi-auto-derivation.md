# Semi-auto derivation by default, deep derivation opt-in via `Json.auto`  (issue #542)
Status: approved (independent reviewer, 2026-11-24, 4 rounds; owner decisions 2026-11-24); being implemented (owner direction 2026-11-24: "design it similarly to circe/pureconfig")

## Problem
`Json.schema[T]` derives every nested type that has no implicit `Schema` or `Predef`, from its structure, at any depth (Scala 2 `Macroses.scala` `genTree`;
Scala 3 `Resolve.scala` `resolve`, then `structural`). So deep ("auto") derivation is the default and needs no import. `import json.Json.auto._` adds little: it
makes `implicitly[Predef[X]]` work outside a derivation.

```scala
import cats.data.NonEmptyList, json.Json, com.github.andyglow.jsonschema.CatsSupport._
case class P(a: Int)
case class Holder(xs: NonEmptyList[P])

Json.schema[Holder]   // 0.9.0, 2.13 and 3.3: xs = {"type":"object","properties":{"head":{..P..},"tail":{"type":"array",..}},"required":["head","tail"]}
```

`NonEmptyList[P]` has an instance (`nelSchemaFromPredef`) only when `P` has one. When `P` has none, the macro does what it does for any nested type: it derives
`NonEmptyList` as a case class. The macro can't tell "a type the user wants derived" from "a type that has an instance, but whose element has none".

The two versions also differ under `import Json.auto._`, measured on master `5b24a80b`:
- **Scala 2:** `NonEmptyList[P]` is an array. The search finds `nelSchemaFromPredef(auto.derived[P])`, because `UImplicits.isAutoDerived` rejects auto only at the
  root (`UImplicits.scala:146-157`).
- **Scala 3:** it stays `{head, tail}`. `AutoGuard` declines every `auto.derived` during a derivation's searches (`AutoGuard.scala:36-37`, `Implicits.scala:69,123`).

## Why it is a problem
- **Silent wrong schemas.** A type the user never meant to derive (from a library, or one that has an instance under conditions) is derived from its internals.
  Validators then reject real data. #482 and #171 are instances of this.
- **The common pattern is the other one.** circe and pureconfig users expect members to need instances, with deep derivation as an explicit import. We do the
  opposite, implicitly.
- **Scala 2 and 3 disagree** once auto is imported.

## How circe and pureconfig do it
Studied: circe core and generic for Scala 3, circe-derivation (Scala 2, pure macros), pureconfig core for Scala 3. The shapeless-based modules were left out.
- **Semi-auto derives one type; members need instances.**
  - circe-derivation (Scala 2) looks up each member with `c.inferImplicitValue(Encoder[F])`, and on failure:
    `c.abort(c.enclosingPosition, s"Could not find $tc instance for $tpe")`.
  - circe (Scala 3): `summonFrom { case e: Encoder[A] => e; case _: Mirror.Of[A] => error("Failed to find an instance of Encoder[" + typeName[A] + "]") }`.
  - The only exception is the cases of a sealed family, which are derived as part of it (`derivingForSum`).
- **Auto is an explicit import that returns `Exported[TC[A]]`.**
  ```scala
  case class Exported[+T](instance: T) extends AnyVal                     // circe core; pureconfig core has the same
  object auto { implicit inline def deriveEncoder[A](using inline A: Mirror.Of[A]): Exported[Encoder.AsObject[A]] = Exported(Encoder.AsObject.derived[A]) }
  private[circe] trait LowPriorityEncoders {                              // the lowest-priority parent of object Encoder
    implicit final def importedEncoder[A](implicit exported: Exported[Encoder[A]]): Encoder[A] = exported.instance
  }
  ```
  An `Exported[Encoder[P]]` is not an `Encoder[P]`, so the import never competes with real instances. It is reached only through the lowest-priority bridge.
  So the user's instances, in a companion or local, always win, and on Scala 3 the import can't shadow implicit-scope instances. Container instances
  (`encodeNonEmptyList[A](implicit e: Encoder[A])`) get their element through the bridge.

One difference matters for us: circe's instances are functions, so a recursive auto instance is fine at runtime (or handled by shapeless `Lazy`). Ours are data
built at compile time, so a nested auto must never run a derivation of its own (see "Auto inside a derivation").

## The model (decision)
| entry point | derives | a nested type without an instance, of a gated kind |
|---|---|---|
| `Json.schema[T]`, `Json.objectSchema[T]`, `derives json.Schema`, `DerivedSchema[T]` (semi-auto) | `T` itself | compile error (after the rollout phase below) |
| any of those with `import json.Json.auto._`, and `Json.auto.derived[T]` (auto) | `T` and every nested type, in one expansion, as today | derived in place, as today |

**Gated kinds:** the types a user writes or a library defines, which semi-auto must not derive behind the user's back:
- non-tuple case classes;
- sealed families with a case class case (other than `Either`).

Value classes, all-case-object families and Scala 3 `enum`s without a parameterized case stay native, as derived today (open question 3 has the alternative of
gating them too).

**Not gated, native as today:**
- the types handled by the macros themselves: `Option`, `java.util.Optional`, collections and arrays, maps (including map keys that are enums or value classes,
  derived natively for the key pattern: `UDictionaries.scala:59-71`, `Collections.scala:18-20`), tuples, `Either`, Java enums, `scala.Enumeration#Value`;
- anything with an implicit `Predef` or `Schema` (built-ins such as `String` and java.time, the Predef constructors `listS`/`setS`/`vectorS`/`arrayS`/`dictS`, module
  instances such as cats).

The gate is defined by kind, not by family. On Scala 2, tuples go through `CaseClass`, `Either` through `SumType`, and Java enums through `Enum`
(`Macroses.scala:146-151`, `USumTypes.scala:54,117`, `UEnums.scala:252`). On Scala 3 they go through `deriveProduct`, `deriveSum` and `deriveEnum`
(`Products.scala:142`, `Sums.scala:111`, `Enums.scala:86`).

**What "`T` itself" covers:**
- the root, of any kind;
- for a sealed family, its cases: case classes, case objects, nested sealed sub-families, and on Scala 2 value-class members (`USumTypes.scala:75-80`). They are
  derived as part of it, as in circe;
- everything else is resolved: fields, the underlying type of a value class, the elements of built-ins.

**Resolving a type** means the core rule, unchanged:
1. Recursion to a type being derived gives a `$ref`.
2. An implicit `Schema` gives a definition plus a `$ref`.
3. An implicit `Predef` is inlined.
4. A native type is handled by the macro, and its parts are resolved.
5. A gated kind:
   - **semi-auto:** the error;
   - **auto:** derived in place.

**The error** keeps the current prefix (`Errors.scala:28`, pinned by `ErrorMessagesSpec`):
```
schema for com.example.Address is not supported (in com.example.Person): no implicit json.Schema or json.schema.Predef for it.
Define one: `implicit val addressSchema: json.Schema[Address] = json.Json.schema[Address]` (a definition + $ref; for a generic type
`implicit def boxSchema[T](implicit p: json.schema.Predef[T]): json.Schema[Box[T]] = json.Json.schema[Box[T]]`),
or derive nested types automatically: `import json.Json.auto._` (Scala 3: `import json.Json.auto.given`)
```

For #482, `Holder(xs: NonEmptyList[P])` fails naming `NonEmptyList[P]` (in `Holder`). It has no instance, because the search for `Predef[NonEmptyList[P]]`
failed on `P`, and it is a gated kind (a case class). Scala 3 adds the reason it got from the search, so `P` is named too. Scala 2's silent search gives no
reason (S2-R6). With auto, `xs` is an array. Nothing in the macros knows about cats.

## Options

### A. circe's model: semi-auto by default, `Exported` auto — WON
1. **`Exported` and the bridge** (core):
   ```scala
   // json/schema/Exported.scala
   final case class Exported[+T](instance: T) extends AnyVal
   // json/schema/Predef.scala
   trait LowPriorityPredefs { ...; implicit def importedPredef[T](implicit exported: Exported[Predef[T]]): Predef[T] = exported.instance }
   ```
2. **`Json.auto.derived[T]: Exported[Predef[T]]`** (api, both versions). Called by the user's code, outside a derivation, it derives `T` deeply in one expansion,
   exactly as `Json.auto.derived` does today.
3. **Auto inside a derivation never derives by itself.** This was the review's main finding (S-R1). With the bridge, auto reaches every element of a Predef
   constructor. For `Node(children: List[Node])` the search for `Predef[List[Node]]` finds `listS(importedPredef(auto.derived[Node]))`. If that `auto.derived[Node]`
   ran its own derivation, it would loop (the 0.7 StackOverflow, `UImplicits.scala:139-140`). It would also cost an exponential amount of work (`AutoGuard.scala:10-11`).
   So:
   - **During a derivation's implicit search, `auto.derived[X]` expands to a cheap placeholder,** `Exported(AutoHole.hole[X])`, instead of deriving or declining.
     - Scala 3: `AutoGuard.isSearching` (the existing ThreadLocal).
     - Scala 2: a ThreadLocal "searching" flag set around `Implicit.infer`, the counterpart of `AutoGuard`. It is not `c.openMacros`, which also contains the
       derivation during its own silent `c.typecheck` and the `c.eval` of default values (S2-R9). Whether scalac expands a nested blackbox implicit macro during
       the outer `c.inferImplicitValue` at all is a spike (S-R3, `-Ymacro-debug-lite`). If it doesn't, the found tree carries the unexpanded `auto.derived[X]`,
       which is treated as a hole.
   - **The derivation then walks the accepted tree:**
     - A hole at the root (`importedPredef(hole[X])`) means auto is available for `X`: derive `X` in place, in the current expansion.
     - A nested hole, e.g. `listS(importedPredef(hole[Node]))` or `nelSchemaFromPredef(importedPredef(hole[P]))`, becomes a **child node in the IR** (S2-R1):
       - Scala 3 `FromImplicit` and Scala 2 `` `-from-tree-` `` carry `holes: List[(id, Node)]`, with `X` resolved by `resolve(X, ctx)` in the same context.
       - `children` / `mapChildren` (Scala 2: `transformSchema`) include the holes, so the recursion passes see them: `substituteRecursive` (`Recursion.scala:29-36`)
         and `recursiveTypes.substitute` (`Macroses.scala:87`). A recursive `X` becomes a `$ref` to a definition that exists.
       - Substitution happens at emission: Scala 3 with a `TreeMap` over the found term; Scala 2 by splicing the emitted tree in place of a local val.
         At construction, Scala 2 typechecks the shape silently with a stub, `{ val e1: Predef[X] = null; <tree with e1> }`.
     - **If the substitution can't be done, it is a compile error** naming the type and the instance found. It never falls back to deriving the type from its
       structure, which for `NonEmptyList[P]` would bring back `{head, tail}` (S2-R2).
   - **No hole is ever emitted.** After emission the macro checks the output tree for `AutoHole.hole` and aborts if one is left, a compile-time check (S2-R2).
   - **This is not the rejected marker probe.** A hole exists only when auto is imported and a real instance asked for an `X`: the user opted in, and the instance
     came from the normal search.
   - **Built-in collections keep their output** (S2-R8). With auto, `List[Node]` is found as `listS(hole)` where today it was the native `Arr` node. The two must
     render the same: goldens compare auto-imported against plain output for `List`, `Set`, `Vector`, `Array`, `Map[String, _]` and `Map[Int, _]`. If any
     differ, the native node wins for core's own Predef constructors.
   - **Holes change which candidates apply** (S2-R7). Under auto every `Predef[X]` now succeeds during a derivation, even for an `X` that can't be derived, so a
     candidate that failed before can now apply. It may then fail at replacement with "not supported", or make a search ambiguous. Tested in `AutoSpec`.
4. **The import counts as used** (S-R4, S2-R3). Today an import used only by the macro's searches is reported unused (`SelfImplicitLintSpec.scala:124-135`,
   `AutoDerivationSpec.scala:97`, build.sbt's refined comment). With a hole at the root, the common case, the found tree is discarded, so nothing in the output
   goes through the import. Instead, `auto` gets a marker, `implicit val jsonSchemaAutoEnabled: AutoEnabled`, a given so `import auto.given` brings it too, and
   every auto-mode expansion splices a use of it:
   - Scala 3: the marker's search result, wrapped in `Inlined(None, Nil, _)` (as `Implicits.scala:62-66` does). If the marker search fails, nothing is spliced.
   - Scala 2: an untyped `Ident(TermName("jsonSchemaAutoEnabled"))` in a discarded val, so the call-site typer resolves it through the import. It is spliced only
     after a silent `c.typecheck` shows it resolves to the marker's symbol. A user who imported only `auto.derived` by name, or who has a local name that clashes,
     gets no splice and no error (S3-R2).

   This is spike 0(d). A test compiles a zero-change migration with unused-import warnings and `-Werror` (Scala 3: `-Wunused:imports`; 2.13:
   `-Ywarn-unused:imports`).
5. **Scala 3 cleanup.** `containsAuto` becomes "no hole outside the replaced positions". `DerivedSchema` runs in typer and gets unexpanded trees, so there the
   unexpanded `auto.derived[X]` is treated as a hole too (S-R8). The `recordFailure` filter (`Implicits.scala:90`) also drops bridge failures (`importedPredef`,
   `Exported`), so errors don't gain an "an implicit for it failed" suffix.

Pros:
- The same model and precedence as circe and pureconfig.
- #482 is fixed with no cats knowledge in the macros.
- Scala 2 and 3 agree.
- The user's instances always win over auto.
- Auto keeps one expansion, so recursion and definitions behave as today.

Cons:
- It breaks almost every user, hence the rollout.
- Tree substitution of holes on both versions (a spike first).
- Mutual recursion through explicit instances needs a lazy reference (next section).

### B. Keep deep derivation; mark containers (`Container[T]`) — LOST
A core marker tells the macro "resolve through implicits, never derive". It fixes #482 only, keeps the implicit deep default the owner ruled out, and every
library has to declare its containers.

### C. Keep deep derivation; probe for a Predef constructor with internal marker types — LOST
`Predef[F[Hole]]` found means `F` is a container. It needs no API, but it keeps the deep default, and the macro derives the element with no import.

### D. Semi-auto, but auto stays `Predef[T]` (no `Exported`) — LOST
Smaller, but on Scala 3 the lexical `auto.derived[P]` shadows an instance in `P`'s companion (contextual scope is searched first), and Scala 2 keeps its
shadowing re-search (`UImplicits.scala:150-157`). `Exported` fixes both by construction.

## Mutual recursion through explicit instances (must solve)
```scala
case class A(b: Option[B]); case class B(a: Option[A])
implicit val aS: Schema[A] = Json.schema[A]   // the field b -> bS: definition + $ref
implicit val bS: Schema[B] = Json.schema[B]   // the field a -> aS
```
This already fails today. The emitted `def.adapt(x, sig)` (`UImplicits.scala:208-209`) is strict (`Schema.scala:700`), so `aS` captures `def(sig, null)`. On
Scala 3, `derives` givens are lazy, and a self-reference deadlocks (`Implicits.scala:12`). So `A derives Schema` plus `B derives Schema`, the usual semi-auto
shape on Scala 3, has the same problem. Under semi-auto this becomes the normal way to write such models, so it must work.

**Recommended:** a `ref` that carries a by-name body, emitted only when the instance isn't available yet (S2-R4). The macro emits
`` `def`.adaptOrLazy(x, sig) ``, with `x` by-name. Cycles are detected for any instance shape (S3-R1):
- **Registration:** every expansion registers its root's signature while its schema is being built: `Schema.building(sigA) { ... }`, a ThreadLocal set.
  - The entry is removed in a `finally`.
  - Registration and `adaptOrLazy` compute the signature the same way, from the dealiased type.
  - A type that mentions an abstract type has no compile-time name (`Implicits.scala:34-35`), so it is not registered. A cycle through such a generic instance
    stays unsupported, and is documented.
- **A cycle:** `adaptOrLazy(x, sig)` returns `lazy-ref` without evaluating `x` when `sig` is being built on this thread. That covers a mutual cycle through:
  - `implicit val` (where `x` would be `null`);
  - `implicit lazy val`;
  - Scala 3 `given` and `derives` givens (where evaluating would deadlock or loop);
  - `implicit def`.
- **No cycle:** `x` is evaluated at once and gives today's `def`, so the Scala value and equality are unchanged, `derives` givens included.
- **A remaining `null`** (a val in an object that is read before it is initialized, with no cycle through the macro) also gives `lazy-ref`.
- A `lazy-ref` evaluates `x` again when it is forced.

The node:
```scala
// core, json/Schema.scala (a new node; equality and hashCode by sig, toString without the body)
final class `lazy-ref`[T] private (val sig: String, body: => Schema[_]) extends Schema[T] { lazy val definition: Schema[_] = /* def.adapt rules applied here */ body }
```
- Every existing traversal sees a ref, so the tree stays finite and equality is by `sig`. Those traversals keep no seen-set: `extractDefs` and `lowered`
  (`AsDraftSupport.scala:33-44,342-353`), `DefinitionNames.assign`, `toDefinition.deepCopy` (`Schema.scala:672-689`), `equals`, `hashCode`, `toString`.
- Only definition collection and name assignment force the body, deduplicated by `sig`. `adapt`'s rules (value-class unwrap, keeping an inner def's `sig`, the
  #297 generic rename) apply at force time.
- A forced body is lowered with the active `JsonSchemaProfile`, the same as an inline definition, so its `option` nodes follow the user's profile. `lowered`
  (`AsDraftSupport.scala:44`) doesn't descend into a `lazy-ref`; definition collection applies the profile to the body when it forces it (S2-R5).
- The bridges and the parser match on nodes: the bridges render through `AsValue`, so no change; the parser never builds one.
- This is a new public `Schema` node, so it falls under the pending ruling on new Schema nodes.

**Alternative:** document that mutually recursive types use auto (one expansion) or one `Json.schema` of a common root. This doesn't fix Scala 3 `derives`.

## Rollout
- **Phase 1 (0.10.0), recommended:**
  - Semi-auto is the documented model; `Exported` auto and the precedence fixes ship.
  - A gated nested type without an instance is still derived in place, with a warning. The warning has a stable prefix that `-Wconf:msg=` can match (macros can't
    set a category): `[json-schema-semi-auto] schema for X is derived implicitly (in ...); this becomes an error in a future release. <the two fixes>`.
  - Opt in early with `Flag.SemiAuto`, which gives the error now. Flags are read with one implicit `Flag` search (`UFlags.scala:11`, `Implicits.scala:138`), so
    several flags combine in one value: `implicit val flags: Flag with Flag.EnumsAsOneOf with Flag.SemiAuto = null`. Two separate flag vals are ambiguous, so
    both would silently turn off; this is documented.
  - Users on `-Werror` see errors at once. The release notes say so, and give `-Wconf:msg=json-schema-semi-auto:s` as the way to keep phase 1 silent.
  - Macros run inside implicit searches can lose or duplicate warnings for discarded candidates: Scala 2's whitebox `DerivedSchema`, Scala 3's transparent
    `DerivedSchema`, and Scala 3 auto expanding during a search. The warning is emitted only from the outermost expansion, the one not inside a search.
  - So `DerivedSchema`, which always expands inside a search, gets no phase-1 warning. Its users go straight from silence to phase 2's error, which is the terse
    "no implicit DerivedSchema[X]". The migration notes say so (S2-R10).
- **Phase 2 (the next breaking release):** the warning becomes the error, and `Flag.SemiAuto` is deprecated, a no-op.
- **Alternative:** the error in 0.10.0 directly. Simpler, but every user's build breaks at once.

## Implementation plan
0. **Spikes first, result into this doc:**
   - (a) Scala 2: is a nested blackbox implicit macro expanded during the outer `c.inferImplicitValue`?
   - (b) Scala 2: hole substitution with a local val, typechecked.
   - (c) Scala 3: the placeholder under `AutoGuard.isSearching`, `TreeMap` substitution, and `DerivedSchema` in typer.
   - (d) The unused-import behaviour of both, under `-Werror`.
1. **core:**
   - Add `Exported`, the `importedPredef` bridge, `AutoHole.hole[X]` (internal API; it throws if ever evaluated), and `Flag.SemiAuto`.
   - Add the `lazy-ref` node and its handling in `AsDraftSupport` (definition collection, name assignment), the `JsonSchemaProfile` passes and `Schema.def.adapt`.
2. **api:** `Json.auto.derived[T]: Exported[Predef[T]]`, Scala 2 macro and Scala 3 inline. Inside a derivation's search it gives the hole. Update the scaladoc of
   `Json` and `Json.auto` for the model.
3. **Scala 2 macros:**
   - `Macroses`: a mode (semi-auto / auto) in the resolution context.
   - `UImplicits.inferPredef`: holes at the root and nested (step A.3). Drop the shadowing re-search.
   - `genTree`: the gate by kind (above), for non-root types that aren't cases of the family being derived: semi-auto → warning (phase 1) or error
     (`Flag.SemiAuto`, phase 2); auto → derive.
   - `UFlags`: read `Flag.SemiAuto` next to `EnumsAsOneOf` (`UFlags.scala:11`) (S2-R11).
   - Holes as IR children of `` `-from-tree-` ``, handled by `transformSchema` and the recursion substitution. The output is checked for leftover holes.
   - An implicit `Schema` is emitted through `` `def`.adaptOrLazy ``.
4. **Scala 3 derivation:**
   - `Resolve.resolve`: the same gate by kind.
   - `AutoGuard`: the placeholder instead of the decline.
   - `Implicits.searchAccepted`: holes, the `containsAuto` change and the `recordFailure` filter (A.5).
   - `Mode`: `Auto` derives deeply; `Schema`, `ObjectSchema`, `Derived` and `DerivedSchema` are semi-auto.
   - `hasFlag[Flag.SemiAuto]` (`Implicits.scala:138`) (S2-R11).
   - Holes as IR children of `FromImplicit` (`Nodes.scala:85`), seen by `substituteRecursive`. The output is checked for leftover holes.
   - `FromImplicit` emits through `` `def`.adaptOrLazy ``.
   - Every expansion wraps its result in `Schema.building(sig)`. Scala 2 does the same in step 3.
5. **modules:**
   - `derived` (`DerivedSchema`, which runs in typer on Scala 3 and is whitebox on Scala 2): a semi-auto root, with the unexpanded-auto handling.
   - cats, refined, enumeratum and joda provide `Predef`s, so no change in main code.
6. **Tests:**
   - The existing suites, about 1,150 `Json.schema`/`objectSchema` call sites in about 100 test files, rely on deep derivation, and must keep testing what they
     test now. So no auto import is added to them; that would silently turn them into auto-mode tests (S-R6). Instead, Test scopes silence the phase-1 warning
     with `-Wconf:msg=json-schema-semi-auto:s` in build.sbt. Goldens must not change: run the full matrix before and after.
   - `SemiAutoSpec` (api, shared, with `Flag.SemiAuto`):
     - the error for each gated kind, nested directly and in `Option`, `List`, `Map` values, tuples and `Either`;
     - native types without instances;
     - the root sealed family's cases being derived and their fields resolved;
     - recursion to the root;
     - map keys that are enums.
   - `AutoSpec`:
     - auto with recursion through `List`, `Map`, `Array`, `Option` and cats `NonEmptyList`, on 2 and 3 (S-R1), including a type that only contains a recursive
       one (`Forest(trees: List[Node])`, S2-R1);
     - an element that can't be derived, inside `List` and `NonEmptyList` (S2-R7);
     - auto-imported output equal to plain output for `List`, `Set`, `Vector`, `Array`, `Map[String, _]`, `Map[Int, _]` (S2-R8);
     - the existing `AutoDerivationSpec` cases, unchanged;
     - `implicitly[Predef[Foo]]` through the bridge, outside a derivation.
   - `AutoPrecedenceSpec`, where each of these beats auto:
     - built-ins;
     - a companion instance;
     - a generic companion instance;
     - an instance inherited by the companion from a parent trait;
     - a local instance;
     - an imported instance;
     - on Scala 3, a companion `given`, compiled also with the newest 3.x (3.6+ prioritization applies between givens; the bridge is an `implicit def`).
   - Mutual recursion: `A`/`B` with explicit vals, and Scala 3 `derives`, rendered with definitions; no `null`, no loop, equality and `toString` finite.
   - Unused imports: a zero-change migration compiled with `-Werror` and unused-import warnings on (A.4).
   - The phase-1 warning: there's no infrastructure to check a warning today, so add a small compile-and-capture helper (Scala 2 `ToolBox`; Scala 3
     `scala.compiletime.testing` only reports errors, so there it is a compile of a snippet through the compiler API in a test).
   - The #297 rename for generic instances (`UImplicits.scala:201-207`): a generic implicit `Schema` used under semi-auto renders its definition under the
     instantiated name.
7. **Docs:**
   - README and every paradox page with nested types: show semi-auto (instances) and auto (the import).
   - `migration.md`: "0.10: semi-auto by default", covering:
     - the phases;
     - `import Json.auto._` as the zero-change path;
     - `Flag.SemiAuto` and combining flags;
     - the `-Wconf` silencer;
     - the output difference when migrating with explicit `Schema` instances: definitions plus `$ref`s, generic ones renamed per #297.
   - CLAUDE.md "core design rule": add step 5.

## Compatibility
- **Source:**
  - Phase 1: compiles as before, with warnings for gated nested types derived implicitly.
  - Phase 2: those become errors.
  - `Json.auto.derived[T]` called explicitly returns `Exported[Predef[T]]` (use `.instance`). Code that only imports it is unaffected.
- **Output and Scala values:**
  - Rendered JSON: none for code that compiles unchanged, or that migrates with `import Json.auto._`.
  - Scala values: an implicit `Schema` found by the macro is still a `def` node, except in a recursion cycle between instances, or for a val read before it is
    initialized. Both are cases that give `null` or loop today. There it is the new `lazy-ref` node, which matches as neither `def` nor `ref`, and compares
    equal by `sig`.
  - Migrating with explicit implicit `Schema`s turns those types into definitions plus `$ref`s, per the core rule. Generic ones are named per #297.
  - Scala 3 with auto: Predef-constructor containers of derived elements (cats) become arrays, as on Scala 2.
  - Mutually recursive explicit instances render correctly (today: `null` or a deadlock).
- **Binary:** additions (`Exported`, `importedPredef`, `AutoHole`, `Flag.SemiAuto`, `lazy-ref`). `auto.derived` is a macro / inline, with no runtime signature.
  `lazy-ref` is a new `Schema` case: code matching exhaustively on `Schema` gets a new case.
- **PVP:** breaking, so 0.10.0. `lazy-ref` falls under the pending ruling on new Schema nodes.
- **Legacy profile:** not affected (rendering only).
- **Core rule:** gains step 5 (gated kinds: semi-auto → error, auto → derive). Steps 1 to 4 are unchanged.

## Owner decisions (2026-11-24)
The owner accepted all four recommendations:
- **Rollout:** a phase-1 warning in 0.10.0; the error in the next breaking release.
- **Mutual recursion:** the `lazy-ref` node.
- **Gated kinds:** only case classes and sealed families with a case-class case. Value classes, case-object enums and Scala 3 enums stay native.
- **Names:** `Flag.SemiAuto`, `Exported`, `lazy-ref`.

## Open questions for the owner (as asked; answered above)
1. **Rollout:** warning first (phase 1, recommended), or the error in 0.10.0.
2. **Mutual recursion:** the `lazy-ref` node (recommended), or documentation only.
3. **Which nested kinds are gated.** Value classes, all-case-object families and Scala 3 `enum`s are as common nested as case classes. Gating them is
   consistent with circe (`deriveUnwrapped` and enum codecs are explicit there). Not gating them (native, like Java enums and `Enumeration`) is less migration.
   Recommended: gate case classes and sealed families. Leave value classes, case-object enums and Scala 3 enums native: they have no "container whose element
   has no instance" problem, and deriving them is what any user expects.
4. **Names:** `Flag.SemiAuto`, `Exported`, `lazy-ref`.

## Follow-ups
- #482 / PR #488 (cats collections) becomes: the two cats fixes (`nesSchema` without `uniqueItems`, `Chain` with `minItems`), the cats docs, and tests of the
  three cases on top of this change.

## Implementation notes (2026-11-25)

**Spike results:**
- **(a)** Scala 2 expands a nested blackbox implicit macro during the outer `c.inferImplicitValue`. So a ThreadLocal "searching" flag around the derivation's
  lookups is what tells `auto.derived` to give the hole. On Scala 3, `Json.schema`, `objectSchema` and `derives` search after typer and get expanded holes;
  `DerivedSchema` searches in typer and gets the unexpanded `auto.derived[X]` call. Both are treated as holes.
- **(b)** Scala 2 substitution: each hole becomes a fresh `Ident`, the tree is untypechecked, and the shape is typechecked with a stub
  (`null.asInstanceOf[T]`, because `Exported` is a value class). Package qualifiers are rewritten to `_root_` paths after `untypecheck`.
- **(c)** Scala 3: a `TreeMap` with `changeOwner` works in both phases, under `-Xcheck-macros`.
- **(d)** The marker splice is needed on both versions. It is done in every expansion where the marker resolves, not only in auto mode. Otherwise
  `Json.schema[Int]` with `import Json.auto._` in scope (auto never needed) reports the import unused.

**Deviations from the design:**
- **Core's own collection constructors** (`listS`, `setS`, `vectorS`, `arrayS`, `dictS`) found with a hole use the native node, instead of filling the hole.
  The output stays exactly as before: `dictS` would give `Map[String, X]` no key pattern, where the native node gives `^[\s\S]*$`. Holes are filled for
  everything else: cats, a user's Predef constructors, a generic `implicit def ...: Schema[Box[T]]`.
- **Holes are resolved in the same context** as the type that holds them; the container is not pushed on the stack.
- **No `Schema.building` when the root itself came from an implicit.** Otherwise `Json.schema[Inner]` with an implicit `Schema[Inner]` would be a `lazy-ref`.
- **Warnings:** Scala 3 reports only one diagnostic per position, so it gives one warning per expansion, with one prefixed line per gated type. Scala 2 gives
  one warning per type.
- **No warning-capture test:** there is no ToolBox or compiler API on the test classpath. The warnings were checked by hand with the `-Wconf` silencer removed.

**Open problem (owner decision):** on Scala 2, `{ import Json.auto._; implicitly[Predef[Unsupported]] }` now fails with scalac's plain "could not find
implicit value for parameter p: json.schema.Predef[Unsupported]", without the derivation's reason.
- Why: `auto.derived` is now the argument of the bridge, so scalac expands it while the user's own search is still choosing. A failed expansion only makes
  `importedPredef` inapplicable, and Scala 2 doesn't report why a nested implicit failed.
- `Json.schema[Unsupported]` (the macro path) still gives the full reason, and Scala 3 is unaffected.
- The existing `AutoDerivationSpec` test "auto: a type it can't derive is a compile error that says why" fails on 2.12 and 2.13 because of this. It is left
  unchanged, pending the owner's decision.

## Review log
Round 1 (independent reviewer, 2026-11-24, against `5b24a80b`):
- S-R1 (must) auto through the bridge reaches every Predef constructor's element (`listS` etc. are Predef constructors, `Predef.scala:143-155`); recursion would
  loop → accepted: a nested auto never derives by itself; holes are replaced from the current expansion (A.3); the wrong "collections have no instance" line is
  fixed.
- S-R2 (must) the gate placed before the families would catch tuples, `Either`, and Java enums → accepted: the gate is by kind; `Optional`, map keys and Scala 2
  value-class family members are listed; the duplicate enum entries are fixed.
- S-R3 (must) Scala 2 may expand a nested implicit macro during the outer search → accepted: spike 0(a); the `c.openMacros` placeholder is the fallback.
- S-R4 (must) the auto import would be reported unused → accepted: A.4, with a `-Werror` test.
- S-R5 (must) mutual recursion is real today too; a forced by-name body loops in the traversals → accepted: `lazy-ref` modelled as a ref with a by-name body,
  forced only by definition collection (deduplicated by `sig`); the Scala 3 `derives` case is added.
- S-R6 (must) the rollout's test plan doesn't work → accepted: the warning gets a stable prefix; Test scopes silence it with `-Wconf`; suites don't get the
  import; a warning-capture helper; warnings only from the outermost expansion.
- S-R7 (nit) combining flags → accepted: documented.
- S-R8 (nit) Scala 3 `containsAuto`, `recordFailure`, and type comparison → accepted. A.5. The `show`-based comparison is gone (holes replace the root-only
  decline, so no cross-`Quotes` comparison is needed).
- S-R9 (nit) the error text → accepted: the current prefix is kept, the generic hint is added, and the #297 rename is in tests and Compatibility.
- S-R10 (nit) nested enums → accepted: open question 3, with a recommendation.
- S-R11 (nit) precedence cases → accepted: added to `AutoPrecedenceSpec`.

Round 2 (same reviewer):
- S2-R1 (must) holes spliced during construction escape the recursion passes, giving a dangling `$ref` → accepted: holes are IR children, substituted at emission
  (A.3).
- S2-R2 (must) the fallback to structural derivation would bring #482 back → accepted: a compile error instead, plus a compile-time check for leftover holes.
- S2-R3 (must) the import isn't used once a root hole's tree is discarded → accepted: an `AutoEnabled` marker in `auto`, spliced into every auto-mode expansion
  (A.4, spike 0(d)).
- S2-R4 (must) `lazy-ref` for every implicit `Schema` changes every user's Scala value → accepted: `` `def`.adaptOrLazy ``; `lazy-ref` only for `null` vals and
  Scala 3 `derives` givens; the ADT-level change is in Compatibility.
- S2-R5 (nit) the profile on forced bodies → accepted.
- S2-R6 (nit) the #482 error names `NonEmptyList[P]` → accepted.
- S2-R7 (nit) holes change which candidates apply → accepted: an `AutoSpec` case.
- S2-R8 (nit) auto output through `listS` etc. → accepted: equality goldens; the native node wins if they differ.
- S2-R9 (nit) the Scala 2 trigger → accepted: a ThreadLocal around `infer`.
- S2-R10 (nit) no phase-1 warning for `DerivedSchema` → accepted: documented.
- S2-R11 (nit) reading the flag → accepted: plan steps 3 and 4.

Round 3 (same reviewer):
- S3-R1 (must) `adaptOrLazy` missed `lazy val`, `given` and `def` instances in a cycle → accepted: cycle detection through `Schema.building(sig)` (a ThreadLocal
  set); the `derives` special case and its Compatibility caveat are gone.
- S3-R2 (must) the Scala 2 marker `Ident` could fail to resolve or resolve to the wrong symbol → accepted: a distinctive name, spliced only after a silent
  typecheck confirms the symbol; Scala 3 skips it when the search fails.
- Nit: "evaluated once, at once" → fixed.

Round 4: **approved** (no musts). Nits folded in: `Schema.building` cleans up in `finally`; the signature is computed the same way on both sides, and types
with an abstract type aren't registered.
