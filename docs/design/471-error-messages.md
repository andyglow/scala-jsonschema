# Compile-error texts: one format, no leaks, no crashes  (issues #471, #291, item B11)
Status: draft

## Problem
The derivation's compile errors are wrong in some places, leak internal text in others, and three inputs crash the macro
instead of reporting an error. Repro: `api/src/test/scala/com/github/andyglow/jsonschema/errors/ErrorMessagesSpec.scala`
(on the branch; today a probe that prints the texts, step 1 of the plan turns it into the spec). Models:

```scala
case object Lonely;              case class HasLonely(o: Lonely.type)
class Plain(val x: Int);         case class HasPlain(p: Plain)
case class HasNull(x: Null);     case class HasNothing(x: Nothing)
case class HasBoxed(x: java.lang.Integer);  case class HasSb(x: java.lang.StringBuilder)
sealed trait Shape;  case class Circle(r: Int) extends Shape;  class Odd extends Shape
sealed trait Fam;    object Fam { val anon: Fam = new Fam {} }
case class Box[A](a: A)
sealed trait GFam[A]; case class GI(i: Int) extends GFam[Int]; case class GS(s: String) extends GFam[String]
```

Actual texts on 0.9.0 (`M.` stands for the spec's object `com.github.andyglow.jsonschema.errors.ErrorMessagesSpec.`):

| # | input | Scala 2.12 / 2.13 | Scala 3.3.9 |
|---|---|---|---|
| D1 | `Null` root, `Option[Null]`, `List[Null]` | `schema for Null is not supported: a Map must take its key and value types as its only type parameters (like Map[K, V]), and Null is a Map[]. Provide ...` | the same, `scala.Null` |
| D2 | field `x: Null` / `x: Nothing` | **crash** `exception during macro expansion: java.util.NoSuchElementException: head of empty list` at `UProductTypes.toField` (`UProductTypes.scala:158`); also with an implicit `Predef[Null]` in scope | `Null`: D1's text. `Nothing`: `No json.Schema[_] for Nothing` (no path) |
| D3 | field `java.lang.Integer` / `java.lang.Long` / `java.lang.StringBuilder` | `schema for Integer is not supported, M.HasBoxed` / `schema for Long ...` (reads as `scala.Long`, which is supported) / `schema for StringBuilder ..., M.HasSb` (reads as `scala.StringBuilder`, an alias of `scala.collection.mutable.StringBuilder`) | `schema for java.lang.Long ...` / `java.lang.StringBuilder` |
| D4 | any unsupported root (`Plain`, `java.lang.StringBuilder`, ...) | `schema for M.Plain is not supported, ` (trailing `, `) | the same trailing `, ` |
| D5 | sealed family with a plain-class member | `Supported Type:false`⏎`Only Scala case classes/objects are supported for Sum Type leaves.`⏎`Actual: M.Odd.`⏎`Please consider using of them for Sum Types with base 'M.Shape' or provide a custom implicitly accessible json.Schema for the Sum Type.` | `schema for M.Shape is not supported: only Scala case classes/objects ...`⏎`Actual: com...ErrorMessagesSpec$.Odd.` (`$` leaks), same broken English |
| D6 | `Lonely.type`, `None.type`, `List[Lonely.type]` | `schema for M.Lonely.type is not supported, ` | `schema for M.Lonely is not supported, ` (no `.type`: reads like a class `Lonely`), `scala.None` |
| D7 | field `o: Lonely.type` (#291) | `schema for M.Lonely.type is not supported, M.HasLonely` (no hint) | the same without `.type` |
| D8 | `Map[String, _]`, `Option[_]`, `Either[String, _]`, `(Int, _)`, `List[Option[_]]` (root or field); `Map[_, Int]` | `schema for Any is not supported, ...` / the key message for `Any` | **crash** `Exception occurred while executing macro expansion. java.lang.AssertionError: assertion failed: TypeBounds(...)`: the bare wildcard reaches `Implicits.lookupImplicit` (`tpe.asType`, `Implicits.scala:20`) through `resolve`; a Map key reaches `Collections.keyPattern` (`Collections.scala:182`). After the crash Scala 3 stops reporting the file's other errors |
| D9 | `List[_]`, `Set[_]`, field `List[_]` | `schema for Any is not supported, List[Any]` (2.13: `List[_]` *is* `List[Any]`, List is covariant) / `..., scala.collection.immutable.Set[_]` | `schema for scala.collection.immutable.List[_ >: scala.Nothing <: scala.Any] is not supported: a collection must take its element type as its only type parameter (like List[T] or Set[T]), and ... is a collection of scala.Any` (wrong reason) |
| D9b | `Box[_]`, `Box[_ <: Plain]` (case class with a wildcard argument) | `schema for Any is not supported, M.Box[_]` / `schema for M.Plain ...` (the bound) | `schema for M.Box[_ >: scala.Nothing <: scala.Any].A is not supported, M.Box[...]` (a projection nobody wrote) |
| D10 | sealed family with an anonymous / block-local member (`new Fam {}`) | `Supported Type:false ... Actual: TypeName("$anon"). ...` (`showRaw`) | a dedicated reason: `... it has a subclass that is local to a block or anonymous (e.g. `new Fam {}`), which Scala 3 can't list. ...` |

Wildcards that work today, and must keep working: a generic sealed family `GFam[_]` (root or field) compiles on both
versions (its members fix the argument). So a blanket "an applied type with a wildcard argument" check is not an option;
the checks go exactly where today's failures are.

Also: only the generic "not supported" message carries the derivation path; the dedicated ones (Map, collection, Map key,
sum member) don't, and the generic one is the only one without a "provide a Predef or Schema" hint. The enum variant
of D5 (`UEnums.scala:107`, Scala 2) has the same "using of them" text. JVM and JS give the same texts (compile time).
#291's other half (an empty case class) is already supported since 0.8.0 (`{"type":"object","additionalProperties":false}`).

## Why it is a problem
- D1 sends the user to Maps for a type that has nothing to do with them. D3 names the wrong type: `java.lang.Long` fails,
  the message says `Long`, and `scala.Long` works. D5/D6 read like internal debugging output / a different type.
- D2 and D8 are compiler crashes with a stack trace (Scala 2, Scala 3), the worst kind of message; D2 also blocks the
  workaround (an implicit `Predef[Null]`), which works on Scala 3 and at the root on Scala 2.
- #291 (owner): a singleton type as a field should say what to do. Nobody can act on `..., M.HasLonely`.
- Parity: the project's rule is the same behaviour on Scala 2 and 3. Here even the same failure reads differently.

## Options

### A. Targeted fixes in both derivations, one message format, pinned by an exact-text spec — WON
One format for every "not supported" error, on both versions:

```
schema for <T> is not supported[ (in <A> :: <B>)][: <reason>]. <hint>
```
- `<T>` and the path entries printed by a new type printer, fully qualified and the same on 2 and 3 (below);
- the path ` (in A :: B)` (outermost first) only when the stack is non-empty: fixes D4, and the dedicated messages get it too;
- `<hint>` defaults to `Provide a json.schema.Predef or json.Schema for it` (the dedicated messages' existing wording; the
  generic one gets it now); a reason that carries its own advice (Null, singleton, sum member, wildcard) replaces it;
- reasons for the special cases, checked only where derivation fails today (nothing that compiles today changes):

| case | where | text after `not supported (in ...)` |
|---|---|---|
| `Null` (D1, D2) | before the families (S2 `genTree`, S3 `structural`): after the implicit search, so a user `Predef[Null]` still wins | `: Null has no schema of its own. For a JSON null, provide an implicit json.schema.Predef[Null], e.g. json.schema.Predef(json.Schema.`null`)` |
| `Nothing` (D2) | the existing check, keeps `No json.Schema[_] for Nothing` (phrase pinned in `ErrorPhrases`) | gets the path: `No json.Schema[_] for Nothing (in M.HasNothing)` |
| singleton `X.type` of an object with a sealed base `B` (D6, `None.type`) | the generic fallback | `: a singleton type (the type of one object) has no schema of its own. Use the type of its sealed family <B> instead, or provide a json.schema.Predef or json.Schema for it` |
| singleton `X.type` of a standalone object (D7, #291's own example, `Lonely`) | the generic fallback | `: a singleton type (the type of one object) has no schema of its own; objects are derived only as members of a sealed family. Provide a json.schema.Predef or json.Schema for it` |
| sum member that isn't a case class/object (D5) | S2 `USumTypes`, S3 `Sums.unsupportedLeaf` | `schema for M.Shape is not supported (in ...): only case classes and case objects are supported as members of a sealed family, and M.Odd is neither. Make it one, or provide a json.schema.Predef or json.Schema for M.Shape` |
| anonymous / block-local sum member (D10) | S2 `USumTypes` (new: `sym.isAnonymousClass` or a term owner), S3 `Sums.scala:254` (exists) | the same text on both: `schema for M.Fam is not supported (in ...): it has a subclass that is local to a block or anonymous (e.g. `new Fam {}`), which can't be a member of a derived family. Declare the subclasses at the top level or in an object, or provide an implicit json.Schema for M.Fam` (Scala 3 drops "which Scala 3 can't list") |
| wildcard (D8, D9, D9b; Scala 3 only, see below) | S3, three points, each one a place that fails today: (1) `resolve`, **before** `lookupImplicit`, when the type is a bare `TypeBounds` or a type-parameter projection of a wildcard-applied type (`Box[?]#A`); (2) `deriveArray`'s shape check when the element is a wildcard; (3) `dictKey`/`keyPattern` when the key is a wildcard | `schema for <owner> is not supported (in ...): a wildcard type argument (?) has no schema. Use a concrete type, or provide a json.schema.Predef or json.Schema for it`, `<owner>` = the type that has the wildcard argument (`scala.Option[?]`, `scala.util.Right[scala.Predef.String, ?]`, `M.Box[?]`), the path up to its parent |

Literal types (`"a"`, `1`) keep the generic text: E6 decides their support.

**Type printer** (`showType`), the same output on both versions for named types:
- Scala 2 (new, `USignatures` next to `signature`): `TypeRef` → decoded full name of the symbol (`java.lang.Integer`,
  `scala.collection.immutable.List`) + `[args]` joined with `, `; a module (`SingleType`, or a `TypeRef` to a module class)
  → full name + `.type`; anything else (existentials, refinements, constants) → the compiler's `toString`. Never aborts
  (unlike `signature`). Aliases are printed as written (`scala.Predef.String`), as Scala 3's `show` does.
- Scala 3 (new, `Signatures`): `AppliedType` → printer(tycon) + `[args]`; `TermRef` of a module → `show` + `.type`;
  `TypeBounds` (a wildcard) → `?` (`? <: X` / `? >: X` when bounded); otherwise `show`. The sum member uses the existing `fullName(sym)` (no `$`).
- Pinned to the same string on both versions, including the `, ` argument separator and aliases as written: a path entry
  `scala.collection.immutable.Map[scala.Predef.String, M.Plain]` (field `Map[String, Plain]`).

Every Scala 2 "schema for $tpe ..." message (Macroses, UDictionaries, UArrays, USumTypes, USignatures) prints
through `showType`; so does the path (`ctx.stack`).

Sample (both versions):
```
schema for M.Lonely.type is not supported (in M.HasLonely): a singleton type (the type of one object) has no schema of its own; objects are derived only as members of a sealed family. Provide a json.schema.Predef or json.Schema for it
schema for java.lang.StringBuilder is not supported (in M.HasSb). Provide a json.schema.Predef or json.Schema for it
schema for scala.Option[?] is not supported (in M.A1): a wildcard type argument (?) has no schema. Use a concrete type, or provide a json.schema.Predef or json.Schema for it   (Scala 3)
schema for M.Plain is not supported. Provide a json.schema.Predef or json.Schema for it
```

Crash fixes:
- D2 (Scala 2): `UProductTypes.toField` takes `fieldTpe <:< T.option` as "is an Option", which holds for `Null` and
  `Nothing`, then reads `typeArgs.head`. Exclude the bottom types there; the field then goes through `resolve` like on
  Scala 3 (and an implicit `Predef[Null]` works for a field, as it already does at the root and on Scala 3).
- D8 (Scala 3): check (1) runs before `lookupImplicit`, which is where the bare `TypeBounds` crashes (`asType`); a bare
  wildcard can't be resolved today anyway, so nothing that works changes. The implicit search for the *owner* (`Option[?]`,
  `Map[String, ?]`) still runs first, as today, so a user implicit for it keeps the precedence it has. Check (3) runs
  before `keyPattern`'s search. `GFam[_]` passes none of the three points (its members are derived, not its argument).

Wildcards, Scala 2: 2.13 normalizes `List[_]` to `List[Any]` (covariant), and for invariant `Set[_]`/`Map[String, _]`
both versions resolve the element as `Any`; the text `schema for scala.Any is not supported (in ...Set[_])` is then
accurate, and an implicit `Predef[Any]` makes it work. So Scala 2 keeps that behaviour (only the format changes), and
Scala 3 gets a dedicated error instead of a wrong one or a crash. (Making Scala 3 resolve a wildcard to its upper bound,
as Scala 2 does, would be new support: out of scope, noted below.)

Pros: small, local diffs; every message reads the same on both versions; the spec pins full texts, so a regression shows.
Cons: the wording lives twice (Scala 2 macros, Scala 3 core); the spec is what keeps them in step.

### B. A shared message catalogue in core — LOST
A `private[jsonschema] object DerivationErrors` in core with pure string builders (`notSupported(tpe: String, path: List[String], reason: Option[String])`),
called by the Scala 2 macros (which already depend on core) and the Scala 3 derivation, both passing pre-printed types.
Pros: one source of truth for the wording. Cons: compile-time-only strings ship in the runtime `core` artifact (JVM and
JS) for every user; the per-version parts (type printing, where each case is detected) stay in the macros anyway, so the
catalogue saves little. The exact-text spec of A gives the same "same wording" guarantee without the coupling.
Easy to switch to later: A's messages are built in a handful of helpers.

### C. Minimal: strip the leaks only — LOST
Drop `Supported Type:...`, the trailing `, `, qualify `java.lang` names on Scala 2, fix the two crashes; no new reasons
or hints, no shared format, the path stays `, A :: B`.
Pros: the smallest diff. Cons: D1 then says `schema for scala.Null is not supported` with no reason (or still the Map
text); #291 asks for a hint; the Scala 3 wildcard text stays wrong. It doesn't make the messages actionable, which is the item.

Owner alternatives inside A, easy to pick at review:
- path format: ` (in A :: B)` (A's pick) vs keeping `, A :: B` without the trailing separator at the root;
- `Null`: an error with the `Predef[Null]` hint (A's pick, messages only) vs supporting it as `{"type":"null"}` (the
  `Schema.`null`` node exists; new type support, a follow-up for E8 / #288);
- Scala 3 wildcards: a dedicated error (A's pick) vs resolving to the upper bound like Scala 2 (new support, follow-up).

## Decision
A. Every "not supported" error gets the same shape on Scala 2 and 3: a fully qualified type, the path only when there
is one, a reason where we know it (Null, singleton, sum member, wildcard) and a "provide a Predef or Schema" hint. The
three crashes become errors. Texts are pinned by one shared spec.

## Implementation plan
1. **Spec** `api/src/test/scala/com/github/andyglow/jsonschema/errors/ErrorMessagesSpec.scala` (shared, all versions):
   replace the step-1 property tests with exact-text assertions (`errorOf(assertCompiles(...)) shouldBe ...`) for every row
   D1–D10, plus: a nested path (`Outer :: HasLonely`, `List[Plain]` in a field), the aliased path entry
   `scala.collection.immutable.Map[scala.Predef.String, M.Plain]` (R7), `None.type` (sealed base `scala.Option`), both
   singleton texts (standalone `Lonely`, a member of a sealed family), an implicit `Predef[Null]` making `HasNull` and
   `Option[Null]` compile (the field renders `{"type":"null"}`); no crash for `case class X(x: Null = null)` and an
   `Option[Nothing]` field (R6; declared in the object, since Scala 2 refuses defaults of block-local case classes);
   wildcard fields `Option[_]`, `Either[String, _]`, `(Int, _)`, `List[Option[_]]`, `Box[_]`, `Map[_, Int]`; and
   `GFam[_]` (root and field) still compiling on both versions. D3/D4 use `java.lang.StringBuilder`, which no item
   supports (E4 adds the boxed numbers, so `Integer`/`Long` would start compiling; R2). Version switches only where the
   texts legitimately differ (wildcards: Scala 2 keeps `schema for scala.Any ...`; 2.12 vs 2.13 existential printing: `include`).
   Models are spec-local, so fully qualified names coincide on both versions.
2. **Scala 2 printer + format** (`macros/src/main/scala-2/...`): `showType` in `USignatures`; a helper
   `notSupported(tpe, ctx, reason = "", hint = DefaultHint): Nothing` (and `pathOf(ctx)`) in `UCommons`/`ULogging`;
   switch `Macroses.resolve`'s fallback, `UDictionaries` (3 messages), `UArrays`, `USignatures` (unnamed type) and
   `USumTypes` to it. `USumTypes`' `otherwise`: an anonymous or block-local non-case member (`sym.isAnonymousClass` or a
   term owner) → D10's text, otherwise D5's; drop `Supported Type:` and `showRaw`; the dead-looking "candidates" abort
   keeps its text. Block-local *case class* members keep deriving (T5 `LocalKid`). `UEnums.allResolved`'s non-silent
   branch is unreachable (only called with `silent = true`, `UEnums.scala:142`): remove it (R5).
   `Null` check first in `genTree`; singleton reasons in the fallback (sealed base: the nearest sealed class among
   `baseClasses`). `Nothing` check gets the path.
3. **Scala 2 crash** D2: `UProductTypes.toField` `isOption` excludes `Null`/`Nothing`.
4. **Scala 3** (`core/src/main/scala-3/.../derivation/`): `showType` in `Signatures`; `Ctx.show` → ` (in ...)` via
   `showType`, empty at the root; `Resolve.resolve`: the wildcard check (1) first, before `lookupImplicit`; the Nothing
   path; `Resolve.structural` (Null check before the families, singleton reasons, hint; refined's `an implicit for it
   failed` suffix stays at the end); `Collections` (wildcard checks (2) in `deriveArray`'s shape check and (3) in
   `dictKey` before `keyPattern`; path + printer in the three messages and `dictKey`'s); `Sums.unsupportedLeaf` (new
   text, `fullName(sym)`, path) and `Sums.scala:254` (D10's shared text); `Signatures:46` printer.
5. **Existing tests**: `SumsEnumsCharSpec` T5 (`AnonKid`, `Email`: anonymous members) → D10's text on **both** versions
   (today Scala 2 pins the leaf text, Scala 3 the "local to a block or anonymous" one); T5 `LocalKid` unchanged.
   `ErrorPhrases` keeps all phrases (`is not supported`, `No json.Schema[_] for Nothing`, ...); add `SumMember` /
   `NullHint` / `SingletonHint` / `Wildcard` phrases if other specs want them. `TypeFamiliesCharSpec` T2 (`Lonely.type`) stays `include(NotSupported)`.
6. `clean` after every macro/derivation edit. Gates per the protocol (all three versions JVM; `rootJS/test` on 2.13 and 3;
   `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`).
7. Docs: none needed (README/paradox quote only the phrase `is not supported`, which stays; CHANGELOG is the lead's).

No golden-file changes (messages only). No public API change.

## Compatibility
- Source/binary: no public signature changes; all new helpers are inside the macros / `private[jsonschema]` derivation.
- Behaviour: code that compiles today compiles to the same tree (the new checks run only on paths that fail today).
  Newly compiling: a field of type `Null` with an implicit `Predef[Null]` on Scala 2 (was a crash; already worked on Scala 3).
- Output: none (no rendering change; Legacy untouched).
- Error texts change on both versions. They are not API; tests that grep for `is not supported` keep working.
- PVP: patch.

## Review log
Round 1 (r-b11 on 33c5eb6):
- B11-R1 (must) Scala 3 wildcard crash also hits fields `Option[_]`, `Either[String, _]`; `Box[_]` gives a projection
  text; prefer one check every type passes → accepted, with one change. I probed where each crash happens:
  `lookupImplicit`'s `asType` on a bare `TypeBounds` (via `resolve`: Option, Either, Tuple, Map value, `List[Option[_]]`)
  and `Collections.keyPattern` (`Map[_, Int]`). One check at the top of `resolve` (before the search) covers the first
  group and `Box[_]` (the projection `Box[?]#A` also comes through `resolve`). A single check on *every applied type
  with a wildcard argument* is not possible: `GFam[_]` (a generic sealed family) compiles today on both versions and
  would break. So: (1) `resolve`, (2) `deriveArray`'s shape check, (3) `dictKey` before `keyPattern`, each where a
  failure happens today. The owner's implicit search still runs first. Spec rows added for all of them and for
  `GFam[_]` compiling. Problem table rows D8/D9b updated.
- B11-R2 (must) E4 makes `java.lang.Integer`/`Long` compile → accepted: D3/D4 use `java.lang.StringBuilder` (verified:
  Scala 2 prints `StringBuilder`, i.e. reads as `scala.StringBuilder`; Scala 3 `java.lang.StringBuilder`; both fail).
- B11-R3 (must) Scala 2 anonymous member prints `TypeName("$anon")`; T5 on Scala 3 hits the anonymous message → accepted:
  new row D10, one shared text on both versions (Scala 3's dedicated reason, minus "which Scala 3 can't list"), Scala 2
  detects an anonymous / block-local non-case member in `USumTypes`' `otherwise`; plan step 5 corrected (T5 pins D10 on
  both; `LocalKid`, a block-local case class, keeps deriving on Scala 2).
- B11-R4 (nit) "use the type of its sealed family" misleads for a standalone object → accepted: two texts, chosen by
  whether the object has a sealed base (named in the text, e.g. `scala.Option` for `None.type`). Both pinned.
- B11-R5 (nit) `UEnums.scala:107` is unreachable → accepted: remove the non-silent branch, no rewording.
- B11-R6 (nit) `x: Null = null`, `Option[Nothing]` field → accepted: spec rows, no crash on any version.
- B11-R7 (nit) aliased path entry → accepted: pins `scala.collection.immutable.Map[scala.Predef.String, M.Plain]`.
- B11-R8 (nit) keep the Null hint aligned with E8's `Schema.null` docs → accepted: noted for the PR body.
