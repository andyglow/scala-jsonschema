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
case class HasBoxed(x: java.lang.Integer)
sealed trait Shape;  case class Circle(r: Int) extends Shape;  class Odd extends Shape
```

Actual texts on 0.9.0 (`M.` stands for the spec's object `com.github.andyglow.jsonschema.errors.ErrorMessagesSpec.`):

| # | input | Scala 2.12 / 2.13 | Scala 3.3.9 |
|---|---|---|---|
| D1 | `Null` root, `Option[Null]`, `List[Null]` | `schema for Null is not supported: a Map must take its key and value types as its only type parameters (like Map[K, V]), and Null is a Map[]. Provide ...` | the same, `scala.Null` |
| D2 | field `x: Null` / `x: Nothing` | **crash** `exception during macro expansion: java.util.NoSuchElementException: head of empty list` at `UProductTypes.toField` (`UProductTypes.scala:158`); also with an implicit `Predef[Null]` in scope | `Null`: D1's text. `Nothing`: `No json.Schema[_] for Nothing` (no path) |
| D3 | field `java.lang.Integer` / `java.lang.Long` | `schema for Integer is not supported, M.HasBoxed` / `schema for Long ...` (reads as `scala.Long`, which is supported) | `schema for java.lang.Long ...` |
| D4 | any unsupported root (`Plain`, `java.lang.Integer`, ...) | `schema for M.Plain is not supported, ` (trailing `, `) | the same trailing `, ` |
| D5 | sealed family with a plain-class member | `Supported Type:false`⏎`Only Scala case classes/objects are supported for Sum Type leaves.`⏎`Actual: M.Odd.`⏎`Please consider using of them for Sum Types with base 'M.Shape' or provide a custom implicitly accessible json.Schema for the Sum Type.` | `schema for M.Shape is not supported: only Scala case classes/objects ...`⏎`Actual: com...ErrorMessagesSpec$.Odd.` (`$` leaks), same broken English |
| D6 | `Lonely.type`, `None.type`, `List[Lonely.type]` | `schema for M.Lonely.type is not supported, ` | `schema for M.Lonely is not supported, ` (no `.type`: reads like a class `Lonely`), `scala.None` |
| D7 | field `o: Lonely.type` (#291) | `schema for M.Lonely.type is not supported, M.HasLonely` (no hint) | the same without `.type` |
| D8 | `Map[String, _]`, `Map[_, Int]` | `schema for Any is not supported, scala.collection.immutable.Map[String,Any]` / the key message for `Any` | **crash** `Exception occurred while executing macro expansion. java.lang.AssertionError: assertion failed: TypeBounds(...)` |
| D9 | `List[_]`, `Set[_]`, field `List[_]` | `schema for Any is not supported, List[Any]` (2.13: `List[_]` *is* `List[Any]`, List is covariant) / `..., scala.collection.immutable.Set[_]` | `schema for scala.collection.immutable.List[_ >: scala.Nothing <: scala.Any] is not supported: a collection must take its element type as its only type parameter (like List[T] or Set[T]), and ... is a collection of scala.Any` (wrong reason) |

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
| singleton `X.type` of an object (D6, D7, #291; also `None.type`) | the generic fallback | `: a singleton type (the type of one object) has no schema of its own. Use the type of its sealed family instead, or provide a json.schema.Predef or json.Schema for it` |
| sum member that isn't a case class/object (D5) | S2 `USumTypes` (and `UEnums:107`), S3 `Sums.unsupportedLeaf` | `schema for M.Shape is not supported (in ...): only case classes and case objects are supported as members of a sealed family, and M.Odd is neither. Make it one, or provide a json.schema.Predef or json.Schema for M.Shape` |
| wildcard type argument (D8, D9; Scala 3 only, see below) | S3 `deriveDictionary` / `deriveArray`, before anything else looks at the arguments | `: a wildcard type argument (?) has no schema. Use a concrete type, or provide a json.schema.Predef or json.Schema for it` |

Literal types (`"a"`, `1`) keep the generic text: E6 decides their support.

**Type printer** (`showType`), the same output on both versions for named types:
- Scala 2 (new, `USignatures` next to `signature`): `TypeRef` → decoded full name of the symbol (`java.lang.Integer`,
  `scala.collection.immutable.List`) + `[args]` joined with `, `; a module (`SingleType`, or a `TypeRef` to a module class)
  → full name + `.type`; anything else (existentials, refinements, constants) → the compiler's `toString`. Never aborts
  (unlike `signature`). Aliases are printed as written (`scala.Predef.String`), as Scala 3's `show` does.
- Scala 3 (new, `Signatures`): `AppliedType` → printer(tycon) + `[args]`; `TermRef` of a module → `show` + `.type`;
  `TypeBounds` (a wildcard) → `?`; otherwise `show`. The sum member uses the existing `fullName(sym)` (no `$`).

Every Scala 2 "schema for $tpe ..." message (Macroses, UDictionaries, UArrays, USumTypes, UEnums, USignatures) prints
through `showType`; so does the path (`ctx.stack`).

Sample (both versions):
```
schema for M.Lonely.type is not supported (in M.HasLonely): a singleton type (the type of one object) has no schema of its own. Use the type of its sealed family instead, or provide a json.schema.Predef or json.Schema for it
schema for java.lang.Integer is not supported (in M.HasBoxed). Provide a json.schema.Predef or json.Schema for it
schema for M.Plain is not supported. Provide a json.schema.Predef or json.Schema for it
```

Crash fixes:
- D2 (Scala 2): `UProductTypes.toField` takes `fieldTpe <:< T.option` as "is an Option", which holds for `Null` and
  `Nothing`, then reads `typeArgs.head`. Exclude the bottom types there; the field then goes through `resolve` like on
  Scala 3 (and an implicit `Predef[Null]` works for a field, as it already does at the root and on Scala 3).
- D8 (Scala 3): the wildcard check above runs before `sameArgs`/`baseType` see the `TypeBounds` argument.

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
   replace the probe with exact-text assertions (`errorOf(assertCompiles(...)) shouldBe ...`) for every row D1–D9,
   plus: a nested path (`Outer :: HasLonely`, `List[Plain]` in a field), `None.type`, an implicit `Predef[Null]` making
   `HasNull` and `Option[Null]` compile (and render `{"type":"null"}` for the field). Version switches only where the
   printers legitimately differ (Scala 2 existentials `Set[_]` vs Scala 3 `?`; 2.12 vs 2.13 existential printing: `include`).
   Models are spec-local, so fully qualified names coincide on both versions.
2. **Scala 2 printer + format** (`macros/src/main/scala-2/...`): `showType` in `USignatures`; a helper
   `notSupported(tpe, ctx, reason = "", hint = DefaultHint): Nothing` (and `pathOf(ctx)`) in `UCommons`/`ULogging`;
   switch `Macroses.resolve`'s fallback, `UDictionaries` (3 messages), `UArrays`, `USignatures` (unnamed type),
   `USumTypes` (leaf; drop `Supported Type:`; also the dead-looking "candidates" abort keeps its text), `UEnums:107` to it.
   `Null` check first in `genTree`; singleton reason in the fallback. `Nothing` check gets the path.
3. **Scala 2 crash** D2: `UProductTypes.toField` `isOption` excludes `Null`/`Nothing`.
4. **Scala 3** (`core/src/main/scala-3/.../derivation/`): `showType` in `Signatures`; `Ctx.show` → ` (in ...)` via
   `showType`, empty at the root; `Resolve.structural` (Null check before the families, singleton reason, hint; refined's
   `an implicit for it failed` suffix stays at the end); `Resolve.resolve` Nothing path; `Collections` (wildcard check in
   `deriveDictionary`/`deriveArray`, path + printer in the three messages and `dictKey`'s); `Sums.unsupportedLeaf`
   (new text, `fullName(sym)`, path); `Signatures:46` printer.
5. **Existing tests**: `SumsEnumsCharSpec` T5 pins the Scala 2 leaf text → the new shared text on both versions.
   `ErrorPhrases` keeps all phrases (`is not supported`, `No json.Schema[_] for Nothing`, ...); add `SumMember` /
   `NullHint` / `SingletonHint` phrases if other specs want them. `TypeFamiliesCharSpec` T2 (`Lonely.type`) stays `include(NotSupported)`.
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
