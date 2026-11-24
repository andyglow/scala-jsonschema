# A `null` field default throws or renders an invalid `default`  (issue #502, item B17)
Status: draft

## Problem
Both macros turn a field's Scala default into a JSON `default` by calling the companion's default getter **at runtime**
and passing the value to a `ToValue[F]` (Scala 2: `UProductTypes.toField`, `q"Some($toV($getter))"`; Scala 3:
`Products.defaultValue`, `'{ Some($toV($v)) }`). Nothing checks for `null`, so a `= null` default reaches the `ToValue`
as is. What happens next depends on the field type. Repro (`notes/scratch/c-b17/B17ReproSpec.scala`,
`B17PlayReproSpec.scala`, `B17OptSpec.scala`), 0.9.0, the same on 2.12.21, 2.13.18 and 3.3.9 (JVM):

```scala
case class S(s: String = null)
object Defs { val s: String = null }
case class SV(s: String = Defs.s)
case class BD(x: BigDecimal = null)
case class BI(x: BigInt = null)
case class L(xs: List[Int] = null)
case class M(m: Map[String, Int] = null)
case class P(p: Inner = null)            // with import AsPlay._ (ToValue from Writes)
case class O(o: Option[String] = null)
```

| case | where it breaks | what you get |
|---|---|---|
| `S`, `SV` | rendering | the tree holds `Value.str(null)` (its `toString` is `"null"`, hence the `/"null"` in `Json.schema[S].toString`); `draft04/06/07/...` and `stringify` throw `NullPointerException: Cannot invoke "String.length()"` (JsonFormatter's escaping). Every profile, Legacy too |
| `BD` | nothing breaks | `{"default": null, "type": "number"}`: `num(null: BigDecimal)` prints as `null` |
| `BI` | `Json.schema` | `NullPointerException` in `ToValue[BigInt]` (`BigDecimal(null: BigInt)`) |
| `L`, `M` | `Json.schema` | `NullPointerException` in `ArrV` / `StrMapV` (`items.map`) |
| `P` (play bridge) | `Json.schema` | `NullPointerException` in the `OWrites` |
| `S` (play bridge) | rendering | as `S` above (`Writes[String]` → `JsString(null)` → `str(null)`) |
| `S` rendered by a bridge (r-b17's probe) | rendering | `asPlay` renders `"default": null` (play writes `JsString(null)` as `null`); `asCirce` throws NPE (`String.isEmpty()`) |
| `O`, Scala 2 | compile | "exception during macro expansion: TypecheckException: not found: value null" (`UProductTypes` re-parses the default's source text with `parseFCQN`, which makes `null` an identifier) |
| `O`, Scala 3 | compile | "Can't infer json value for default value ... Please provide `ToValue[Option[String]]`" (`defaultIsNone` sees the word `null`, not `None`) |
| `O`, Scala 3 + a bridge `ToValue[Option[_]]` | `Json.schema` | `NullPointerException` in the generated `d.isEmpty` |
| `Field("s", Schema.string, false, null: String)` | rendering | the public `Field.apply[T: ToValue](..., default: T)` factory has the same `Some(ToValue(default))`: NPE as `S` |

A `NullPointerException` from `Json.schema` usually shows up as an `ExceptionInInitializerError` of the object holding
`implicit val schema = Json.schema[X]`. On Scala.js the same code paths hit `null` dereferences (a `TypeError` or
undefined behaviour under the default unchecked null-pointer semantics).

(ITEMS.md says `"default": "null"` is rendered. That's the tree's `toString`; the rendered JSON never gets that far.)

## Why it is a problem
- `= null` defaults are common in models shared with Java, or generated from Java/Avro/OpenAPI sources. Users get a
  runtime crash at schema construction or rendering, far from the field that causes it, and on Scala 2 an internal
  macro exception for `Option = null`.
- Where it doesn't crash (`BigDecimal`), the output is `"default": null` next to `"type": "number"`: JSON Schema says a
  `default` SHOULD validate against its own schema, and tools that fill in defaults (form generators, some validators
  with "apply defaults") insert an invalid value.
- The neighbours make it more common: E4 (#473, PR #478) adds boxed `java.lang.Integer` etc., whose `= null` default is
  the normal way to say "absent" in Java-shaped models; B18 adds `ToValue`s for Predef'd types (`UUID`, `Duration`, ...),
  each of which would need its own null check otherwise.

## Options

### A. Omit `default` when the value is `null`: one null-guard used by both macros and the `Field` factory  — WON
A new helper in core, next to `ToValue.apply`:

```scala
object ToValue {
  /** The JSON form of a Scala default value: `None` for `null` (no JSON `default`), else `Some(to(x))`. */
  def ofDefault[T](x: T)(implicit to: ToValue[T]): Option[Value] = if (x == null) None else Some(to(x))
}
```

- Scala 2 (`UProductTypes.toField`): `q"Some($toV($defaultGetterTree))"` → `q"_root_.com.github.andyglow.json.ToValue.ofDefault($defaultGetterTree)($toV)"`.
- Scala 3 (`Products.defaultValue`): `'{ Some($toV($v)) }` → `'{ ToValue.ofDefault[f]($v)($toV) }`.
- `Schema.object.Field.apply[T: ToValue](..., default: T, ...)`: `Some(ToValue(default))` → `ToValue.ofDefault(default)`.
  `hasDefault = true` stays (the Scala field has a default).
- The field keeps `required = false` and `hasDefault = true` (the macros already add `.withHasDefault(true)` whenever
  the Scala field has a default, as for `= None`), so profiles that care (upickle omits a field equal to its default)
  keep working.
- `Option = null` is treated like `= None`, at compile time where the macro decides there:
  - Scala 2: the `isNone` check recognises a default whose source text is `null` (before `parseFCQN`) as `Literal(Constant(null))`,
    and evaluates `{ val d = <default>; d == null || d.isEmpty }` instead of `<default>.isEmpty` (covers a module value
    that is `null`, through `c.eval`).
  - Scala 3: `defaultIsNone` counts the word `null` (lexical path) and a `Literal(NullConstant())` (tree path,
    `isNoneTree`) as None; the runtime branch with a `ToValue[Option[E]]` becomes `if (d == null || d.isEmpty) None else ...`.

Output: `S`, `SV`, `BD`, `BI`, `L`, `M`, `P`, `O` all render the field with no `default` keyword, not required:
`{"type":"object","properties":{"s":{"type":"string"}},"additionalProperties":false}`. Nothing throws.

Pros: one place, every field type, every present and future `ToValue` (core, E4's boxed ones, B18's, the bridges',
users' own): no `ToValue` has to be null-safe. The output is valid (no default that fails its own type). The generic `x == null` on an unconstrained `T` compiles without warnings on 2.12,
2.13 and 3 (a primitive `T` is boxed and never `null`), so the macros don't need to know whether the field is a
reference type.
Cons: a user who wanted `"default": null` (to say "null by default" to a tool) can't get it; they'd need the field to
allow null (an `Option`) anyway for that default to be valid. A small public addition to `ToValue`'s companion.

Known limitation (A and B alike, not B17's to fix): an *instance* that holds the `null` default is written by most
libraries as `{"s": null}` (play's `Json.writes` does), which `"type": "string"` rejects, because a `String` that can be
`null` isn't modelled as nullable. B17 only makes the schema derivable and its `default` valid; making `null`-able
reference fields nullable in the schema is a separate (style) question.

### B. `null` → `Value.null` (render `"default": null`)  — LOST
The same helper with `if (x == null) Some(Value.`null`)`, or (E4's approach for its boxed types) every `ToValue` maps
`null` to `Value.null`. Output: `"s": {"type": "string", "default": null}`.

Pros: says "null" literally; consistent with E4's current plan for boxed types; a one-line difference from A if done
through the helper, so it stays easy to pick.
Cons: the default doesn't validate against `"type": "string"` (SHOULD in the spec) and a default-applying tool writes an
invalid value. Done per `ToValue` instead of through the helper, it only fixes the instances we touch: bridge and user
`ToValue`s still NPE, and every new `ToValue` (B18) must remember it. Lost on validity.

### C. `"default": null` only where the field's schema allows null; omitted otherwise  — LOST
The only fields whose schema allows `null` are `Option`s (under `OptionStyle.Nullable`; Legacy and the bridge profiles
differ). The macro can't know the profile, so this would need a render-time decision (keep a "null default" marker in
`Field`, let the profile emit `null` or nothing). For `Option = null` the result would be `"default": null` next to
`["string","null"]` under Neutral, nothing elsewhere.
Pros: valid everywhere, and says "null" where it can.
Cons: a new marker in `Field` (a public model change) and profile code for a corner of a corner (`Option = null` is
itself a smell; `None` is the idiom and gives no default). Not worth it; A gives the same output for every non-Option
field and a valid one for `Option`.

### D. Fix it at rendering: render `str(null)` (and other `null` payloads) as JSON `null`  — LOST
Make `JsonFormatter` / `AsValue` treat `str(null)` as `null`.
Pros: tiny.
Cons: fixes only `S`/`SV`; `BI`, `L`, `M`, `P` and `O` still throw in `ToValue` or at compile time, and the output is B's
invalid `"default": null`. A symptom fix.

### E. Reject a `null` default at compile time  — LOST
Detect a literal `= null` and abort with a clear message (or drop the default) at compile time.
Pros: no runtime check.
Cons: misses runtime nulls (`= Defs.s`, a Java constant), and a compile error breaks the Java-shaped models this is
about. Compile-time detection is still used for `Option = null` in A, because that's where the macros decide `None`.

### F. A + no `ToValue` needed for a literal `= null`  — LOST (B17-R2)
On top of A: when the default is the literal `null`, emit no default at compile time and don't summon a `ToValue`, so
`n: Inner = null` compiles without a bridge (today, and under A, it is "Can't infer json value ... ToValue[Inner]",
like any case-class default without a bridge).
Detection is the same as for `Option = null` (plan 3/4): Scala 2 parses the default's source (`hasSource`) or `c.eval`s
the compiled getter (`!hasSource`), so it can always tell. Scala 3 can't evaluate: it needs the getter's tree
(`-Yretain-trees`, which users don't set) or the source of a class compiled **in the same run**
(`isDefinedInCurrentRun`). A model from another module, or one whose file an incremental build didn't recompile,
can't be told apart, and then the macro must either
- keep the `ToValue` error: the same model compiles in a clean build and fails after editing only the call site's
  file, and always fails when the model lives in another module; or
- assume "no default" without a `ToValue`: then a non-null default of a type with no `ToValue`, a compile error today,
  silently loses its default in those builds.
Either way the result depends on the build mode and on module layout (and Scala 2 and 3 would differ), which is worse
than the consistent "provide a `ToValue`" error that every other default of such a type gets. For `Option = null` the
same "can't tell" case is harmless: it already falls back to "no default" (`optionDefault`), which is A's answer for
`null` too.
Pros: Java-shaped models with `c: SomeCaseClass = null` compile without a bridge.
Cons: the clean-vs-incremental and same-vs-other-module difference above on Scala 3. Lost; the workaround is a
bridge import (which such models usually have, since something serialises them) or `Option[Inner] = None`.

## Decision
A: a `null` default value gives no JSON `default` (the field stays optional), decided at runtime by one helper,
`ToValue.ofDefault`, which both macros and the public `Field` factory call; `Option = null` is treated as `= None` at
compile time. B is the same helper with `Some(Value.null)` if the owner prefers to say "null" literally.

## Implementation plan
1. **core** `core/src/main/scala/com/github/andyglow/json/ToValue.scala`: add `ToValue.ofDefault[T](x: T)(implicit to: ToValue[T]): Option[Value]`
   (scaladoc: "`None` for `null`: a `null` default has no JSON form of the field's type"). Shared source; no JS-specific code.
2. **core** `core/src/main/scala/json/Schema.scala`, `object Field`, `apply[T: ToValue](name, tpe, required, default: T, rwMode)`:
   `Some(ToValue(default))` → `ToValue.ofDefault(default)`, `hasDefault = true` unchanged.
3. **Scala 2 macro** `macros/src/main/scala-2/com/github/andyglow/jsonschema/UProductTypes.scala` (`toField`, the
   `default` block, ~L86–150):
   - non-Option: `q"Some($toV($defaultGetterTree))"` → `q"_root_.com.github.andyglow.json.ToValue.ofDefault($defaultGetterTree)($toV)"`
     (the field's `Option[Value]` slot gets the helper's result; `Field.tree` already adds `.withHasDefault(true)`).
   - Option, `hasSource` branch: if `c.parse(parseParameter(defaultGetterSym).default.get)` is `Literal(Constant(null))`
     (so `null` and `(null)` both count), `isNone = true` **without** any `c.eval` (B17-R1: a `null` literal in
     `{ val d = null; d.isEmpty }` types `d` as `Null` and doesn't typecheck). Wrap the `c.parse` in `try`: on a parse
     error fall through to today's `parseFCQN` path.
   - Option, every other default (both branches: the parsed source of a non-literal default, and the compiled getter
     when `!hasSource`): evaluate `q"{ val d = $effectiveDefaultGetterTree; d == null || d.isEmpty }"` instead of
     `q"$effectiveDefaultGetterTree.isEmpty"`. No type ascription (B17-R6): `d`'s type is inferred from the default
     expression or the getter (a literal `null`, the only case that would infer `Null`, is short-circuited above), and an
     ascribed declared type would put a generic class's unbound `T` into the eval'd tree (`Box[T](o: Option[T] = None)`
     → "not found: type T", only in incremental builds). A module `val` that is `null` (`= Defs.nullOpt`, already
     compiled) then gives no default instead of today's "Can't evaluate" abort. `isNone == true` gives no default, as
     for `None`.
4. **Scala 3 macro** `core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/Products.scala`:
   - `defaultValue`: `'{ Some($toV($v)) }` → `'{ ToValue.ofDefault[f]($v)($toV) }`.
   - `optionDefault`, with a `ToValue[Option[e]]`: `if (d.isEmpty)` → `if (d == null || d.isEmpty)`.
   - `defaultIsNone` lexical path: the word `null` counts as None (next to `None` / `scala.None` / `Option.empty`).
     This is the path that matters (a top-level class compiled in this run, without `-Yretain-trees`).
   - `isNoneTree`: add an explicit `case Literal(NullConstant()) => true`. It is documentation only: the existing
     `t.tpe <:< None.type` already holds for a `null` literal (`Null <: None.type` without explicit nulls; B17-R3).
     Keep both, with a comment saying why, so neither gets "simplified" away. Update both scaladocs (they list what
     counts as None).
   - `clean` before testing (CLAUDE.md).
5. **Tests**, a new shared spec `api/src/test/scala/com/github/andyglow/jsonschema/NullDefaultSpec.scala` (runs on 2.12,
   2.13, 3 and on JS via `apiJS`); models at the top level of the file (Scala 2 can't take defaults of block-local classes):
   - trees: `S`, `SV` (runtime `null` from a `val`), `BD`, `BI`, `L`, `M`, `O` each equal
     `object(Field(name, <schema>, required = false).withHasDefault(true))` (`O` with `option(string)`);
   - rendering: `Json.schema[S].draft07(...)` under Neutral and Legacy has no `default` key and doesn't throw;
   - control: `s: String = "x"` still gives `"default": "x"`; `x: BigDecimal = 1.5` still gives `1.5`;
   - the factory: `Field("s", string, false, null: String)` has `default == None`, `hasDefault == true`.
   - a bridge spec in `modules/play-json/src/test/.../NullDefaultPlaySpec.scala` (or added to `AsPlaySpec`): with
     `import AsPlay._`, `p: Inner = null` (an `OWrites[Inner]`) and `o: Option[String] = null` (play's `ToValue[Option]`
     path on Scala 3, the compile-time path on Scala 2) give no default; and `S` rendered with `asPlay` has no
     `default` (today `"default": null`). One circe check too (`modules/circe-json`): `S` with `asCirce` doesn't throw.
   - pin option F's verdict: `n: Inner = null` with no bridge still fails to compile with the `ToValue[Inner]` message
     (`assertDoesNotCompile` / the characterization harness's `errorOf`), on 2.12, 2.13 and 3.
   - If E4 (#478) has merged first: its `java.lang.Integer = null` test (plan 6 of `473-boxed-bigmath.md`, "renders
     `"default": null`") now expects no default; change it in this PR. If B17 merges first, E4's PR needs that change
     (mail the lead). E4's `ToValue`s mapping `null` to `Value.null` can stay (only reached through a direct `ToValue` call).
6. **Golden files**: none change (no golden model has a `null` default; checked with a grep of `api/src/test/resources`).
   `golden-legacy` unchanged.
7. **Docs**: README ~L114 ("`Option` fields and fields with a default value are optional. ... A default is written
   into the schema when it can be turned into JSON: ..."): add "A `null` default (`s: String = null`) gives no
   `default` keyword; the field is still optional. A type that needs a `ToValue` still needs one when its default is
   `null`." The same paragraph is in `docs/src/main/paradox/getting-started.md`
   ~L75: same sentence. `makeDocs` after.
8. **Gates**: as the protocol's step 8 (core and macros are touched: `clean test` on all three Scala versions, `rootJS/test`
   on all three, `scalafmtCheckAll` + `rootJS/scalafmtCheckAll`).

Risks:
- Scala 2's `c.eval` of an `Option` default only works for literals and values of already compiled modules (an existing
  limitation, the "Can't evaluate the default value" error). An `Option` default that is a `null` `val` of a module
  compiled in the same run keeps that error; the null-safe expression only matters for the incremental (`!hasSource`)
  path. Not testable in one run; the literal `= null` test covers the main case. On Scala 3 the same default is "not
  None" (a compile error without a `ToValue[Option[_]]`, the runtime guard with one; the play test covers the latter).
- `-Xlint` / `-Xcheck-macros`: the `x == null` sits in library code (`ofDefault`), not in generated code, so user call
  sites get no new warnings. `ofDefault` itself must compile warning-free on all three versions (generic `T`, so no
  "comparing values of types Int and Null" warning).

## Compatibility
- Source/binary: one new public method (`ToValue.ofDefault`), binary-compatible addition. No signature changes.
- Output: only fields with a `null` default change, and only from a crash or a compile error to "no `default`", except
  `BigDecimal = null` (and E4's boxed types once merged), which change from `"default": null` to no `default`. That is
  an output change for that case; it's the point of the fix (the old default was invalid for its own type).
- Legacy: 0.7 had the same crashes (a bug, not style), so per the lead's Legacy policy the fix applies under Legacy too;
  no golden-legacy file changes. Alternative kept: none needed, since Legacy never produced valid output for these.
- PVP: patch-level (0.9.x): a bug fix plus an additive method.

## Open questions for the owner
- **Omit vs `"default": null`, one decision for B17 and E4 (#478).** E4 (approved, PR open) renders a boxed
  `java.lang.Integer = null` as `"default": null`, with "omit" as the alternative; this doc picks omit for every type.
  With A, `ToValue.ofDefault` is the single switch: `None` (A, omit) or `Some(Value.null)` (B) flips both items at once.
  Whichever PR merges second aligns E4's `= null` test (plan 5).
- Option F (a literal `= null` needing no `ToValue`): lost on Scala 3 build-mode consistency; say if the convenience
  is worth it anyway.

## Review log
Round 1 (r-b17, on ebc06f8):
- B17-R1 (must) Scala 2 `{ val d = null; d.isEmpty }` doesn't typecheck → accepted: plan 3 short-circuits a literal
  `null` (recognised with `c.parse`, so `(null)` counts) to `isNone = true` with no `c.eval`; the `!hasSource` getter
  path evaluates a typed `val d: $fieldTpe`.
- B17-R2 (must) literal `= null` without a `ToValue` is still a compile error; decide → pushed back, decided as new
  option F (LOST): Scala 3 can only tell a literal `null` for a class compiled in the same run (or with
  `-Yretain-trees`), so the model would compile clean and fail incrementally / from another module (or silently drop
  non-null defaults if made lenient), and Scala 2/3 would differ. Kept the consistent `ToValue` error, pinned by a test
  (plan 5), documented in the README sentence (plan 7), listed as an owner question.
- B17-R3 (nit) `isNoneTree` already true for `null` via `Null <:< None.type` → accepted: plan 4 says the explicit case
  is documentation and the lexical path is the one that matters; keep both with a comment.
- B17-R4 (nit) "matches what JSON libraries do" is wrong (play writes `{"s": null}`) → accepted: sentence dropped; a
  "known limitation" paragraph in option A. Also added r-b17's bridge findings (`asPlay` `"default": null`, `asCirce`
  NPE) to the problem table and plan 5.
- B17-R5 (nit) E4 decided the opposite; one owner decision → accepted: "Open questions for the owner" section.

Round 2 (r-b17, on 32d300c): R2 withdrawn (F's verdict holds).
- B17-R6 (nit) the `: $fieldTpe` ascription puts a generic class's unbound `T` into the `c.eval` tree, on an
  incremental-only path → accepted: plan 3 drops the ascription and uses the null-safe
  `{ val d = <default>; d == null || d.isEmpty }` for every non-literal Option default, in both branches.
