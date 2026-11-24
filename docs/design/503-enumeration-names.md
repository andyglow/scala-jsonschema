# `scala.Enumeration`: list what the values are written as, not the val names  (issue #503, item B16)
Status: draft

## Problem
For a `scala.Enumeration`, both derivations list the names of the object's `val`s (Scala 2: `UEnums.fromScalaEnumeration`,
every getter whose type has a base class named `Value`; Scala 3: `Enums.scalaEnumeration`, quirk 12 kept for parity).
A value's name is something else: `Value("C++")`, `Value(10, "Haskell")`, or, with no name given, the val's name found
at runtime (JVM reflection; on Scala.js the compiler adds the name to `Value`/`Value(id)` in a val definition). The same
val names make the `Map[E.Value, V]` key pattern.

```scala
object Lang extends Enumeration {
  val CPP     = Value("C++")
  val Scala   = Value
  val Hs      = Value(10, "Haskell")
  val Default = Scala // an alias, not a value of its own
}
case class Repo(lang: Lang.Value, stars: Map[Lang.Value, Int])
```

`Json.schema[Repo]`, draft-07, the same on 2.12.21, 2.13.18 and 3.3.9, JVM and Scala.js (run):

```json
"lang":  { "type": "string", "enum": ["CPP", "Scala", "Hs", "Default"] },
"stars": { "type": "object", "patternProperties": { "^(?:CPP|Scala|Hs|Default)$": { "type": "integer" } } }
```

`Lang.values.map(_.toString)` (run):

| platform | toString of the values |
|---|---|
| JVM, 2.12 / 2.13 / 3.3 | `C++`, `Scala` **or** `Default`, `Haskell`: the aliased value is named after either val, by JVM reflection order (both seen on 2.13 in different runs of the same build) |
| Scala.js, 2.13 / 3.3 | `C++`, `Scala`, `Haskell` |

A field default contradicts the schema it is in (r-b16's repro, 2.13 + 3.3, JVM + JS): with play's `Format` in scope
(on Scala 2 play's implicit `enumNameWrites` is enough), `case class D(l: Lang.Value = Lang.CPP)` renders
`{"default": "C++", "type": "string", "enum": [..., "CPP", ...]}`: the default isn't a member of its own enum.

Also today (run on master, 2.13.18 and 3.3.9):
- values declared in a parent class of the object aren't listed (only `decls`);
- on Scala 2 a val of another type named `Value` is listed (`WithOtherValue` in `SumsEnumsCharSpec`: `enum("A", "notOne")`);
- an empty `object Empty extends Enumeration`: Scala 2 fails with "overloaded method of with alternatives ... cannot be
  applied to (json.Schema.string[Empty.Value])", Scala 3 derives `enum[string]()`, i.e. `"enum": []`, which no value matches;
- `val e: Enumeration = Lang; Json.schema[e.Value]`: the same two outcomes (no `Value` vals in `Enumeration`'s decls).

## Why it is a problem
The JSON libraries write `toString` and read with `withName` (run, `notes/scratch/c-b16/repro/`):
- play-json `Json.formatEnum(Lang)`, `Writes.enumNameWrites` (an implicit on Scala 2): `"C++"`, `"Scala"`, `"Haskell"`.
  `Reads.enumNameReads(Lang)` rejects `"CPP"` (`error.expected.validenumvalue`). Without a `KeyWrites`, play writes a
  `Map[Lang.Value, V]` as an array of pairs (`[["C++",1]]`, r-b16), so a key pattern only applies with one.
- circe `Encoder.encodeEnumeration(Lang)`: the same; `Decoder.decodeEnumeration(Lang)` rejects `"CPP"`.
- json4s-ext `EnumNameSerializer` writes `toString` too (`EnumSerializer` writes the id: out of scope, an id-based
  encoding would be a profile question). Not run: json4s-ext isn't a dependency of the build; from its source.
- upickle, spray-json: no codec for `Enumeration#Value` (run: no implicit). Users write one, normally with `toString`/`withName`.

So for any value with an explicit name, the schema rejects what the codec writes, and allows a string the decoder
rejects. With B6's key enforcement (#479), `{"C++": 1}` is rejected as a key too. An alias adds a name nobody writes.

## Options
### A. Read the names from the object at runtime, with a compile-time path for the object's own initializer  — WON
The macro emits a reference to the `Enumeration` object and a call to a small core helper that reads
`obj.values.toList.map(_.toString)` when the schema is built. It also passes the compile-time names as a fallback for the
one case where the runtime read can't work, a schema built while the object is still initializing:

```scala
// core/src/main/scala/com/github/andyglow/jsonschema/Enumerations.scala (shared, JS-clean)
object Enumerations {
  def names(e: Enumeration): List[String] = e.values.toList.map(_.toString).distinct
  /** `fallback`: the names the macro saw; used when `e` has no values yet (built during `e`'s initialization). */
  def schema[T](e: Enumeration, fallback: List[String]): Schema.`enum`[T]  = mk(namesOr(e, fallback))
  def keyPattern(e: Enumeration, fallback: List[String]): String         = namesOr(e, fallback).map(KeyPattern.escape).mkString("^(?:", "|", ")$")
  private def namesOr(e: Enumeration, fallback: List[String])             = names(e) match {
    case Nil => if (fallback.nonEmpty) fallback else throw new IllegalStateException(s"$e has no values")
    case ns  => ns
  }
}
// Scala 2 expansion of Json.schema[Lang.Value]:
//   _root_.com.github.andyglow.jsonschema.Enumerations.schema[Lang.Value](Lang, List("C++", "Scala", "Haskell", "Default"))
```

**Inside the object's initializer** (R1). The common home of an Enumeration's implicit schema is the object itself, often
at the top:

```scala
object TopSchema extends Enumeration {
  implicit val schema: json.Schema[Value] = Json.schema[Value] // runs before the vals below
  val A = Value("a-1"); val B = Value
}
```

A runtime read there sees no values (top) or some of them (middle). So the macro checks whether the expansion runs in
the initializer of the very `Enumeration` object it derives: walking the owners from the call site (Scala 2
`c.internal.enclosingOwner`, Scala 3 `Symbol.spliceOwner`), it reaches that object's module class through strict vals,
the constructor and anonymous functions only (a `def`, a `lazy val`, or another class/object on the way means "not
during initialization"). If so, and the call site comes **before** the last of the object's own `Value` vals (same
source, so both positions are known: `c.enclosingPosition` / `Position.ofMacroExpansion` vs the val symbols' `pos`), it
doesn't read at runtime. Below the last own value the runtime read is complete and exact, so it is used there (with the
fallback). Before it, the macro reads the names from the object's trees, which are available because the call site is
in the same compilation unit (Scala 2: the enclosing `ModuleDef`, `c.enclosingClass`; Scala 3: `moduleClass.tree`, same
run):

| a val's right-hand side | name |
|---|---|
| `Value("x")`, `Value(id, "x")` (a string literal) | `"x"` |
| `Value`, `Value(id)` | the val's name (what `toString` gives on the JVM and JS) |
| an alias (`Scala`, not a call of `Value`) | not a value: skipped |
| anything else (`Value(prefix + "x")`, a custom `Val`) | the val's name, plus a compile warning: "can't tell the name of `TopSchema.B` at compile time; build the schema after the values, or in a lazy val or def, to read it at runtime" |
| a value inherited from a parent class (`abstract class Base extends Enumeration { val P = Value("p!") }`) | read at runtime through a reference to the val (`this.P.toString`): the parent's constructor has run, so it exists. Listed first (its id is lower) |

So the top-of-object case gives `enum("a-1", "B")`: correct, no warning, no crash. The runtime `fallback` covers what the
owner walk can't see (a `def` called by an early strict val): there the macro's names are today's val names, so the
output is at worst today's, never a crash.

Output for `Repo`: `"enum": ["C++", "Scala", "Haskell"]` (JVM: `Scala` or `Default`, whichever `toString` says), key
pattern `^(?:C\+\+|Scala|Haskell)$`. Order: by id (`values`), which is declaration order unless explicit ids say
otherwise (`Value(10, "high")` declared before `Value(5, "low")` lists `low` first).

Prototyped (runtime part only, no fallback, no initializer path; patch `notes/scratch/c-b16/prototype.patch`,
about 60 lines; reverted) and run on 2.13.18 and 3.3.9, JVM and JS:

| case | prototype output |
|---|---|
| `Lang.Value` | `enum("C++","Default","Haskell")` on the JVM run, `enum("C++","Scala","Haskell")` on JS: equal to `toString` both times |
| a local `object Local extends Enumeration { val X = Value("x!"); val Y = Value }` (value + Map) | `enum("x!","Y")`, `^(?:x!|Y)$` |
| `implicit val schema = Json.schema[Value]` inside the object, after the values (prefix is `this`) | `enum("a-1","a-2")` |
| the same, before the values | `IllegalStateException` when the object initializes (JVM: inside an `ExceptionInInitializerError`): the case the initializer path and the fallback now avoid |
| `val e: Enumeration = Lang; Json.schema[e.Value]` | `enum("C++",...)` (today: compile error / `"enum": []`) |
| `private[access] object QHidden extends Enumeration`, used as `Outer.QL` (a public `type QL = QHidden.Value`) from another package | `enum("q!")` (2.13, 3.3, JVM and JS): the macro's reference is attributed, so no access check, and `private[pkg]` is public in bytecode |
| a `private object` used in its own scope (`private case class UsesP(p: PHidden.Value)`) | `enum("p!")`, same four builds. A public alias of a `private` object doesn't compile in user code ("escapes its defining scope"), so no call site outside can name it |
| custom `Val` without a name (`Planet`, `super.Val(nextId, null)`) | JVM `enum("Mercury","Venus")`; JS `enum("<Unknown name for enum field #0 of class ...>", ...)`, which is also what `toString`, and so every library, writes there |
| existing tests | only the two pinned T8 behaviors fail (below); `SchemaMacroSpec` passes on the JVM |

Pros: equal to what the libraries write by construction, on every platform, including aliases, computed names
(`Value(prefix + "x")`), inherited values and explicit ids. The schema default and the enum agree. Removes the Scala 2
`WithOtherValue` quirk (only real `Value`s are in `values`). Today's working in-object schemas keep working.

Cons: the names are no longer a compile-time constant outside the object. Two code paths (runtime; the object's own
initializer). The id order differs from B7's declaration order where explicit ids are out of order (for an `Enumeration`
it is its own order: `values`, `withName` and circe's decoder error use it).

### B. Read the `Value("...")` literals at compile time, everywhere  — LOST
Look at each val's right-hand side, as A does inside the initializer. Keeps a compile-time constant. But outside the
object's own compilation unit the right-hand side is mostly not available: a Scala 2 macro has no `Symbol.tree`, and Scala
3's `Symbol.tree` is empty for a class loaded from a classfile or TASTy unless that module was compiled with
`-Yretain-trees` (from the compiler docs; not run). So it would work for an `Enumeration` in the same module and silently
fall back to val names for one from a dependency: two answers for one type. It misses computed names and inherited
values, and it can't know the JVM's name for an alias. A uses it only where it is reliable.

### C. Keep the val names; document; users who name values provide the schema  — LOST
Document that `Value("x")` names aren't read, and point to
`implicit val s: Schema[Lang.Value] = Schema.enum.of("C++", "Scala", "Haskell")` (or a `Predef`). No output change.
That is the workaround today. It leaves a derived schema that rejects every codec's output for named values, which is
the bug. Also the shape of "Legacy keeps the old output" (below).

### D. A with a compile-time fallback for unnamed values on Scala.js  — LOST
Also emit `List("Mercury" -> Planet.Mercury, ...)` and use the val name when the runtime name is Scala.js'
`<Unknown name ...>`. It makes the schema disagree with what every codec writes on JS for that `Enumeration` (they all
write the unknown-name string), it depends on that string's text, and Scala.js already warns at compile time on a
`Val` without a name. Not worth it.

### E. Runtime `toString` per val (`Value.str(Lang.CPP.toString)`, ...)  — LOST
Its one pro is declaration order. But it misses inherited values, needs `distinct` for aliases, can't reach `private`
vals from outside the object, and in the initializer case reads `null` fields (an NPE, not an empty list).

## Decision
A: outside the object, the derivation emits a reference to the `Enumeration` object, and the schema (value enum and Map
key pattern) lists `values.map(_.toString)` when it is built, which is by definition what the codecs write and read.
Inside the object's own initializer, before its last value, it reads the names from the object's trees (literals, else
val names, with a warning when it can't tell; inherited values at runtime). An object with no values is a compile error. A type projection (`E#Value`, no stable path) keeps
today's val names.

## Implementation plan
Build on B6 (#479: `KeyPattern.escape`, key enforcement) and B7 (#464: `enum.of` keeps order). Both are in review;
implement after they merge. If B16 lands first, `keyPattern` joins without `escape` (as today) and B6 adds it there.

1. **core** `core/src/main/scala/com/github/andyglow/jsonschema/Enumerations.scala` (new, shared, JS-clean: only
   `values`/`toString`): `names`, `schema[T](e, fallback)`, `keyPattern(e, fallback)` as in option A; `schema` builds
   `Schema.enum.of[T](Schema.string[T](None), ...)`. Scaladoc: it is what derived code calls; public because macro
   expansions call it from user code.
2. **Names at compile time** (both derivations, shared logic in each):
   - `members`: the object's `Value` vals, own and inherited (`objTpe.members`, not `decls`), in declaration order. If
     the object is a module (not a stable val like `e`) and `members` is empty: compile error "schema for `Empty.Value`
     is not supported: the Enumeration has no values" (today: the Scala 2 overload error / Scala 3 `"enum": []`).
   - `inInitializer`: the owner walk of option A. Scala 2: from `c.internal.enclosingOwner` while the owner isn't a class;
     stop with `false` at a method that isn't an anonymous function or at a lazy val; `true` if the class reached is
     `objTpe.typeSymbol.moduleClass`. Scala 3: the same from `Symbol.spliceOwner` (`Flags.Lazy`, `isDefDef` and not
     `isAnonymousFunction`, `isClassDef`). Then `beforeLastValue`: the call site's start is before the start of the last
     own (`decls`) `Value` val, both in the same source file. The tree path is taken when both hold; otherwise the
     runtime path.
   - `literalNames`: only on the tree path: the inherited `Value` vals (in `members`, not in `decls`) as runtime
     `toString` of a reference to each (`Value.str(this.P.toString)`: the parent constructor has run; a `private` one
     in the parent is unreachable: val name + the warning), followed by the object's own template `ValDef`s, the table
     in option A. On this path the enum is emitted from a list of `Expr[Value]`/trees (some literal, some runtime). Strip
     `Typed`/`Inlined`/`Block` wrappers (Scala 3); match `Apply(Ident(Value) | Select(_, Value), args)` syntactically on
     Scala 2 (later vals are not typechecked yet). Warn with `c.warning(c.enclosingPosition, ...)` /
     `report.warning(...)`. `c.enclosingClass` is deprecated in 2.12/2.13: `@nowarn` it (2.12: `scala.annotation.nowarn`
     exists from 2.12.13).
3. **Scala 2** (`macros/src/main/scala-2/.../`):
   - `SchemaTypes.Enum`: a new last field `enumeration: Option[(Tree, List[String])] = None` (the object reference and
     the fallback names); `prefix` emits `${N.internal.jsonschema}.Enumerations.schema[$tpe]($obj, List(..$fallback))`
     when set. The only other matches are the two `U.Enum(...)` patterns in `UDictionaries` (`Macroses.scala:141`
     `case Enum(x) => x` uses the extractor, unaffected).
   - `UEnums.fromScalaEnumeration`: if `inInitializer` → today's `U.Enum` with `literalNames`. Else if `objTpe` is a
     `SingleType` or `ThisType` → `enumeration = Some(gen.mkAttributedQualifier(objTpe) -> valNames)`. Else (type
     projection) → today's val names. The `isScalaEnum` test is unchanged (the `WithInner.Inner` quirk stays: out of scope).
   - `UDictionaries`: before the generic `U.Enum` case, `case Enum(U.Enum(_, _: SchemaType.Str, _, _, Some((obj, fb))))` →
     `patternProperties := ${N.internal.jsonschema}.Enumerations.keyPattern($obj, List(..$fb))`.
4. **Scala 3** (`core/src/main/scala-3/.../derivation/`):
   - `Enums.scala`: `enumerationRef(tpe): Option[Term]` (the object of `X.Value` / an alias / `X.type`: `TermRef` →
     `Ref.term`, `ThisType(tr)` → `This(tr.typeSymbol)`), and a node `EnumerationEnum(tpe, obj: Term, fallback:
     List[String], extra)` (`children = Nil`, `emitRaw` = `'{ Enumerations.schema[t]($o, $fb) }`). `scalaEnumeration`
     returns today's `EnumFamily` with `literalNames` when `inInitializer`, else `EnumerationEnum` when `enumerationRef`
     is defined, else today's `EnumFamily`. Update the trait's scaladoc (point 2, "quirk 12").
   - `Collections.scala`: `KeyKind.Enumeration(obj: Term, fallback: List[String])`, chosen before `enumKeyNames` when the
     key is an `EnumerationEnum`; `patternOf` emits `'{ Enumerations.keyPattern($o, $fb) }`. (`enumKeyNames` only sees
     `EnumFamily`, so the in-initializer case keeps going through it; nothing else matches on the new node: `FamilyEmit`
     handles sealed families and Scala 3 enums only.)
   - Run `clean` after these edits (CLAUDE.md).
5. **Tests** (shared under `api/src/test/scala` unless said otherwise):
   - New `EnumerationNamesSpec`, the `Lang` model above plus: `Value(id, name)` with ids out of declaration order, a
     duplicate `toString`, a computed name, values inherited from an abstract `Enumeration` subclass, a local object, a
     stable non-object path, and `private[pkg]` object reached through a public alias from another package. Assert
     `Json.schema[Lang.Value] == enum.of(Lang.values.toList.map(_.toString))` (platform-independent; aliases only this
     way, never in a golden: their JVM name is unstable) and the explicit names exactly (`"C++"`, `"Haskell"`). Map key
     pattern `^(?:C\+\+|...)$`. A field default `= Lang.CPP` (with a `ToValue` from play or a hand-written one) is a
     member of the enum. With networknt (JVM; canceled on JS): `"C++"` and `{"C++":1}` valid, `"CPP"` and `{"CPP":1}`
     invalid, on 04/07/2020-12.
   - In-object cases, each an object of its own with `implicit val schema: Schema[Value]`, expected output:
     top (before the values) → literal names, `enum("a-1", "B")`, no warning; middle → the same (all values, from the
     trees); bottom → runtime (a computed name there is exact, no warning); `lazy val` at the top → runtime; `def` at the
     top, called from a strict val at the top → fallback (today's val names); a computed name or a custom `Val` at the
     top → val names, with the warning; an object extending an abstract `Enumeration` class with a named value
     (`Value("p!")`), schema at the top → `"p!"` first, then the own names. No crash in any of them: assert that touching
     the object doesn't throw.
   - The warning in tests (R10): the models that trigger it carry `@nowarn("msg=can't tell the name")`. First check that
     this filter suppresses a macro-reported warning (`c.warning` at `c.enclosingPosition` / `report.warning`) on 2.12.21,
     2.13.18 and 3.3.9; if it doesn't on one, scope a `-Wconf:msg=can't tell the name:s` to that test file's compilation
     instead (e.g. a `Test / scalacOptions` entry limited to the version), so the build stays warning-free.
   - Empty: `Json.schema[Empty.Value]` doesn't compile, with the new message (`CompileErrors.errorOf`, both versions).
   - `characterization/SumsEnumsCharSpec`: T8 becomes "uses toString (the Value(name) strings)", expecting
     `enum.of("custom-red", "Green", "custom-blue")`; the quirk test: `WithOtherValue.Value` is `enum.of("A")` on both
     versions now (only `WithInner.Inner` stays a Scala 2 quirk). Its `Planet` assertion (and `SchemaMacroSpec`'s
     `Planet`, a custom `Val` without names) holds on the JVM only: on JS assert against `Planet.values.map(_.toString)`,
     or give the model names, and fix the comment that says the schema doesn't need reflection.
   - Petshop acceptance: an `Enumeration` with a named value as a field and as a Map key, encoded by play
     (`Json.formatEnum` + an explicit `KeyWrites(_.toString)`: without it play writes an array of pairs) and circe
     (`Encoder.encodeEnumeration` + a `KeyEncoder`), validated under each profile. json4s: only if adding `json4s-ext` as a
     test dependency is acceptable (`EnumNameSerializer`); otherwise skip.
6. **Golden files**: `api/src/test/resources/golden/char/types/Colors.Value.json` and `HasColors.json`:
   `["Blue","Green","Red"]` → `["Green","custom-blue","custom-red"]` (the harness sorts arrays), in the tree and every
   draft. No `golden-legacy/` file has an `Enumeration` (grep). Regenerate with `GOLDEN_UPDATE=1 sbt clean api/test` and
   check that the diff is only these two files.
7. **Docs** (README + paradox, same words): "Sum types and enums" (README ~L319, `types.md` L58): "`scala.Enumeration`
   values are string enums of what they are written as (`toString`: the `Value("name")` name, else the val's name), in
   id order (`values`), read from the object when the schema is built." Notes: a schema inside the object at the top
   reads `Value("literal")` names from the source and warns when it can't (build it below the values or as a `lazy
   val`/`def`); give an aliased value an explicit `Value("name")` (on the JVM its name otherwise depends on reflection
   order); on Scala.js a custom `Val` without a name has no name at runtime (Scala.js warns): name it. README ~L935 /
   `scala3.md` L17: the Scala 2 vs 3 difference about other `Value` vals is gone; keep the one about a class inside an
   `Enumeration`.
8. Gates per the protocol (all three Scala versions JVM, `rootJS/test` on all three: core and macros change).

Risks: the position check depends on both positions being in the same source (always true for a call in the object's
own body; if a position is missing, take the tree path, the safe one); a `def` building the schema, called indirectly during initialization after some values exist, lists only those
(the fallback only kicks in when there are none; documented); the owner walk is new code on both versions and needs
the in-object tests above; a schema now forces the object to initialize (it would anyway to encode a value); the JVM's
alias name is unstable across runs (inherent to `Enumeration`; the schema and the codec read the same cached `toString`
in one JVM, but a schema file written at build time can disagree with another JVM: hence the doc note).

## Compatibility
- Source/binary: a new public object `com.github.andyglow.jsonschema.Enumerations`; nothing removed or changed. Code
  compiled against the new version needs the new core at runtime (as with any macro change). An empty `Enumeration` is
  now a compile error on Scala 3 too (it derived `"enum": []`, which rejects everything; Scala 2 already failed, less
  clearly). A new compile warning, only for a schema built in its own object's initializer when a value's name isn't a
  literal. Not breaking under PVP.
- Output: changes only for an `Enumeration` whose `toString` differs from its val names, or that has aliases, inherited
  values, or (Scala 2) vals of another `Value` type; and the order is `values` (id) order where explicit ids differ from
  declaration order. Unchanged for `val A, B, C = Value`. Value enums and Map key patterns, all drafts, every profile
  (the change is in the derived tree, before profiles).
- Legacy: per the lead's policy this is a bug fix (the schema rejects what was written), so it applies under Legacy too;
  no `golden-legacy/` file changes. Alternative, "Legacy keeps the val names": the tree would have to carry both name
  lists (a new node, or an `enum` with the 0.7 names attached) and `JsonSchemaProfile.Legacy` pick them. Choose it only if
  byte-exact 0.7 output for named `Enumeration` values matters to someone.

## Review log
Round 1 (r-b16, on 6b63d1d):
- B16-R1 (must) schema in the object before the values crashes the object under A → accepted: both of the reviewer's
  ways. The macro detects the object's own initializer (owner walk) and reads the names from the object's trees there
  (literal `Value("x")` names, val names otherwise, a warning when it can't tell); the runtime helper takes the
  compile-time names as a fallback instead of throwing. Option A, plan steps 2–4, the in-object test list (top, middle,
  bottom, `lazy val`, `def`, computed name) and Compatibility updated.
- B16-R2 (must) empty Enumeration and inaccessible object → accepted. Empty: ran master (Scala 2 overload error, Scala 3
  `"enum": []`); now a compile error with a clear message on both (plan step 2, test, Compatibility). Access: ran the
  prototype on 2.13 + 3.3, JVM + JS: a `private[pkg]` object behind a public alias from another package works (attributed
  reference, public in bytecode); a public alias of a `private` object doesn't compile in user code, and a `private`
  object in its own scope works. So no fallback is needed; the case is a test (step 5) and a row in option A.
- B16-R3 (nit) default contradiction → accepted: in Problem, and a test (default ∈ enum).
- B16-R4 (nit) per-val runtime option → accepted: option E, LOST.
- B16-R5 (nit) alias instability → accepted: doc note (step 7), aliases only against `values.map(_.toString)`, never in
  goldens (step 5), Risks.
- B16-R6 (nit) order in the docs text → accepted: "in id order (`values`)" in step 7, example in option A.
- B16-R7 (nit) plan details → accepted: "only these" matches named in step 3; the play `KeyWrites` note in Why and step 5.

Round 2 (r-b16, on b4a39de):
- B16-R8 (nit) inherited values on the tree path → accepted: read at runtime through a reference to each inherited val
  (the parent constructor has run), listed first; a `private` one: val name + warning. Table row in option A, step 2,
  an in-object test.
- B16-R9 (nit, optional) position refinement → taken: the tree path only when the call site is before the last own
  `Value` val; below it the runtime path (exact for computed names, no warning). Option A, step 2, tests (bottom →
  runtime), Risks (the noted risk is gone; missing positions → tree path).
- B16-R10 (nit) `@nowarn` for the macro warning → accepted: a plan item in step 5 to check it on all three versions,
  with a scoped `-Wconf` as the fallback.
