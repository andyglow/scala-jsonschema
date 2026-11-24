# `scala.Enumeration`: list what the values are written as, not the val names  (issue #503, item B16)
Status: draft

## Problem
For a `scala.Enumeration`, both derivations list the names of the object's `val`s (Scala 2: `UEnums.fromScalaEnumeration`,
every getter whose type has a base class named `Value`; Scala 3: `Enums.scalaEnumeration`, quirk 12 kept for parity).
A value's name is something else: `Value("C++")`, `Value(10, "Haskell")`, or, with no name given, the val's name found
at runtime (JVM reflection; on Scala.js the compiler adds the name). The same val names make the `Map[E.Value, V]`
key pattern.

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

Also today (read from the code, the Scala 2 ones run): values declared in a parent class of the object aren't listed
(only `decls`); on Scala 2 a val of another type named `Value` is listed (`WithOtherValue` in `SumsEnumsCharSpec`:
`enum("A", "notOne")`).

## Why it is a problem
The JSON libraries write `toString` and read with `withName` (run, `notes/scratch/c-b16/repro/`):
- play-json `Json.formatEnum(Lang)`, `Writes.enumNameWrites` (an implicit on Scala 2): `"C++"`, `"Scala"`, `"Haskell"`.
  `Reads.enumNameReads(Lang)` rejects `"CPP"` (`error.expected.validenumvalue`).
- circe `Encoder.encodeEnumeration(Lang)`: the same; `Decoder.decodeEnumeration(Lang)` rejects `"CPP"`.
- json4s-ext `EnumNameSerializer` writes `toString` too (`EnumSerializer` writes the id). Not run: json4s-ext isn't a
  dependency of the build; from its source.
- upickle, spray-json: no codec for `Enumeration#Value` (run: no implicit). Users write one, normally with `toString`/`withName`.

So for any value with an explicit name, the schema rejects what the codec writes, and allows a string the decoder
rejects. With B6's key enforcement (#479), `{"C++": 1}` is rejected as a key too. An alias adds a name nobody writes.

## Options
### A. Read the names from the object at runtime  — WON
The macro emits a reference to the `Enumeration` object and a call to a small core helper that reads
`obj.values.toList.map(_.toString)` when the schema is built:

```scala
// core/src/main/scala/com/github/andyglow/jsonschema/Enumerations.scala (shared, JS-clean)
object Enumerations {
  def names(e: Enumeration): List[String] = e.values.toList.map(_.toString).distinct
  def schema[T](e: Enumeration): Schema.`enum`[T] = names(e) match {
    case h :: t => Schema.`enum`.of[T](Schema.`string`[T](None), Value.str(h), t.map(Value.str(_)): _*)
    case Nil    => throw new IllegalStateException(s"$e has no values (yet): build its schema after the values, or in a lazy val or def")
  }
  def keyPattern(e: Enumeration): String = names(e).map(KeyPattern.escape).mkString("^(?:", "|", ")$")
}
// Scala 2 expansion of Json.schema[Lang.Value]:   _root_.com.github.andyglow.jsonschema.Enumerations.schema[Lang.Value](Lang)
// Map[Lang.Value, Int]: ... .withValidation(patternProperties := Enumerations.keyPattern(Lang))
```

Output for `Repo`: `"enum": ["C++", "Scala", "Haskell"]` (JVM: `Scala` or `Default`, whichever `toString` says),
key pattern `^(?:C\+\+|Scala|Haskell)$`.

Prototyped on both derivations (patch: `notes/scratch/c-b16/prototype.patch`, about 60 lines; reverted) and run
on 2.13.18 and 3.3.9, JVM and JS:

| case | prototype output |
|---|---|
| `Lang.Value` | `enum("C++","Default","Haskell")` on the JVM run, `enum("C++","Scala","Haskell")` on JS: equal to `toString` both times |
| a local `object Local extends Enumeration { val X = Value("x!"); val Y = Value }` (value + Map) | `enum("x!","Y")`, `^(?:x!|Y)$` |
| `implicit val schema = Json.schema[Value]` **inside** the object, after the values (prefix is `this`) | `enum("a-1","a-2")` |
| the same, **before** the values | `IllegalStateException: InsideFirst has no values (yet) ...` when the object initializes (JVM: inside an `ExceptionInInitializerError`) |
| `val e: Enumeration = Lang; Json.schema[e.Value]` (a stable non-object path) | `enum("C++",...)` (today: no values at all) |
| custom `Val` without a name (`Planet`, `super.Val(nextId, null)`) | JVM `enum("Mercury","Venus")`; JS `enum("<Unknown name for enum field #0 of class ...>", ...)`, which is also what `toString`, and so every library, writes there |
| existing tests | only the two pinned T8 behaviors fail (below); `SchemaMacroSpec` passes on the JVM |

Pros: equal to what the libraries write by construction, on every platform, including aliases, computed names
(`Value(prefix + "x")`), inherited values and explicit ids. Small: one helper, one branch per derivation. Removes the
Scala 2 `WithOtherValue` quirk (only real `Value`s are in `values`).

Cons: the names are no longer a compile-time constant. A schema built while the object is still initializing sees no
values (caught: the helper throws) or some of them (not caught: documented). The order is `values` order, i.e. by id:
declaration order unless explicit ids say otherwise (B7 asks for declaration order; for an `Enumeration` the id order is
its own declared order, and what circe's decoder error lists).

### B. Read the `Value("...")` literals at compile time  — LOST
Look at each val's right-hand side: `Value("x")` / `Value(id, "x")` → `"x"`, `Value` / `Value(id)` → the val name.
Keeps a compile-time constant. But the right-hand side is mostly not available: a Scala 2 macro has no `Symbol.tree`
(only the trees of sources compiled in the same run, via `c.enclosingRun.units`), and Scala 3's `Symbol.tree` is empty
for a class loaded from a classfile or TASTy unless that module was compiled with `-Yretain-trees` (from the compiler
docs; not run). So it works for an `Enumeration` in the same module and silently falls back to val names for one from a
dependency: two answers for one type. It misses computed names, aliases (needs a second pass to see that `Default`
holds `Scala`) and inherited values, and it can't know the JVM's name for an alias.

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

## Decision
A: the derivation emits a reference to the `Enumeration` object, and the schema (value enum and Map key pattern) lists
`values.map(_.toString)` when it is built, which is by definition what the codecs write and read. When the type's prefix
isn't a stable path (a type projection `E#Value`), it keeps today's val names.

## Implementation plan
Build on B6 (#479: `KeyPattern.escape`, key enforcement) and B7 (#464: `enum.of` keeps order). Both are in review;
implement after they merge. If B16 lands first, `keyPattern` joins without `escape` (as today) and B6 adds it there.

1. **core** `core/src/main/scala/com/github/andyglow/jsonschema/Enumerations.scala` (new, shared, JS-clean: only
   `values`/`toString`): `names`, `schema[T]`, `keyPattern` as in option A. Scaladoc: it is what derived code calls;
   public because macro expansions call it from user code. The exception message names the object and says what to do.
2. **Scala 2** (`macros/src/main/scala-2/.../`):
   - `SchemaTypes.Enum`: a new last field `enumeration: Option[Tree] = None`; `prefix` emits
     `${N.internal.jsonschema}.Enumerations.schema[$tpe]($obj)` when it is set. Check the other `U.Enum(...)` patterns
     (`UDictionaries` has two) for the new arity.
   - `UEnums.fromScalaEnumeration`: when `objTpe` is a `SingleType` or `ThisType`, set
     `enumeration = Some(c.universe.internal.gen.mkAttributedQualifier(objTpe))`; otherwise keep the val names. Keep
     `valueTrees` (the fallback). The `isScalaEnum` test is unchanged (the `WithInner.Inner` quirk stays: out of scope,
     the type's prefix still decides).
   - `UDictionaries`: before the generic `U.Enum` case, `case Enum(U.Enum(_, _: SchemaType.Str, _, _, Some(obj)))` →
     `patternProperties := ${N.internal.jsonschema}.Enumerations.keyPattern($obj)` (runtime, so escaping happens in the helper).
3. **Scala 3** (`core/src/main/scala-3/.../derivation/`):
   - `Enums.scala`: `enumerationRef(tpe): Option[Term]` (the object of `X.Value` / an alias / `X.type`: `TermRef` →
     `Ref.term`, `ThisType(tr)` → `This(tr.typeSymbol)`), and a node `EnumerationEnum(tpe, obj: Term, extra)`
     (`children = Nil`, `emitRaw` = `'{ Enumerations.schema[t](${obj.asExprOf[Enumeration]}) }`). `scalaEnumeration`
     returns it when `enumerationRef` is defined, else today's `EnumFamily` (source `Enumeration`). Update the trait's
     scaladoc (point 2, "quirk 12").
   - `Collections.scala`: `KeyKind.Enumeration(obj: Term)`, chosen before `enumKeyNames` when `enumerationRef(keyTpe)` is
     defined; `patternOf` emits `'{ Enumerations.keyPattern($o) }`. (`enumKeyNames` only sees `EnumFamily`, so it
     returns `None` for the new node; nothing else matches on it: `FamilyEmit` handles sealed families and Scala 3 enums only.)
   - Run `clean` after these edits (CLAUDE.md).
4. **Tests** (shared under `api/src/test/scala` unless said otherwise):
   - New `EnumerationNamesSpec` with the `Lang` model above plus: `Value(id, name)`, an alias, a computed name, values
     inherited from an abstract `Enumeration` subclass, a local object, `this.Value` inside the object (after the
     values), a stable non-object path. Assert `Json.schema[Lang.Value] == enum.of(Lang.values.toList.map(_.toString))`
     (platform-independent), and on the explicit names exactly (`"C++"`, `"Haskell"`). The Map key pattern is
     `^(?:C\+\+|Scala|Haskell)$` (the alias name taken from `toString`). A schema before the values: the object's
     initialization fails with the `IllegalStateException` (JVM: as the cause of `ExceptionInInitializerError`; catch
     `Throwable`, `Try` doesn't catch linkage errors). With networknt (JVM; canceled on JS): `"C++"` and `{"C++":1}`
     valid, `"CPP"` and `{"CPP":1}` invalid on 04/07/2020-12.
   - `characterization/SumsEnumsCharSpec`: T8 becomes "uses toString (the Value(name) strings)", expecting
     `enum.of("custom-red", "Green", "custom-blue")`; the quirk test: `WithOtherValue.Value` is `enum.of("A")` on both
     versions now (only `WithInner.Inner` stays a Scala 2 quirk). Its `Planet` assertion (and `SchemaMacroSpec`'s
     `Planet`, a custom `Val` without names) holds on the JVM only: on JS assert against `Planet.values.map(_.toString)`,
     or give the model names, and fix the comment that says the schema doesn't need reflection.
   - Petshop acceptance: an `Enumeration` with a named value as a field and as a Map key, encoded by play
     (`Json.formatEnum` + `KeyWrites(_.toString)`) and circe (`Encoder.encodeEnumeration` + a `KeyEncoder`), validated
     under each profile. json4s: only if adding `json4s-ext` as a test dependency is acceptable (`EnumNameSerializer`);
     otherwise skip.
5. **Golden files**: `api/src/test/resources/golden/char/types/Colors.Value.json` and `HasColors.json`:
   `["Blue","Green","Red"]` → `["Green","custom-blue","custom-red"]` (the harness sorts arrays), in the tree and every
   draft. No `golden-legacy/` file has an `Enumeration` (grep). Regenerate with `GOLDEN_UPDATE=1 sbt clean api/test` and
   check that the diff is only these two files.
6. **Docs** (README + paradox, same words): "Sum types and enums" (README ~L319, `types.md` L58): "`scala.Enumeration`
   values are string enums of what they are written as (`toString`: the `Value("name")` name, else the val's name), read
   from the object when the schema is built. Build a schema inside the `Enumeration` object after its values, or as a
   `lazy val`/`def`." Scala.js note: a custom `Val` without a name has no name at runtime (Scala.js warns): name it.
   README ~L935 / `scala3.md` L17: the Scala 2 vs 3 difference about other `Value` vals is gone; keep the one about a
   class inside an `Enumeration`.
7. Gates per the protocol (all three Scala versions JVM, `rootJS/test` on all three: core and macros change).

Risks: the initialization case (partial values aren't detected; documented, and the common "schema in the companion
or elsewhere" pattern is unaffected); a schema now forces the object to initialize (it would anyway to encode a value);
the JVM's alias name is unstable across runs, but the schema and the codec read the same cached `toString` in one JVM.

## Compatibility
- Source/binary: a new public object `com.github.andyglow.jsonschema.Enumerations`; nothing removed or changed.
  Code compiled against the new version needs the new core at runtime (as with any macro change). Not breaking under PVP.
- Output: changes only for an `Enumeration` whose `toString` differs from its val names, or that has aliases, inherited
  values, or (Scala 2) vals of another `Value` type; and the order is `values` (id) order where explicit ids differ from
  declaration order. Unchanged for `val A, B, C = Value`. Value enums and Map key patterns, all drafts, every profile
  (the change is in the derived tree, before profiles).
- Legacy: per the lead's policy this is a bug fix (the schema rejects what was written), so it applies under Legacy too;
  no `golden-legacy/` file changes. Alternative, "Legacy keeps the val names": the tree would have to carry both name
  lists (a new node, or an `enum` with the 0.7 names attached) and `JsonSchemaProfile.Legacy` pick them. Choose it only if
  byte-exact 0.7 output for named `Enumeration` values matters to someone.

## Review log
