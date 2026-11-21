- **0.8.0** (unreleased)
  - `json.Schema` equality: `` `ref` `` compares its signature (before, any two refs were equal, so e.g.
    `oneof(Set(ref(A), ref(B)))` collapsed to one member); `object.Field` equality includes the description;
    a free object no longer equals the strict one; `hashCode` is consistent with `equals` for every node
  - `def.toDefinition(name)` renames only its own definition and the refs to it (before, it renamed every nested
    definition and left refs to the old name dangling)
  - `import json.Json.auto._` works (before, any use crashed scalac with a StackOverflowError): it provides an
    implicit `Predef[T]` for any derivable `T`, and doesn't change what `Json.schema` derives
  - an implicit `Schema` is used only for exactly its type: an implicit `Schema[Sub]` no longer stands in for
    a sealed `Base` (which lost its other subtypes) or makes a non-sealed trait derivable
  - type aliases are seen through: `Json.schema[IntBox]` for `type IntBox = Box[Int]` works, refs and definitions
    reached through an alias get the same name (also when only a type argument is spelled through an alias), and
    a root implicit `Schema[String]` is the definition `java.lang.String` (was `scala.Predef.String`), the same
    name a field already got. Type arguments aren't dealiased: `Box[String]` is still `Box[scala.Predef.String]`
  - clear compile errors instead of errors in the generated code or macro crashes for: a Map key without a
    `KeyPattern` (e.g. `UUID`), an enum key with `Flag.EnumsAsOneOf`, a `mutable.Map`, and collections whose
    type parameter isn't the element type (e.g. `BitSet`), or Maps that don't take the key and value types as
    their type parameters (e.g. `IntMap[V]`)
  - a Map other than `Map` itself (`SortedMap[Int, X]`, `ListMap[Long, X]`) with a key pattern derives (before: an
    error in the generated code)
  - `withValidation` on a `Seq` schema (`` Json.schema[Seq[String]].withValidation(`minItems` := 1) ``) compiles
    without explicit type arguments (new `Magnet.seqMagnet`); the collection magnets are ordered by priority
    (exact collection > `Seq` > `Iterable`), which Scala 3 needs for `List`/`Vector`/`Set`. No existing call
    picks a different magnet
  - more clear compile errors instead of macro crashes: `Json.sig`/definition names of structural types,
    `Json.objectSchema` with decorations that aren't literal pairs, `Json.objectSchema` of a type that resolves to
    a definition, and an `Option` default that can't be evaluated at compile time. Definition names of compound
    (`A with B`) and annotated types work
  - generics: a default like `xs: List[T] = Nil` and a generic value class `Id[T](v: T)` work; the type
    arguments of a generic subclass are matched by name (`Impl[B, A] extends Base[A, B]`), not by position
  - a case class without fields derives an empty object; annotations on backticked/symbolic field names
    (`` `my-field` ``) are kept; a scaladoc with only `@param` tags no longer sets the description to ""
  - Scala 2: field annotations aimed at the underlying field (`@(description @field)("...")`, `@(readOnly @field)`)
    are read, together with the parameter's own (before, they were silently ignored)
  - a scaladoc `@param` for a field that doesn't exist is a warning, for the root and nested types alike (before:
    a compile error on the root only). An unknown field in `Json.objectSchema(...)` decorations is still an error
  - case classes local to a method or block are derived (before: a macro crash). On Scala 2 such a class can't
    have default values (a clear error)
  - Scala 2: default values of a case class declared inside a class (`class Holder { case class Inner(a: Int = 1) }`,
    derived inside `Holder` or as `Json.schema[h.Inner]`) work (before: an error in the generated code, `object
    Holder is not a member of package ...`). Through a type projection (`Json.schema[Holder#Inner]`) they're a clear
    compile error on Scala 2 and 3 (before, Scala 3 crashed)
  - Java enums (`java.time.DayOfWeek`, `java.time.Month`, `java.util.concurrent.TimeUnit`, ...) derive a string
    enum of their constant names, also as Map keys (before: a macro crash)
  - Scala 3: sealed families, `scala.Enumeration` and Java enums derive as on Scala 2, and Scala 3 `enum`s follow the
    sealed-family rules (only singleton cases: a string enum of the case names; with class cases: a `oneOf`). Differences
    from Scala 2: a sealed family with a subclass that is anonymous or local to a block (`new Root {}`, a case class
    declared inside a method) is a compile error, because Scala 3 can't list such subclasses (Scala 2 rejects the
    anonymous ones too, but derives a local case class); a class declared inside an `Enumeration` object derives as a
    class (Scala 2: as that object's enum); an `Enumeration`'s enum lists only its `Value`s (Scala 2 also listed vals of
    any type with a base class named `Value`)
  - Scala 3: `scala-jsonschema-refined` is published for Scala 3 (refined 0.11.4), used the same way
    (`import com.github.andyglow.jsonschema.RefinedSupport._`) with literal types (`Greater[5]`, `MatchesRegex["[a-z]+"]`)
    in place of shapeless's ``W.`5`.T``, and gives Scala 2's schemas. Differences from Scala 2: the implicit is
    `refinedJsonSchema[R <: Refined[A, B], A, B]` (was `[A, B]`), and `refined[T]` has no `T <: Refined[_, _]` bound
    (`Refined` is an opaque type on Scala 3; a non-refined `T` is a compile error). An unsupported predicate or an
    empty range found while deriving (`Json.schema[X]` with such a field) fails with the derivation's `is not supported`
    error, followed by the refined macro's message (Scala 2: the refined macro's message alone)
  - rendered definition names follow each draft's grammar: every character a draft doesn't allow in a definition
    name is replaced with `_` (`com.acme.Box[scala.Int]` → `com.acme.Box_scala.Int_`), for draft-04 in
    `#/definitions/...` refs, for 06/07 in `$id`/`$ref` plain-name fragments, for 2019-09/2020-12 in `$anchor`/`$ref`.
    Collisions get `_2`, `_3`, ... (in signature order; a name that already conforms keeps it). `Json.sig`, the
    `Schema` value and the raw renderer keep the readable signature
  - Scala 2: `Json.sig` (and so definition names) of backticked or symbolic type names is decoded (`odd name`, not
    `odd$u0020name`), as on Scala 3; the renderers make it a valid name (`odd_name`)
  - copies of a free object stay free: `withTitle`, `withDescription`, `duplicate`, `withField`, `dropField`,
    `withFieldsUpdated` and `toDefinition` no longer turn `additionalProperties: true` into `false`
  - Scala 2: a class without a doc comment no longer inherits another one. Before, it took the nearest earlier doc
    comment in its file (of another class, a method, a val, or the enclosing class), as its description and its
    `@param` field descriptions; a stray `@param` could even fail the build with `unknown field`. Now only the doc
    comment right before the declaration counts (annotations, modifiers and ordinary comments may sit in between),
    the same as on Scala 3 (scaladoc-parser 0.1.1)
- **0.7.10**
  - draft 2020-12 implemented
  - dependencies updated 
- **0.7.1**
  - draft 2019-09 implemented
  - initial json-diff functionality added  
  ...
- **0.6.3**
  - fixed scala.None handling for the default values of case class parameters (scala 2.11 is not supported this kind of functionality)
- **0.6.2**
  - added annotations for
    - definition enforcement
    - discriminator/discriminatorKey  
- **0.6.1**
  - added support for scala.Enumeration
  - default values won't be inferred for scala.None
- **0.6.0**
  ...
- **0.2.7-M1**
  - Add initial code for supporting #18. Use scaladoc for descriptions. 
    Added support for case classes and fields.
  - todo:
    - add scaladoc support for sum types
    - here is some problem. comments live in source files and get erased after compilation, so resulted bytecode 
      doesn't have this info and thus schemas derived from such models won't have descriptions.
      this might probably be solved by packing scaladocs into annotations by macro or compiler plugin and
      have those reused my SchemaMacro. Here is an example of something like this: 
      https://medium.com/@takezoe/a-compiler-plugin-makes-scaladoc-readable-at-runtime-aecbebccb794 
- **0.2.6**
  - hot-fix release. disable mistakingly left enabled debug mode for Refined macro
- **0.2.5**
  - Improve Schema matching. 
    Since now Ref and Validations are taken into an account during schema comparision.
  - Cats version upgraded to 2.1.1 for scala 2.12 and greater  
  - Support Refined types (0.9.13 for scala 2.12, 2.13, 0.9.12 for scala 2.11) 
- **0.2.4**
  - Support Cats data types (non-empty-*)
  - Support generic product types
  - Module `scala-jsonschema-api` renamed into `scala-jsonschema`
- **0.2.3**
  - Support generic sum types (aka sealed trait hierarchies)
  