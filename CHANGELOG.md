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
    type parameter isn't the element type (e.g. `BitSet`)
  - more clear compile errors instead of macro crashes: `Json.sig`/definition names of structural types,
    `Json.objectSchema` with decorations that aren't literal pairs, `Json.objectSchema` of a type that resolves to
    a definition, and an `Option` default that can't be evaluated at compile time. Definition names of compound
    (`A with B`) and annotated types work
  - generics: a default like `xs: List[T] = Nil` and a generic value class `Id[T](v: T)` work; the type
    arguments of a generic subclass are matched by name (`Impl[B, A] extends Base[A, B]`), not by position
  - a case class without fields derives an empty object; annotations on backticked/symbolic field names
    (`` `my-field` ``) are kept; a scaladoc with only `@param` tags no longer sets the description to ""
  - a scaladoc `@param` for a field that doesn't exist is a warning, for the root and nested types alike (before:
    a compile error on the root only). An unknown field in `Json.objectSchema(...)` decorations is still an error
  - case classes local to a method or block are derived (before: a macro crash). On Scala 2 such a class can't
    have default values (a clear error)
  - Java enums (`java.time.DayOfWeek`, `java.time.Month`, `java.util.concurrent.TimeUnit`, ...) derive a string
    enum of their constant names, also as Map keys (before: a macro crash)
  - rendered definition names follow each draft's grammar: every character a draft doesn't allow in a definition
    name is replaced with `_` (`com.acme.Box[scala.Int]` → `com.acme.Box_scala.Int_`), for draft-04 in
    `#/definitions/...` refs, for 06/07 in `$id`/`$ref` plain-name fragments, for 2019-09/2020-12 in `$anchor`/`$ref`.
    Collisions get `_2`, `_3`, ... (in signature order; a name that already conforms keeps it). `Json.sig`, the
    `Schema` value and the raw renderer keep the readable signature
  - Scala 2: `Json.sig` (and so definition names) of backticked or symbolic type names is decoded (`odd name`, not
    `odd$u0020name`), as on Scala 3; the renderers make it a valid name (`odd_name`)
  - copies of a free object stay free: `withTitle`, `withDescription`, `duplicate`, `withField`, `dropField`,
    `withFieldsUpdated` and `toDefinition` no longer turn `additionalProperties: true` into `false`
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
  