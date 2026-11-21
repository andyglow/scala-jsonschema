# Migrating to 0.8.0

The full list is in the [CHANGELOG](https://github.com/andyglow/scala-jsonschema/blob/master/CHANGELOG.md).

## Build

- The groupId is `io.github.andyglow` (was `com.github.andyglow`). Imports don't change.
- Scala 2.11 is no longer published. Scala 3.3 is.
- Bridges and modules are built against current library versions, notably play-json 3 (`org.playframework`).

## Output that changes

- **Every `Option[X]` is "X or null"** (see @ref:[Option](rendering.md#option)). An `Option` field was X, so
  `"o": null` (what circe and upickle write by default) didn't validate; it is now `"type": [X, "null"]`, still not
  required. An `Option` at the root, in a collection or as a map value was `oneOf` of `{"value": X}` and `"None"`,
  which no JSON library writes. `Option[Option[X]]` is the same as `Option[X]`. To get the 0.7 output back, render
  with `JsonSchemaProfile.Legacy`: one implicit where you render
  (`implicit val profile: JsonSchemaProfile = JsonSchemaProfile.Legacy`), or `schema.withProfile(JsonSchemaProfile.Legacy)`.
  Legacy reproduces 0.7 byte for byte.
- **Definition names in the rendered JSON follow each draft's grammar.** Characters a draft doesn't allow are
  replaced with `_` in `definitions`/`$defs` keys, `$id`, `$anchor` and `$ref`: `com.acme.Box[scala.Int]` becomes
  `com.acme.Box_scala.Int_`. Draft-04 keeps the characters valid in a URI fragment; drafts 06, 07 and 2019-09
  allow `[A-Za-z][-A-Za-z0-9.:_]*`; 2020-12 allows `[A-Za-z_][-A-Za-z0-9._]*` (a name that has to start with a
  letter gets a prefix). If two names collide, the one that already conformed keeps it and the others get `_2`,
  `_3`, ... Names that already conform (`com.acme.Foo`) don't change. `Json.sig`, the `json.Schema` value and
  the raw renderer keep the readable name.
- **A type alias is seen through.** A definition reached through an alias is named after the type it stands for,
  so a root implicit `Schema[String]` is `java.lang.String` (was `scala.Predef.String`), like a `String` field.
  Type arguments keep their spelling: `Box[String]` is still `Box[scala.Predef.String]`.
- **An implicit `Schema` is used only for exactly its type.** An implicit `Schema[Sub]` no longer stands in for its
  sealed parent (that dropped the other subtypes), nor makes a non-sealed trait derivable.
- **Scala 2: `Json.sig` of a backticked type name is decoded** (`` pkg.`odd name` `` gives `pkg.odd name`, was
  `pkg.odd$u0020name`), as on Scala 3. The renderers then make it a valid name (`odd_name`).
- **Scala 2: an undocumented class gets no description.** It used to take the nearest earlier doc comment in its
  file (of another class, a method, ...), including its `@param`s. Only the doc comment right before the
  declaration counts now.
- **Scala 2: field annotations with the `@field` meta-annotation are read** (`@(description @field)("...")`);
  they used to be ignored.
- Copies of a free object stay free (`withTitle`, `withDescription`, `withField`, `toDefinition`, ... used to make
  it strict).

## API

- **The derived `json.Schema` keeps `Option` as an `` `option` `` node.** An `Option` field is
  `Field("o", option(integer), required = false)` (was `integer`), and an `Option` elsewhere is `option(X)` (was
  `oneof` of `Some`/`None`). This matters if you walk the tree yourself (your own renderer, `withFieldsUpdated` on
  `f.tpe`, comparing with a hand-written `json.Schema` in tests). `schema.withProfile(JsonSchemaProfile.Legacy)`
  gives the 0.7 tree. The model also has a `` `null` `` leaf (`{"type": "null"}`).
- The renderers (`AsValue.schema`, `Json.stringify`, `asPlay`, `asCirce`, ...) take an implicit
  `JsonSchemaProfile`. Nothing changes at the call site: without one in scope, `JsonSchemaProfile.Neutral` is used.
- `json.Schema` equality is structural: a `ref` compares its signature (any two refs used to be equal), a field
  compares its description, a free object doesn't equal the strict one.
- `def.toDefinition(name)` renames only its own definition and the refs to it.
- `import Json.auto._` works (it used to crash the compiler). See @ref:[Definitions](definitions.md#predefs) for what it does.
- A scaladoc `@param` for a field that doesn't exist is a warning (it used to fail on the root type).
- Many cases that crashed the macro or produced uncompilable code are now clear compile errors (a `Map` key
  without a pattern, a `mutable.Map`, structural types, ...), and some now just work: local case classes, Java
  enums, generic value classes, `Map` subtypes such as `SortedMap` and `ListMap` with a key pattern.
- refined: the implicit is `refinedJsonSchema[R <: Refined[A, B], A, B]` (was `[A, B]`). This matters only if you
  pass its type arguments explicitly. On Scala 3, predicates take literal types (`Greater[5]`) instead of
  ``W.`5`.T``.
- parser: `ParseJson` is a strict RFC 8259 parser (it used to accept invalid JSON and didn't decode escapes).
  Failures are `JsonParseException`s with the offset; `MutableValue` and the Java classes are gone.
  `ParseJsonSchema` reads `"type": "null"` and `"type": [X, "null"]` (as `oneof{X, null}`), so the schemas this
  version renders for `Option` fields parse.
