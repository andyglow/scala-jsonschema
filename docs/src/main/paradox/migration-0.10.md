# Migrating to 0.10.0

**Semi-auto by default** (issue #542, see @ref:[Semi-auto and auto derivation](definitions.md#semi-auto-and-auto-derivation)):

- Code compiles as before. A nested case class or sealed family that has no implicit `Schema` or `Predef` is
  still derived, with a warning prefixed `[json-schema-semi-auto]`. In the next breaking release it is an error.
  With `-Werror` / `-Xfatal-warnings` the warning fails the build now: add `-Wconf:msg=json-schema-semi-auto:s`
  to keep 0.10 quiet, or migrate.
- **Zero-change migration:** `import Json.auto._` where you derive. The output stays exactly the same.
- **Migration with instances:** `implicit val xSchema: json.Schema[X] = Json.schema[X]` for the nested types.
  Those types become definitions + `$ref`s (the core rule), so the output changes; a `Predef` keeps them inlined.
  Mutually recursive instances work (they used to give a `null` inside the schema, or a deadlock with Scala 3
  `derives`).
- `Flag.SemiAuto` gives the error now. Combine flags in one value: `Flag with Flag.EnumsAsOneOf with Flag.SemiAuto`.
- `DerivedSchema[X]` gives no warning (it always expands inside an implicit search); it fails in the next
  breaking release when a nested type has no instance.
- `Json.auto.derived[T]` is an `Exported[Predef[T]]` now (call `.instance`). Code that only imports `Json.auto`
  is unaffected.
- On Scala 3, `import Json.auto._` now derives the element of library containers too, as on Scala 2: cats
  `NonEmptyList[P]` is an array, not the `{head, tail}` object.
- A schema can hold the new node `` `lazy-ref` `` (a reference to an instance that wasn't built yet, in a recursion
  cycle between instances). It is resolved when the schema is rendered; code that walks derived trees may meet it.
