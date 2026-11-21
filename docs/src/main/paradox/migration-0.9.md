# Migrating to 0.9.0

The full list is in the [CHANGELOG](https://github.com/andyglow/scala-jsonschema/blob/master/CHANGELOG.md).

## Output

- Nothing changes unless you ask for it: the default profile (Neutral) and `JsonSchemaProfile.Legacy` render
  sealed families exactly as 0.8 and 0.7 did. To get a schema that matches your codec, import your library's
  profile: see @ref:[Library profiles](rendering.md#library-profiles).
- Sealed family members (and so the members of the rendered `oneOf` and the values of an enum) are in declaration
  order on Scala 2 too. Before, families of 5 and more members came out in an order that could change between
  builds. Members of a family compiled in another project (no source positions) are in name order.

## API

- **The derived tree has a `` `family` `` node** for each sealed family (and Scala 3 `enum`), where it had a
  `oneof` (or an `enum` for case objects only). Code that walks or compares derived trees sees it.
  `schema.withProfile(JsonSchemaProfile.Neutral)` (or `Legacy`) gives the plain tree, the same as 0.8's.
- **`object.Field` has a 7th parameter, `hasDefault`**: the Scala field has a default, also when it has no JSON form
  (`= None`, or no `ToValue` in scope). `new Field(...)` and `Field(...)` with 6 arguments still compile; a pattern
  `case Field(name, tpe, required, default, description, rwMode)` needs a 7th binder (or match `f: Field[_]` and
  use its accessors).
