package json.schema.derived

import json.Schema

/** A schema derived from `T`'s structure, as a type class (e.g. a context bound `T: DerivedSchema`).
  *
  * The root is always derived: an implicit `Schema[T]` in scope is ignored (it is what the type class is usually asked to stand in for). Nested types follow the usual rules (an implicit `Schema`
  * becomes a definition, a `Predef` is inlined, anything else is derived inline). A type that can't be derived has no `DerivedSchema`: the implicit is not found, it is not a compile error of its own.
  */
final case class DerivedSchema[T](schema: Schema[T])

object DerivedSchema extends DerivedSchemaPlatform
