package json

import com.github.andyglow.jsonschema.derivation.SchemaMacro

/** Version-specific members of `object json.Schema`. */
trait SchemaCompanionPlatform {

  /** Enables `case class Foo(...) derives json.Schema`. The root is always derived from its structure; nested types follow the usual rules (an implicit `Schema` becomes a definition, a `Predef` is
    * inlined, anything else is derived inline). A `derives` given is itself an implicit `Schema`, so a type that derives it becomes a definition when used as a field elsewhere.
    */
  inline def derived[T]: Schema[T] = ${ SchemaMacro.derivedImpl[T] }
}
