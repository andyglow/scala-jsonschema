package json.schema.derived

import scala.quoted.*

import com.github.andyglow.jsonschema.derivation.SchemaMacro

/** The Scala 3 implicit of `object DerivedSchema`: an inline implicit whose expansion runs the Scala 3 derivation.
  *
  * It is `transparent` on purpose: a transparent inline call is expanded while it is typed, i.e. inside the implicit search, so when the derivation aborts (a type it can't derive) the candidate fails
  * and the search goes on without it (a default argument applies, a lower-priority implicit is tried), like Scala 2's whitebox macro. A plain `inline` call is expanded after typer, once the search
  * has already picked it, so the abort would be a hard compile error. The search's error message is then the derivation's own (e.g. `schema for X is not supported, ...`), not "No given instance".
  *
  * It doesn't delegate to `json.Schema.derived`: that one makes the root a definition (`derives json.Schema` counts as a user-provided implicit `Schema`), while `DerivedSchema` keeps it inline, like
  * Scala 2.
  */
trait DerivedSchemaPlatform {

  implicit transparent inline def deriveDerivedSchema[T]: DerivedSchema[T] = ${ DerivedSchemaMacros.deriveDerivedSchema[T] }
}

/** INTERNAL API. */
object DerivedSchemaMacros {

  def deriveDerivedSchema[T: Type](using Quotes): Expr[DerivedSchema[T]] =
    '{ DerivedSchema[T](${ SchemaMacro.derivedSchemaImpl[T] }) }
}
