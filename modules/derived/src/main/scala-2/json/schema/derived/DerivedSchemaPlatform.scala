package json.schema.derived

import com.github.andyglow.jsonschema.MacroCake

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/** The Scala 2 implicit of `object DerivedSchema`: a whitebox macro, so a type it can't derive makes the implicit unavailable. See the Scala 3 copy.
  *
  * A semi-auto root (issue #542): the root is derived, nested case classes and sealed families need an instance (or `import json.Json.auto._`). It always expands inside an implicit search, so it
  * gives no phase-1 warning; with `Flag.SemiAuto` such a type makes the implicit unavailable.
  */
trait DerivedSchemaPlatform {

  implicit def deriveDerivedSchema[T]: DerivedSchema[T] =
    macro DerivedSchemaMacros.deriveDerivedSchema[T]
}

class DerivedSchemaMacros(val c: whitebox.Context) extends MacroCake {
  import c.universe._

  def deriveDerivedSchema[T](implicit
    T: c.WeakTypeTag[T]
  ): c.Expr[DerivedSchema[T]] = {
    val schemaTree        = deriveInternal[T, json.Schema](noImplicitSearch = true).tree
    val derivedSchemaTree = q"_root_.json.schema.derived.DerivedSchema($schemaTree)"
    c.Expr[DerivedSchema[T]](derivedSchemaTree)
  }
}
