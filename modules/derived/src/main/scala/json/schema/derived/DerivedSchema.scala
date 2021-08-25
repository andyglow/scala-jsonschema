package json.schema.derived

import com.github.andyglow.jsonschema.MacroCake
import json.Schema

import scala.language.experimental.macros
import scala.reflect.macros.whitebox


final case class DerivedSchema[T](schema: Schema[T])

object DerivedSchema {

  implicit def deriveDerivedSchema[T]: DerivedSchema[T] = macro DerivedSchemaMacros.deriveDerivedSchema[T]
}

class DerivedSchemaMacros (val c: whitebox.Context) extends MacroCake {
  import c.universe._

  def deriveDerivedSchema[T](
    implicit
    T: c.WeakTypeTag[T]
  ): c.Expr[DerivedSchema[T]] = {
    val schemaTree = deriveInternal[T, json.Schema](noImplicitSearch = true).tree
    val derivedSchemaTree = q"_root_.json.schema.derived.DerivedSchema($schemaTree)"
    c.Expr[DerivedSchema[T]](derivedSchemaTree)
  }
}