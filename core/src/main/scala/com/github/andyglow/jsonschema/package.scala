package com.github.andyglow

import _root_.json.{Schema => S, schema => s}
import s.{Version => v}
import s.JsonSchemaProfile

package object jsonschema {

  implicit class SchemaOps[T](private val x: S[T]) extends AnyVal {

    def stringify(implicit profile: JsonSchemaProfile): String = json.JsonFormatter.format(AsValue.schema(x, v.Raw))

    def stringify[V <: s.Version](v: V)(implicit asValue: AsValueBuilder[V], profile: JsonSchemaProfile): String =
      json.JsonFormatter.format(AsValue.schema(x, v))

    def draft04(implicit profile: JsonSchemaProfile): String = stringify(v.Draft04())

    def draft06(id: String)(implicit profile: JsonSchemaProfile): String = stringify(v.Draft06(id))

    def draft07(id: String)(implicit profile: JsonSchemaProfile): String = stringify(v.Draft07(id))
  }
}
