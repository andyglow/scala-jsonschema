package com.github.andyglow

import _root_.json.{Schema => S, schema => s}
import s.{Version => v}

package object jsonschema {

  implicit class SchemaOps[T](private val x: S[T]) extends AnyVal {

    def stringify: String = json.JsonFormatter.format(AsValue.schema(x, v.Raw))

    def stringify[V <: s.Version : AsValueBuilder](v: V): String = json.JsonFormatter.format(AsValue.schema(x, v))

    def draft04: String = stringify(v.Draft04())

    def draft06(id: String): String = stringify(v.Draft06(id))

    def draft07(id: String): String = stringify(v.Draft07(id))
  }
}
