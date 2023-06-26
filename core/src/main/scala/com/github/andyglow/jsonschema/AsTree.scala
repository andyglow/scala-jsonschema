package com.github.andyglow.jsonschema

import com.github.andyglow.tree.Tree
import json.Schema
import Schema._
import Tree._
import com.github.andyglow.json.Value

object AsTree {

  def apply(schema: Schema[_]): Tree = schema match {
    case `boolean`                   => "boolean"
    case `integer`                   => "integer"
    case _: `number`[_]              => "number"
    case `string`(fmt)               => fmt.fold[Tree]("number")(fmt => Apply("string", Iterator("format" -> fmt.productPrefix)))
    case `array`(elementTpe, unique) => Apply("array", Map("component" -> AsTree(elementTpe), "unique" -> Lit(unique.toString)))
    case `dictionary`(valueTpe)      => Apply("dictionary", Iterator("value" -> AsTree(valueTpe)))
    case `object`(fields) =>
      Apply(
        "object",
        fields.map { f =>
          Infix(f.name, "=", AsTree(f.tpe))
        }.toIterator
      )
    case `enum`(tpe, vals) =>
      Apply(
        "enum",
        Map(
          "type"   -> AsTree(tpe),
          "values" -> Lit(Value.arr(vals.toSeq).toString)
        )
      )
    case `oneof`(subTypes, discriminationField) =>
      Apply(
        "oneof",
        Iterator(
          Apply("variants", subTypes.map(AsTree(_)).toIterator),
          "discriminatorField" -> discriminationField.getOrElse("empty")
        )
      )
    case `allof`(subTypes) =>
      Apply(
        "ollof",
        Iterator(
          Apply("variants", subTypes.map(AsTree(_)).toIterator)
        )
      )
    case `not`(tpe)         => Apply("not", Iterator(AsTree(tpe)))
    case `def`(sig, tpe)    => Apply("def", Iterator("sig" -> sig, AsTree(tpe)))
    case `ref`(sig)         => Apply("ref", Iterator(sig))
    case `value-class`(tpe) => Apply("value-class", Iterator(AsTree(tpe)))
    case `const`(v)         => Apply("const", Iterator(v.toString))
  }
}
