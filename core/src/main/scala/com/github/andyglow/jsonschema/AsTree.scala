package com.github.andyglow.jsonschema

import com.github.andyglow.tree.Tree
import json.Schema
import Schema._
import Tree._
import com.github.andyglow.json.Value

object AsTree {

  def apply(schema: Schema[_]): Tree = schema match {
    case _: `boolean`                => "boolean"
    case _: `integer`                => "integer"
    case _: `number`[_]              => "number"
    case `string`(fmt)               => fmt.fold[Tree]("string")(fmt => Apply("string", Iterator("format" -> fmt.productPrefix)))
    case `array`(elementTpe, unique) => Apply("array", Map("component" -> AsTree(elementTpe), "unique" -> Lit(unique.toString)))
    case `dictionary`(valueTpe)      => Apply("dictionary", Iterator("value" -> AsTree(valueTpe)))
    case `object`(fields)            =>
      Apply(
        "object",
        fields.map { f =>
          Infix(f.name, "=", AsTree(f.tpe))
        }.iterator
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
          Apply("variants", subTypes.map(AsTree(_)).iterator),
          "discriminatorField" -> discriminationField.getOrElse("empty")
        )
      )
    case `allof`(subTypes) =>
      Apply(
        "allof",
        Iterator(
          Apply("variants", subTypes.map(AsTree(_)).iterator)
        )
      )
    case `not`(tpe)         => Apply("not", Iterator(AsTree(tpe)))
    case `def`(sig, tpe)    => Apply("def", Iterator("sig" -> sig, AsTree(tpe)))
    case `ref`(sig)         => Apply("ref", Iterator(sig))
    case `value-class`(tpe) => Apply("value-class", Iterator(AsTree(tpe)))
    case `const`(v)         => Apply("const", Iterator(v.toString))
    case _: `null`          => "null"
    case `option`(tpe)      => Apply("option", Iterator(AsTree(tpe)))
    case f: `family`[_]     =>
      Apply(
        "family",
        Iterator(
          Apply(
            "members",
            f.members.iterator.map { m =>
              Apply(m.name, Iterator[Tree]("kind" -> m.kind.toString, AsTree(m.schema)) ++ m.value.iterator.map[Tree](v => "value" -> v.toString))
            }
          )
        ) ++ f.discriminator.iterator.map(d => "discriminator" -> d.field)
      )
  }
}
