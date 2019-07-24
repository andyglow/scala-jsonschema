package com.github.andyglow.jsonschema

import com.github.andyglow.json.Value
import json.Schema._
import com.github.andyglow.json.Value._

object AsValue {

  def schema(
    tpe: json.Schema[_],
    title: Option[String] = None,
    description: Option[String] = None): obj = {

    def references(tpe: json.Schema[_]): Seq[`ref`[_]] = tpe match {
      case x: `ref`[_]      => references(x.tpe) :+ x
      case `array`(ref)     => references(ref)
      case `object`(fields) => fields.toSeq map { _.tpe } flatMap references
      case _                => Seq.empty
    }

    val out = obj(
      f"$$schema"   -> "http://json-schema.org/draft-04/schema#",
      "description" -> description,
      "title"       -> title)

    val definitions: Map[String, Value] = references(tpe).map { x =>
      val ref = x.tpe.refName getOrElse x.sig
      ref -> AsValue(x.tpe)
    }.toMap

    out ++ AsValue(tpe) ++ {
      if (definitions.nonEmpty) obj("definitions" -> obj(definitions)) else obj()
    }
  }

  def apply(x: json.Schema[_]): obj = {
    val out = if (x.isInstanceOf[`ref`[_]] || x.isInstanceOf[`oneof`[_]]) obj() else obj("type" -> x.jsonType)

    val validations = obj(x.validations.map { d => d.name -> d.json }.toMap)

    val specifics = x match {
      case `string`(format, pattern) =>
        obj(
          ("format", format map { _.productPrefix }),
          ("pattern", pattern))

      case `object`(fields) =>
        val props = fields.map { field =>
          field.name -> AsValue(field.tpe)
        }.toMap

        val required = fields collect {
          case field if field.required => str(field.name)
        }

        obj(
          ("additionalProperties", false),
          ("properties", obj(props)),
          ("required"  , arr(required.toSeq)))

      case `string-map`(valueType) =>
        obj("patternProperties" -> obj(
          "^.*$" -> AsValue(valueType)))

      case `int-map`(valueType) =>
        obj("patternProperties" -> obj(
          "^[0-9]*$" -> AsValue(valueType)))

      case `array`(componentType) =>
        obj("items" -> AsValue(componentType))

      case `set`(componentType) =>
        obj(
          "items" -> AsValue(componentType),
          "uniqueItems" -> true)

      case `enum`(values) =>
        obj(
          "type" -> "string",
          "enum" -> values.toArr)

      case `oneof`(schemas) =>
        val subTypesSeq = schemas.toArray
        obj(
          "oneOf" -> `arr`(
            AsValue(subTypesSeq.head),
            subTypesSeq.tail.toSeq.map(AsValue.apply): _*))

      case `ref`(sig, t) =>
        val ref = t.refName getOrElse sig
        obj(f"$$ref" -> s"#/definitions/$ref")

      case _ =>
        obj()
    }

    out ++ validations ++ specifics
  }
}
