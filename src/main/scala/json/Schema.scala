package json

import com.github.andyglow.jsonschema.{SchemaMacro, TypeRegistry, TypeSignature}
import json.Type._
import json.Value._

import scala.language.experimental.macros

sealed trait Schema {

  def asJson: obj

  def definition(implicit typeRegistry: TypeRegistry): Schema
}

object Schema {

  case class Ref[T](signature: TypeSignature[T]) extends Schema {

    override def asJson: obj = obj("$ref" -> s"#/definitions/${signature.signature}")

    def definition(implicit typeRegistry: TypeRegistry): Schema = typeRegistry.get(signature) getOrElse sys.error(s"Type ${signature.signature} is not defined on provided type registry")
  }

  case class Def(
    `type`: Type,
    description: Option[String] = None) extends Schema {

    def asJson: obj = {
      val out = obj(
        ("type"        , `type`.productPrefix),
        ("description" , description))

      val specifics: obj = `type` match {
        case `string`(format, pattern) =>
          obj(
            ("format", format map { _.productPrefix }),
            ("pattern", pattern))

        case `object`(fields) =>
          val props = fields.map { field => field.name -> field.schema.asJson }.toMap
          val required = fields collect {
            case field if field.required => str(field.name)
          }

          obj(
            ("properties", obj(props)),
            ("required"  , arr(required.toSeq)))

        case `array`(of) =>
          obj("items" -> of.asJson)

        case _ =>
          obj()
      }

      out ++ specifics
    }

    def definition(implicit typeRegistry: TypeRegistry): Schema = this
  }

  def apply[T]: Schema = macro SchemaMacro.impl[T]

  def apply(
    `type`: Type,
    description: Option[String] = None) = Def(`type`, description)
}
