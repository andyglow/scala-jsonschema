package json

import Type._
import Value._
import com.github.andyglow.jsonschema.Macro

sealed trait Schema {
  def v: obj
}


object Schema {
  case class Invalid(error: String) extends Schema {
    def v: obj = obj("error" -> error)
  }

  case class Valid(
    `type`: Type,
    description: Option[String] = None) extends Schema {

    def v: obj = {
      val out = obj(
        ("type"        , `type`.productPrefix),
        ("description" , description))

      val specifics: obj = `type` match {
        case `string`(format, pattern) =>
          obj(
            ("format", format),
            ("pattern", pattern))

        case `number`(format) =>
          obj("format" -> format)

        case `object`(fields) =>
          val props = fields map { field =>
            field.schema.v ++ obj("name" -> field.name)
          }
          val required = fields collect {
            case field if field.required => str(field.name)
          }

          obj(
            ("properties", arr(props.toSeq)),
            ("required"  , arr(required.toSeq)))

        case `array`(of) =>
          obj("items" -> of.v)

        case _ =>
          obj()
      }

      out ++ specifics
    }
  }

  import scala.language.experimental.macros

  def apply[T]: Schema = macro Macro.impl[T]

  def apply(`type`: Type, description: Option[String] = None): Schema = Schema.Valid(`type`, description)
}
