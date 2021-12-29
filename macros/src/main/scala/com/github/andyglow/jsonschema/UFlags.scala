package com.github.andyglow.jsonschema

private[jsonschema] trait UFlags { this: UContext =>
  import c.universe._

  case class Flags(
      enumsAsOneOf: Boolean = false
  )

  lazy val flags: Flags = {
    val ff = c.inferImplicitValue(typeOf[json.schema.Flag])
    if (ff.isEmpty) Flags()
    else {
      val enumsAsOneOf = ff.tpe <:< typeOf[json.schema.Flag.EnumsAsOneOf]
      Flags(enumsAsOneOf = enumsAsOneOf)
    }
  }
}
