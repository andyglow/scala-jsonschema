package com.github.andyglow.jsonschema

private[jsonschema] trait UFlags { this: UContext =>
  import c.universe._

  case class Flags(
    enumsAsOneOf: Boolean = false,
    semiAuto: Boolean = false
  )

  // one implicit `Flag`: several flags combine in one value (`Flag with Flag.EnumsAsOneOf with Flag.SemiAuto`)
  lazy val flags: Flags = {
    val ff = c.inferImplicitValue(typeOf[json.schema.Flag])
    if (ff.isEmpty) Flags()
    else {
      val enumsAsOneOf = ff.tpe <:< typeOf[json.schema.Flag.EnumsAsOneOf]
      val semiAuto     = ff.tpe <:< typeOf[json.schema.Flag.SemiAuto]
      Flags(enumsAsOneOf = enumsAsOneOf, semiAuto = semiAuto)
    }
  }
}
