package com.github.andyglow.jsonschema

object ScalaParts {

  case class ParsedParameter(name: String, tpe: String, default: Option[String])

  object ParsedParameter {

    def fromString(x: String): ParsedParameter = {
      val colonIdx = x.indexOf(':')
      val eqIdx    = x.indexOf('=')
      require(eqIdx < 0 || colonIdx < eqIdx)

      val name = x.substring(0, colonIdx).trim
      val (tpe, default) = if (eqIdx >= 0) {
        (x.substring(colonIdx + 1, eqIdx).trim, Some(x.substring(eqIdx + 1).trim))
      } else {
        (x.substring(colonIdx + 1).trim, None)
      }

      ParsedParameter(name, tpe, default)
    }
  }
}
