package com.github.andyglow.jsonschema

import play.api.libs.json._

case class Credentials(login: String, password: String)
object Credentials {
  implicit val writes: Writes[Credentials] = Json.writes[Credentials]
}

