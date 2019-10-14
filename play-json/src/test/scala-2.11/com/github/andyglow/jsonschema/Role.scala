package com.github.andyglow.jsonschema

import play.api.libs.json._

// members goes inside companion

sealed trait Role

object Role {

  case object User extends Role

  case object Admin extends Role

  case object Manager extends Role

  implicit val RoleW: Writes[Role] = new Writes[Role] {
    override def writes(o: Role): JsValue = o match {
      case User     => JsString("e-user")
      case Manager  => JsString("e-manager")
      case Admin    => JsString("e-admin")
    }
  }
}