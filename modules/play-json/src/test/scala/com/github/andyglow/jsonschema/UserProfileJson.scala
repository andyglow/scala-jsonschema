package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.model._
import play.api.libs.json._

object UserProfileJson {

  implicit val CredentialsW: Writes[Credentials] = Json.writes[Credentials]

  implicit val NotesW: Writes[Notes] = Json.writes[Notes]

  implicit val BetaFeatureW: Writes[BetaFeature] = new Writes[BetaFeature] {
    override def writes(o: BetaFeature): JsValue = o match {
      case F0  => JsString("feature-0-name")
      case F1  => JsString("feature-1-name")
      case F2  => JsString("feature-2-name")
    }
  }

  implicit val RoleW: Writes[Role] = new Writes[Role] {
    import Role._

    override def writes(o: Role): JsValue = o match {
      case User     => JsString("e-user")
      case Manager  => JsString("e-manager")
      case Admin    => JsString("e-admin")
    }
  }
}
