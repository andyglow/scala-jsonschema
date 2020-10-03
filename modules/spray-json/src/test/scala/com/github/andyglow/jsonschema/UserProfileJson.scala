package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.model._
import spray.json._

object UserProfileJson extends DefaultJsonProtocol {

  implicit val CredentialsW: JsonWriter[Credentials] = jsonFormat2(Credentials)

  implicit val NotesW: JsonFormat[Notes] = jsonFormat2(Notes)

  implicit val BetaFeatureW: JsonWriter[BetaFeature] = new JsonWriter[BetaFeature] {
    override def write(o: BetaFeature): JsValue = o match {
      case F0  => JsString("feature-0-name")
      case F1  => JsString("feature-1-name")
      case F2  => JsString("feature-2-name")
    }
  }

  implicit val RoleW: JsonWriter[Role] = new JsonWriter[Role] {
    import Role._

    override def write(o: Role): JsValue = o match {
      case User     => JsString("e-user")
      case Manager  => JsString("e-manager")
      case Admin    => JsString("e-admin")
    }
  }
}
