package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.model._
import org.json4s.Writer
import org.json4s.JsonAST._

object UserProfileJson {

  implicit val CredentialsW: Writer[Credentials] = new Writer[Credentials] {
    override def write(o: Credentials): JValue = JObject(
      "login"     -> JString(o.login),
      "password"  -> JString(o.password))
  }

  implicit val NotesW: Writer[Notes] = new Writer[Notes] {
    override def write(o: Notes): JValue = JObject(
      "head"  -> JString(o.head),
      "tail"  -> JArray(o.tail map JString.apply))
  }

  implicit val BetaFeatureW: Writer[BetaFeature] = new Writer[BetaFeature] {
    override def write(o: BetaFeature): JValue = o match {
      case F0  => JString("feature-0-name")
      case F1  => JString("feature-1-name")
      case F2  => JString("feature-2-name")
    }
  }

  implicit val RoleW: Writer[Role] = new Writer[Role] {
    import Role._

    override def write(o: Role): JValue = o match {
      case User     => JString("e-user")
      case Manager  => JString("e-manager")
      case Admin    => JString("e-admin")
    }
  }
}
