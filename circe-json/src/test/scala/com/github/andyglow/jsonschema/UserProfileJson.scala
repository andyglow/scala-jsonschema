package com.github.andyglow.jsonschema


import com.github.andyglow.jsonschema.model._
import io.circe._
import io.circe.generic.semiauto._

object UserProfileJson {

  implicit val CredentialsW: Encoder[Credentials] = deriveEncoder[Credentials]

  implicit val BetaFeatureW: Encoder[BetaFeature] = new Encoder[BetaFeature] {
    override def apply(o: BetaFeature): Json = o match {
      case F0  => Json.fromString("feature-0-name")
      case F1  => Json.fromString("feature-1-name")
      case F2  => Json.fromString("feature-2-name")
    }
  }

  implicit val RoleW: Encoder[Role] = new Encoder[Role] {
    import Role._

    override def apply(o: Role): Json = o match {
      case User     => Json.fromString("e-user")
      case Manager  => Json.fromString("e-manager")
      case Admin    => Json.fromString("e-admin")
    }
  }
}
