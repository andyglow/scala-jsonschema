package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.model._
import upickle.core.Visitor
import upickle.default._

object UserProfileJson {

  implicit val CredentialsW: Writer[Credentials] = macroW[Credentials]

  implicit val BetaFeatureW: Writer[BetaFeature] = new Writer[BetaFeature] {
    override def write0[V](
      out: Visitor[_, V],
      o: BetaFeature): V = o match {
      case F0  => out.visitString("feature-0-name", 0)
      case F1  => out.visitString("feature-1-name", 0)
      case F2  => out.visitString("feature-2-name", 0)
    }
  }

  implicit val RoleW: Writer[Role] = new Writer[Role] {
    import Role._

    override def write0[V](
      out: Visitor[_, V],
      o: Role): V = o match {
      case User  => out.visitString("e-user", 0)
      case Manager  => out.visitString("e-manager", 0)
      case Admin  => out.visitString("e-admin", 0)
    }
  }
}
