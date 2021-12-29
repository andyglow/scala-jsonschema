package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.model._
import upickle.core.Visitor
import upickle.default._

object UserProfileJson {

  // OptionPickler
  // https://www.lihaoyi.com/upickle/#uJson
  // by default u-json treats scala options as json arrays
  // this makes it use standard notion
  implicit def OptionWriterOverride[T: Writer]: Writer[Option[T]] =
    implicitly[Writer[T]].comap[Option[T]] {
      case None    => null.asInstanceOf[T]
      case Some(x) => x
    }

  implicit val CredentialsW: Writer[Credentials] = macroW[Credentials]

  implicit val NotesW: Writer[Notes] = macroW[Notes]

  implicit val BetaFeatureW: Writer[BetaFeature] = new Writer[BetaFeature] {
    override def write0[V](out: Visitor[_, V], o: BetaFeature): V = o match {
      case F0 => out.visitString("feature-0-name", 0)
      case F1 => out.visitString("feature-1-name", 0)
      case F2 => out.visitString("feature-2-name", 0)
    }
  }

  implicit val RoleW: Writer[Role] = new Writer[Role] {
    import Role._

    override def write0[V](out: Visitor[_, V], o: Role): V = o match {
      case User    => out.visitString("e-user", 0)
      case Manager => out.visitString("e-manager", 0)
      case Admin   => out.visitString("e-admin", 0)
    }
  }
}
