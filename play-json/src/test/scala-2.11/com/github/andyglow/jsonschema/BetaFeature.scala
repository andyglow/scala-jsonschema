package com.github.andyglow.jsonschema

import play.api.libs.json._

sealed trait BetaFeature

case object F0 extends BetaFeature
case object F1 extends BetaFeature
case object F2 extends BetaFeature

object BetaFeature {

  implicit val BetaFeatureW: Writes[BetaFeature] = new Writes[BetaFeature] {
    override def writes(o: BetaFeature): JsValue = o match {
      case F0  => JsString("feature-0-name")
      case F1  => JsString("feature-1-name")
      case F2  => JsString("feature-2-name")
    }
  }
}
