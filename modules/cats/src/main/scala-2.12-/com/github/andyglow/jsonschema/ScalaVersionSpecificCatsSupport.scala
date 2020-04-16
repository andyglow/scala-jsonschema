package com.github.andyglow.jsonschema

import cats.data._
import json.Schema
import json.schema.Predef

private[jsonschema] trait ScalaVersionSpecificCatsSupport { this: LowPriorityCatsSupport with ScalaVersionSpecificLowPriorityCatsSupport =>

  implicit def nestSchema[T](implicit ss: Schema[T]): Predef[NonEmptyStream[T]] = mkNEx[T, NonEmptyStream](ss)
}
