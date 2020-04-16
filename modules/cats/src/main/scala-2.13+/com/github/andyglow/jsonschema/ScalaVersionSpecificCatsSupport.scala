package com.github.andyglow.jsonschema

import cats.data.NonEmptyLazyList
import json.Schema
import json.schema.Predef

private[jsonschema] trait ScalaVersionSpecificCatsSupport { this: LowPriorityCatsSupport with ScalaVersionSpecificLowPriorityCatsSupport =>

  implicit def nellSchema[T](implicit ss: Schema[T]): Predef[NonEmptyLazyList[T]] = mkNEx[T, NonEmptyLazyList](ss)
}
