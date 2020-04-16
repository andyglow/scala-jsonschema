package com.github.andyglow.jsonschema

import cats.data._
import json.Schema.ValidationBound
import json.Schema.ValidationBound.mk
import json.schema.Predef

private[jsonschema] trait ScalaVersionSpecificLowPriorityCatsSupport { this: LowPriorityCatsSupport =>

  implicit def nestVB[X]: ValidationBound[NonEmptyStream[X], Iterable[_]] = mk[NonEmptyStream[X], Iterable[_]]

  implicit def nestSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyStream[T]] = mkNEx[T, NonEmptyStream](p.schema)
}
