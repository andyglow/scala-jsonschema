package com.github.andyglow.jsonschema

import cats.data._
import json.schema.validation.Magnet
import json.schema.Predef

private[jsonschema] trait ScalaVersionSpecificLowPriorityCatsSupport {
  this: LowPriorityCatsSupport =>
  import Magnet.mk

  implicit def nestVB[X]: Magnet[NonEmptyStream[X], Iterable[_]] =
    mk[NonEmptyStream[X], Iterable[_]]

  implicit def nestSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyStream[T]] =
    mkNEx[T, NonEmptyStream](p.schema)
}
