package com.github.andyglow.jsonschema

import cats.data.NonEmptyLazyList
import json.schema.validation.Magnet
import json.schema.Predef

private[jsonschema] trait ScalaVersionSpecificLowPriorityCatsSupport { this: LowPriorityCatsSupport =>
  import Magnet.mk

  implicit def nellVB[X]: Magnet[NonEmptyLazyList[X], Iterable[_]] = mk[NonEmptyLazyList[X], Iterable[_]]

  implicit def nellSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyLazyList[T]] = mkNEx[T, NonEmptyLazyList](p.schema)
}
