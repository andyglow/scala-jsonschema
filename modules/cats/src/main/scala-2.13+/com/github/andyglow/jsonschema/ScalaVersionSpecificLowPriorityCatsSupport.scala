package com.github.andyglow.jsonschema

import cats.data.NonEmptyLazyList
import json.Schema.ValidationBound
import json.Schema.ValidationBound.mk
import json.schema.Predef

private[jsonschema] trait ScalaVersionSpecificLowPriorityCatsSupport { this: LowPriorityCatsSupport =>

  implicit def nellVB[X]: ValidationBound[NonEmptyLazyList[X], Iterable[_]] = mk[NonEmptyLazyList[X], Iterable[_]]

  implicit def nellSchemaFromPredef[T](implicit p: Predef[T]): Predef[NonEmptyLazyList[T]] = mkNEx[T, NonEmptyLazyList](p.schema)
}
