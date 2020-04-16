package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.refined.RefinedMacro
import eu.timepit.refined.api.Refined
import scala.language.experimental.macros
import scala.reflect.runtime.universe._


object RefinedSupport {

  implicit def refinedJsonSchema[A, B]: json.schema.Predef[Refined[A, B]] = macro RefinedMacro.forTypeParams[A, B]

  def refined[T <: Refined[_, _]]: json.Schema[T] = macro RefinedMacro.forRefinedType[T]
}
