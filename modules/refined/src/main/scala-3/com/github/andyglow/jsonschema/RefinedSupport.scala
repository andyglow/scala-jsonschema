package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.refined.RefinedMacro
import eu.timepit.refined.api.Refined

/** Scala 3 counterpart of `src/main/scala-2/.../RefinedSupport.scala`, used the same way: `import com.github.andyglow.jsonschema.RefinedSupport._`.
  *
  * `refinedJsonSchema` is an `implicit inline def` (not a `given`), so the Scala 2 wildcard import keeps bringing it into scope. It is a `Predef`, so refined types are always inlined.
  *
  * Its type parameters differ from Scala 2's `[A, B]`: implicit search binds `R` to the refined type as written. With `Predef[Refined[A, B]]`, Scala 3 would bind `B` to the dealiased predicate
  * (`Divisible[20]` becomes `Modulo[20, 0]`, `MaxSize[6]` becomes `Size[Interval.Closed[0, 6]]`), and the macro matches refined's aliases before dealiasing.
  */
object RefinedSupport {

  implicit inline def refinedJsonSchema[R <: Refined[A, B], A, B]: json.schema.Predef[R] = ${ RefinedMacro.predef[R] }

  // On Scala 3 `Refined` is an opaque type, and `T <: Refined[?, ?]` can't be written: the macro rejects a `T` that isn't a `Refined[A, B]`
  inline def refined[T]: json.Schema[T] = ${ RefinedMacro.schema[T] }}
