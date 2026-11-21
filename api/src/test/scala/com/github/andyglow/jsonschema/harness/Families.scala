package com.github.andyglow.jsonschema.harness

import json.Schema
import json.Schema._
import json.schema.JsonSchemaProfile

/** For tree specs written against the 0.7/0.8 trees: since 0.9 the macros emit a `family` node for every sealed family. `as07(s)` replaces each one with what Neutral and Legacy make of it (the
  * 0.7 `oneof`/`enum`), leaving every other node, `option` included, as derived. A family's members are lowered by Legacy, so use it for members without `Option`s.
  */
object Families {

  def as07[T](s: Schema[T]): Schema[T] = walk(s).asInstanceOf[Schema[T]]

  // `option` nodes inside a family survive its lowering as this placeholder definition (a def's body is lowered, its node kept)
  private val Opt = "__as07.option__"

  private def walk(s: Schema[_]): Schema[_] = s match {
    case f: `family`[_]   => restore(JsonSchemaProfile.Legacy.lower(f.withMemberSchemas(m => hide(walk(m)))))
    case o: `object`[_]   => o.withFieldsUpdated { case fl => fl.copy(tpe = walk(fl.tpe)) }
    case `array`(c, u)    => `array`[Any, List](walk(c).asInstanceOf[Schema[Any]], u).withExtraFrom(s)
    case `dictionary`(v)  => `dictionary`[Any, Any, Map](walk(v).asInstanceOf[Schema[Any]]).withExtraFrom(s)
    case `oneof`(xs, df)  => `oneof`[Any](xs map walk, df).withExtraFrom(s)
    case `allof`(xs)      => `allof`[Any](xs map walk).withExtraFrom(s)
    case `not`(t)         => `not`[Any](walk(t).asInstanceOf[Schema[Any]]).withExtraFrom(s)
    case `def`(sig, t)    => `def`[Any](sig, walk(t)).withExtraFrom(s)
    case `value-class`(t) => `value-class`[Any, Any](walk(t).asInstanceOf[Schema[Any]]).withExtraFrom(s)
    case `option`(t)      => `option`[Any](walk(t).asInstanceOf[Schema[Any]]).withExtraFrom(s)
    case other            => other
  }

  private def hide(s: Schema[_]): Schema[_]    = map(s) { case `option`(t) => `def`[Any](Opt, hide(t)) }
  private def restore(s: Schema[_]): Schema[_] = map(s) { case `def`(Opt, t) => `option`[Any](restore(t).asInstanceOf[Schema[Any]]) }

  private def map(s: Schema[_])(pf: PartialFunction[Schema[_], Schema[_]]): Schema[_] = {
    def go(x: Schema[_]): Schema[_] = if (pf.isDefinedAt(x)) pf(x).withExtraFrom(x) else x match {
      case o: `object`[_]   => o.withFieldsUpdated { case fl => fl.copy(tpe = go(fl.tpe)) }
      case `array`(c, u)    => `array`[Any, List](go(c).asInstanceOf[Schema[Any]], u).withExtraFrom(x)
      case `dictionary`(v)  => `dictionary`[Any, Any, Map](go(v).asInstanceOf[Schema[Any]]).withExtraFrom(x)
      case `oneof`(xs, df)  => `oneof`[Any](xs map go, df).withExtraFrom(x)
      case `allof`(xs)      => `allof`[Any](xs map go).withExtraFrom(x)
      case `not`(t)         => `not`[Any](go(t).asInstanceOf[Schema[Any]]).withExtraFrom(x)
      case `def`(sig, t)    => `def`[Any](sig, go(t)).withExtraFrom(x)
      case `value-class`(t) => `value-class`[Any, Any](go(t).asInstanceOf[Schema[Any]]).withExtraFrom(x)
      case `option`(t)      => `option`[Any](go(t).asInstanceOf[Schema[Any]]).withExtraFrom(x)
      case f: `family`[_]   => f.withMemberSchemas(go)
      case other            => other
    }
    go(s)
  }
}
