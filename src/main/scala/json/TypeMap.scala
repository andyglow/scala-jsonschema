package json

import scala.reflect.macros.Universe

trait TypeMap {
  def resolve(className: String): Option[Schema]
}

trait LowPriorityTypeMaps {
  implicit object empty extends TypeMap {
    override def resolve(className: String): Option[Schema] = None
  }
}

object TypeMap extends LowPriorityTypeMaps
