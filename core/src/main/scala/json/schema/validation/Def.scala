package json.schema.validation

import com.github.andyglow.json.Value

final case class Def[S, V](validation: Instance[S, V], value: V, json: Value) {

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Def[_, _]]

  override def equals(that: Any): Boolean = that match {
    case that: Def[_, _] =>
      getClass == that.getClass && validation == that.validation && json == that.json
    case _ => false
  }
}
