package com.github.andyglow.json


trait LowPriorityArrayImplicits { this: LowPriorityPrimitiveImplicits =>
  import Value._
  import ToValue._

  implicit def ArrV[T, C[_] <: Traversable[_]](implicit
    to: ToValue[T]): ToValue[C[T]] = {

    mk { items =>
      val v = items map { v: Any => to(v.asInstanceOf[T]) }
      arr(v.toSeq)
    }
  }
}