package json.schema

import scala.annotation.StaticAnnotation

final case class discriminatorKey(label: String) extends StaticAnnotation
