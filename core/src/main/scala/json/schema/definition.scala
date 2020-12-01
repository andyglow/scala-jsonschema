package json.schema

import scala.annotation.StaticAnnotation

final case class definition(label: String = "__default__") extends StaticAnnotation
