package json.schema

import scala.annotation.StaticAnnotation

final case class discriminator(field: String = "type", phantom: Boolean = true) extends StaticAnnotation
