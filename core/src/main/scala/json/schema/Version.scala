package json.schema

sealed trait Version {

  def uri: String
}

object Version {

  final case class Draft04(
    title: Option[String] = None,
    description: Option[String] = None) extends Version {

    val uri: String = "http://json-schema.org/draft-04/schema#"
  }

  final case class Draft06(
    title: Option[String] = None,
    description: Option[String] = None,
    id: String) extends Version {

    val uri: String = "http://json-schema.org/draft-06/schema#"
  }

  final case class Draft07(
    title: Option[String] = None,
    description: Option[String] = None,
    id: String) extends Version {

    val uri: String = "http://json-schema.org/draft-07/schema#"
  }
}