package json.schema

sealed trait Version {

  def uri: String
}

object Version {

  final case object Raw extends Version {
    def uri = ???
  }

  final case class Draft04() extends Version {

    val uri: String = "http://json-schema.org/draft-04/schema#"
  }

  final case class Draft06(id: String) extends Version {

    val uri: String = "http://json-schema.org/draft-06/schema#"
  }

  final case class Draft07(id: String) extends Version {

    val uri: String = "http://json-schema.org/draft-07/schema#"
  }
}