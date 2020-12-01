object ScalaVersions {

  val _211 = "2.11.12"
  val _212 = "2.12.12"
  val _213 = "2.13.4"

  val values: Seq[String] = Set(_211, _212, _213).toSeq

  lazy val value = sys.env.get("SCALA_VER") match {
    case Some("2.11") => _211
    case Some("2.12") => _212
    case Some("2.13") => _213
    case _            => _213
  }
}
