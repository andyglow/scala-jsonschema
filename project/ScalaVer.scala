sealed abstract class ScalaVer(val full: String)
final object ScalaVer {
  final case object _211 extends ScalaVer("2.11.12")
  final case object _212 extends ScalaVer("2.12.12")
  final case object _213 extends ScalaVer("2.13.4")

  val values: Seq[ScalaVer] = Set(_211, _212, _213).toSeq

  lazy val value: ScalaVer = sys.env.get("SCALA_VER") match {
    case Some("2.11") => _211
    case Some("2.12") => _212
    case Some("2.13") => _213
    case _            => _213
  }
}
