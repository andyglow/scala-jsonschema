import sbt._
import Keys._

sealed abstract class ScalaVer(val full: String)

object ScalaVer {

  case object _211 extends ScalaVer("2.11.12")

  case object _212 extends ScalaVer("2.12.18")

  case object _213 extends ScalaVer("2.13.13")

  val values: Seq[ScalaVer] = Set(_213, _212, _211).toSeq

  val default: ScalaVer = _212

  def fromEnv: Option[ScalaVer] = sys.env.get("SCALA_VER") flatMap fromString

  def fromString(full: String): Option[ScalaVer] = full match {
    case x if x startsWith "2.11" => Some(_211)
    case x if x startsWith "2.12" => Some(_212)
    case x if x startsWith "2.13" => Some(_213)
    case _                        => None
  }

  lazy val scalaV = settingKey[ScalaVer]("Current Scala Version").withRank(KeyRanks.Invisible)

  def settings = Seq(
    scalaVersion       := (ScalaVer.fromEnv getOrElse ScalaVer.default).full,
    crossScalaVersions := ScalaVer.values.map(_.full),
    scalaV             := ScalaVer.fromString(scalaVersion.value) getOrElse ScalaVer.default
  )

  def settings213 = Seq(
    scalaVersion       := _213.full,
    crossScalaVersions := Seq(_213.full),
    scalaV             := _213
  )
}
