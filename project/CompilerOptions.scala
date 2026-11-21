object CompilerOptions {

  private val base = Seq("-encoding", "UTF-8", "-feature", "-unchecked", "-deprecation")

  // We build on JDK 21 but publish Java 8 bytecode (classfile 52) compiled against the Java 8 API.
  private val release2 = Seq("-release:8")
  private val release3 = Seq("-release", "8")

  private val base2 = base ++ release2 ++ Seq("-language:existentials", "-language:higherKinds")

  private val opts212 = base2 ++ Seq("-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits", "-Xlint:-unused,_")

  private val opts213 = base2 ++ Seq("-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits", "-Xsource:2.13")

  private val opts3 = base ++ release3 ++ Seq("-Wunused:imports") // same checks as -Ywarn-unused above (only imports are on)

  def apply(v: ScalaVer): Seq[String] = {
    v match {
      case ScalaVer._212 => opts212
      case ScalaVer._213 => opts213
      case ScalaVer._3   => opts3
    }
  }

  // scaladoc options; Scala 3 scaladoc doesn't know the Scala 2 ones
  def doc(v: ScalaVer): Seq[String] = v match {
    case ScalaVer._3 => Seq.empty
    case _           => Seq("-groups", "-implicits", "-no-link-warnings")
  }
}
