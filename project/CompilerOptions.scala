object CompilerOptions {

  private val base = Seq("-encoding", "UTF-8", "-feature", "-unchecked", "-deprecation", "-language:existentials", "-language:higherKinds")

  private val opts212 = base ++ Seq("-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits", "-Xlint:-unused,_")

  private val opts213 = base ++ Seq("-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits", "-Xsource:2.13")

  def apply(v: ScalaVer): Seq[String] = {
    v match {
      case ScalaVer._212 => opts212
      case ScalaVer._213 => opts213
    }
  }
}
