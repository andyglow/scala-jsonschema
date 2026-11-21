package petshop

import org.scalatest.wordspec.AnyWordSpec

// ---- user code: value classes in a family (upickle's Scala 3 derivation has no Mirror for a value class)
object UPickleValueClassCodecs {
  import _root_.upickle.default.{macroRW, ReadWriter => RW}
  implicit val numRW: RW[Num]     = macroRW
  implicit val labelRW: RW[Label] = macroRW
  implicit val identRW: RW[Ident] = macroRW
  implicit val badgeRW: RW[Badge] = macroRW
}

/** Scala 2 only: value class members. */
class UPickle2Spec extends AnyWordSpec {
  import Acceptance._

  "profiles.upickle._" should {
    "describe value class members as macroRW writes them, tagged objects" in {
      import com.github.andyglow.jsonschema.profiles.upickle._
      import UPickleValueClassCodecs._
      assertValid(
        byDraft(v => Schemas.badge.asU(v).render(), v => Schemas.badge.asU(v).render(), v => Schemas.badge.asU(v).render()),
        Samples.badges.map(_root_.upickle.default.write(_))
      )
    }
  }
}
