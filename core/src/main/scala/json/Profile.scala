package json

trait Profile

object Profile {

  // options
  trait OptionAsArray { this: Profile => } // ujson
  trait OptionIsRequired { this: Profile => } // ujson

  trait EitherAsOneOf { this: Profile => } // spray-json
  trait EitherAsLeftOrRight { this: Profile => // circe
    type Left
    type Right
  }
  object EitherAsLeftOrRight {
    type Aux[L, R] = EitherAsLeftOrRight { type Left = L; type Right = R }
  }

  implicit val defaultProfile = new Profile {
    // option: by default optional fields are exposed as non-required, non-nullable
    // either: not supported by default
  }
}