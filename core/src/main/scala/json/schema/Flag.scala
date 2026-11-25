package json.schema

/** Compile-time switches the derivation reads from one implicit `Flag` in scope. Several combine in one value, e.g. `implicit val flags: Flag with Flag.EnumsAsOneOf with Flag.SemiAuto = null`: two
  * separate implicit flags are ambiguous: a compile error on Scala 2, and on Scala 3 neither applies.
  */
sealed trait Flag
object Flag {

  trait EnumsAsOneOf extends Flag

  /** Semi-auto derivation now (issue #542): a nested case class or sealed family without an implicit `Schema` or `Predef` is a compile error, instead of the deprecation warning of 0.10. Import
    * `json.Json.auto._` to derive nested types.
    */
  trait SemiAuto extends Flag
}
