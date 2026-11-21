package petshop

import json.{Json, Schema}

/** The model's derived schemas: library-neutral, shaped by whichever profile renders them. */
object Schemas {
  val pet: Schema[Pet]       = Json.schema[Pet]
  val garage: Schema[Garage] = Json.schema[Garage]
  val paint: Schema[Paint]   = Json.schema[Paint]
  val shape: Schema[Shape]   = Json.schema[Shape]
  val badge: Schema[Badge]   = Json.schema[Badge]

  /** `Card` derived where no implicit `Schema[Card]` is in scope. */
  val card: Schema[Card]       = Json.schema[Card]
  val payment: Schema[Payment] = WithCardSchema.payment

  /** A member of `Animal`'s family, as the macro records it. */
  def animalMember(name: String): Schema.`family`.Member = Json.schema[Animal] match {
    case f: Schema.`family`[_] => f.members.find(_.name == name) getOrElse sys.error(s"no member $name in $f")
    case other                 => sys.error(s"not a family: $other")
  }
}

/** `Payment` derived with an implicit `Schema[Card]` in scope: `Card` is a definition + `$ref`. */
object WithCardSchema {
  implicit val cardSchema: Schema[Card] = Schemas.card
  val payment: Schema[Payment]          = Json.schema[Payment]
}
