package petshop

import json.{Json, Schema}

// Scala 3 enums in the acceptance suite: one of singleton cases only, one mixing a case with parameters and one without
enum Suit {
  case Hearts, Spades
}
enum Mark {
  case Dot
  case Line(length: Int)
}
final case class Card3(suit: Suit, mark: Mark)

object Samples3 {
  val cards: List[Card3] = List(Card3(Suit.Hearts, Mark.Dot), Card3(Suit.Spades, Mark.Line(3)))
}

object Schemas3 {
  val card: Schema[Card3] = Json.schema[Card3]
}
