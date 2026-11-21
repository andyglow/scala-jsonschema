package petshop

import json.schema.discriminator

// The profiles acceptance suite's model (notes/html/profiles-runtime.html). Every bridge module encodes these with its real library and validates the output
// against the schema rendered under the library's profile (see Acceptance). Plain comments only: scaladoc would become schema descriptions.

sealed trait Animal
final case class Dog(name: String) extends Animal
case object Fish                   extends Animal
final case class Pet(name: String, age: Option[Int], animal: Animal)

// a nested family: Vehicle > Car > (Sedan, Coupe), Bike. The libraries flatten it, and so does the macro
sealed trait Vehicle
sealed trait Car                   extends Vehicle
final case class Sedan(seats: Int) extends Car
case object Coupe                  extends Car
final case class Bike(gears: Int)  extends Vehicle
final case class Garage(vehicles: List[Vehicle])

// a family of singletons only
sealed trait Color
case object Red   extends Color
case object Green extends Color
final case class Paint(color: Color)

// Option fields with and without defaults: upickle requires the key of one without a default, and drops a field equal to its default.
// Derived in the bridge modules: Scala 2 reads a Some(..) default only from a separately compiled model, with the bridge's ToValue
final case class Reading(id: String, note: Option[String], unit: Option[String] = None, scale: Option[Int] = Some(1))

// a discriminator annotation: it shapes the schema under Neutral, Legacy and spray (hand-written codecs); library profiles ignore it (R4B), as their derivation does
@discriminator("kind") sealed trait Shape
final case class Circle(r: Double) extends Shape
final case class Square(a: Double) extends Shape

// a member with an implicit json.Schema of its own (a definition + ref): library profiles still tag it (reviewer11 A3)
sealed trait Payment
final case class Card(number: String) extends Payment
final case class Cash(amount: Int)    extends Payment

// value classes as members: the libraries' derived sum codecs write them as objects, {"n": 1} (profiles09 F4: {valueField: v} under library profiles)
sealed trait Ident                   extends Any
final case class Num(n: Int)         extends AnyVal with Ident
final case class Label(text: String) extends AnyVal with Ident
final case class Badge(ident: Ident)

// members nested in the trait's companion (reviewer12 S1): their full names are petshop.Toy.Ball, petshop.Toy.Kite
sealed trait Toy
object Toy {
  final case class Ball(size: Int) extends Toy
  case object Kite                 extends Toy
}
final case class Box(toy: Toy)

object Samples {
  val pets: List[Pet]         = List(Pet("Rex", Some(3), Dog("Rex")), Pet("Nemo", None, Fish))
  val garage: Garage          = Garage(List(Sedan(4), Coupe, Bike(21)))
  val paints: List[Paint]     = List(Paint(Red), Paint(Green))
  val readings: List[Reading] = List(Reading("r1", None), Reading("r2", Some("n"), Some("F"), None))
  val shapes: List[Shape]     = List(Circle(1.5), Square(2))
  val payments: List[Payment] = List(Card("4111"), Cash(10))
  val badges: List[Badge]     = List(Badge(Num(1)), Badge(Label("x")))
  val boxes: List[Box]        = List(Box(Toy.Ball(1)), Box(Toy.Kite))
}
