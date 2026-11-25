# Semi-auto derivation by default, deep derivation opt-in via `Json.auto`  (issue #542)
Status: approved (independent reviewer, 2026-11-24, 4 rounds; owner decisions 2026-11-24); implemented in PR #543; Expectations reviewed 2026-11-25, open problems P1–P8 await the owner (owner direction 2026-11-24: "design it similarly to circe/pureconfig")

## Problem
`Json.schema[T]` derives every nested type that has no implicit `Schema` or `Predef`, from its structure, at any depth (Scala 2 `Macroses.scala` `genTree`;
Scala 3 `Resolve.scala` `resolve`, then `structural`). So deep ("auto") derivation is the default and needs no import. `import json.Json.auto._` adds little: it
makes `implicitly[Predef[X]]` work outside a derivation.

```scala
import cats.data.NonEmptyList, json.Json, com.github.andyglow.jsonschema.CatsSupport._
case class P(a: Int)
case class Holder(xs: NonEmptyList[P])

Json.schema[Holder]   // 0.9.0, 2.13 and 3.3: xs = {"type":"object","properties":{"head":{..P..},"tail":{"type":"array",..}},"required":["head","tail"]}
```

`NonEmptyList[P]` has an instance (`nelSchemaFromPredef`) only when `P` has one. When `P` has none, the macro does what it does for any nested type: it derives
`NonEmptyList` as a case class. The macro can't tell "a type the user wants derived" from "a type that has an instance, but whose element has none".

The two versions also differ under `import Json.auto._`, measured on master `5b24a80b`:
- **Scala 2:** `NonEmptyList[P]` is an array. The search finds `nelSchemaFromPredef(auto.derived[P])`, because `UImplicits.isAutoDerived` rejects auto only at the
  root (`UImplicits.scala:146-157`).
- **Scala 3:** it stays `{head, tail}`. `AutoGuard` declines every `auto.derived` during a derivation's searches (`AutoGuard.scala:36-37`, `Implicits.scala:69,123`).

## Why it is a problem
- **Silent wrong schemas.** A type the user never meant to derive (from a library, or one that has an instance under conditions) is derived from its internals.
  Validators then reject real data. #482 and #171 are instances of this.
- **The common pattern is the other one.** circe and pureconfig users expect members to need instances, with deep derivation as an explicit import. We do the
  opposite, implicitly.
- **Scala 2 and 3 disagree** once auto is imported.

## How circe and pureconfig do it
Studied: circe core and generic for Scala 3, circe-derivation (Scala 2, pure macros), pureconfig core for Scala 3. The shapeless-based modules were left out.
- **Semi-auto derives one type; members need instances.**
  - circe-derivation (Scala 2) looks up each member with `c.inferImplicitValue(Encoder[F])`, and on failure:
    `c.abort(c.enclosingPosition, s"Could not find $tc instance for $tpe")`.
  - circe (Scala 3): `summonFrom { case e: Encoder[A] => e; case _: Mirror.Of[A] => error("Failed to find an instance of Encoder[" + typeName[A] + "]") }`.
  - Sealed families differ between the two:
    - circe for Scala 3 derives the cases of a sealed family as part of it (`derivingForSum`).
    - circe-derivation asks for an instance of every leaf case, after flattening sub-families.
- **Auto is an explicit import that returns `Exported[TC[A]]`.**
  ```scala
  case class Exported[+T](instance: T) extends AnyVal                     // circe core; pureconfig core has the same
  object auto { implicit inline def deriveEncoder[A](using inline A: Mirror.Of[A]): Exported[Encoder.AsObject[A]] = Exported(Encoder.AsObject.derived[A]) }
  private[circe] trait LowPriorityEncoders {                              // the lowest-priority parent of object Encoder
    implicit final def importedEncoder[A](implicit exported: Exported[Encoder[A]]): Encoder[A] = exported.instance
  }
  ```
  An `Exported[Encoder[P]]` is not an `Encoder[P]`, so the import never competes with real instances. It is reached only through the lowest-priority bridge.
  So the user's instances, in a companion or local, always win, and on Scala 3 the import can't shadow implicit-scope instances. Container instances
  (`encodeNonEmptyList[A](implicit e: Encoder[A])`) get their element through the bridge.

One difference matters for us: circe's instances are functions, so a recursive auto instance is fine at runtime (or handled by shapeless `Lazy`). Ours are data
built at compile time, so a nested auto must never run a derivation of its own (see "Auto inside a derivation").

## The model (decision)
| entry point | derives | a nested type without an instance, of a gated kind |
|---|---|---|
| `Json.schema[T]`, `Json.objectSchema[T]`, `derives json.Schema`, `DerivedSchema[T]` (semi-auto) | `T` itself | compile error (after the rollout phase below) |
| any of those with `import json.Json.auto._`, and `Json.auto.derived[T]` (auto) | `T` and every nested type, in one expansion, as today | derived in place, as today |

**Gated kinds:** the types a user writes or a library defines, which semi-auto must not derive behind the user's back:
- non-tuple case classes;
- sealed families with a case class case (other than `Either`).

Value classes, all-case-object families and Scala 3 `enum`s without a parameterized case stay native, as derived today (open question 3 has the alternative of
gating them too).

**Not gated, native as today:**
- the types handled by the macros themselves: `Option`, `java.util.Optional`, collections and arrays, maps (including map keys that are enums or value classes,
  derived natively for the key pattern: `UDictionaries.scala:59-71`, `Collections.scala:18-20`), tuples, `Either`, Java enums, `scala.Enumeration#Value`;
- anything with an implicit `Predef` or `Schema` (built-ins such as `String` and java.time, the Predef constructors `listS`/`setS`/`vectorS`/`arrayS`/`dictS`, module
  instances such as cats).

The gate is defined by kind, not by family. On Scala 2, tuples go through `CaseClass`, `Either` through `SumType`, and Java enums through `Enum`
(`Macroses.scala:146-151`, `USumTypes.scala:54,117`, `UEnums.scala:252`). On Scala 3 they go through `deriveProduct`, `deriveSum` and `deriveEnum`
(`Products.scala:142`, `Sums.scala:111`, `Enums.scala:86`).

**What "`T` itself" covers:**
- the root, of any kind;
- for a sealed family, its cases: case classes, case objects, nested sealed sub-families, and on Scala 2 value-class members (`USumTypes.scala:75-80`). They are
  derived as part of it, as in circe;
- everything else is resolved: fields, the underlying type of a value class, the elements of built-ins.

**Resolving a type** means the core rule, unchanged:
1. Recursion to a type being derived gives a `$ref`.
2. An implicit `Schema` gives a definition plus a `$ref`.
3. An implicit `Predef` is inlined.
4. A native type is handled by the macro, and its parts are resolved.
5. A gated kind:
   - **semi-auto:** the error;
   - **auto:** derived in place.

**The error** keeps the current prefix (`Errors.scala:28`, pinned by `ErrorMessagesSpec`):
```
schema for com.example.Address is not supported (in com.example.Person): no implicit json.Schema or json.schema.Predef for it.
Define one: `implicit val addressSchema: json.Schema[Address] = json.Json.schema[Address]` (a definition + $ref; for a generic type
`implicit def boxSchema[T](implicit p: json.schema.Predef[T]): json.Schema[Box[T]] = json.Json.schema[Box[T]]`),
or derive nested types automatically: `import json.Json.auto._` (Scala 3: `import json.Json.auto.given`)
```

For #482, `Holder(xs: NonEmptyList[P])` fails naming `NonEmptyList[P]` (in `Holder`). It has no instance, because the search for `Predef[NonEmptyList[P]]`
failed on `P`, and it is a gated kind (a case class). Neither version names `P` today: the design expected Scala 3 to add the search's reason, but it doesn't
(Expectations S18, open problem P3). With auto, `xs` is an array. Nothing in the macros knows about cats.

## Expectations
What a user sees in 0.10.0, scenario by scenario, on both Scala 2 and 3 unless a remark says otherwise. Every scenario was compiled and run on 2.13.18 and
3.3.9, with the phase-1 warning visible (2026-11-25, branch `gap/semi-auto-derivation`). 2.12 ran the full test suite, but not these scenarios.
Scenarios marked **known problem** behave as described today, and are listed under "Open problems found by the Expectations review".

Terms used in the remarks:
- **inlined:** the nested type's schema is written in place, as in 0.9.
- **a definition + `$ref`:** the type goes to `definitions` (`$defs`), and the field refers to it.
- **the warning:** the phase-1 deprecation warning, `[json-schema-semi-auto] schema for X is derived implicitly (in Y); this becomes an error in a future
  release. Define one: ... or derive nested types automatically: import json.Json.auto._`. The type is still derived in place, and the output is the same as in
  0.9.
- **the error:** the same text as a compile error, `schema for X is not supported (in Y): no implicit json.Schema or json.schema.Predef for it. Define one: ...`.
  It is what the warning becomes in the next breaking release (phase 2), and what you get now with `Flag.SemiAuto`.

The snippets share this model, and `import json._, json.schema._`:
```scala
case class Address(city: String)
case class Person(name: String, address: Address)
```

### Semi-auto, the default

**S1. A type whose fields are all built-ins.** Positive.
```scala
case class Flat(name: String, age: Int)
Json.schema[Flat]
```
- No warning, and the output is the same as 0.9.
- Built-ins (`String`, numbers, `java.time`, `UUID`, ...) are `Predef`s, so they are always found.

**S2. A nested case class without an instance.** Negative.
```scala
Json.schema[Person]
```
- 0.10: compiles with the warning for `Address` (in `Person`). `address` is inlined, as in 0.9.
- With `Flag.SemiAuto`, or in phase 2: the error for `Address`. It suggests `implicit val addressSchema: json.Schema[Address] = json.Json.schema[Address]` or
  `import json.Json.auto._`.
- `Json.objectSchema[Person]()` behaves the same.

**S3. Fix: an implicit `Schema` in the companion.** Positive.
```scala
case class City(name: String)
object City { implicit val citySchema: Schema[City] = Json.schema[City] }
case class Trip(from: City, to: City)

Json.schema[Trip]
```
- No warning.
- `City` is a definition + `$ref`, shared by `from` and `to`.
- The output differs from 0.9, where `City` was inlined twice.
- `Json.schema[City]` inside `City`'s own instance doesn't find that instance directly. It can find it through a container, `listS(citySchema)`, which a
  `Schema` handles with `lazy-ref`, but a `Predef` doesn't (S4).

**S4. Fix: an implicit `Predef`, to keep the 0.9 output.** Positive.
```scala
implicit val addressPredef: Predef[Address] = Predef(Json.schema[Address])
Json.schema[Person]
```
- No warning.
- `address` is inlined, so the output is the same as 0.9.
- This is the migration for users who want no definitions, but not for a recursive type (S16). **Known problem P4:**
  - `implicit lazy val treeP: Predef[Tree] = Predef(Json.schema[Tree])` throws `StackOverflowError` when it is first used.
  - `implicit val` gives `kids: array(null)`.
  - The derivation of `Tree` finds `treeP` itself, through `listS(treeP)`. It was the same in 0.9.
  - A recursive type needs a `Schema` (S5) or auto.

**S5. Fix: a local instance next to the use.** Positive.
```scala
implicit val addressSchema: Schema[Address] = Json.schema[Address]
Json.schema[Person]
```
- No warning; `address` is a definition + `$ref`.
- The local `val` doesn't find itself while it is being initialized.

**S6. A chain of nested types without instances.** Negative.
```scala
case class Deep(person: Person)
Json.schema[Deep]
```
- Error (`Flag.SemiAuto`): names `Person` (in `Deep`), the outermost type without an instance.
- Once `Person` has an instance, deriving `Person` reports `Address`.
- Warning (0.10):
  - Scala 2 warns for `Person` only.
  - Scala 3 warns for `Person` and for `Address` inside it, in one warning with a line per type.

**S7. Nested inside containers.** Negative.
```scala
case class InOption(address: Option[Address])
case class InList(addresses: List[Address])
case class InMap(addresses: Map[String, Address])
case class InTuple(pair: (Int, Address))
case class InEither(either: Either[String, Address])
```
- Each gets the warning, or the error, for `Address`.
- The containers themselves (`Option`, collections, maps, tuples, `Either`) are native and never need an instance. Their elements are resolved by the same rule.

**S8. Native kinds: no instance needed.** Positive.
```scala
sealed trait Color; case object Red extends Color; case object Blue extends Color
case class UserId(value: String) extends AnyVal
case class Natives(
  color: Color,                 // all-case-object sealed family: an enum
  id: UserId,                   // value class
  day: java.time.DayOfWeek,     // Java enum
  keyed: Map[Color, Int],       // enum map key: patternProperties ^(?:Red|Blue)$
  t: (Int, String),
  e: Either[String, Int],
  l: List[Int],
  o: Option[Double]
)
Json.schema[Natives]
```
- No warning, and the output is the same as 0.9.
- `scala.Enumeration` values and Scala 3 enums without a parameterized case (S15) are native too.

**S9. A value class that wraps a case class.** Negative.
```scala
case class Wrap(a: Address) extends AnyVal
case class HasWrap(w: Wrap)
Json.schema[HasWrap]
```
- The value class is native, but its underlying type is resolved.
- So: the warning, or the error, for `Address` (in `HasWrap :: Wrap`).

**S10. A generic case class.** Negative, then positive.
```scala
case class Pair[A](a: A, b: A)
case class HasPair(p: Pair[Int])
Json.schema[HasPair]                    // the warning / the error for Pair[scala.Int]

implicit def pairSchema[A](implicit p: Predef[A]): Schema[Pair[A]] = Json.schema[Pair[A]]
Json.schema[HasPair]                    // ok
```
- The error suggests the generic `implicit def` form.
- With it, `p` is a definition + `$ref`, named for the instantiated type, `...Pair[scala.Int]` (#297).
- **Known problem P1:** the hint takes a `Predef[A]`. So `Pair[Address]` with an `implicit val addressSchema: Schema[Address]` still fails, and the error repeats
  the same hint. A `Schema` is not a `Predef`.

**S11. A type alias.** Negative.
```scala
type Addr = Address
case class HasAlias(a: Addr)
Json.schema[HasAlias]
```
- The warning, or the error, names `Address`, the dealiased type. An instance for `Address` fixes it.

**S12. A class that isn't a case class.** Negative, unchanged from 0.9.
```scala
class Plain(val x: Int)
case class HasPlain(p: Plain)
Json.schema[HasPlain]
```
- Error, as in 0.9: `schema for Plain is not supported (in HasPlain). Provide a json.schema.Predef or json.Schema for it`.
- `import Json.auto._` doesn't change it: auto derives only what the macros can derive.

**S13. A sealed family at the root.** Positive, with one negative inside.
```scala
case class Point(x: Int, y: Int)
sealed trait Shape
object Shape {
  case class Circle(r: Double, center: Point)         extends Shape
  case class Group(items: List[Shape], main: Circle) extends Shape
  sealed trait Poly                                   extends Shape
  case class Square(side: Double)                     extends Poly
  case object Empty                                   extends Shape
}
Json.schema[Shape]
```
- The family's cases are part of `T` itself, as in circe for Scala 3 (circe-derivation asks for an instance per case): case classes, case objects, and the
  sub-family `Poly` with its cases. None needs an instance.
- A case used as a field type of another case (`Group.main: Circle`) is part of the family too. This goes beyond circe for Scala 3, which asks for an instance
  of a product's fields (`derivingForSum = false`). It is exempt because it is part of the type being derived, and the exemption holds at any depth.
- A case with an instance of its own (`object Circle { implicit val s: Schema[Circle] = ... }`) is used, and becomes a definition + `$ref` inside the family.
- Recursion to the root (`items: List[Shape]`) is a `$ref`. `Shape` is the only definition.
- The fields of the cases are resolved: `center: Point` gets the warning, or the error, for `Point` (in `Shape :: Circle`).

**S14. A sealed family as a field.** Negative.
```scala
sealed trait Status; case object Active extends Status; case class Banned(reason: String) extends Status
case class Account(color: Color, status: Status)   // Color from S8
case class Drawing(shape: Shape)                   // Shape from S13
case class Painting(poly: Shape.Poly)
```
- `Account`: the warning, or the error, for `Status`. A family with a case-class case is gated. `color` is not: an all-case-object family is an enum.
- `Drawing`: the warning, or the error, for `Shape`.
- `Painting`: for `Shape.Poly`. A sub-family is part of `Shape` only while `Shape` is being derived.
- The same goes for a case used outside its family, `case class HasCircle(c: Shape.Circle)`: the warning, or the error, for `Circle`.

**S15. Scala 3 enums.** Scala 3 only.
```scala
enum Level   { case Low, High }
enum Payment { case Cash; case Card(number: String) }
case class Order(level: Level, payment: Payment)
Json.schema[Order]
```
- `level` is native, with no warning.
- `payment` gets the warning, or the error, for `Payment`: a parameterized case makes it a sealed family with a case-class case.

**S16. Recursion to the root.** Positive.
```scala
case class Tree(value: Int, kids: List[Tree], parent: Option[Tree])
Json.schema[Tree]
```
- No warning: a type on the recursion stack is never gated.
- `Tree` is a definition, and `kids` and `parent` are `$ref`s, as in 0.9.
- This goes beyond circe, where `deriveEncoder[Tree]` works only if the result is itself the implicit instance that `List[Tree]` finds.

**S17. The root has its own instance in scope.** Positive, a corner case.
```scala
implicit val addressSchema: Schema[Address] = Json.schema[Address]
Json.schema[Address]
```
- The root's search finds the instance, so the result is it: a definition of `Address`, not a plain object.
- circe's `deriveEncoder` never returns an existing instance for the root. Here this is behaviour from before #542, kept as it was.

**S18. A container instance whose element has no instance (cats `NonEmptyList`, any user `Predef` constructor).** Negative.
```scala
case class Box[A](a: A)
object Box { implicit def boxPredef[A](implicit p: Predef[A]): Predef[Box[A]] = ??? /* an array of A */ }
case class Boxed(box: Box[Address])
Json.schema[Boxed]
```
- `boxPredef` needs a `Predef[Address]`, which doesn't exist, so `Box[Address]` has no instance either. `Box` is a case class, so it is gated.
- 0.10:
  - the warning for `Box[Address]` (Scala 3 also for `Address`);
  - `box` is still derived from `Box`'s fields, `{a: {city}}`, as in 0.9. This is #482, now with a warning.
- The error (`Flag.SemiAuto`) names `Box[Address]` (in `Boxed`), on both versions, and not `Address`. **Known problem P3:**
  - It suggests `implicit val boxSchema: json.Schema[Box[Address]] = json.Json.schema[Box[Address]]`, or the generic form.
  - Both derive `Box` from its fields, the wrong schema of #482, and silence the warning.
- The fixes that work: an implicit `Predef[Address]` (S4), or auto (A5).
- An implicit `Schema[Address]` (S3) does **not** fix it: `boxPredef` asks for a `Predef`. **Known problem P1.** Under auto it does work, because the macro
  fills the element itself, and looks for a `Schema` first.
- PR #488 has the same tests for every cats type.

**S19. Scala 3 `derives`.** Scala 3 only.
```scala
case class Address(city: String) derives Schema
case class Person(name: String, address: Address) derives Schema
case class Street(n: String)
case class Home(street: Street) derives Schema
```
- `Person`: `address` is a definition + `$ref`, from `Address`'s derived given. No warning.
- `Home`: the warning for `Street` (in `Home`), at the `derives` clause. `derives` is semi-auto like `Json.schema`.
- **Known problem P5, a generic class:** `case class GP[A](a: A) derives Schema` gets a given that asks for `Schema[A]`; the compiler generates it so.
  - So `GP[Int]` has no instance, with or without the flag, because `Int` has only a `Predef`.
  - The error then says "no implicit json.Schema", although the `derives` given exists.
  - For a parameter that has a `Schema`, the given unwraps it and inlines it.
  - Same as 0.9. For a generic type, use an `implicit def` with `Json.schema` (S10).

**S20. `DerivedSchema` (module `derived`).** Negative, a corner case.
```scala
implicitly[DerivedSchema[Person]]
```
- 0.10: compiles with **no** warning. `DerivedSchema` always expands inside an implicit search, where warnings are dropped.
- The output is the same as 0.9.
- With `Flag.SemiAuto`, or in phase 2, the error is the compiler's, not ours:
  - Scala 2: `could not find implicit value for parameter e: DerivedSchema[Person]`.
  - Scala 3: `No given instance of type DerivedSchema[Person] ... But macro expansion was stopped`.
- `Json.schema[Person]` at the same place shows the real reason.

**S21. Mutual recursion through instances.** Positive (0.9: a `null` inside the schema, or a deadlock on Scala 3).
```scala
case class A(b: Option[B]); case class B(a: Option[A])
object Schemas {
  implicit val aS: Schema[A] = Json.schema[A]
  implicit val bS: Schema[B] = Json.schema[B]
}
```
- Both render with two definitions, `A` and `B`, each referring to the other.
- In the Scala value, the reference that closes the cycle is a `lazy-ref` (see "New entities").
- `implicit lazy val`, `given`, `implicit def` and Scala 3 `A derives Schema` / `B derives Schema` work the same.
- Equality, `hashCode` and `toString` are finite.

**S22. An instance used before it is initialized.** Positive (0.9: `def(sig, null)`, which fails when rendered).
```scala
object Schemas {
  implicit val personS: Schema[Person]   = Json.schema[Person]    // refers to addressS, still null here
  implicit val addressS: Schema[Address] = Json.schema[Address]
}
```
- `personS.address` is `lazy-ref(Address)`.
- It renders as a definition + `$ref`, the same as with the vals in the other order.

### Flags and the rollout

**F1. Opt into phase 2 now.**
```scala
implicit val jsonSchemaFlags: Flag with Flag.SemiAuto = null
```
- Every warning above becomes the error, in the scope of the implicit.

**F2. Several flags: one value.** Positive.
```scala
implicit val jsonSchemaFlags: Flag with Flag.EnumsAsOneOf with Flag.SemiAuto = null
```
- Both flags apply.

**F3. Several flags: separate vals.** Negative, and the versions differ.
```scala
implicit val f1: Flag.SemiAuto     = null
implicit val f2: Flag.EnumsAsOneOf = null
Json.schema[Person]
```
- Scala 2: a compile error, `ambiguous implicit values: both value f2 ... and value f1 ... match expected type json.schema.Flag`.
- Scala 3: compiles, and **neither** flag applies; `Person` gets the warning.
- Use F2.

**F4. Keep phase 1 silent, for example under `-Werror`.**
```scala
// build.sbt
scalacOptions += "-Wconf:msg=json-schema-semi-auto:s"
```
- The warning has no category (macros can't set one), so it is matched by its prefix.
- With `-Werror` and without the silencer, every warning above fails the build at once.

### Auto, opt-in

**A1. Zero-change migration.** Positive.
```scala
import json.Json.auto._          // both versions; on Scala 3 `import json.Json.auto.given` works too
Json.schema[Person]
```
- No warning; every nested type is derived in place.
- Every scenario above without an explicit instance compiles, with the 0.9 output. Exceptions: S12 still fails, and container instances change on Scala 3 (A5).
- Scenarios with an explicit instance (S3, S5, S10, S17, S19, S21, S22) keep the output they have without the import.

**A2. The import with nothing to derive.** Positive, a corner case.
```scala
import json.Json.auto._
Json.schema[Int]
```
- No unused-import warning, with `-Wunused:imports` / `-Ywarn-unused:imports` and `-Werror`.

**A3. Your instances beat auto.** Positive.
```scala
case class Money(cents: Long)
object Money { implicit val moneyPredef: Predef[Money] = Predef(Schema.`string`[Money]) }
case class Price(amount: Money, tax: Address)

import json.Json.auto._
Json.schema[Price]
```
- `amount` is the companion's `string`; `tax` is derived by auto.
- The same holds for these instances:
  - built-ins;
  - a local instance;
  - an imported instance;
  - an instance inherited by the companion from a trait;
  - a generic companion instance;
  - an implicit `Schema`, which gives a definition + `$ref`;
  - Scala 3 givens and `derives` givens, also on 3.6+.
- Auto is an `Exported`, reached only through the lowest-priority bridge.

**A4. Recursion under auto.** Positive.
```scala
case class Node(name: String, children: List[Node])
case class Forest(trees: List[Node])
import json.Json.auto._
Json.schema[Forest]
```
- `Node` is a definition, and `children` refers to it, as in 0.9.
- The same through `Map`, `Array`, `Option`, and container instances (A5).

**A5. A container instance whose element is derived (the #482 fix).** Positive; on Scala 3 the output changes.
```scala
import json.Json.auto._
Json.schema[Boxed]                                    // Boxed from S18
case class BNode(name: String, children: Box[BNode])
Json.schema[BNode]
```
- `box` is `boxPredef`'s array of the derived `Address`, never `Box`'s fields.
- `BNode.children` is an array of `$ref`s to `BNode`.
- Scala 2 gave the same in 0.9. Scala 3 gave `{a: ...}` in 0.9 (#482), so the output changes there.
- cats: `NonEmptyList[P]` etc. become arrays with `minItems: 1` on both versions (PR #488).

**A6. Built-in collections under auto.** Positive.
```scala
import json.Json.auto._
Json.schema[Map[Int, Address]]
```
- The output is the same as without the import, `patternProperties` included.
- The same holds for `List`, `Set`, `Vector`, `Array` and `Map[String, _]`. The macro keeps its native handling for core's own collection instances.

**A7. A generic implicit `Schema` whose element comes from auto.** Positive.
```scala
case class Wrapper[A](data: A)
implicit def wrapperSchema[A](implicit p: Predef[A]): Schema[Wrapper[A]] = Json.schema[Wrapper[A]]
case class HasWrapper(w: Wrapper[Address])

import json.Json.auto._
Json.schema[HasWrapper]
```
- `w` is a definition named `...Wrapper[...Address]`, and `data` is the derived `Address`, inlined.

**A8. Auto outside a derivation.** Positive.
```scala
import json.Json.auto._
implicitly[Predef[Person]].schema               // through the bridge
implicitly[Exported[Predef[Person]]].instance
Json.auto.derived[Person].instance.schema       // explicit call: an Exported[Predef[Person]] now (0.9: a Predef[Person])
```
- All three are `Person` derived deeply, the same as `Json.schema[Person]` with the import.
- Source change: an explicit `Json.auto.derived[T]` needs `.instance`.

**A9. A type auto can't derive.** Negative.
```scala
import json.Json.auto._
Json.schema[HasPlain]                      // HasPlain from S12
implicitly[Predef[HasPlain]]
```
- `Json.schema`: the error names `Plain` (in `HasPlain`), on both versions. The same holds inside `List` and inside a container instance.
- `implicitly`:
  - Scala 3: the same error.
  - Scala 2: only `could not find implicit value for parameter e: json.schema.Predef[HasPlain]`, without the reason. This is the open problem below.

**A10. `DerivedSchema` under auto.** Positive.
```scala
import json.Json.auto._
implicitly[DerivedSchema[Person]].schema
```
- `address` is inlined, as in 0.9.
- On Scala 3, container instances get their element from auto too (A5).

**A11. Auto gives a `Predef`, never a `Schema`.** A corner case.
```scala
case class MyList[T](items: List[T])
object MyList { implicit def myListPredef[T](implicit s: Schema[T]): Predef[MyList[T]] = ??? }
case class HasMyList(xs: MyList[Address])

import json.Json.auto._
implicitly[Schema[Person]]       // not found
Json.schema[HasMyList]           // myListPredef doesn't apply: MyList is derived from its fields
```
- An instance constructor that asks for a `Schema` of its element never gets one from auto.
- cats has a `Predef` variant of each constructor, so cats is not affected.
- In circe, auto answers whatever type class is asked for. Here, a derived type has no definition to refer to, so auto gives the inlined kind.

## New entities
What #542 adds, why each piece is needed, and what it does. The public ones are user-facing API; the internal ones are referenced from macro output and
are marked INTERNAL API in their scaladoc.

| entity | where | visibility |
|---|---|---|
| `Exported[+T]` | core, `json.schema` | public |
| `Predef.importedPredef` | core, `LowPriorityPredefs` | public (implicit) |
| `Json.auto.derived[T]: Exported[Predef[T]]` | api | public (changed result type) |
| `Flag.SemiAuto` | core, `json.schema.Flag` | public |
| the `[json-schema-semi-auto]` warning | macros | public text (a stable prefix) |
| `` Schema.`lazy-ref` `` | core, `json.Schema` | public node; constructed only by the library |
| `Schema.building`, `` `def`.adaptOrLazy ``, `Schema.materialized` | core, `json.Schema` | internal API, public for macro output |
| `AutoHole.hole[T]` | core, `com.github.andyglow.jsonschema.internal` | internal |
| `AutoEnabled`, `Json.auto.jsonSchemaAutoEnabled` | core / api | internal |
| `AutoSearch` (Scala 2), `AutoGuard.isSearching` (Scala 3, existed) | macros / core | internal, compile time only |

**`Exported[+T]`** (`final case class Exported[+T](instance: T) extends AnyVal`).
- Why: auto must never compete with a real instance.
  - If `auto.derived[T]` were a `Predef[T]`, as in 0.9, it would be one more `Predef[T]` candidate.
  - On Scala 3, a lexically imported candidate is preferred over the companion's, so auto would shadow `object Money { implicit val ... }`.
  - On Scala 2, 0.9 needed a re-search to undo the shadowing (option D).
- Role: an `Exported[Predef[T]]` is not a `Predef[T]`, so no search for a `Predef` sees it directly. The same type as circe's and pureconfig's.

**`Predef.importedPredef`** (`implicit def importedPredef[T](implicit e: Exported[Predef[T]]): Predef[T] = e.instance`).
- Why: something has to turn the `Exported` back into a `Predef`, at a priority below every real instance.
- Role:
  - It sits in `LowPriorityPredefs`, the lowest-priority parent of `object Predef`. It is the only way auto reaches a search, so a user's or a library's
    instance wins (A3).
  - It is also how container instances get their element: `boxPredef(importedPredef(auto.derived[Address]))` (A5). No cats knowledge is needed in the macros.
  - circe's `importedEncoder`.

**`Json.auto.derived[T]`, now `Exported[Predef[T]]`.**
- Why: the result type has to be `Exported` (above).
- Role:
  - Called by user code, outside a derivation, it derives `T` deeply in one expansion (A8).
  - Inside a derivation's implicit search, it gives a hole instead (next entry).

**`AutoHole.hole[T]`** (internal, `def hole[T]: Predef[T]`, throws if evaluated).
- Why: through the bridge, auto answers every `Predef[X]` the derivation searches for, also the element of `List[Node]` inside `Node`. If that nested
  `auto.derived[Node]` ran a derivation of its own:
  - it would loop on recursive types (the 0.7 StackOverflow);
  - it would cost time exponential in the depth of the model;
  - its schema would come from a second expansion with its own recursion stack, so a recursive `X` couldn't become a `$ref` to the outer definition.
- Role: while a derivation searches, `auto.derived[X]` expands to `Exported(AutoHole.hole[X])`, which means "auto is available for `X` here".
  - A hole at the root of the found tree means: derive `X` here, in the current expansion.
  - A hole inside a found instance (`boxPredef(importedPredef(hole[X]))`) becomes a child of that node in the macro's IR. `X` is resolved in the same context,
    so recursion gives `$ref`s, and the hole is replaced with that schema when the tree is emitted.
  - For core's own `listS` / `setS` / `vectorS` / `arrayS` / `dictS` with a hole, the native collection node is used, to keep the output (A6).
  - After emission, the macro checks that no hole is left and aborts if one is. Evaluating one at runtime would be a library bug, hence the exception.

**`AutoSearch` (Scala 2) / `AutoGuard.isSearching` (Scala 3).** Internal, compile time only.
- Why: `auto.derived[X]` has to know whether it is being expanded inside a derivation's search (give a hole) or by user code (derive).
  - Scala 2 expands a nested implicit macro during the outer `c.inferImplicitValue` (spike a).
  - `c.openMacros` can't tell: it also holds the derivation during its own silent `c.typecheck` and the `c.eval` of default values.
- Role: a `ThreadLocal` counter, raised around every implicit search a derivation runs.
  - `ThreadLocal`, because sbt runs several compilers in one JVM.
  - Scala 3's `AutoGuard` existed and used to make auto decline; it now gives the hole.
  - Scala 3's `DerivedSchema` runs in typer and gets the unexpanded `auto.derived[X]` call back; that is treated as a hole too.
  - `AutoGuard.Allowed` (Scala 3) no longer does anything. It was kept so that `auto.derived`'s signature stays the same, but the result type changed anyway,
    so it can go (open problem P7).

**`AutoEnabled` and `Json.auto.jsonSchemaAutoEnabled`** (internal marker).
- Why: when the root's hole is filled from the current expansion, the found tree is discarded, so nothing in the output refers to the import. The compilers would
  then report `import json.Json.auto._` as unused, and break `-Werror` builds (spike d).
- Role:
  - `auto` carries an `implicit val jsonSchemaAutoEnabled: AutoEnabled`. Both `import auto._` and Scala 3's `import auto.given` bring it.
  - Every expansion where that name resolves to the marker splices a reference to it, so the import counts as used (A2).
  - "Every expansion" because Scala 2 counts only an import whose implicit the macro actually selected: `Json.schema[Int]` with the import would otherwise warn.
  - It is spliced only after a silent typecheck shows that the name means the marker. An import of `auto.derived` alone, or a local of that name, gets
    nothing, and no error.

**`Flag.SemiAuto`.**
- Why: phase 1 only warns. Users who want the phase-2 behaviour, the error, need a way to opt in now.
- Role:
  - A marker trait read by the same single `Flag` search as `Flag.EnumsAsOneOf`.
  - It combines with other flags in one value (F2); separate vals don't (F3).
  - In phase 2 it becomes a deprecated no-op.

**The `[json-schema-semi-auto]` prefix.**
- Why: a macro warning has no category, so `-Wconf:cat=...` can't match it.
- Role: a stable prefix, matched by `-Wconf:msg=json-schema-semi-auto:s` (F4). Test scopes of this build use it, so that the existing suites, which rely on deep
  derivation, stay as they are.

**`` Schema.`lazy-ref`[T] ``** (a new node: `sig` plus a by-name body).
- Why: under semi-auto, a model is a set of instances that refer to each other, and a cycle among them can't be built strictly.
  - `implicit val aS = Json.schema[A]` reads `bS`, which reads `aS` while it is still `null`.
  - With Scala 3 `derives` givens, which are lazy, the same cycle deadlocks.
  - A val read before it is initialized gives `null` too (S22).
  - In 0.9 all of these gave `def(sig, null)` or a deadlock (S21).
- Role: a reference to the instance of `sig` that isn't available yet.
  - Every existing traversal (`equals`, `hashCode`, `toString`, the definition passes) sees it as a leaf, so the tree stays finite.
  - Equality is by `sig`.
  - It appears only where a cycle or a `null` would have been. Everywhere else an implicit `Schema` is still the `def` node of 0.9.

**`Schema.building(sig)`, `` `def`.adaptOrLazy(x, sig) ``, `Schema.materialized(x)`** (internal API, public for macro output).
- Why: the library needs to know, at runtime, that an implicit `Schema` is part of a cycle, whatever its shape (`val`, `lazy val`, `given`, `def`), and then
  turn the lazy references into ordinary definitions before rendering. A `Predef` in a cycle is not covered (S4, P4): it is inlined, so it has no definition to
  refer to.
- Role:
  - `building(sig)(body)` wraps every derived root. It registers `sig` in a `ThreadLocal` set while `body` runs, and removes it in a `finally`.
  - `adaptOrLazy` is what the macros now emit for a found implicit `Schema`, instead of `def.adapt`. It returns a `lazy-ref` without evaluating `x` when
    `sig` is being built on this thread, or when `x` is `null`. Otherwise it is `def.adapt(x, sig)`, as in 0.9.
  - `materialized` runs at the start of rendering (`JsonSchemaProfile.lower`): the first `lazy-ref` of a `sig` becomes its `def`, and later and recursive ones
    become `ref`s. A tree without a `lazy-ref` is returned as is.

**Inside the macros (no API):**
- a mode per expansion: semi-auto for `Json.schema`, `objectSchema`, `derives` and `DerivedSchema`; auto when auto is the root or is imported and available for
  the type;
- the gate by kind (`isGatedKind`): non-tuple case classes, and sealed families with a case-class case, other than `Either`;
- the exemption for cases of a family being derived (`isCaseOfFamilyOnStack`);
- the leftover-hole check.

## Open problems found by the Expectations review (2026-11-25, owner decision)
The Expectations review compared the scenarios with circe's own macro derivation. It found the problems below. I reproduced P1, P2, P4 and P5, and P3 shows
in the scenario runs. P1 to P5 share one root cause: circe has one type class, and we have two (`Schema`, a definition, and `Predef`, inlined) with no bridge
from `Schema` to `Predef`.

**P1. An element's `Schema` doesn't satisfy an instance that asks for a `Predef`** (S10, S18).
- `boxPredef[A](implicit p: Predef[A])` and the generic hint `pairSchema[A](implicit p: Predef[A])` both ignore an `implicit val addressSchema: Schema[Address]`.
- Under auto the same instance does count, because the macro fills the element itself, so the two modes disagree.
- Options:
  - (a) A low-priority bridge in `LowPriorityPredefs`, `implicit def predefFromSchema[T](implicit s: Schema[T]): Predef[T]`, giving a `Predef` that carries the
    `def` node. When inlined, it still renders as a definition + `$ref`, so the core rule holds. It must rank above `importedPredef` and below every real
    `Predef`. A spike checks ambiguity with the built-ins, and the `lazy-ref` handling for it.
  - (b) Change the generic hint to ask for a `Schema[T]`. That fixes S10 only, and breaks for elements that have only a `Predef` (`Int`).
- Recommended: (a). It also fixes P2, and cats with `Schema` elements.

**P2. The generic hint in the error doesn't compose with the other fix it suggests.** Follow both pieces of advice (`Schema[Address]`, plus `pairSchema` with
a `Predef[A]`), and the error comes back. P1 (a) fixes it.

**P3. For a container instance whose element has no instance, the error names the container and suggests deriving it** (S18).
- It suggests `json.Json.schema[Box[Address]]`, which is the wrong schema of #482, now without even a warning.
- The design expected Scala 3 to add the reason from the search (naming `Address`). It doesn't: `Implicits.recordFailure` doesn't keep this failure.
- Options:
  - (a) Scala 3: keep that failure, and print "Box's instance `boxPredef` needs a `Predef[Address]`".
  - (b) Both versions: when the type's companion has an implicit whose result type is `Predef[Box[_]]` or `Schema[Box[_]]`, say so, and suggest an instance for
    the type arguments instead of for `Box`. This is a lookup of the companion's members, not a derivation, so it is not the rejected marker probe.
  - (c) Leave the text as it is.
- Recommended: (a) and (b).

**P4. A recursive type with a `Predef` instance loops or gives `null`** (S4).
- `implicit lazy val treeP: Predef[Tree] = Predef(Json.schema[Tree])` throws `StackOverflowError`; `implicit val` gives `array(null)`.
- It is the same in 0.9, but S4 is now a documented migration.
- An inlined schema of a recursive type can't exist, so a lazy reference is no fix here.
- Options:
  - (a) A compile error: while deriving `Tree` for the initializer of `treeP`, the found instance is `treeP` itself (Scala 2 `c.internal.enclosingOwner`,
    Scala 3 `Symbol.spliceOwner`). The error says "a recursive type needs a `json.Schema` instance (a definition + `$ref`), not a `Predef`".
  - (b) Document it only.
- Recommended: (a).

**P5. Generic `derives Schema` on Scala 3** (S19).
- The compiler makes `derives` ask for `Schema[A]` for a type parameter, so `GP[Int]` has no instance, and a `Schema` parameter is unwrapped and inlined.
- It is the same in 0.9.
- Options:
  - (a) Document that `derives` is for monomorphic types, and that generic types use an `implicit def` (S10).
  - (b) Change the error for a type that has a `derives` given that failed, so it doesn't claim "no implicit json.Schema".
- Recommended: (a) and (b).

**P6. The Scala 3 import hint.** `import json.Json.auto._` works on Scala 3 too. The error's "(Scala 3: `import json.Json.auto.given`)" and the scaladoc of
`Json.auto` suggest it doesn't. Recommended: drop the parenthetical and say that `_` works on both versions.

**P7. `AutoGuard.Allowed` is dead weight** (New entities).
- Options:
  - (a) Remove it.
  - (b) Replace it with a `Mirror.Of[T]` bound, as circe has, so auto answers only for derivable types. That would shrink S2-R7, holes that make candidates
    apply which failed before. It applies to Scala 3 only.
- Recommended: (a), and keep (b) as a follow-up.

**P8.** The Scala 2 `implicitly` error under auto (A9) is the open problem already listed in "Implementation notes".

## Options

### A. circe's model: semi-auto by default, `Exported` auto — WON
1. **`Exported` and the bridge** (core):
   ```scala
   // json/schema/Exported.scala
   final case class Exported[+T](instance: T) extends AnyVal
   // json/schema/Predef.scala
   trait LowPriorityPredefs { ...; implicit def importedPredef[T](implicit exported: Exported[Predef[T]]): Predef[T] = exported.instance }
   ```
2. **`Json.auto.derived[T]: Exported[Predef[T]]`** (api, both versions). Called by the user's code, outside a derivation, it derives `T` deeply in one expansion,
   exactly as `Json.auto.derived` does today.
3. **Auto inside a derivation never derives by itself.** This was the review's main finding (S-R1). With the bridge, auto reaches every element of a Predef
   constructor. For `Node(children: List[Node])` the search for `Predef[List[Node]]` finds `listS(importedPredef(auto.derived[Node]))`. If that `auto.derived[Node]`
   ran its own derivation, it would loop (the 0.7 StackOverflow, `UImplicits.scala:139-140`). It would also cost an exponential amount of work (`AutoGuard.scala:10-11`).
   So:
   - **During a derivation's implicit search, `auto.derived[X]` expands to a cheap placeholder,** `Exported(AutoHole.hole[X])`, instead of deriving or declining.
     - Scala 3: `AutoGuard.isSearching` (the existing ThreadLocal).
     - Scala 2: a ThreadLocal "searching" flag set around `Implicit.infer`, the counterpart of `AutoGuard`. It is not `c.openMacros`, which also contains the
       derivation during its own silent `c.typecheck` and the `c.eval` of default values (S2-R9). Whether scalac expands a nested blackbox implicit macro during
       the outer `c.inferImplicitValue` at all is a spike (S-R3, `-Ymacro-debug-lite`). If it doesn't, the found tree carries the unexpanded `auto.derived[X]`,
       which is treated as a hole.
   - **The derivation then walks the accepted tree:**
     - A hole at the root (`importedPredef(hole[X])`) means auto is available for `X`: derive `X` in place, in the current expansion.
     - A nested hole, e.g. `listS(importedPredef(hole[Node]))` or `nelSchemaFromPredef(importedPredef(hole[P]))`, becomes a **child node in the IR** (S2-R1):
       - Scala 3 `FromImplicit` and Scala 2 `` `-from-tree-` `` carry `holes: List[(id, Node)]`, with `X` resolved by `resolve(X, ctx)` in the same context.
       - `children` / `mapChildren` (Scala 2: `transformSchema`) include the holes, so the recursion passes see them: `substituteRecursive` (`Recursion.scala:29-36`)
         and `recursiveTypes.substitute` (`Macroses.scala:87`). A recursive `X` becomes a `$ref` to a definition that exists.
       - Substitution happens at emission: Scala 3 with a `TreeMap` over the found term; Scala 2 by splicing the emitted tree in place of a local val.
         At construction, Scala 2 typechecks the shape silently with a stub, `{ val e1: Predef[X] = null; <tree with e1> }`.
     - **If the substitution can't be done, it is a compile error** naming the type and the instance found. It never falls back to deriving the type from its
       structure, which for `NonEmptyList[P]` would bring back `{head, tail}` (S2-R2).
   - **No hole is ever emitted.** After emission the macro checks the output tree for `AutoHole.hole` and aborts if one is left, a compile-time check (S2-R2).
   - **This is not the rejected marker probe.** A hole exists only when auto is imported and a real instance asked for an `X`: the user opted in, and the instance
     came from the normal search.
   - **Built-in collections keep their output** (S2-R8). With auto, `List[Node]` is found as `listS(hole)` where today it was the native `Arr` node. The two must
     render the same: goldens compare auto-imported against plain output for `List`, `Set`, `Vector`, `Array`, `Map[String, _]` and `Map[Int, _]`. If any
     differ, the native node wins for core's own Predef constructors.
   - **Holes change which candidates apply** (S2-R7). Under auto every `Predef[X]` now succeeds during a derivation, even for an `X` that can't be derived, so a
     candidate that failed before can now apply. It may then fail at replacement with "not supported", or make a search ambiguous. Tested in `AutoSpec`.
4. **The import counts as used** (S-R4, S2-R3). Today an import used only by the macro's searches is reported unused (`SelfImplicitLintSpec.scala:124-135`,
   `AutoDerivationSpec.scala:97`, build.sbt's refined comment). With a hole at the root, the common case, the found tree is discarded, so nothing in the output
   goes through the import. Instead, `auto` gets a marker, `implicit val jsonSchemaAutoEnabled: AutoEnabled`, a given so `import auto.given` brings it too, and
   every auto-mode expansion splices a use of it:
   - Scala 3: the marker's search result, wrapped in `Inlined(None, Nil, _)` (as `Implicits.scala:62-66` does). If the marker search fails, nothing is spliced.
   - Scala 2: an untyped `Ident(TermName("jsonSchemaAutoEnabled"))` in a discarded val, so the call-site typer resolves it through the import. It is spliced only
     after a silent `c.typecheck` shows it resolves to the marker's symbol. A user who imported only `auto.derived` by name, or who has a local name that clashes,
     gets no splice and no error (S3-R2).

   This is spike 0(d). A test compiles a zero-change migration with unused-import warnings and `-Werror` (Scala 3: `-Wunused:imports`; 2.13:
   `-Ywarn-unused:imports`).
5. **Scala 3 cleanup.** `containsAuto` becomes "no hole outside the replaced positions". `DerivedSchema` runs in typer and gets unexpanded trees, so there the
   unexpanded `auto.derived[X]` is treated as a hole too (S-R8). The `recordFailure` filter (`Implicits.scala:90`) also drops bridge failures (`importedPredef`,
   `Exported`), so errors don't gain an "an implicit for it failed" suffix.

Pros:
- The same model and precedence as circe and pureconfig.
- #482 is fixed with no cats knowledge in the macros.
- Scala 2 and 3 agree.
- The user's instances always win over auto.
- Auto keeps one expansion, so recursion and definitions behave as today.

Cons:
- It breaks almost every user, hence the rollout.
- Tree substitution of holes on both versions (a spike first).
- Mutual recursion through explicit instances needs a lazy reference (next section).

### B. Keep deep derivation; mark containers (`Container[T]`) — LOST
A core marker tells the macro "resolve through implicits, never derive". It fixes #482 only, keeps the implicit deep default the owner ruled out, and every
library has to declare its containers.

### C. Keep deep derivation; probe for a Predef constructor with internal marker types — LOST
`Predef[F[Hole]]` found means `F` is a container. It needs no API, but it keeps the deep default, and the macro derives the element with no import.

### D. Semi-auto, but auto stays `Predef[T]` (no `Exported`) — LOST
Smaller, but on Scala 3 the lexical `auto.derived[P]` shadows an instance in `P`'s companion (contextual scope is searched first), and Scala 2 keeps its
shadowing re-search (`UImplicits.scala:150-157`). `Exported` fixes both by construction.

## Mutual recursion through explicit instances (must solve)
```scala
case class A(b: Option[B]); case class B(a: Option[A])
implicit val aS: Schema[A] = Json.schema[A]   // the field b -> bS: definition + $ref
implicit val bS: Schema[B] = Json.schema[B]   // the field a -> aS
```
This already fails today. The emitted `def.adapt(x, sig)` (`UImplicits.scala:208-209`) is strict (`Schema.scala:700`), so `aS` captures `def(sig, null)`. On
Scala 3, `derives` givens are lazy, and a self-reference deadlocks (`Implicits.scala:12`). So `A derives Schema` plus `B derives Schema`, the usual semi-auto
shape on Scala 3, has the same problem. Under semi-auto this becomes the normal way to write such models, so it must work.

**Recommended:** a `ref` that carries a by-name body, emitted only when the instance isn't available yet (S2-R4). The macro emits
`` `def`.adaptOrLazy(x, sig) ``, with `x` by-name. Cycles are detected for any instance shape (S3-R1):
- **Registration:** every expansion registers its root's signature while its schema is being built: `Schema.building(sigA) { ... }`, a ThreadLocal set.
  - The entry is removed in a `finally`.
  - Registration and `adaptOrLazy` compute the signature the same way, from the dealiased type.
  - A type that mentions an abstract type has no compile-time name (`Implicits.scala:34-35`), so it is not registered. A cycle through such a generic instance
    stays unsupported, and is documented.
- **A cycle:** `adaptOrLazy(x, sig)` returns `lazy-ref` without evaluating `x` when `sig` is being built on this thread. That covers a mutual cycle through:
  - `implicit val` (where `x` would be `null`);
  - `implicit lazy val`;
  - Scala 3 `given` and `derives` givens (where evaluating would deadlock or loop);
  - `implicit def`.
- **No cycle:** `x` is evaluated at once and gives today's `def`, so the Scala value and equality are unchanged, `derives` givens included.
- **A remaining `null`** (a val in an object that is read before it is initialized, with no cycle through the macro) also gives `lazy-ref`.
- A `lazy-ref` evaluates `x` again when it is forced.

The node:
```scala
// core, json/Schema.scala (a new node; equality and hashCode by sig, toString without the body)
final class `lazy-ref`[T] private (val sig: String, body: => Schema[_]) extends Schema[T] { lazy val definition: Schema[_] = /* def.adapt rules applied here */ body }
```
- Every existing traversal sees a ref, so the tree stays finite and equality is by `sig`. Those traversals keep no seen-set: `extractDefs` and `lowered`
  (`AsDraftSupport.scala:33-44,342-353`), `DefinitionNames.assign`, `toDefinition.deepCopy` (`Schema.scala:672-689`), `equals`, `hashCode`, `toString`.
- Only definition collection and name assignment force the body, deduplicated by `sig`. `adapt`'s rules (value-class unwrap, keeping an inner def's `sig`, the
  #297 generic rename) apply at force time.
- A forced body is lowered with the active `JsonSchemaProfile`, the same as an inline definition, so its `option` nodes follow the user's profile. `lowered`
  (`AsDraftSupport.scala:44`) doesn't descend into a `lazy-ref`; definition collection applies the profile to the body when it forces it (S2-R5).
- The bridges and the parser match on nodes: the bridges render through `AsValue`, so no change; the parser never builds one.
- This is a new public `Schema` node, so it falls under the pending ruling on new Schema nodes.

**Alternative:** document that mutually recursive types use auto (one expansion) or one `Json.schema` of a common root. This doesn't fix Scala 3 `derives`.

## Rollout
- **Phase 1 (0.10.0), recommended:**
  - Semi-auto is the documented model; `Exported` auto and the precedence fixes ship.
  - A gated nested type without an instance is still derived in place, with a warning. The warning has a stable prefix that `-Wconf:msg=` can match (macros can't
    set a category): `[json-schema-semi-auto] schema for X is derived implicitly (in ...); this becomes an error in a future release. <the two fixes>`.
  - Opt in early with `Flag.SemiAuto`, which gives the error now. Flags are read with one implicit `Flag` search (`UFlags.scala:11`, `Implicits.scala:138`), so
    several flags combine in one value: `implicit val flags: Flag with Flag.EnumsAsOneOf with Flag.SemiAuto = null`. Two separate flag vals are ambiguous:
    a compile error on Scala 2, and on Scala 3 neither applies (Expectations F3); this is documented.
  - Users on `-Werror` see errors at once. The release notes say so, and give `-Wconf:msg=json-schema-semi-auto:s` as the way to keep phase 1 silent.
  - Macros run inside implicit searches can lose or duplicate warnings for discarded candidates: Scala 2's whitebox `DerivedSchema`, Scala 3's transparent
    `DerivedSchema`, and Scala 3 auto expanding during a search. The warning is emitted only from the outermost expansion, the one not inside a search.
  - So `DerivedSchema`, which always expands inside a search, gets no phase-1 warning. Its users go straight from silence to phase 2's error, which is the terse
    "no implicit DerivedSchema[X]". The migration notes say so (S2-R10).
- **Phase 2 (the next breaking release):** the warning becomes the error, and `Flag.SemiAuto` is deprecated, a no-op.
- **Alternative:** the error in 0.10.0 directly. Simpler, but every user's build breaks at once.

## Implementation plan
0. **Spikes first, result into this doc:**
   - (a) Scala 2: is a nested blackbox implicit macro expanded during the outer `c.inferImplicitValue`?
   - (b) Scala 2: hole substitution with a local val, typechecked.
   - (c) Scala 3: the placeholder under `AutoGuard.isSearching`, `TreeMap` substitution, and `DerivedSchema` in typer.
   - (d) The unused-import behaviour of both, under `-Werror`.
1. **core:**
   - Add `Exported`, the `importedPredef` bridge, `AutoHole.hole[X]` (internal API; it throws if ever evaluated), and `Flag.SemiAuto`.
   - Add the `lazy-ref` node and its handling in `AsDraftSupport` (definition collection, name assignment), the `JsonSchemaProfile` passes and `Schema.def.adapt`.
2. **api:** `Json.auto.derived[T]: Exported[Predef[T]]`, Scala 2 macro and Scala 3 inline. Inside a derivation's search it gives the hole. Update the scaladoc of
   `Json` and `Json.auto` for the model.
3. **Scala 2 macros:**
   - `Macroses`: a mode (semi-auto / auto) in the resolution context.
   - `UImplicits.inferPredef`: holes at the root and nested (step A.3). Drop the shadowing re-search.
   - `genTree`: the gate by kind (above), for non-root types that aren't cases of the family being derived: semi-auto → warning (phase 1) or error
     (`Flag.SemiAuto`, phase 2); auto → derive.
   - `UFlags`: read `Flag.SemiAuto` next to `EnumsAsOneOf` (`UFlags.scala:11`) (S2-R11).
   - Holes as IR children of `` `-from-tree-` ``, handled by `transformSchema` and the recursion substitution. The output is checked for leftover holes.
   - An implicit `Schema` is emitted through `` `def`.adaptOrLazy ``.
4. **Scala 3 derivation:**
   - `Resolve.resolve`: the same gate by kind.
   - `AutoGuard`: the placeholder instead of the decline.
   - `Implicits.searchAccepted`: holes, the `containsAuto` change and the `recordFailure` filter (A.5).
   - `Mode`: `Auto` derives deeply; `Schema`, `ObjectSchema`, `Derived` and `DerivedSchema` are semi-auto.
   - `hasFlag[Flag.SemiAuto]` (`Implicits.scala:138`) (S2-R11).
   - Holes as IR children of `FromImplicit` (`Nodes.scala:85`), seen by `substituteRecursive`. The output is checked for leftover holes.
   - `FromImplicit` emits through `` `def`.adaptOrLazy ``.
   - Every expansion wraps its result in `Schema.building(sig)`. Scala 2 does the same in step 3.
5. **modules:**
   - `derived` (`DerivedSchema`, which runs in typer on Scala 3 and is whitebox on Scala 2): a semi-auto root, with the unexpanded-auto handling.
   - cats, refined, enumeratum and joda provide `Predef`s, so no change in main code.
6. **Tests:**
   - The existing suites, about 1,150 `Json.schema`/`objectSchema` call sites in about 100 test files, rely on deep derivation, and must keep testing what they
     test now. So no auto import is added to them; that would silently turn them into auto-mode tests (S-R6). Instead, Test scopes silence the phase-1 warning
     with `-Wconf:msg=json-schema-semi-auto:s` in build.sbt. Goldens must not change: run the full matrix before and after.
   - `SemiAutoSpec` (api, shared, with `Flag.SemiAuto`):
     - the error for each gated kind, nested directly and in `Option`, `List`, `Map` values, tuples and `Either`;
     - native types without instances;
     - the root sealed family's cases being derived and their fields resolved;
     - recursion to the root;
     - map keys that are enums.
   - `AutoSpec`:
     - auto with recursion through `List`, `Map`, `Array`, `Option` and cats `NonEmptyList`, on 2 and 3 (S-R1), including a type that only contains a recursive
       one (`Forest(trees: List[Node])`, S2-R1);
     - an element that can't be derived, inside `List` and `NonEmptyList` (S2-R7);
     - auto-imported output equal to plain output for `List`, `Set`, `Vector`, `Array`, `Map[String, _]`, `Map[Int, _]` (S2-R8);
     - the existing `AutoDerivationSpec` cases, unchanged;
     - `implicitly[Predef[Foo]]` through the bridge, outside a derivation.
   - `AutoPrecedenceSpec`, where each of these beats auto:
     - built-ins;
     - a companion instance;
     - a generic companion instance;
     - an instance inherited by the companion from a parent trait;
     - a local instance;
     - an imported instance;
     - on Scala 3, a companion `given`, compiled also with the newest 3.x (3.6+ prioritization applies between givens; the bridge is an `implicit def`).
   - Mutual recursion: `A`/`B` with explicit vals, and Scala 3 `derives`, rendered with definitions; no `null`, no loop, equality and `toString` finite.
   - Unused imports: a zero-change migration compiled with `-Werror` and unused-import warnings on (A.4).
   - The phase-1 warning: there's no infrastructure to check a warning today, so add a small compile-and-capture helper (Scala 2 `ToolBox`; Scala 3
     `scala.compiletime.testing` only reports errors, so there it is a compile of a snippet through the compiler API in a test).
   - The #297 rename for generic instances (`UImplicits.scala:201-207`): a generic implicit `Schema` used under semi-auto renders its definition under the
     instantiated name.
7. **Docs:**
   - README and every paradox page with nested types: show semi-auto (instances) and auto (the import).
   - `migration.md`: "0.10: semi-auto by default", covering:
     - the phases;
     - `import Json.auto._` as the zero-change path;
     - `Flag.SemiAuto` and combining flags;
     - the `-Wconf` silencer;
     - the output difference when migrating with explicit `Schema` instances: definitions plus `$ref`s, generic ones renamed per #297.
   - CLAUDE.md "core design rule": add step 5.

## Compatibility
- **Source:**
  - Phase 1: compiles as before, with warnings for gated nested types derived implicitly.
  - Phase 2: those become errors.
  - `Json.auto.derived[T]` called explicitly returns `Exported[Predef[T]]` (use `.instance`). Code that only imports it is unaffected.
- **Output and Scala values:**
  - Rendered JSON: none for code that compiles unchanged, or that migrates with `import Json.auto._`.
  - Scala values: an implicit `Schema` found by the macro is still a `def` node, except in a recursion cycle between instances, or for a val read before it is
    initialized. Both are cases that give `null` or loop today. There it is the new `lazy-ref` node, which matches as neither `def` nor `ref`, and compares
    equal by `sig`.
  - Migrating with explicit implicit `Schema`s turns those types into definitions plus `$ref`s, per the core rule. Generic ones are named per #297.
  - Scala 3 with auto: Predef-constructor containers of derived elements (cats) become arrays, as on Scala 2.
  - Mutually recursive explicit instances render correctly (today: `null` or a deadlock).
- **Binary:** additions (`Exported`, `importedPredef`, `AutoHole`, `Flag.SemiAuto`, `lazy-ref`). `auto.derived` is a macro / inline, with no runtime signature.
  `lazy-ref` is a new `Schema` case: code matching exhaustively on `Schema` gets a new case.
- **PVP:** breaking, so 0.10.0. `lazy-ref` falls under the pending ruling on new Schema nodes.
- **Legacy profile:** not affected (rendering only).
- **Core rule:** gains step 5 (gated kinds: semi-auto → error, auto → derive). Steps 1 to 4 are unchanged.

## Owner decisions (2026-11-24)
The owner accepted all four recommendations:
- **Rollout:** a phase-1 warning in 0.10.0; the error in the next breaking release.
- **Mutual recursion:** the `lazy-ref` node.
- **Gated kinds:** only case classes and sealed families with a case-class case. Value classes, case-object enums and Scala 3 enums stay native.
- **Names:** `Flag.SemiAuto`, `Exported`, `lazy-ref`.

## Open questions for the owner (as asked; answered above)
1. **Rollout:** warning first (phase 1, recommended), or the error in 0.10.0.
2. **Mutual recursion:** the `lazy-ref` node (recommended), or documentation only.
3. **Which nested kinds are gated.** Value classes, all-case-object families and Scala 3 `enum`s are as common nested as case classes. Gating them is
   consistent with circe (`deriveUnwrapped` and enum codecs are explicit there). Not gating them (native, like Java enums and `Enumeration`) is less migration.
   Recommended: gate case classes and sealed families. Leave value classes, case-object enums and Scala 3 enums native: they have no "container whose element
   has no instance" problem, and deriving them is what any user expects.
4. **Names:** `Flag.SemiAuto`, `Exported`, `lazy-ref`.

## Follow-ups
- #482 / PR #488 (cats collections) becomes: the two cats fixes (`nesSchema` without `uniqueItems`, `Chain` with `minItems`), the cats docs, and tests of the
  three cases on top of this change.

## Implementation notes (2026-11-25)

**Spike results:**
- **(a)** Scala 2 expands a nested blackbox implicit macro during the outer `c.inferImplicitValue`. So a ThreadLocal "searching" flag around the derivation's
  lookups is what tells `auto.derived` to give the hole. On Scala 3, `Json.schema`, `objectSchema` and `derives` search after typer and get expanded holes;
  `DerivedSchema` searches in typer and gets the unexpanded `auto.derived[X]` call. Both are treated as holes.
- **(b)** Scala 2 substitution: each hole becomes a fresh `Ident`, the tree is untypechecked, and the shape is typechecked with a stub
  (`null.asInstanceOf[T]`, because `Exported` is a value class). Package qualifiers are rewritten to `_root_` paths after `untypecheck`.
- **(c)** Scala 3: a `TreeMap` with `changeOwner` works in both phases, under `-Xcheck-macros`.
- **(d)** The marker splice is needed on both versions. It is done in every expansion where the marker resolves, not only in auto mode. Otherwise
  `Json.schema[Int]` with `import Json.auto._` in scope (auto never needed) reports the import unused.

**Deviations from the design:**
- **Core's own collection constructors** (`listS`, `setS`, `vectorS`, `arrayS`, `dictS`) found with a hole use the native node, instead of filling the hole.
  The output stays exactly as before: `dictS` would give `Map[String, X]` no key pattern, where the native node gives `^[\s\S]*$`. Holes are filled for
  everything else: cats, a user's Predef constructors, a generic `implicit def ...: Schema[Box[T]]`.
- **Holes are resolved in the same context** as the type that holds them; the container is not pushed on the stack.
- **No `Schema.building` when the root itself came from an implicit.** Otherwise `Json.schema[Inner]` with an implicit `Schema[Inner]` would be a `lazy-ref`.
- **Warnings:** Scala 3 reports only one diagnostic per position, so it gives one warning per expansion, with one prefixed line per gated type. Scala 2 gives
  one warning per type.
- **Two separate flag vals** (Expectations F3): Scala 2 reports the ambiguity as a compile error; Scala 3 silently applies neither. The design assumed "neither" on both.
- **No warning-capture test:** there is no ToolBox or compiler API on the test classpath. The warnings were checked by hand with the `-Wconf` silencer removed.

**Open problem (owner decision):** on Scala 2, `{ import Json.auto._; implicitly[Predef[Unsupported]] }` now fails with scalac's plain "could not find
implicit value for parameter p: json.schema.Predef[Unsupported]", without the derivation's reason.
- Why: `auto.derived` is now the argument of the bridge, so scalac expands it while the user's own search is still choosing. A failed expansion only makes
  `importedPredef` inapplicable, and Scala 2 doesn't report why a nested implicit failed.
- `Json.schema[Unsupported]` (the macro path) still gives the full reason, and Scala 3 is unaffected.
- The existing `AutoDerivationSpec` test "auto: a type it can't derive is a compile error that says why" fails on 2.12 and 2.13 because of this. It is left
  unchanged, pending the owner's decision.

## Review log
Round 1 (independent reviewer, 2026-11-24, against `5b24a80b`):
- S-R1 (must) auto through the bridge reaches every Predef constructor's element (`listS` etc. are Predef constructors, `Predef.scala:143-155`); recursion would
  loop → accepted: a nested auto never derives by itself; holes are replaced from the current expansion (A.3); the wrong "collections have no instance" line is
  fixed.
- S-R2 (must) the gate placed before the families would catch tuples, `Either`, and Java enums → accepted: the gate is by kind; `Optional`, map keys and Scala 2
  value-class family members are listed; the duplicate enum entries are fixed.
- S-R3 (must) Scala 2 may expand a nested implicit macro during the outer search → accepted: spike 0(a); the `c.openMacros` placeholder is the fallback.
- S-R4 (must) the auto import would be reported unused → accepted: A.4, with a `-Werror` test.
- S-R5 (must) mutual recursion is real today too; a forced by-name body loops in the traversals → accepted: `lazy-ref` modelled as a ref with a by-name body,
  forced only by definition collection (deduplicated by `sig`); the Scala 3 `derives` case is added.
- S-R6 (must) the rollout's test plan doesn't work → accepted: the warning gets a stable prefix; Test scopes silence it with `-Wconf`; suites don't get the
  import; a warning-capture helper; warnings only from the outermost expansion.
- S-R7 (nit) combining flags → accepted: documented.
- S-R8 (nit) Scala 3 `containsAuto`, `recordFailure`, and type comparison → accepted. A.5. The `show`-based comparison is gone (holes replace the root-only
  decline, so no cross-`Quotes` comparison is needed).
- S-R9 (nit) the error text → accepted: the current prefix is kept, the generic hint is added, and the #297 rename is in tests and Compatibility.
- S-R10 (nit) nested enums → accepted: open question 3, with a recommendation.
- S-R11 (nit) precedence cases → accepted: added to `AutoPrecedenceSpec`.

Round 2 (same reviewer):
- S2-R1 (must) holes spliced during construction escape the recursion passes, giving a dangling `$ref` → accepted: holes are IR children, substituted at emission
  (A.3).
- S2-R2 (must) the fallback to structural derivation would bring #482 back → accepted: a compile error instead, plus a compile-time check for leftover holes.
- S2-R3 (must) the import isn't used once a root hole's tree is discarded → accepted: an `AutoEnabled` marker in `auto`, spliced into every auto-mode expansion
  (A.4, spike 0(d)).
- S2-R4 (must) `lazy-ref` for every implicit `Schema` changes every user's Scala value → accepted: `` `def`.adaptOrLazy ``; `lazy-ref` only for `null` vals and
  Scala 3 `derives` givens; the ADT-level change is in Compatibility.
- S2-R5 (nit) the profile on forced bodies → accepted.
- S2-R6 (nit) the #482 error names `NonEmptyList[P]` → accepted.
- S2-R7 (nit) holes change which candidates apply → accepted: an `AutoSpec` case.
- S2-R8 (nit) auto output through `listS` etc. → accepted: equality goldens; the native node wins if they differ.
- S2-R9 (nit) the Scala 2 trigger → accepted: a ThreadLocal around `infer`.
- S2-R10 (nit) no phase-1 warning for `DerivedSchema` → accepted: documented.
- S2-R11 (nit) reading the flag → accepted: plan steps 3 and 4.

Round 3 (same reviewer):
- S3-R1 (must) `adaptOrLazy` missed `lazy val`, `given` and `def` instances in a cycle → accepted: cycle detection through `Schema.building(sig)` (a ThreadLocal
  set); the `derives` special case and its Compatibility caveat are gone.
- S3-R2 (must) the Scala 2 marker `Ident` could fail to resolve or resolve to the wrong symbol → accepted: a distinctive name, spliced only after a silent
  typecheck confirms the symbol; Scala 3 skips it when the search fails.
- Nit: "evaluated once, at once" → fixed.

Round 4: **approved** (no musts). Nits folded in: `Schema.building` cleans up in `finally`; the signature is computed the same way on both sides, and types
with an abstract type aren't registered.

Round 5, the Expectations review (independent reviewer, 2026-11-25), against circe-derivation (`DerivationMacros.scala`) and circe for Scala 3 (`derivation`,
`ConfiguredEncoder`, `Exported`, `importedEncoder`), with compiled probes:
- Agrees with circe: `Exported` plus the lowest-priority bridge; precedence, also on 3.7.0; what is derived; sub-families inside the family; the error names the
  missing type.
- Differs on purpose, and justified: value classes and enums are native; `Flag.SemiAuto`; `AutoHole`, `AutoSearch` and `AutoEnabled`; `lazy-ref`.
- musts, accepted as open problems P1 to P5 (above): an element's `Schema` doesn't satisfy a `Predef` constructor; the generic hint doesn't compose; the
  container error suggests the wrong schema, and Scala 3 adds no reason; a recursive type with a `Predef` loops; generic `derives`.
- shoulds, fixed in Expectations:
  - "as in circe" corrected: circe-derivation asks for an instance per case, and the case-as-field exemption goes beyond circe for Scala 3;
  - S16 and S17 labelled as differences from circe;
  - the Scala 3 import style (P6 for the error text);
  - the A1 claim;
  - new cases: a case with its own instance (S13), a case outside its family (S14), auto never gives a `Schema` (A11);
  - `AutoGuard.Allowed` (P7);
  - 2.12 not run for the scenarios (preamble).
- nits fixed: S21's snippet is in an object. The generic hint's fixed name `boxSchema`, and `gPSchema` for `GP`, are left for P3's rewrite of the text.
