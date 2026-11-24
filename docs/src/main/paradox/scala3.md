# Scala 3

What's new on Scala 3: `derives json.Schema` (see @ref:[Definitions](definitions.md)), native `enum`s (see
@ref:[Types and Enums](types.md)), `import Json.auto.given`, and `-Yread-docs` for scaladoc from other modules (see
@ref:[Descriptions](descriptions.md#scaladoc)). Everything else is shared with Scala 2 and gives the same schemas.

## Deliberate differences

- **Aliases that only rename a type are gone before the macro sees them.** For `type MyList[A] = List[A]`,
  `Json.sig` and definition names show `scala.List[scala.Int]` (Scala 2: `pkg.MyList[scala.Int]`). Standard
  collections in type arguments are spelled the usual way (`scala.List`, `scala.Predef.Map`) even if you wrote
  `scala.collection.immutable.List`, so a name doesn't depend on where the model was compiled. Other aliases
  (`type Users = List[User]`) give the same names as on Scala 2.
- **Sealed families can't have anonymous or block-local subclasses** (`new Root {}`, a case class declared inside
  a method): a compile error, because Scala 3 can't list them. A local case class on its own works, also with
  default values (Scala 2 rejects those defaults).
- **`Enumeration`:** the enum lists only the object's `Value`s; a class declared inside an `Enumeration` object
  is derived as a class.
- **`Option` defaults.** On both versions `= None` gives no default, and any other `Option` default needs a
  `ToValue[Option[X]]` (the bridges provide one). Scala 2 evaluates the default at compile time to see whether it
  is `None`; Scala 3 recognizes only `None` and `Option.empty` written out, so `= Defaults.none`, `= (None)` or
  `= { None }` need the `ToValue` too.
- **A generic `derives` class** has a `Schema[Box[X]]` only when `X` has an implicit `Schema`: then `Box[X]` is a
  definition, otherwise it is derived inline (see @ref:[Definitions](definitions.md)). Inside the derived schema, an
  implicit `Schema` for an abstract type parameter is used as it is at runtime: an existing definition keeps its
  name, anything else is inlined.
- **refined:** `refined[T]` has no `T <: Refined[_, _]` bound (Refined is an opaque type on Scala 3; a non-refined
  `T` is a compile error), and an unsupported predicate found while deriving fails with the derivation's
  `is not supported` error followed by refined's own message.
- **`*:` tuples** (`Int *: String *: EmptyTuple`, what `Tuple.Concat`/`Tuple.Map` produce) are the same object as
  `(Int, String)`, also beyond 22 elements (`_1` .. `_23` ...). `Tuple`, `NonEmptyTuple`, `Int *: Tuple` (no known
  arity) and `EmptyTuple` are compile errors.
- Compile errors quote Scala 3 code, so their text differs from Scala 2's.

## Debugging

To see the code the macro generates, compile with `-Djsonschema.print-code=true` (e.g.
`sbt -Djsonschema.print-code=true compile`): every expansion is printed as an info message.

## Not supported (yet)

Union types, literal types, opaque types (beyond refined) and named tuples.

## Known issues

- **Recursive generic `derives`.** For `case class Tree[A](value: A, children: List[Tree[A]]) derives json.Schema`,
  `Tree[Int]` used as a field or as the root of `Json.schema` is a definition named `pkg.Tree[scala.Int]`, but a
  summoned `Schema[Tree[X]]` (e.g. `summon[json.Schema[Tree[Company]]]`, `Company derives json.Schema`) and a
  `Tree[A]` inside another generic `derives` class keep the type parameter's name (`pkg.Tree[pkg.Tree.A]`), so two
  instantiations in one schema share it.

Known issues on both versions (and Scala 2-only ones) are listed in the
[README](https://github.com/andyglow/scala-jsonschema#known-issues).
