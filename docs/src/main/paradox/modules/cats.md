# Cats

```scala mdoc:compile-only
import json._
import com.github.andyglow.jsonschema.CatsSupport._
import cats.data.{NonEmptyList, NonEmptyMap}

case class Team(members: NonEmptyList[String], scores: NonEmptyMap[String, Int])

Json.schema[Team] // members: an array with "minItems": 1; scores: an object with "minProperties": 1
```

Supported: `NonEmptyList`, `NonEmptyVector`, `NonEmptySet`, `NonEmptyChain`, `NonEmptySeq`, `NonEmptyMap`, `OneAnd`, `Chain`,
and `NonEmptyLazyList` (2.13, 3) / `NonEmptyStream` (2.12).

An element without an instance follows the @ref:[semi-auto rules](../definitions.md#semi-auto-and-auto-derivation):

| in scope | `NonEmptyList[P]` (`case class P(a: Int)`) |
|---|---|
| nothing | in 0.10 a `[json-schema-semi-auto]` warning (and still the old `{head, tail}` object); an error in the next breaking release, or now with `Flag.SemiAuto` |
| `import Json.auto._` | an array of `P`, derived and inlined (Scala 2 and 3) |
| an implicit `Predef[P]` or `Schema[P]` | an array of that |
