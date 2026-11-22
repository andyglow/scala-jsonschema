# Cats

```scala mdoc:compile-only
import json._
import com.github.andyglow.jsonschema.CatsSupport._
import cats.data.{NonEmptyList, NonEmptyMap}

case class Team(members: NonEmptyList[String], scores: NonEmptyMap[String, Int])

Json.schema[Team] // members: an array with "minItems": 1; scores: an object with "minProperties": 1
```

Supported: `NonEmptyList`, `NonEmptyVector`, `NonEmptySet`, `NonEmptyChain`, `NonEmptyMap`, `OneAnd`, `Chain`,
and `NonEmptyLazyList` (2.13, 3) / `NonEmptyStream` (2.12).
