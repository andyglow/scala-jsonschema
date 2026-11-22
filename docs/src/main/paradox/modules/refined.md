# Refined

[refined](https://github.com/fthomas/refined) predicates become validations:

```scala mdoc:compile-only
import json._
import com.github.andyglow.jsonschema.RefinedSupport._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._

case class Signup(
  login: String Refined MatchesRegex["^[a-z][a-z0-9]{2,15}$"], // {"type": "string", "pattern": "^[a-z][a-z0-9]{2,15}$"}
  age: Int Refined Greater[17],                                // {"type": "integer", "exclusiveMinimum": 17}
  tags: List[String] Refined MaxSize[5]                        // {"type": "array", "maxItems": 5, ...}
)

Json.schema[Signup]
```

Predicates are written with literal types on Scala 2.13 and 3. On Scala 2.12 use shapeless's witnesses:
``Greater[W.`17`.T]``. Supported predicates:

- boolean: `And`, `Or`, `Not`
- numeric: `Positive`, `Negative`, `NonPositive`, `NonNegative`, `Greater`, `Less`, `GreaterEqual`, `LessEqual`, `Divisible`
- string: `MatchesRegex`, `StartsWith`, `EndsWith`, `Uuid`, `Uri`, `Url`, `IPv4`, `IPv6`, `Xml`, `Trimmed`
- collection (strings and collections): `Size`, `MinSize`, `MaxSize`, `Empty`, `NonEmpty`

An unsupported predicate, or a range with no values (`Less[2] And Greater[7]`), is a compile error.
