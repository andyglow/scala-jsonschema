# DerivedSchema

`scala-jsonschema-derived` provides `json.schema.derived.DerivedSchema[T]`, a type class for "a schema derived
from `T`'s structure", e.g. for a context bound:

```scala mdoc:compile-only
import json.schema.derived.DerivedSchema

def describe[T](implicit ds: DerivedSchema[T]): json.Schema[T] = ds.schema

case class Point(x: Int, y: Int)

describe[Point]
```

The root type is always derived and inlined, even when an implicit `Schema[T]` exists (nested types follow the
usual @ref:[rules](../definitions.md)). A type that can't be derived has no instance, so a default argument or a
lower-priority implicit applies instead.
