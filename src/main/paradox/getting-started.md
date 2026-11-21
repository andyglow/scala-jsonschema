# Getting Started

## SBT

> **New groupId in 0.8.0:** artifacts are published as `io.github.andyglow` from 0.8.0 on, because Maven Central no longer
> accepts new releases under `com.github.andyglow`. Releases up to 0.7.x stay under `com.github.andyglow`. Only the sbt
> coordinates change; the Scala packages (`json.*`, `com.github.andyglow.*`) are the same, so no imports change.

### Main:

```scala
libraryDependencies += "io.github.andyglow" %% "scala-jsonschema" % <version> // <-- required
```

### Modules

```scala
libraryDependencies ++= Seq(
  "io.github.andyglow" %% "scala-jsonschema-core" % <version>,              // <-- transitive
  "io.github.andyglow" %% "scala-jsonschema-macros" % <version> % Provided, // <-- transitive
  // json bridge. pick one
  "io.github.andyglow" %% "scala-jsonschema-play-json" % <version>,         // <-- optional
  "io.github.andyglow" %% "scala-jsonschema-spray-json" % <version>,        // <-- optional
  "io.github.andyglow" %% "scala-jsonschema-circe-json" % <version>,        // <-- optional
  "io.github.andyglow" %% "scala-jsonschema-json4s-json" % <version>,       // <-- optional
  "io.github.andyglow" %% "scala-jsonschema-ujson" % <version>,             // <-- optional
  // joda-time support
  "io.github.andyglow" %% "scala-jsonschema-joda-time" % <version>,         // <-- optional
  // cats support
  "io.github.andyglow" %% "scala-jsonschema-cats" % <version>,              // <-- optional
  // refined support
  "io.github.andyglow" %% "scala-jsonschema-refined" % <version>,           // <-- optional
  // zero-dependency json and jsonschema parser
  "io.github.andyglow" %% "scala-jsonschema-parser" % <version>             // <-- optional
)
```
