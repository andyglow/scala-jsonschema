# Getting Started

## SBT

### Main:

```scala
libraryDependencies += "com.github.andyglow" %% "scala-jsonschema" % <version> // <-- required
```

### Modules

```scala
libraryDependencies ++= Seq(
  "com.github.andyglow" %% "scala-jsonschema-core" % <version>,              // <-- transitive
  "com.github.andyglow" %% "scala-jsonschema-macros" % <version> % Provided, // <-- transitive
  // json bridge. pick one
  "com.github.andyglow" %% "scala-jsonschema-play-json" % <version>,         // <-- optional
  "com.github.andyglow" %% "scala-jsonschema-spray-json" % <version>,        // <-- optional
  "com.github.andyglow" %% "scala-jsonschema-circe-json" % <version>,        // <-- optional
  "com.github.andyglow" %% "scala-jsonschema-json4s-json" % <version>,       // <-- optional
  "com.github.andyglow" %% "scala-jsonschema-ujson" % <version>,             // <-- optional
  // joda-time support
  "com.github.andyglow" %% "scala-jsonschema-joda-time" % <version>,         // <-- optional
  // cats support
  "com.github.andyglow" %% "scala-jsonschema-cats" % <version>,              // <-- optional
  // refined support
  "com.github.andyglow" %% "scala-jsonschema-refined" % <version>,           // <-- optional
  // zero-dependency json and jsonschema parser
  "com.github.andyglow" %% "scala-jsonschema-parser" % <version>             // <-- optional
)
```
