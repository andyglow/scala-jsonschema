# scala-jsonschema

Generates JSON Schema from Scala types at compile time (Scala 2 macros), in the spirit of Coursera's AutoSchema.
A macro turns a type into a JSON-library-neutral `json.Schema[T]` tree; renderers turn that tree into
Draft 04 / 06 / 07 / 2019-09 / 2020-12 JSON, and optional bridges convert it to play-json, circe, etc.

Scala 2.12 and 2.13 only. Scala 3 and Scala.js are not supported yet. If they're added, they'll most likely be added together.

## Layout

```
core/      json.Schema ADT, json.schema.* annotations/Predef/validations, own JSON AST (com.github.andyglow.json.Value),
           draft renderers (com.github.andyglow.jsonschema.AsDraftXX). No dependencies.
macros/    the derivation macro: a cake of U* traits (UProductTypes, USumTypes, UImplicits, ...) building an IR, then trees
api/       artifact `scala-jsonschema`: json.Json.{schema, objectSchema, sig, auto.derived}; most macro tests live here
modules/
  play-json, spray-json, circe-json, json4s-json, u-json   bridges: render a schema into that library's AST
  joda-time, cats, refined, enumeratum                     Predefs/support for those types (refined has its own macro)
  derived                                                  DerivedSchema[T] type class
  parser                                                   the reverse direction: strict JSON parser + JSON Schema (draft-04 subset) → json.Schema
```

## The core design rule

For each type it meets, the macro looks for an implicit in this order:

1. an implicit `json.Schema[T]`, which becomes a **definition + `$ref`**;
2. otherwise an implicit `json.schema.Predef[T]`, which is **inlined** (all built-ins such as String, numbers, java.time, collections are Predefs);
3. otherwise it derives the schema from the type's structure (case classes, sealed families, value classes, enums).

Whether a type becomes a definition depends on which of the two implicits is in scope. Keep this distinction intact when changing derivation or adding type support.

## Build & test

- JDK 11 matches CI. JDK 17+ works as well.
- `sbt test` runs on Scala 2.12 by default. For another version: `sbt "++2.13.18" test`, or set `SCALA_VER=2.13`. Versions live in `project/ScalaVer.scala`.
- Before calling something done, run `clean test` on **both** 2.12 and 2.13, plus `scalafmtCheckAll`.
- **Run `clean` after editing anything in `macros/`.** Incremental compilation doesn't recompile macro call sites, so stale tests can pass or fail misleadingly.
- `build.sbt` restricts compilation to one module at a time (`Tags.limit(Tags.Compile, 1)`). The macro evaluates `Option` default values with `c.eval`, and that races when several modules compile in parallel. Don't remove the restriction until the macro is fixed.
- Coverage: `sbt "++2.13.18" clean coverage test coverageAggregate`. CI runs this on 2.13.

## Conventions

- Format with scalafmt (`.scalafmt.conf`: maxColumn 200, aggressive alignment). Put reformat-only commits in `.git-blame-ignore-revs`.
- Keep the build warning-free on both Scala versions.
- Code that differs per Scala version goes in `src/{main,test}/scala-2.12` / `scala-2.13`, or `scala-2.12-` / `scala-2.13+` (the custom dirs are wired up in `build.sbt`).
  `com.github.andyglow.scalamigration` holds the small collection shims.
- `.github/workflows/*.yml` are **generated** by sbt-github-actions. Change `build.sbt` or `project/CustomGithubActions.scala`, then run `sbt githubWorkflowGenerate`. CI fails if they're out of sync (`githubWorkflowCheck`).
- Renderer tests compare full JSON output per draft (`api/src/test/.../AsDraftXXSpec.scala`). When output changes on purpose, update the expected JSON. When it changes by accident, it's a bug.
- Scala 2.12 decodes `\uXXXX` anywhere in source, including comments and triple-quoted strings. Write JSON unicode escapes in test inputs as `"\\u0041"`.

## Release & dependencies

- Versioning is PVP (`versionScheme := pvp`). Breaking changes bump the second number (0.7.x → 0.8.0).
- Publishing uses sbt's built-in Sonatype Central Portal support: `sbt release` runs `publishSigned` (cross-built, staged into `target/sona-staging`) and then `sonaRelease`. This needs a PGP key and a Central Portal token.
- Scala Steward opens dependency PRs. `master` requires a code-owner review.
