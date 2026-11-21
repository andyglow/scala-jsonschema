# scala-jsonschema

Generates JSON Schema from Scala types at compile time (Scala 2 macros, Scala 3 quotes), in the spirit of Coursera's AutoSchema.
A macro turns a type into a JSON-library-neutral `json.Schema[T]` tree; renderers turn that tree into
Draft 04 / 06 / 07 / 2019-09 / 2020-12 JSON, and optional bridges convert it to play-json, circe, etc.

Cross-built for Scala 2.12, 2.13 and 3.3 LTS (3.3 from 0.8.0, together with `derives json.Schema` and Scala 3
`enum`s). Every module and every shared test runs on all three; `refined` and `derived` have their own Scala 3
macros. Scala.js is deferred. User docs: README.md (the main reference) and the paradox site in `docs/`. They say
the same things, mostly in the same words: a user-visible change updates both (and CHANGELOG.md).

## Layout

```
core/      json.Schema ADT, json.schema.* annotations/Predef/validations, own JSON AST (com.github.andyglow.json.Value),
           draft renderers (com.github.andyglow.jsonschema.AsDraftXX). No dependencies on Scala 2; on Scala 3 only
           scaladoc-parser (the Scala 3 derivation lives here and parses `Symbol.docstring`).
macros/    the Scala 2 derivation macro (src/main/scala-2; empty on Scala 3): a cake of U* traits (UProductTypes, USumTypes, UImplicits, ...) building an IR, then trees
core/src/main/scala-3/com/github/andyglow/jsonschema/derivation/
           the Scala 3 derivation (quotes), in core because `derives json.Schema` needs `Schema.derived`. A cake of traits,
           one file per concern/type family: IR (Nodes) -> recursion pass -> emit. Guide: notes/reports/skeleton-ir.md
api/       artifact `scala-jsonschema`: json.Json.{schema, objectSchema, sig, auto.derived}; most macro tests live here
modules/
  play-json, spray-json, circe-json, json4s-json, u-json   bridges: render a schema into that library's AST
  joda-time, cats, refined, enumeratum                     Predefs/support for those types (refined has its own macro)
  derived                                                  DerivedSchema[T] type class
  parser                                                   the reverse direction: strict JSON parser + JSON Schema (draft-04 subset) → json.Schema
docs/      the documentation site (paradox + mdoc): docs/src/main/paradox. Not aggregated by root, not published
```

Tests: shared specs under `api/src/test/scala`, Scala 3-only ones under `scala-3/`. Golden files
(`api/src/test/resources/golden`, harness in `api/src/test/.../harness/Golden.scala`) hold the target output per model;
`<name>@<ver>.json` overrides one Scala version. Regenerate with `GOLDEN_UPDATE=1 sbt clean api/test` (a CLEAN build,
or Scala 2 scaladoc is silently missing) and review the diff. Tags `TRIAGE-A/B/C`, `UNTRIAGED` (`harness/Triage.scala`)
mark tests of decided-but-not-current behavior.

## The core design rule

For each type it meets, the macro looks for an implicit in this order:

1. an implicit `json.Schema[T]`, which becomes a **definition + `$ref`**;
2. otherwise an implicit `json.schema.Predef[T]`, which is **inlined** (all built-ins such as String, numbers, java.time, collections are Predefs);
3. otherwise it derives the schema from the type's structure (case classes, sealed families, value classes, enums).

Whether a type becomes a definition depends on which of the two implicits is in scope. Keep this distinction intact when changing derivation or adding type support.

## Build & test

- Build and CI use **JDK 21**. The bytecode and Java API level are pinned to Java 8 (`-release:8` / `-release 8` in
  `project/CompilerOptions.scala`), so artifacts are classfile 52 and still run on Java 8+. Don't use Java 9+ APIs.
- Scala versions: 2.12.21, 2.13.18 and 3.3.9 (`project/ScalaVer.scala`). `sbt test` runs on 2.12 by default.
  For another version: `sbt "++2.13.18" test` / `sbt "++3.3.9" test`, or set `SCALA_VER=2.13` / `SCALA_VER=3`.
- Before calling something done, run `clean test` on 2.12, 2.13 **and** 3.3.9, plus `scalafmtCheckAll` under each
  (it only checks the current version's source dirs) and `scalafmtSbtCheck`. sbt-scalafmt caches results across
  `clean`: `scalafmtCheckAll` can pass on files it never re-checked. Touch the sources (or use a fresh checkout) before trusting it.
- Docs: `sbt makeDocs` (switches to 2.13, runs mdoc, which compiles the ```` ```scala mdoc:compile-only ```` snippets
  against the modules, then paradox; output `docs/target/site`). Internal links must be `@ref:[text](page.md)`. mdoc wraps
  snippets in a class, so value classes and (Scala 2) case classes with defaults can't be `mdoc` snippets; keep those plain
  `scala` and verify them some other way. Scala 3-only snippets are plain `scala` too.
- **Run `clean` after editing anything in `macros/` or `core/src/main/scala-3/.../derivation`.** Incremental compilation doesn't recompile macro call sites, so stale tests can pass or fail misleadingly.
- Scala 3 test builds compile with `-Yread-docs` (scaladoc from TASTy, as users need for models in other modules) and
  `-Xcheck-macros` (except play-json: its own macro fails the check).
  To see the code the Scala 3 macro generates: `sbt -Djsonschema.print-code=true ...` (prints every expansion as an info).
- `build.sbt` restricts compilation to one module at a time (`Tags.limit(Tags.Compile, 1)`). The macro evaluates `Option` default values with `c.eval`, and that races when several modules compile in parallel. Don't remove the restriction until the macro is fixed.
- Coverage: `sbt "++2.13.18" clean coverage test coverageAggregate`. CI runs this on 2.13.
- git worktrees: `build.sbt` sets `useReadableConsoleGit`, because sbt-git's JGit reader (pulled in by sbt-ghpages)
  can't load the build in a `git worktree` checkout. Keep it.

## Conventions

- Format with scalafmt (`.scalafmt.conf`: maxColumn 200, aggressive alignment). Put reformat-only commits in `.git-blame-ignore-revs`.
- Keep the build warning-free on every Scala version.
- Source dirs per Scala version (plain `crossScalaVersions`, no projectmatrix):
  - `src/{main,test}/scala`: shared by 2.12, 2.13 and 3. Put code here unless it can't compile everywhere.
  - `scala-2.12-` / `scala-2.13+`: collection-API shims. `scala-2.13+` is used by 2.13 **and** 3 (wired up in
    `build.sbt` via `versionRangeDirs`). `com.github.andyglow.scalamigration` holds the small collection shims.
  - `scala-2` / `scala-3` (sbt built-ins): language differences, i.e. Scala 2 macros vs Scala 3 inline/quotes.
  - Don't use the exact-binary `scala-2.12` / `scala-2.13` / `scala-3.x` dirs for shims: a 2.13 shim would have to be
    copied for Scala 3 (that's how an old `core/src/main/scala-3` copy went stale).
- `.github/workflows/*.yml` are **generated** by sbt-github-actions. Change `build.sbt` or `project/CustomGithubActions.scala`, then run `sbt githubWorkflowGenerate`. CI fails if they're out of sync (`githubWorkflowCheck`).
- Renderer tests compare full JSON output per draft (`api/src/test/.../AsDraftXXSpec.scala`). When output changes on purpose, update the expected JSON. When it changes by accident, it's a bug.
- Scala 2.12 decodes `\uXXXX` anywhere in source, including comments and triple-quoted strings. Write JSON unicode escapes in test inputs as `"\\u0041"`.

## Release & dependencies

- groupId is `io.github.andyglow` from 0.8.0 on (0.7.x and older: `com.github.andyglow`). Only the sbt coordinates
  changed; Scala packages stay `com.github.andyglow.*` and `json.*`. The scaladoc libraries moved the same way.
- Versioning is PVP (`versionScheme := pvp`). Breaking changes bump the second number (0.7.x → 0.8.0).
- Publishing uses sbt's built-in Sonatype Central Portal support: `sbt release` runs `publishSigned` (cross-built, staged into `target/sona-staging`) and then `sonaRelease`. This needs a PGP key and a Central Portal token.
- Scala Steward opens dependency PRs. `master` requires a code-owner review.
