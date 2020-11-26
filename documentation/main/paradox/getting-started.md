# Getting Started

`scala-jsonschema` is a set of modules.

There are required ones:

- Core `scala-jsonschema-core` 
- Macro `scala-jsonschema-macro` (depends on `core`)
- API `scala-jsonschema` (depends on `macro`, `core`)

and optional ones.

Optional, in turn, are separated into 2 categories:

- `integrations` for integration with scala json libraries
- `extensions` for support for specific types

@@@ note

It's not necessary to pull in all 3 required libraries. 

You pull in `scala-jsonschema` and it pulls in the rest transitively.

@@@

## Install

<div 
    id="dependency-selector"
    x-version="@VERSION@"
    x-scala-version="@SCALA_VERSION@"></div>

<hr/>
     
@@dependency[sbt,Maven,Gradle] {
  group="foo"
  artifact="bar_$scala.binary.version$"
  version="x.x.x"
}