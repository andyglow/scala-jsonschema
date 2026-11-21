# Golden files

Written and checked by `com.github.andyglow.jsonschema.harness.Golden` (api test sources). Each `<name>.json` holds:

- `schema`: a lossless dump of the `json.Schema` value. Field order is the Schema's field order. Set members
  (oneOf/allOf members, enum values, validations) are sorted, since the Schema value has no order for them.
- `draft-04`, `draft-07`, `draft-2020-12`: the rendered JSON with object keys and arrays sorted (canonical form).

## Base files and per-version overrides

The base file `<name>.json` is the **target** behavior, the one every Scala version (including the Scala 3 macro)
must produce. A file `<name>@2.12.json` / `<name>@2.13.json` / `<name>@3.json` overrides it on that Scala version
only. Overrides record known differences that are bugs of that version, to be fixed later. Current overrides:

- `shared/DocumentationSpec.FromSpec.objectSchema@2.12`, `@2.13` and `char/scaladoc/Undocumented@2.12`, `@2.13`:
  the Scala 2 macro reads scaladoc from the source text, and a class WITHOUT a doc comment picks up the nearest
  preceding doc comment in its file. The override holds that leaked description.

The two base files above were made by hand (the override with the leaked description removed): no compiler has
produced them yet. They become real once a Scala version derives these classes without the leak.

## Regenerating

```
GOLDEN_UPDATE=1 sbt clean api/test       # or -Dgolden.update=true
git diff api/src/test/resources/golden   # review every change
```

- Update mode writes to the file in use (the override if one exists for the current Scala version, else the base).
  To add an override, copy the base file to `<name>@<version>.json` and run update mode on that version.
- **Use a CLEAN build.** Scala 2 reads scaladoc only from sources compiled in the same run. After an incremental
  compile that recompiled the spec but not the model's file, the descriptions are silently missing.
- Without the variable, a missing or different golden file fails the test.
