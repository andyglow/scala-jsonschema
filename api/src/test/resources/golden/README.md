# Golden files

Written and checked by `com.github.andyglow.jsonschema.harness.Golden` (api test sources). Each `<name>.json` holds:

- `schema`: a lossless dump of the `json.Schema` value. Field order is the Schema's field order. Set members
  (oneOf/allOf members, enum values, validations) are sorted, since the Schema value has no order for them.
- `draft-04`, `draft-07`, `draft-2020-12`: the rendered JSON with object keys and arrays sorted (canonical form).

## Base files and per-version overrides

The base file `<name>.json` is the **target** behavior, the one every Scala version (including the Scala 3 macro)
must produce. A file `<name>@2.12.json` / `<name>@2.13.json` / `<name>@3.json` overrides it on that Scala version
only. Overrides record known differences that are bugs of that version, to be fixed later. There are no overrides
at the moment. (The last ones, for the Scala 2 scaladoc leak where an undocumented class took the nearest preceding
doc comment in its file, went away with scaladoc-parser 0.1.1.)

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
