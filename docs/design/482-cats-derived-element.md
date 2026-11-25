# cats collections of an element without an implicit  (issue #482, item E1)
Status: draft, rewritten 2026-11-24: depends on #542 (semi-auto derivation by default, `Exported` auto)

## Problem
The cats instances (`CatsSupport._`) are Predef constructors: they build a `Predef[NonEmptyList[T]]` from an implicit `Predef[T]` or `Schema[T]` (since 0.2.5,
commit 4003b4b2). When the element has no implicit, no instance applies, and the macro derives the cats type from its structure. Measured on master `5b24a80b`
(`case class P(a: Int)`, `import CatsSupport._`):

| in scope | Scala 2.13 | Scala 3.3 |
|---|---|---|
| nothing | `NonEmptyList[P]`: object `{head, tail}` | `NonEmptyList[P]`: `{head, tail}`; `NonEmptyVector[P]`: "is not supported" |
| `import json.Json.auto._` | array, `minItems: 1` (correct) | `NonEmptyList[P]`: `{head, tail}`; `NonEmptyVector[P]`: "is not supported" |
| `implicit val ps: Schema[P]` | array, `minItems: 1`, `P` inlined | the same |

The other types:
- `Chain[P]` gives a `oneOf` of Chain's internals.
- `NonEmptySet`, `NonEmptyChain`, `NonEmptyMap` and `NonEmptyLazyList` are newtypes, and give "is not supported".
- `OneAnd[List, P]` and 2.12's `NonEmptyStream[P]` give `{head, tail}`.

Two more bugs, with any element:
- `nesSchema`, the `Schema[T]` variant, calls `mkNEx(ss)` without `unique = true`, so `NonEmptySet[X]` with an implicit `Schema[X]` has no `uniqueItems`.
- `Chain[T]` (both variants) gets `minItems: 1`, but a `Chain` can be empty.

## The rules (owner, 2026-11-24)
| in scope | result, the same on Scala 2 and 3 |
|---|---|
| neither `Predef[P]` nor `Schema[P]`, no auto | compile error naming `P`, suggesting an instance or `import json.Json.auto._` |
| `import json.Json.auto._` | `P`'s own `Predef` or `Schema` if one exists, otherwise `P` is derived |
| `Predef[P]` or `Schema[P]` | used, as today |

## Decision
These rules are the general semi-auto / auto model of #542 (`docs/design/542-semi-auto-derivation.md`), applied to cats. #542 fixes them for every container,
with no cats knowledge in the macros:
- **Without an instance:** `NonEmptyList[P]` as a nested type has none, so it is the semi-auto error, which names `P`, because the search for
  `Predef[NonEmptyList[P]]` failed.
- **With auto:** `nelSchemaFromPredef(importedPredef(auto.derived[P]))` resolves on both versions (#542, option A.4).

This item keeps only what is specific to the cats module.

The previous designs of this doc lost:
- the macro probing for a Predef constructor with marker types and deriving the element itself (the owner: no derivation of the element without an instance or the
  auto import);
- a `Container[T]` marker (not needed once the default is semi-auto).

## Implementation plan (after #542)
1. **`CatsSupport.scala`:**
   - `nesSchema` passes `unique = true`.
   - `chainSchemaFromPredef` / `chainSchema` build a plain `` `array`[T, Chain] `` without `minItems`.
   - The `Schema[T]` variants stay implicit, so an element with an implicit `Schema` stays inlined. Whether it should be a `$ref` is a separate question, out of
     scope here.
2. **Tests** in `modules/cats` (shared, plus the version-specific specs for `NonEmptyStream` / `NonEmptyLazyList`):
   - the three rules for `NonEmptyList`, `NonEmptyVector`, `NonEmptySet`, `NonEmptyChain`, `NonEmptySeq`, `Chain`, `NonEmptyMap[String, P]` and `OneAnd[List, P]`, as
     a root and as a field;
   - the exact error message;
   - `Chain[String]` has no `minItems`;
   - `NonEmptySet[X]` with an implicit `Schema[X]` has `uniqueItems`.
3. **Recursion through a container under auto**, e.g. `case class Tree(v: Int, kids: NonEmptyList[Tree])`. The element is derived by a separate `auto.derived[Tree]`
   expansion, inside the derivation of `Tree`. Add a test. If it loops, the fix belongs in #542's recursion handling (a shared recursion stack across auto
   expansions); this item only adds the case.
4. **Docs:** README "Cats" and paradox `modules/cats.md`: the three rules, with examples.

## Compatibility
- **Output:**
  - `Chain[T]` loses `minItems: 1`.
  - `NonEmptySet[T]` with an implicit `Schema[T]` gains `uniqueItems: true`.
  - Both are corrections, so under Legacy too (Legacy policy).
- Everything else comes from #542.
- **PVP:** part of 0.10.0 with #542.

## Review log
- Round 1 (r-e1, 2026-11-23) reviewed the earlier marker-probe design, which the owner has since replaced. Its open points are moot here or moved to #542 (auto
  parity, precedence).
