package com.github.andyglow.jsonschema.derivation

/** Collections and dictionaries (Scala 2's `UArrays`, `UDictionaries`). STREAM 3.
  *
  * Most collections of built-ins never get here: `Predef` has `List`/`Set`/`Vector`/`Array`/`Map` instances for elements that have a `Predef`, so they are inlined by the implicit lookup. These
  * families handle the rest (elements that are derived, e.g. `List[MyCaseClass]`).
  *
  * TODO(stream 3):
  *   - [[deriveDictionary]]: `Map[K, V]` (Scala 2: `<: Map[_, _]`) -> `dictionary`; keep the key's kind in the IR (String | integral | Char | UUID | value class | string enum: profiledesign fact 6)
  *     besides the `patternProperties` validation (a [[Nodes.Validation]] in the node's `extra`); key patterns per inventory T10 / quirk 13; an unsupported key -> a clear compile error naming the key
  *     type (TRIAGE-B);
  *   - [[deriveArray]]: `Array` / `Iterable` -> `array`, unique iff `<: immutable.Set`; non-generic iterables and parameterless aliases (`type Users = List[User]`, `BitSet`) -> no crash (TRIAGE-B).
  *     The element is `resolve(elem, ctx.descend(tpe))`.
  *   - `sig` alias policy for collections: see [[Signatures]] (`Scala2Spelling`).
  */
trait Collections { this: Derivation =>
  import q.reflect.*

  /** TODO(stream 3). */
  def deriveDictionary(tpe: TypeRepr, ctx: Ctx): Option[Node] = None

  /** TODO(stream 3). */
  def deriveArray(tpe: TypeRepr, ctx: Ctx): Option[Node] = None
}
