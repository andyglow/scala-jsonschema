package com.github.andyglow.jsonschema.derivation

/** Enums (Scala 2's `UEnums`). STREAM 2.
  *
  * TODO(stream 2): [[deriveEnum]] for
  *   - all-case-object sealed families: a string enum of the object names, or `ToValue`-driven values with `@typeHint` (inventory T6, quirk 10); `Flag.EnumsAsOneOf` -> oneOf of consts (G1);
  *   - Scala 3 `enum`s: singleton cases only -> string enum (v1 scope); parameterized cases are products, so a mixed enum is a sum ([[Sums]]);
  *   - `scala.Enumeration`: val names in declaration order (quirk 12);
  *   - Java enums (`java.time.DayOfWeek`, `TimeUnit`): string enum of the constants (bugs-1 decision 3A).
  * Keep per value: its name, its kind (case object / enum case / Enumeration value / Java constant) and a reference to the value (profiledesign fact 2). Map keys (stream 3) need the names in
  * declaration order for `^(?:A|B)$` (quirk 13), so expose them.
  */
trait Enums { this: Derivation =>
  import q.reflect.*

  /** TODO(stream 2). */
  def deriveEnum(tpe: TypeRepr, ctx: Ctx): Option[Node] = None

  /** The value names of `tpe` if it is an enum (all-case-object family, Scala 3 `enum` of singletons, `scala.Enumeration#Value`, Java enum), in declaration order; `None` otherwise.
    *
    * TODO(stream 2): implement. Stream 3 codes against it for string-enum map keys (`^(?:A|B)$`, quirk 13), so keep the signature (reviewer3 N3).
    */
  def enumValueNames(tpe: TypeRepr): Option[List[String]] = None

  /** For a `Map[K, _]` key (stream 3's [[Collections]]; contract agreed between streams 2 and 3): `None` if `tpe` isn't an enum; `Right(names)` for an enum of strings, its value names in declaration
    * order (Scala 2's `knownString`s); `Left(why)` for an enum that can't be a key, `why` being "is an enum, but with Flag.EnumsAsOneOf its members aren't plain strings" or "is an enum of non-string
    * values" (s2fix's texts).
    *
    * TODO(stream 2): implement (this stub keeps every enum key unsupported).
    */
  def enumKeyNames(tpe: TypeRepr, ctx: Ctx): Option[Either[String, List[String]]] = None
}
