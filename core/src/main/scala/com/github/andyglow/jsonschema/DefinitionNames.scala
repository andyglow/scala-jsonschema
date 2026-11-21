package com.github.andyglow.jsonschema

import json.Schema
import json.Schema._

/** Names of definitions in a rendered document.
  *
  * A definition's signature (`Json.sig`, `toDefinition(name)`) is a readable type name such as `com.acme.Box[scala.Int]`. Each draft limits the characters a definition name may use where it appears
  * in `$id`, `$anchor` or `$ref`, so the renderers replace every character that doesn't conform with `_` (owner decision 2026-11-20). The signature itself never changes.
  *
  * Collisions are resolved deterministically: a signature that already conforms keeps its name; the others, in sorted signature order, get the sanitized name, or `<name>_2`, `<name>_3`, ... if that's
  * taken.
  */
private[jsonschema] object DefinitionNames {

  /** Draft 04: `"$ref": "#/definitions/<name>"` must be a URI fragment holding a JSON pointer. Keeps unreserved characters, sub-delims, ':' and '@' (RFC 3986 `pchar`), minus '~' and '/' (JSON pointer
    * escapes).
    */
  def uriFragment(sig: String): String = replace(sig)(c => isAsciiLetterOrDigit(c) || "-._!$&'()*+,;=:@".indexOf(c.toInt) >= 0)

  /** Drafts 06, 07 (`"$id": "#<name>"`, a plain-name fragment) and 2019-09 (`$anchor`): `^[A-Za-z][-A-Za-z0-9.:_]*$`. */
  def plainName(sig: String): String = {
    val s = replace(sig)(c => isAsciiLetterOrDigit(c) || "-.:_".indexOf(c.toInt) >= 0)
    if (s.nonEmpty && isAsciiLetter(s.head)) s else "T" + s
  }

  /** Draft 2020-12 `$anchor`: `^[A-Za-z_][-A-Za-z0-9._]*$`. */
  def anchor2020(sig: String): String = {
    val s = replace(sig)(c => isAsciiLetterOrDigit(c) || "-._".indexOf(c.toInt) >= 0)
    if (s.nonEmpty && (isAsciiLetter(s.head) || s.head == '_')) s else "_" + s
  }

  /** The name of each definition/ref signature in `root`, made valid by `sanitize`. */
  def assign(root: Schema[_], sanitize: String => String): Map[String, String] = {
    val all                  = signatures(root).toList.sorted
    val (conforming, others) = all.partition(s => sanitize(s) == s)
    var used                 = conforming.toSet
    val assigned             = others map { sig =>
      val base = sanitize(sig)
      var name = base
      var i    = 2
      while (used contains name) { name = s"${base}_$i"; i += 1 }
      used += name
      sig -> name
    }
    conforming.map(s => s -> s).toMap ++ assigned
  }

  def signatures(x: Schema[_]): Set[String] = x match {
    case `def`(sig, t)    => signatures(t) + sig
    case `ref`(sig)       => Set(sig)
    case o: `object`[_]   => o.fields.flatMap(f => signatures(f.tpe)).toSet
    case `array`(t, _)    => signatures(t)
    case `dictionary`(t)  => signatures(t)
    case `oneof`(ts, _)   => ts.flatMap(signatures)
    case `allof`(ts)      => ts.flatMap(signatures)
    case Schema.`not`(t)  => signatures(t)
    case `value-class`(t) => signatures(t)
    case `option`(t)      => signatures(t)
    case _                => Set.empty
  }

  private def isAsciiLetter(c: Char): Boolean                   = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  private def isAsciiLetterOrDigit(c: Char): Boolean            = isAsciiLetter(c) || (c >= '0' && c <= '9')
  private def replace(s: String)(keep: Char => Boolean): String = s.map(c => if (keep(c)) c else '_')
}
