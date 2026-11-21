package com.github.andyglow.jsonschema.derivation

import scala.quoted.*

import com.github.andyglow.json.{ToValue, Value}
import json.Schema

/** Products: case classes and tuples -> `object` (Scala 2's `UProductTypes`).
  *
  * Fields are the first parameter list, in declaration order. Field types have the case class's type arguments substituted (`memberType`) and are dealiased at the top level (Scala 2 quirk 2). An
  * `Option[X]` field becomes an [[Opt]] around X, emitted as X and not required. A default makes the field not required; its value is computed at RUNTIME by calling the companion's default getter
  * (`$lessinit$greater$default$N`) and converted with `ToValue`. So `= None` emits no default, without any compile-time evaluation (TRIAGE-A).
  *
  * Owner: skeleton (fields, Option, defaults) + stream 1 (field annotations, descriptions, RW mode, default/ToValue errors). Stream 1 extension points: [[fieldDescriptions]], [[fieldRWMode]],
  * [[optionDefault]].
  */
trait Products { this: Derivation =>
  import q.reflect.*

  /** What kind of product the object stands for (profiledesign fact 4). */
  enum ProductKind {
    case CaseClass
    case Tuple(arity: Int)
  }

  enum RWMode {
    case ReadWrite, ReadOnly, WriteOnly
  }

  /** A field of an [[Obj]].
    * @param name
    *   the (decoded) parameter name
    * @param tpe
    *   the declared type, type arguments substituted, dealiased at the top level
    * @param schema
    *   the field type's node; an [[Opt]] for `Option` fields
    * @param optional
    *   the field is an `Option` (kept apart from `required` and `hasDefault`: profiledesign fact 1)
    * @param hasDefault
    *   the parameter has a default
    * @param default
    *   the default value, computed at runtime (`None` at runtime for `= None`)
    * @param annotations
    *   the annotations of the constructor parameter and of the field
    */
  final case class Field(
    name: String,
    tpe: TypeRepr,
    schema: Node,
    optional: Boolean,
    hasDefault: Boolean,
    default: Option[Expr[Option[Value]]],
    description: Option[String],
    rwMode: RWMode,
    annotations: List[Term]
  ) {
    def required: Boolean = !optional && !hasDefault

    /** The node whose schema the field renders: the Option's element for `Option` fields (v1 rendering). */
    def valueNode: Node = schema match {
      case o: Opt if optional => o.inner
      case other              => other
    }
  }

  final case class Obj(tpe: TypeRepr, kind: ProductKind, fields: List[Field], extra: Extra = Extra()) extends Node {
    def withExtra(x: Extra): Node          = copy(extra = x)
    def children: List[Node]               = fields.map(_.schema)
    def mapChildren(f: Node => Node): Node = copy(fields = fields.map(fl => fl.copy(schema = f(fl.schema))))
    def emitRaw: Expr[Schema[?]]           = tpe.asType match {
      case '[t] =>
        val fs = Expr.ofList(fields.map(emitField))
        '{ Schema.`object`.fromList[t]($fs) }
    }
  }

  def emitField(f: Field): Expr[Schema.`object`.Field[?]] = f.valueNode.tpe.asType match {
    case '[ft] =>
      val schema = emit(f.valueNode).asExprOf[Schema[ft]]
      val name   = Expr(f.name)
      val req    = Expr(f.required)
      val base   = f.default match {
        case Some(d) => '{ Schema.`object`.Field.fromJson[ft]($name, $schema, $req, $d) }
        case None    => '{ Schema.`object`.Field[ft]($name, $schema, $req) }
      }
      val withDescr = f.description.fold(base) { d =>
        val de = Expr(d)
        '{ $base.withDescription(Some($de)) }
      }
      f.rwMode match {
        case RWMode.ReadWrite => withDescr
        case RWMode.ReadOnly  => '{ $withDescr.withRWMode(Schema.`object`.Field.RWMode.ReadOnly) }
        case RWMode.WriteOnly => '{ $withDescr.withRWMode(Schema.`object`.Field.RWMode.WriteOnly) }
      }
  }

  // ---- construction

  /** A constructor parameter of a product type. `declared` has the owner's type arguments substituted; `tpe` is `declared` dealiased at the top level (Scala 2 dealiases field types). */
  final case class Param(name: String, sym: Symbol, declared: TypeRepr, index: Int, annotations: List[Term]) {
    def tpe: TypeRepr = declared.dealias
  }

  /** The first parameter list of `tpe`'s primary constructor, with field types substituted and dealiased. */
  def params(tpe: TypeRepr): List[Param] = {
    val cls   = classSymbol(tpe)
    val owner = tpe.dealias
    val first = cls.primaryConstructor.paramSymss.filterNot(_.headOption.exists(_.isTypeParam)).headOption getOrElse Nil
    first.zipWithIndex map { (p, i) =>
      val field = cls.fieldMember(p.name)
      val ft    = if (field.isNoSymbol) owner.memberType(p) else owner.memberType(field)
      val ann   = annotationsOf(p) ++ annotationsOf(field)
      Param(p.name, p, ft, i, ann)
    }
  }

  def deriveProduct(tpe: TypeRepr, ctx: Ctx): Option[Node] =
    Option.when(isCaseClass(tpe))(product(tpe, ctx))

  def product(tpe: TypeRepr, ctx: Ctx): Obj = {
    val cls = classSymbol(tpe)
    if (cls.flags.is(Flags.Abstract)) report.errorAndAbort(s"No apply function found for ${fullName(cls)}")
    val ps     = params(tpe)
    val descrs = fieldDescriptions(tpe, ps, ctx)
    val fields = ps map { p =>
      val inner = optionElement(p.tpe)
      val node  = inner match {
        case Some(e) => Opt(p.tpe, resolve(e, ctx.descend(tpe)))
        case None    => resolve(p.tpe, ctx.descend(tpe))
      }
      val hasDefault = p.sym.flags.is(Flags.HasDefault)
      Field(
        name = p.name,
        tpe = p.tpe,
        schema = node,
        optional = inner.isDefined,
        hasDefault = hasDefault,
        default = Option.when(hasDefault)(defaultValue(tpe, p, inner)),
        description = descrs.get(p.name),
        rwMode = fieldRWMode(p),
        annotations = p.annotations
      )
    }
    val kind = if (defn.isTupleClass(cls)) ProductKind.Tuple(fields.size) else ProductKind.CaseClass
    Obj(tpe, kind, fields)
  }

  /** `Some(X)` if `tpe` is an `Option[X]` (Scala 2: `<:< Option[_]`). */
  def optionElement(tpe: TypeRepr): Option[TypeRepr] =
    if (tpe <:< TypeRepr.of[Option[Any]]) tpe.baseType(TypeRepr.of[Option[Any]].typeSymbol).typeArgs.headOption
    else None

  /** Field descriptions by field name. Skeleton: only the `Json.objectSchema` decorations, which apply to the root only (and must name its fields).
    *
    * TODO(stream 1): scaladoc `@param` and `@description`, precedence objectSchema config > scaladoc `@param` > `@description` (inventory F7), validation of every key on the root (F8; TRIAGE-B: the
    * same rule everywhere).
    */
  def fieldDescriptions(tpe: TypeRepr, ps: List[Param], ctx: Ctx): Map[String, String] =
    if (!ctx.isRoot) Map.empty
    else {
      objectSchemaDecorations foreach { (k, _) =>
        if (!ps.exists(_.name == k)) report.errorAndAbort(s"unknown field: $k. ${tpe.show} fields are: ${ps.map(_.name).mkString(", ")}")
      }
      objectSchemaDecorations.toMap
    }

  /** TODO(stream 1): `@readOnly` / `@writeOnly` (both = neither, inventory F11). */
  def fieldRWMode(p: Param): RWMode = RWMode.ReadWrite

  // ---- defaults

  /** The runtime default of `p`: a call of the companion's default getter, converted with `ToValue`. */
  def defaultValue(owner: TypeRepr, p: Param, optionOf: Option[TypeRepr]): Expr[Option[Value]] = {
    val call = defaultGetterCall(owner, p)
    optionOf match {
      case Some(e) => optionDefault(owner, p, call, e)
      case None    =>
        p.tpe.asType match {
          case '[f] =>
            val toV = Expr.summon[ToValue[f]] getOrElse noToValue(owner, p)
            val v   = call.asExprOf[f]
            '{ Some($toV($v)) }
        }
    }
  }

  /** The default of an `Option[E]` field: `None` -> no default; otherwise its `ToValue[Option[E]]` (Scala 2: a bridge's Option encoding), else `ToValue[E]` of the element.
    *
    * TODO(stream 1): with neither ToValue in scope, Scala 2 fails compilation unless the default is `None`. Here the default is dropped (we can't tell `None` from `Some(..)` at compile time without
    * `-Yretain-trees`). Decide: keep (document) or warn.
    */
  def optionDefault(owner: TypeRepr, p: Param, call: Term, e: TypeRepr): Expr[Option[Value]] = e.asType match {
    case '[e] =>
      val v = call.asExprOf[Option[e]]
      Expr.summon[ToValue[Option[e]]] match {
        case Some(toV) => '{ val d = $v; if (d.isEmpty) None else Some($toV(d)) }
        case None      =>
          Expr.summon[ToValue[e]] match {
            case Some(toV) => '{ $v.map(x => $toV(x)) }
            case None      => '{ None }
          }
      }
  }

  /** `Companion.$lessinit$greater$default$N` (applied to the owner's type arguments if it is generic). */
  def defaultGetterCall(owner: TypeRepr, p: Param): Term = {
    val companion = classSymbol(owner).companionModule
    val getter    = companion.methodMember(s"$$lessinit$$greater$$default$$${p.index + 1}").headOption getOrElse {
      report.errorAndAbort(s"no default getter found for ${owner.show}.${p.name}")
    }
    val call = companionRef(owner, companion).select(getter)
    if (getter.paramSymss.headOption.exists(_.exists(_.isTypeParam))) call.appliedToTypes(owner.dealias.typeArgs) else call
  }

  /** A reference to `owner`'s companion through `owner`'s prefix: `o.Inner` for `val o: Outer; Json.schema[o.Inner]`, `this.Inner` inside `Outer` (reviewer3 S1: a bare `Ref(companion)` is a malformed
    * tree there; Scala 2 fails to compile it). A local class has no prefix.
    */
  private def companionRef(owner: TypeRepr, companion: Symbol): Term = {
    def prefix(t: TypeRepr): Option[TypeRepr] = t match {
      case AppliedType(tc, _) => prefix(tc)
      case TypeRef(pre, _)    => Some(pre)
      case _                  => None
    }
    prefix(owner.dealias) match {
      case Some(_: NoPrefix) | None => Ref(companion)
      case Some(pre)                => Ref.term(TermRef(pre, companion.name))
    }
  }

  def noToValue(owner: TypeRepr, p: Param): Nothing = {
    val ft = p.tpe.show
    report.errorAndAbort(
      s"""Can't infer json value for default value of `${owner.show}(..., ${p.name}: $ft)`
         |Please provide `ToValue[$ft]` type class. It can also be automatically derived in case
         |- your json library knows how to convert `$ft` into json and
         |- the bridge between scala-jsonschema and your library is configured. Namely
         | - corresponding library is in classpath (eg. scala-jsonschema-spray-json, scala-jsonschema-play-json, scala-jsonschema-circe-json, etc)
         | - corresponding import is taking place (eg. `import com.github.andyglow.jsonschema.AsSpray._`, `import com.github.andyglow.jsonschema.AsPlay._`, `import com.github.andyglow.jsonschema.AsCirce._` etc)
         |""".stripMargin
    )
  }
}
