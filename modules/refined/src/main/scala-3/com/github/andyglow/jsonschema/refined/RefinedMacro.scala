package com.github.andyglow.jsonschema.refined

import json.Schema
import json.schema.Predef
import json.schema.validation.{Def, Instance, Magnet}

import scala.quoted.*

/** Scala 3 port of the Scala 2 refined macro (`src/main/scala-2/.../refined`): the same predicate table, the same normalization and the same quirks (inventory appendix C §1.1).
  *
  *   - Predicate symbols are matched before dealiasing (`Positive`, `MinSize`, `GreaterEqual`, ... are aliases in refined), then on the dealiased predicate (user aliases).
  *   - Parameters are literal types (`Greater[5]`, `MatchesRegex["[a-z]+"]`). Bounds and `multipleOf` are emitted as Doubles.
  *   - `Size[P]` takes `P`'s bound and ignores its inclusiveness (`Size[Greater[5]]` is `minLength 5`).
  *   - `And`/`Or` are matched on the raw (unnormalized) operands, `Not` nested in `allof`/`oneof` is not simplified, `A And B And C` gives nested `allof`.
  */
private[jsonschema] object RefinedMacro {

  def predef[T: Type](using Quotes): Expr[Predef[T]] = {
    val tree = schema[T]
    '{ Predef[T]($tree) }
  }

  def schema[T: Type](using Quotes): Expr[Schema[T]] = {
    val tree = new RefinedMacro().gen(Type.of[T])
    '{ $tree.asInstanceOf[Schema[T]] }
  }
}

private final class RefinedMacro(using val q: Quotes) {
  import q.reflect.*

  private def err(msg: String): Nothing = report.errorAndAbort(msg)

  private def member(module: String, name: String): Symbol = {
    val s = Symbol.requiredModule(s"eu.timepit.refined.$module").moduleClass.typeMember(name)
    if (s.exists) s else err(s"refined: eu.timepit.refined.$module.$name not found") // a refined version mismatch
  }

  private val sRefined = TypeRepr.of[_root_.eu.timepit.refined.api.Refined].typeSymbol

  // generic
  // TODO: Equal -> `const`

  // collection
  private val sMinSize  = member("collection", "MinSize")
  private val sMaxSize  = member("collection", "MaxSize")
  private val sSize     = member("collection", "Size")
  private val sEmpty    = member("collection", "Empty")
  private val sNonEmpty = member("collection", "NonEmpty")

  // string
  private val sUUID         = member("string", "Uuid")
  private val sURI          = member("string", "Uri")
  private val sURL          = member("string", "Url")
  private val sStartsWith   = member("string", "StartsWith")
  private val sEndsWith     = member("string", "EndsWith")
  private val sMatchesRegex = member("string", "MatchesRegex")
  private val sIPv4         = member("string", "IPv4")
  private val sIPv6         = member("string", "IPv6")
  private val sTrimmed      = member("string", "Trimmed")

  // numeric
  private val sPositive     = member("numeric", "Positive")
  private val sNonPositive  = member("numeric", "NonPositive")
  private val sNegative     = member("numeric", "Negative")
  private val sNonNegative  = member("numeric", "NonNegative")
  private val sGreater      = member("numeric", "Greater")
  private val sGreaterEqual = member("numeric", "GreaterEqual")
  private val sLess         = member("numeric", "Less")
  private val sLessEqual    = member("numeric", "LessEqual")
  private val sDivisable    = member("numeric", "Divisible")
  private val sNonDivisable = member("numeric", "NonDivisible")

  // boolean
  private val sNot = member("boolean", "Not")
  private val sAnd = member("boolean", "And")
  private val sOr  = member("boolean", "Or")

  // the refined type is dealiased (`type IP = String Refined IPFormat`), its predicate isn't
  def gen(tpe: Type[?]): Expr[Schema[?]] = gen(TypeRepr.of(using tpe).dealias)

  private def gen(tt: TypeRepr): Expr[Schema[?]] = tt match {
    case AppliedType(tycon, List(t, p)) if tycon.typeSymbol == sRefined =>
      ex(t, p).map(_.norm) match {
        case Some(pp) => pp.norm.tree
        case None     => err(s"Can't infer Predicate out of ${p.show}")
      }
    case _ => err(s"Can't infer Refined out of ${tt.show}")
  }

  // ----------
  // EXTRACTORS
  // ----------

  // a literal type: `5`, `"foo"`
  private object C {
    def unapply(t: TypeRepr): Option[Any] = t match {
      case ConstantType(c) => Some(c.value)
      case _               => None
    }
  }

  // the predicate's own symbol (an alias is not dealiased) and its type arguments
  private def symAndArgs(p: TypeRepr): Option[(Symbol, List[TypeRepr])] = p match {
    case AppliedType(tycon, args) => Some((tycon.typeSymbol, args))
    case p: TypeRef               => Some((p.typeSymbol, Nil))
    case _                        => None
  }

  // the predicate as written first, then dealiased
  private def ex(t: TypeRepr, p: TypeRepr): Option[Pred] = unmatch(t, p) orElse unmatch(t, p.dealias)

  private def unmatch(t: TypeRepr, p: TypeRepr): Option[Pred] = {
    import Pred.*

    symAndArgs(p) flatMap {
      // collection
      case (`sEmpty`, _)                 => Some(Size(t, SizeDef.Const(0)))
      case (`sNonEmpty`, _)              => Some(Size(t, SizeDef.Min(1)))
      case (`sMinSize`, List(C(v: Int))) => Some(Size(t, SizeDef.Min(v)))
      case (`sMaxSize`, List(C(v: Int))) => Some(Size(t, SizeDef.Max(v)))
      case (`sSize`, List(arg))          =>
        ex(t, arg) match {
          case Some(pp) =>
            pp.norm match {
              case pp: NumericPred => Some(Size(t, pp.asSize))
              case _               => None
            }
          case None =>
            arg match {
              case C(v: Int) => Some(Size(t, SizeDef.Const(v)))
              case _         => None
            }
        }
      // string
      case (`sUUID`, _)                  => Some(UUID(t))
      case (`sURL`, _)                   => Some(URI(t))
      case (`sURI`, _)                   => Some(URI(t))
      case (`sStartsWith`, List(C(v)))   => Some(MatchesRegex(t, s"^${v.toString}.*$$"))
      case (`sEndsWith`, List(C(v)))     => Some(MatchesRegex(t, s"^.*${v.toString}$$"))
      case (`sMatchesRegex`, List(C(v))) => Some(MatchesRegex(t, v.toString))
      case (`sIPv4`, _)                  => Some(IPv4(t))
      case (`sIPv6`, _)                  => Some(IPv6(t))
      case (`sTrimmed`, _)               => Some(Trimmed(t))
      // numeric
      case (`sPositive`, _)              => Some(Pos(t))
      case (`sNonPositive`, _)           => Some(Not(Pos(t)))
      case (`sNegative`, _)              => Some(Neg(t))
      case (`sNonNegative`, _)           => Some(Not(Neg(t)))
      case (`sGreater`, List(C(v)))      => Some(Ge(t, v))
      case (`sGreaterEqual`, List(C(v))) => Some(Ge(t, v, inclusive = true))
      case (`sLess`, List(C(v)))         => Some(Le(t, v))
      case (`sLessEqual`, List(C(v)))    => Some(Le(t, v, inclusive = true))
      case (`sDivisable`, List(C(v)))    => Some(Divisable(t, v))
      case (`sNonDivisable`, List(C(v))) => Some(Not(Divisable(t, v)))
      // boolean
      case (`sNot`, List(p))    => ex(t, p) map { Not(_) }
      case (`sAnd`, List(l, r)) => for { ll <- ex(t, l); rr <- ex(t, r) } yield And(ll, rr)
      case (`sOr`, List(l, r))  => for { ll <- ex(t, l); rr <- ex(t, r) } yield Or(ll, rr)
      case _                    => None
    }
  }

  // ----
  // EMIT
  // ----

  private def summonOrErr[X: Type]: Expr[X] = Expr.summon[X] getOrElse err(s"No implicit ${TypeRepr.of[X].show} found")

  // `schema.withValidation(defs: _*)`, with the `Magnet[S, B]` that checks the validations against the schema's type
  private def withV[B: Type](schema: Expr[Schema[?]], st: TypeRepr, defs: List[Expr[Def[B, ?]]]): Expr[Schema[?]] = st.asType match {
    case '[s] =>
      val magnet = Expr.summon[Magnet[s, B]] getOrElse err(
        s"Implicit not found: validation.Magnet[${st.show(using Printer.TypeReprShortCode)}, ${TypeRepr.of[B].show(using Printer.TypeReprShortCode)}]. Some of validations doesn't match schema type"
      )
      val sc   = '{ $schema.asInstanceOf[Schema[s]] }
      val head = defs.head
      val tail = Varargs(defs.tail)
      '{ $sc.withValidation[s, B]($head, $tail*)(using $magnet) }
  }

  private def jsonSchema(t: TypeRepr): Expr[Schema[?]] = t.asType match { case '[x] => '{ json.Json.schema[x] } }

  private def stringOf(t: TypeRepr): Expr[Schema[?]] = t.asType match { case '[x] => '{ Schema.`string`[x] } }

  private def stringOf(t: TypeRepr, format: Expr[Schema.`string`.Format]): Expr[Schema[?]] = t.asType match { case '[x] => '{ Schema.`string`[x]($format) } }

  private def num(v: Double): Expr[Number] = { val e = Expr(v); '{ java.lang.Double.valueOf($e) } }

  private def pattern(v: String): Expr[Def[String, ?]]                     = { val e = Expr(v); '{ Instance.`pattern` := $e } }
  private def minLength(v: Int): Expr[Def[String, ?]]                      = { val e = Expr(v); '{ Instance.`minLength` := $e } }
  private def maxLength(v: Int): Expr[Def[String, ?]]                      = { val e = Expr(v); '{ Instance.`maxLength` := $e } }
  private def minItems(v: Int): Expr[Def[Iterable[?], ?]]                  = { val e = Expr(v); '{ Instance.`minItems` := $e } }
  private def maxItems(v: Int): Expr[Def[Iterable[?], ?]]                  = { val e = Expr(v); '{ Instance.`maxItems` := $e } }
  private def minimum(v: Double, inclusive: Boolean): Expr[Def[Number, ?]] = { val e = num(v); if (inclusive) '{ Instance.`minimum` := $e } else '{ Instance.`exclusiveMinimum` := $e } }
  private def maximum(v: Double, inclusive: Boolean): Expr[Def[Number, ?]] = { val e = num(v); if (inclusive) '{ Instance.`maximum` := $e } else '{ Instance.`exclusiveMaximum` := $e } }
  private def multipleOf(v: Double): Expr[Def[Number, ?]]                  = { val e = num(v); '{ Instance.`multipleOf` := $e } }

  private def setOf(trees: List[Expr[Schema[?]]]): Expr[Set[Schema[?]]] = { val xs = Varargs(trees); '{ Set[Schema[?]]($xs*) } }

  private def isString(t: TypeRepr): Boolean = t =:= TypeRepr.of[String]

  private def asNumber(v: Any): Double = v match {
    case v: Number => v.doubleValue()
    case v         => err(s"refined: $v is not a number")
  }

  // -------------------------------
  // AST (Scala 2: refined/AST.scala)
  // -------------------------------

  private sealed trait Pred {
    def t: TypeRepr
    def tree: Expr[Schema[?]] = norm.tree
    def norm: Pred            = this
  }

  private enum SizeDef(val v: Int) {
    case Min(override val v: Int)   extends SizeDef(v)
    case Max(override val v: Int)   extends SizeDef(v)
    case Const(override val v: Int) extends SizeDef(v)
  }

  private object Pred {

    // ----------
    // COLLECTION
    // ----------
    case class Size(t: TypeRepr, d: SizeDef) extends Pred {
      override def tree: Expr[Schema[?]] = {
        val base = jsonSchema(t)
        (d, isString(t)) match {
          case (SizeDef.Min(v), true)    => withV(base, t, List(minLength(v)))
          case (SizeDef.Min(v), false)   => withV(base, t, List(minItems(v)))
          case (SizeDef.Max(v), true)    => withV(base, t, List(maxLength(v)))
          case (SizeDef.Max(v), false)   => withV(base, t, List(maxItems(v)))
          case (SizeDef.Const(v), true)  => withV(base, t, List(minLength(v), maxLength(v)))
          case (SizeDef.Const(v), false) => withV(base, t, List(minItems(v), maxItems(v)))
        }
      }
    }

    // used to specify precomputed predicate
    // helping normalize Or over numeric predicates
    case class Naked(t: TypeRepr, override val tree: Expr[Schema[?]]) extends Pred

    // ------
    // STRING
    // ------
    case class IPv4(t: TypeRepr) extends Pred {
      override def tree: Expr[Schema[?]] = stringOf(t, '{ Schema.`string`.Format.`ipv4` })
    }

    case class IPv6(t: TypeRepr) extends Pred {
      override def tree: Expr[Schema[?]] = stringOf(t, '{ Schema.`string`.Format.`ipv6` })
    }

    case class MatchesRegex(t: TypeRepr, v: String) extends Pred {
      override def tree: Expr[Schema[?]] = withV(stringOf(t), t, List(pattern(v)))
    }

    case class URI(t: TypeRepr) extends Pred {
      override def tree: Expr[Schema[?]] = stringOf(t, '{ Schema.`string`.Format.`uri` })
    }

    case class UUID(t: TypeRepr) extends Pred {
      override def tree: Expr[Schema[?]] =
        withV(stringOf(t), t, List(pattern("^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$")))
    }

    case class Trimmed(t: TypeRepr) extends Pred {
      override def tree: Expr[Schema[?]] = withV(stringOf(t), t, List(pattern("^(?!\\s)[\\S ]*(?<!\\s)$")))
    }

    // -------
    // NUMERIC
    // -------

    sealed trait NumPred extends Pred {
      def isInt: Boolean = t =:= TypeRepr.of[Int] || t =:= TypeRepr.of[java.lang.Integer] || t =:= TypeRepr.of[Long] || t =:= TypeRepr.of[java.lang.Long]

      // `integer` (a Schema[Int]) for Int and Long, `number`[t] otherwise
      def prefixType: TypeRepr = if (isInt) TypeRepr.of[Int] else t

      def prefix: Expr[Schema[?]] =
        if (isInt) '{ Schema.`integer` }
        else
          t.asType match {
            case '[x] =>
              val numeric = summonOrErr[Numeric[x]]
              '{ Schema.`number`[x](using $numeric) }
          }

      def prefixWith(defs: Expr[Def[Number, ?]]*): Expr[Schema[?]] = withV(prefix, prefixType, defs.toList)
    }

    def Pos(t: TypeRepr): Ge = Ge(t, 0)
    def Neg(t: TypeRepr): Le = Le(t, 0)

    sealed trait NumericPred {
      def v: Any
      def vv: Double = asNumber(v)
      def asSize: SizeDef
      def inclusive: Boolean
    }

    case class Divisable(t: TypeRepr, v: Any) extends NumPred {
      override def tree: Expr[Schema[?]] = prefixWith(multipleOf(asNumber(v)))
    }

    // inside or
    // outside
    case class Bounded(t: TypeRepr, min: NumericPred, max: NumericPred) extends NumPred {
      // TODO: if minv == maxv => const
      override def tree: Expr[Schema[?]] = prefixWith(minimum(min.vv, min.inclusive), maximum(max.vv, max.inclusive))
    }

    case class Ge(t: TypeRepr, v: Any, inclusive: Boolean = false) extends NumPred with NumericPred {
      def asSize: SizeDef                = SizeDef.Min(vv.toInt)
      override def tree: Expr[Schema[?]] = prefixWith(minimum(vv, inclusive))

      def min(o: Ge): Ge = {
        require(t =:= o.t)

        Ge(t, math.min(v, o.v), inclusive || o.inclusive)
      }

      def max(o: Ge): Ge = {
        require(t =:= o.t)

        Ge(t, math.max(v, o.v), inclusive || o.inclusive)
      }
    }

    case class Le(t: TypeRepr, v: Any, inclusive: Boolean = false) extends NumPred with NumericPred {
      def asSize: SizeDef                = SizeDef.Max(vv.toInt)
      override def tree: Expr[Schema[?]] = prefixWith(maximum(vv, inclusive))

      def max(o: Le): Le = {
        require(t =:= o.t)

        Le(t, math.max(v, o.v), inclusive || o.inclusive)
      }

      def min(o: Le): Le = {
        require(t =:= o.t)

        Le(t, math.min(v, o.v), inclusive || o.inclusive)
      }
    }

    // -------
    // BOOLEAN
    // -------

    case class Not(p: Pred) extends Pred {
      def t: TypeRepr                    = p.t
      override def tree: Expr[Schema[?]] = {
        val inner = p.tree
        t.asType match { case '[x] => '{ Schema.`not`[x]($inner.asInstanceOf[Schema[x]]) } }
      }
      override def norm: Pred = {
        def compile(p: Pred): Pred = p match {
          case Not(Ge(t, v, i))               => Le(t, v, !i)
          case Not(Le(t, v, i))               => Ge(t, v, !i)
          case Not(Not(p))                    => compile(p)
          case Not(Size(t, SizeDef.Const(0))) => Size(t, SizeDef.Min(1))
          case np                             => np // there is no way to simplify it better
        }

        compile(this)
      }
    }

    case class OneOf(t: TypeRepr, preds: ::[Pred]) extends Pred {
      override def tree: Expr[Schema[?]] = {
        val set = setOf(preds map { _.tree })
        t.asType match { case '[x] => '{ Schema.`oneof`[x]($set) } }
      }
      override def norm: Pred = {
        val ppreds = preds.flatMap {
          case OneOf(tt, ppreds) => require(t =:= tt); ppreds
          case p                 => Some(p)
        }

        OneOf(t, ::(ppreds.head, ppreds.tail))
      }
    }

    case class Or(l: Pred, r: Pred) extends NumPred {
      require(l.t =:= r.t)
      def t: TypeRepr         = l.t
      override def norm: Pred = {

        def compile(l: Pred, r: Pred): Pred = (l, r) match {
          case (l: Ge, r: Ge)    => l min r
          case (l: Le, r: Le)    => l max r
          case Outside(min, max) => Not(Bounded(t, min, max))
          case Inside(_, _)      => Naked(l.t, prefix)
          case (l, r)            => OneOf(t, ::(l, r :: Nil))
        }

        compile(l, r)
      }
    }

    case class AllOf(t: TypeRepr, preds: ::[Pred]) extends Pred {
      override def tree: Expr[Schema[?]] = {
        val set = setOf(preds map { _.tree })
        t.asType match { case '[x] => '{ Schema.`allof`[x]($set) } }
      }
      override def norm: Pred = {
        val ppreds = preds.flatMap {
          case AllOf(tt, ppreds) => require(t =:= tt); ppreds
          case p                 => Some(p)
        }

        AllOf(t, ::(ppreds.head, ppreds.tail))
      }
    }

    case class And(l: Pred, r: Pred) extends Pred {
      require(l.t =:= r.t)
      def t: TypeRepr         = l.t
      override def norm: Pred = {

        def compile(l: Pred, r: Pred): Pred = (l, r) match {
          case (l: Ge, r: Ge)    => l max r
          case (l: Le, r: Le)    => l min r
          case Inside(min, max)  => Bounded(t, min, max)
          case Outside(min, max) =>
            err(s"There are no values inside of specified range [${min.vv} .. ${max.vv}]")
          case (l, r) => AllOf(t, ::(l, r :: Nil))
        }

        compile(l, r)
      }
    }

    object Inside {

      def unapply(tuple: (Pred, Pred)): Option[(NumericPred, NumericPred)] = tuple match {
        case (l: Ge, r: Le) if l.vv < r.vv => Some((l, r))
        case (l: Le, r: Ge) if l.vv > r.vv => Some((r, l))
        case _                             => None
      }
    }

    object Outside {

      def unapply(tuple: (Pred, Pred)): Option[(NumericPred, NumericPred)] = tuple match {
        case (l: Ge, r: Le) if l.vv > r.vv => Some((l, r))
        case (l: Le, r: Ge) if l.vv < r.vv => Some((l, r))
        case _                             => None
      }
    }
  }

  // ---------------------------------
  // MATH (Scala 2: refined/Math.scala)
  // ---------------------------------

  private object math {

    def min(l: Any, r: Any): Any = (l, r) match {
      case (l: Byte, r: Byte)             => l min r
      case (l: Short, r: Short)           => l min r
      case (l: Int, r: Int)               => l min r
      case (l: Long, r: Long)             => l min r
      case (l: Double, r: Double)         => l min r
      case (l: Float, r: Float)           => l min r
      case (l: BigDecimal, r: BigDecimal) => l min r
      case (l: BigInt, r: BigInt)         => l min r
      case (l: Number, r: Number)         => if (l.doubleValue() <= r.doubleValue()) l else r
      case _                              => err(s"Can't find minimal out of $l and $r")
    }

    def max(l: Any, r: Any): Any = (l, r) match {
      case (l: Byte, r: Byte)             => l max r
      case (l: Short, r: Short)           => l max r
      case (l: Int, r: Int)               => l max r
      case (l: Long, r: Long)             => l max r
      case (l: Double, r: Double)         => l max r
      case (l: Float, r: Float)           => l max r
      case (l: BigDecimal, r: BigDecimal) => l max r
      case (l: BigInt, r: BigInt)         => l max r
      case (l: Number, r: Number)         => if (l.doubleValue() >= r.doubleValue()) l else r
      case _                              => err(s"Can't find maximal out of $l and $r")
    }
  }
}
