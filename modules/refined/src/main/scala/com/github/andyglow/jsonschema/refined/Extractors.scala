package com.github.andyglow.jsonschema.refined

import eu.timepit.refined._

import scala.reflect.macros.blackbox


private[jsonschema] trait Extractors { this: HasLog with AST with HasContext =>
  import Pred._
  import c.universe._

  private lazy val `Refined` = typeOf[eu.timepit.refined.api.Refined[_, _]].typeSymbol

  // generic
  // TODO: Equal -> `const`

  // collection
  private lazy val sMinSize = smbOf[collection.MinSize[_]]
  private lazy val sMaxSize = smbOf[collection.MaxSize[_]]
  private lazy val sSize = smbOf[collection.Size[_]]
  private lazy val sEmpty = smbOf[collection.Empty]

  // string
  private lazy val sUUID = smbOf[string.Uuid]
  private lazy val sURI = smbOf[string.Uri]
  private lazy val sURL = smbOf[string.Url]
  private lazy val sStartsWith = smbOf[string.StartsWith[_]]
  private lazy val sEndsWith = smbOf[string.EndsWith[_]]
  private lazy val sMatchesRegex = smbOf[string.MatchesRegex[_]]
  private lazy val sIPv4 = smbOf[string.IPv4]
  private lazy val sIPv6 = smbOf[string.IPv6]
  private lazy val sXML = smbOf[string.Xml]

  // numeric
  private lazy val sPositive = smbOf[numeric.Positive]
  private lazy val sNonPositive = smbOf[numeric.NonPositive]
  private lazy val sNegative = smbOf[numeric.Negative]
  private lazy val sNonNegative = smbOf[numeric.NonNegative]
  private lazy val sGreater = smbOf[numeric.Greater[_]]
  private lazy val sGreaterEqual = smbOf[numeric.GreaterEqual[_]]
  private lazy val sLess = smbOf[numeric.Less[_]]
  private lazy val sLessEqual = smbOf[numeric.LessEqual[_]]
  private lazy val sDivisable = smbOf[numeric.Divisible[_]]
  private lazy val sNonDivisable = smbOf[numeric.NonDivisible[_]]

  // boolean
  private lazy val sNot = smbOf[boolean.Not[_]]
  private lazy val sAnd = smbOf[boolean.And[_, _]]
  private lazy val sOr = smbOf[boolean.Or[_, _]]

  def smbOf[T](implicit ttag: TypeTag[T]): Symbol = ttag.tpe match {
    case TypeRef(_, s, _)                     => s
    case ExistentialType(_, TypeRef(_, s, _)) => s
    case t                                    => t.typeSymbol
  }

  object refined {

    // Type and Ref extractor
    object R {

      def unapply(t: Type): Option[(Type, TypeRef)] =
        t match {
          case TypeRef(_, `Refined`, List(t, p: TypeRef)) => Some((t, p))
          case _                                          => None
        }
    }

    // constant extractor
    object C {

      def unapply(t: Type): Option[Any] = t match {
        case ConstantType(Constant(v))  => Some(v)
        case _                          => None
      }
    }

    // predicate extractor
    object P {

      def unapply(tuple: (Type, TypeRef)): Option[Pred] = {
        val (t, p) = tuple
        dbg(s"P.unapply: t=${showRaw(t)}, p=${showRaw(p)}")

        object Ex {
          def unapply(p: Type): Option[Pred] = {
            dbg(s"Ex.unapply: p=${showRaw(p)}")

            val v = p match {
              // collection
              case TypeRef(_, `sEmpty`        , _)                    => Some(Size(t, Size.Def.Const(0)))
              case TypeRef(_, `sMinSize`      , List(C(v: Int)))      => Some(Size(t, Size.Def.Min(v)))
              case TypeRef(_, `sMaxSize`      , List(C(v: Int)))      => Some(Size(t, Size.Def.Max(v)))
              case TypeRef(_, `sSize`         , List(Ex(pp)))         => pp.norm match {
                case pp: NumericPred                                    => Some(Size(t, pp.asSize))
                case _                                                  => None
              }
              case TypeRef(_, `sSize`         , List(C(v: Int)))      => Some(Size(t, Size.Def.Const(v)))
              // string
              case TypeRef(_, `sUUID`         , _)                    => Some(UUID(t))
              case TypeRef(_, `sURL`          , _)                    => Some(URI(t))
              case TypeRef(_, `sURI`          , _)                    => Some(URI(t))
              case TypeRef(_, `sStartsWith`   , List(C(v)))           => Some(MatchesRegex(t, s"^${v.toString}.*$$"))
              case TypeRef(_, `sEndsWith`     , List(C(v)))           => Some(MatchesRegex(t, s"^.*${v.toString}$$"))
              case TypeRef(_, `sMatchesRegex` , List(C(v)))           => Some(MatchesRegex(t, v.toString))
              case TypeRef(_, `sIPv4`         , _)                    => Some(IPv4(t))
              case TypeRef(_, `sIPv6`         , _)                    => Some(IPv6(t))
              case TypeRef(_, `sXML`          , _)                    => Some(XML(t))
              // numeric
              case TypeRef(_, `sPositive`     , _)                    => Some(Pos(t))
              case TypeRef(_, `sNonPositive`  , _)                    => Some(Not(Pos(t)))
              case TypeRef(_, `sNegative`     , _)                    => Some(Neg(t))
              case TypeRef(_, `sNonNegative`  , _)                    => Some(Not(Neg(t)))
              case TypeRef(_, `sGreater`      , List(C(v)))           => Some(Ge(t, v))
              case TypeRef(_, `sGreaterEqual` , List(C(v)))           => Some(Ge(t, v, inclusive = true))
              case TypeRef(_, `sLess`         , List(C(v)))           => Some(Le(t, v))
              case TypeRef(_, `sLessEqual`    , List(C(v)))           => Some(Le(t, v, inclusive = true))
              case TypeRef(_, `sDivisable`    , List(C(v)))           => Some(Divisable(t, v))
              case TypeRef(_, `sNonDivisable` , List(C(v)))           => Some(Not(Divisable(t, v)))
              // boolean
              case TypeRef(_, `sNot`          , List(Ex(pp)))         => Some(Not(pp))
              case TypeRef(_, `sAnd`          , List(Ex(ll), Ex(rr))) => Some(And(ll, rr))
              case TypeRef(_, `sOr`           , List(Ex(ll), Ex(rr))) => Some(Or(ll, rr))
              case _                                                  => None
            }
            dbg(s"Ex.unapply: result ${showRaw(v)}")

            v
          }
        }

        val pp = Ex unapply p map { _.norm }
        if (debugEnabled)
          pp foreach { pp =>
            dbg(s"Ex.unapply: final normalized result ${showRaw(pp)}")
          }

        pp
      }
    }
  }
}
