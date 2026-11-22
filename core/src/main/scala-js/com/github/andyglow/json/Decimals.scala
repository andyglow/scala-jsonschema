package com.github.andyglow.json

import scala.scalajs.js.JSNumberOps._

/** `Double` to `BigDecimal` the way the JVM does it, so that schemas render the same on Scala.js as on the JVM.
  *
  * On the JVM, `BigDecimal(d)` goes through `java.lang.Double.toString`: `16.0` becomes `16.0` (unscaled 160, scale 1) and `1.0E7` becomes `1.0E+7`. On Scala.js, `Double.toString` is JavaScript's,
  * which gives `16` and `10000000`. Both print the shortest digits that identify the double; only the layout differs. This takes JavaScript's digits and lays them out the way Java does: plain
  * notation with at least one fraction digit for 1e-3 <= |d| < 1e7, else `d.ddd` times a power of ten, with at least one digit after the point.
  */
private[andyglow] object Decimals {

  def fromDouble(d: Double): BigDecimal =
    if (d.isNaN || d.isInfinite) BigDecimal(d)  // throws, as on the JVM
    else if (d == 0.0) BigDecimal(BigInt(0), 1) // "0.0" (and "-0.0")
    else {
      val a                  = math.abs(d)
      val Array(mantissa, e) = a.toExponential().split('e') // shortest digits, e.g. "1.6e+1"
      val digits             = mantissa.replace(".", "")
      val exp                = Integer.parseInt(e)
      val unscaled0          = BigInt(digits)
      val (unscaled, scale)  =
        if (a >= 1e-3 && a < 1e7) {
          val natural = digits.length - 1 - exp // fraction digits of the plain notation
          val scale   = math.max(1, natural)
          (unscaled0 * BigInt(10).pow(scale - natural), scale)
        } else if (digits.length == 1) {
          // Java prints at least two significant digits here, the two closest to the exact value: `d.0` for normal doubles, but e.g. 4.9E-324 for Double.MinPositiveValue (JS: 5e-324)
          val two = new java.math.BigDecimal(a).round(new java.math.MathContext(2, java.math.RoundingMode.HALF_EVEN))
          val u   = BigInt(two.unscaledValue)
          if (u < 10) (u * 10, two.scale + 1) else (u, two.scale)
        } else (unscaled0, digits.length - 1 - exp)
      BigDecimal(if (d < 0) -unscaled else unscaled, scale)
    }
}
