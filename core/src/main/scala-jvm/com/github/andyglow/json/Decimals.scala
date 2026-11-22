package com.github.andyglow.json

/** `Double` to `BigDecimal` the way the JVM does it: through `java.lang.Double.toString`, so `16.0` stays `16.0` (scale 1) when rendered. The Scala.js counterpart reproduces that, because
  * JavaScript's number-to-string gives `16` there.
  */
private[andyglow] object Decimals {

  def fromDouble(d: Double): BigDecimal = BigDecimal(d)
}
