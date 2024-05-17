package space.scown.adventofcode
package lib

import scala.math.{Pi, ScalaNumber, ScalaNumericConversions}

case class Complex(re: Double, im: Double) extends ScalaNumber with ScalaNumericConversions {

  def conjugate: Complex = Complex(re, -im)

  override def underlying(): AnyRef = this

  override def isWhole: Boolean = true

  override def intValue: Int = re.toInt

  override def longValue: Long = re.toLong

  override def floatValue: Float = re.toFloat

  override def doubleValue: Double = re

}

case object Complex {

  val ZERO: Complex = Complex(0, 0)
  val ONE: Complex = Complex(1, 0)
  val I: Complex = Complex(0, 1)

  trait ComplexIsFractional extends Fractional[Complex] {
    override def zero: Complex = ZERO
    override def one: Complex = ONE

    override def plus(x: Complex, y: Complex): Complex = Complex(x.re + y.re, x.im + y.im)
    override def minus(x: Complex, y: Complex): Complex = Complex(x.re - y.re, x.im - y.im)
    override def times(x: Complex, y: Complex): Complex = Complex(x.re * y.re - x.im * y.im, x.im * y.re + x.re * y.im)
    override def negate(x: Complex): Complex = Complex(-x.re, -x.im)
    override def fromInt(x: Int): Complex = Complex(x.toDouble, 0)
    override def parseString(str: String): Option[Complex] = ???
    override def toInt(x: Complex): Int = x.intValue
    override def toLong(x: Complex): Long = x.longValue
    override def toFloat(x: Complex): Float = x.floatValue
    override def toDouble(x: Complex): Double = x.doubleValue

    override def div(x: Complex, y: Complex): Complex = {
      val q = x * y.conjugate
      val d = y * y.conjugate
      Complex(q.re / d.toDouble, q.im / d.toDouble)
    }

    override def abs(x: Complex): Complex = {
      val modSquare = x.re * x.re + x.im * x.im
      Complex(Math.sqrt(modSquare), 0)
    }

    override def sign(x: Complex): Complex = {
      if (x.re == 0) Complex(Pi / 2 * x.im.sign, 0)
      else Complex(Math.atan(x.im / x.re), 0)
    }

    override def compare(x: Complex, y: Complex): Int = x.abs.toDouble.compare(y.abs.toDouble)
  }

  implicit object ComplexIsFractional extends ComplexIsFractional with Ordering[Complex]

}
