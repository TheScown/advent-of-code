package space.scown.adventofcode
package lib

import scala.collection.immutable.NumericRange
import scala.math.{ScalaNumber, ScalaNumericConversions}

case class Complex(re: BigInt, im: BigInt) extends ScalaNumber with ScalaNumericConversions {

  def conjugate: Complex = Complex(re, -im)

  override def underlying(): AnyRef = this

  override def isWhole: Boolean = true

  override def intValue: Int = re.toInt

  override def longValue: Long = re.toLong

  override def floatValue: Float = re.toFloat

  override def doubleValue: Double = re.toDouble

  def to(end: Complex): IndexedSeq[Complex] = {
    (this.re to end.re).flatMap { n =>
      NumericRange(Complex(n, this.im), Complex(n, end.im), Complex.I).inclusive
    }
  }
}

case object Complex {

  val ZERO: Complex = Complex(0, 0)
  val ONE: Complex = Complex(1, 0)
  val I: Complex = Complex(0, 1)

  trait ComplexIsIntegral extends Integral[Complex] {
    override def zero: Complex = ZERO
    override def one: Complex = ONE

    override def plus(x: Complex, y: Complex): Complex = Complex(x.re + y.re, x.im + y.im)
    override def minus(x: Complex, y: Complex): Complex = Complex(x.re - y.re, x.im - y.im)
    override def times(x: Complex, y: Complex): Complex = Complex(x.re * y.re - x.im * y.im, x.im * y.re + x.re * y.im)
    override def negate(x: Complex): Complex = Complex(-x.re, -x.im)
    override def fromInt(x: Int): Complex = Complex(BigInt(x), 0)
    override def parseString(str: String): Option[Complex] = ???
    override def toInt(x: Complex): Int = x.intValue
    override def toLong(x: Complex): Long = x.longValue
    override def toFloat(x: Complex): Float = x.floatValue
    override def toDouble(x: Complex): Double = x.doubleValue

    override def quot(x: Complex, y: Complex): Complex = {
      val q = x * y.conjugate
      val d = y * y.conjugate
      Complex(q.re / d.re, q.im / d.re)
    }

    override def rem(x: Complex, y: Complex): Complex = {
      val q = x * y.conjugate
      val d = y * y.conjugate
      Complex(q.re % d.re, q.im % d.re)
    }

    override def compare(x: Complex, y: Complex): Int = {
      (x.re * x.re + x.im * x.im).compare(y.re * y.re + y.im * y.im)
    }
  }

  implicit object ComplexIsIntegral extends ComplexIsIntegral with Ordering[Complex]

}
