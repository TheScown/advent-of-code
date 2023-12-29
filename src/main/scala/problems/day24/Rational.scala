package space.scown.advent2023
package problems.day24

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.math.{ScalaNumber, ScalaNumericConversions}

case class Rational(d: BigInt, q: BigInt) extends ScalaNumber with ScalaNumericConversions {

  if (q == 0) {
    throw new ArithmeticException("/ by zero")
  }

  def simplify: Rational = {
    val factor = gcd(d, q)
    if (factor == 1) this
    else Rational(d / factor, q / factor).simplify
  }

  private def gcd(x: BigInt, y: BigInt): BigInt = {
    @tailrec
    def helper(a: BigInt, b: BigInt): BigInt = {
      if (b == 0) a
      else helper(b, a % b)
    }

    helper(if (x > y) x else y, if (x > y) y else x)
  }

  override def underlying(): AnyRef = this

  override def isWhole: Boolean = q == 1

  override def intValue: Int = d.toInt / q.toInt

  override def longValue: Long = d.toLong / q.toLong

  override def floatValue: Float = d.toFloat / q.toFloat

  override def doubleValue: Double = d.toDouble / q.toDouble
}
case object Rational {
  val ZERO: Rational = Rational(0, 1)

  trait RationalIsFractional extends Fractional[Rational] {

    override def plus(x: Rational, y: Rational): Rational = {
      if (x.q == y.q) Rational(x.d + y.d, x.q).simplify
      else {
        Rational(x.d * y.q + y.d * x.q, x.q * y.q).simplify
      }
    }

    override def minus(x: Rational, y: Rational): Rational = {
      if (x.q == y.q) Rational(x.d - y.d, x.q).simplify
      else {
        Rational(x.d * y.q - y.d * x.q, x.q * y.q).simplify
      }
    }

    override def times(x: Rational, y: Rational): Rational = Rational(x.d * y.d, x.q * y.q).simplify

    override def div(x: Rational, y: Rational): Rational = {
      if (y.d == 0) throw new ArithmeticException("/ by zero")
      else Rational(x.d * y.q, x.q * y.d).simplify
    }
    override def negate(x: Rational): Rational = Rational(-x.d, x.q).simplify

    override def fromInt(x: Int): Rational = Rational(x, 1)

    override def parseString(str: String): Option[Rational] = ???

    override def toInt(x: Rational): Int = x.d.toInt / x.q.toInt

    override def toLong(x: Rational): Long = x.d.toLong / x.q.toLong

    override def toFloat(x: Rational): Float = x.d.toFloat / x.q.toFloat

    override def toDouble(x: Rational): Double = x.d.toDouble / x.q.toDouble

    override def compare(x: Rational, y: Rational): Int = {
      if (x.q == y.q) x.d.compareTo(y.d)
      else compare(Rational(x.d * y.q, x.q * y.q), Rational(y.d * x.q, y.q * x.q))
    }
  }

  implicit object RationalIsFractional extends RationalIsFractional with Ordering[Rational]

  implicit def long2Rational(x: Long): Rational = Rational(x, 1)
}
