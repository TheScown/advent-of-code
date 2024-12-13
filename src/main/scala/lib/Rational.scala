package space.scown.adventofcode
package lib

import scala.language.implicitConversions
import scala.math.{ScalaNumber, ScalaNumericConversions}

case class Rational[T](d: T, q: T)(implicit n: Integral[T]) extends ScalaNumber with ScalaNumericConversions {

  if (q == n.zero) {
    throw new ArithmeticException("/ by zero")
  }

  def +(x: Rational[T]): Rational[T] = {
    if (n.equiv(q, x.q)) Rational(n.plus(d, x.d), q).simplify
    else {
      Rational(n.plus(n.times(d, x.q), n.times(x.d, q)), n.times(q, x.q)).simplify
    }
  }

  def -(x: Rational[T]): Rational[T] = {
    if (n.equiv(q, x.q)) Rational(n.minus(d, x.d), q).simplify
    else {
      Rational(n.minus(n.times(d, x.q), n.times(x.d, q)), n.times(q, x.q)).simplify
    }
  }

  def *(x: Rational[T]): Rational[T] = Rational(n.times(d, x.d), n.times(q, x.q)).simplify
  def /(x: Rational[T]): Rational[T] = Rational(n.times(d, x.q), n.times(q, x.d)).simplify
  def unary_-(): Rational[T] = Rational(n.negate(d), q)

  def simplify: Rational[T] = {
    val factor = Integers.gcd(d, q)
    if (factor == n.one) this
    else Rational(n.quot(d, factor), n.quot(q, factor)).simplify
  }

  override def underlying(): AnyRef = this

  override def isWhole: Boolean = q == n.one

  override def intValue: Int = n.toInt(d) / n.toInt(q)

  override def longValue: Long = n.toLong(d) / n.toLong(q)

  override def floatValue: Float = n.toFloat(d) / n.toFloat(q)

  override def doubleValue: Double = n.toDouble(d) / n.toDouble(q)
}
case object Rational {
  def ZERO[T](implicit n: Integral[T]): Rational[T] = Rational(n.zero, n.one)

  trait RationalIntIsFractional extends Fractional[Rational[Int]] {

    override def plus(x: Rational[Int], y: Rational[Int]): Rational[Int] = {
      if (x.q == y.q) Rational(x.d + y.d, x.q).simplify
      else {
        Rational(x.d * y.q + y.d * x.q, x.q * y.q).simplify
      }
    }

    override def minus(x: Rational[Int], y: Rational[Int]): Rational[Int] = {
      if (x.q == y.q) Rational(x.d - y.d, x.q).simplify
      else {
        Rational(x.d * y.q - y.d * x.q, x.q * y.q).simplify
      }
    }

    override def times(x: Rational[Int], y: Rational[Int]): Rational[Int] = Rational(x.d * y.d, x.q * y.q).simplify

    override def div(x: Rational[Int], y: Rational[Int]): Rational[Int] = {
      if (y.d == 0) throw new ArithmeticException("/ by zero")
      else Rational(x.d * y.q, x.q * y.d).simplify
    }
    override def negate(x: Rational[Int]): Rational[Int] = Rational(-x.d, x.q).simplify

    override def fromInt(x: Int): Rational[Int] = Rational(x, 1)

    override def parseString(str: String): Option[Rational[Int]] = ???

    override def toInt(x: Rational[Int]): Int = x.d / x.q

    override def toLong(x: Rational[Int]): Long = x.d.toLong / x.q.toLong

    override def toFloat(x: Rational[Int]): Float = x.d.toFloat / x.q.toFloat

    override def toDouble(x: Rational[Int]): Double = x.d.toDouble / x.q.toDouble

    override def compare(x: Rational[Int], y: Rational[Int]): Int = {
      if (x.q == y.q) x.d.compareTo(y.d)
      else compare(Rational(x.d * y.q, x.q * y.q), Rational(y.d * x.q, y.q * x.q))
    }
  }

  implicit object RationalIntIsFractional extends RationalIntIsFractional with Ordering[Rational[Int]]

  trait RationalLongIsFractional extends Fractional[Rational[Long]] {

    override def plus(x: Rational[Long], y: Rational[Long]): Rational[Long] = {
      if (x.q == y.q) Rational(x.d + y.d, x.q).simplify
      else {
        Rational(x.d * y.q + y.d * x.q, x.q * y.q).simplify
      }
    }

    override def minus(x: Rational[Long], y: Rational[Long]): Rational[Long] = {
      if (x.q == y.q) Rational(x.d - y.d, x.q).simplify
      else {
        Rational(x.d * y.q - y.d * x.q, x.q * y.q).simplify
      }
    }

    override def times(x: Rational[Long], y: Rational[Long]): Rational[Long] = Rational(x.d * y.d, x.q * y.q).simplify

    override def div(x: Rational[Long], y: Rational[Long]): Rational[Long] = {
      if (y.d == 0) throw new ArithmeticException("/ by zero")
      else Rational(x.d * y.q, x.q * y.d).simplify
    }
    override def negate(x: Rational[Long]): Rational[Long] = Rational(-x.d, x.q).simplify

    override def fromInt(x: Int): Rational[Long] = Rational(x, 1)

    override def parseString(str: String): Option[Rational[Long]] = ???

    override def toInt(x: Rational[Long]): Int = x.d.toInt / x.q.toInt

    override def toLong(x: Rational[Long]): Long = x.d / x.q

    override def toFloat(x: Rational[Long]): Float = x.d.toFloat / x.q.toFloat

    override def toDouble(x: Rational[Long]): Double = x.d.toDouble / x.q.toDouble

    override def compare(x: Rational[Long], y: Rational[Long]): Int = {
      if (x.q == y.q) x.d.compareTo(y.d)
      else compare(Rational(x.d * y.q, x.q * y.q), Rational(y.d * x.q, y.q * x.q))
    }
  }

  implicit object RationalLongIsFractional extends RationalLongIsFractional with Ordering[Rational[Long]]

  trait RationalBigIntIsFractional extends Fractional[Rational[BigInt]] {

    override def plus(x: Rational[BigInt], y: Rational[BigInt]): Rational[BigInt] = {
      if (x.q == y.q) Rational(x.d + y.d, x.q).simplify
      else {
        Rational(x.d * y.q + y.d * x.q, x.q * y.q).simplify
      }
    }

    override def minus(x: Rational[BigInt], y: Rational[BigInt]): Rational[BigInt] = {
      if (x.q == y.q) Rational(x.d - y.d, x.q).simplify
      else {
        Rational(x.d * y.q - y.d * x.q, x.q * y.q).simplify
      }
    }

    override def times(x: Rational[BigInt], y: Rational[BigInt]): Rational[BigInt] = Rational(x.d * y.d, x.q * y.q).simplify

    override def div(x: Rational[BigInt], y: Rational[BigInt]): Rational[BigInt] = {
      if (y.d == 0) throw new ArithmeticException("/ by zero")
      else Rational(x.d * y.q, x.q * y.d).simplify
    }
    override def negate(x: Rational[BigInt]): Rational[BigInt] = Rational(-x.d, x.q).simplify

    override def fromInt(x: Int): Rational[BigInt] = Rational(x, 1)

    override def parseString(str: String): Option[Rational[BigInt]] = ???

    override def toInt(x: Rational[BigInt]): Int = x.d.toInt / x.q.toInt

    override def toLong(x: Rational[BigInt]): Long = x.d.toLong / x.q.toLong

    override def toFloat(x: Rational[BigInt]): Float = x.d.toFloat / x.q.toFloat

    override def toDouble(x: Rational[BigInt]): Double = x.d.toDouble / x.q.toDouble

    override def compare(x: Rational[BigInt], y: Rational[BigInt]): Int = {
      if (x.q == y.q) x.d.compareTo(y.d)
      else compare(Rational(x.d * y.q, x.q * y.q), Rational(y.d * x.q, y.q * x.q))
    }
  }

  implicit object RationalBigIntIsFractional extends RationalBigIntIsFractional with Ordering[Rational[BigInt]]

  implicit def long2Rational(x: Long): Rational[Long] = Rational(x, 1)
  implicit def bigInt2Rational(x: BigInt): Rational[BigInt] = Rational(x, 1)
  implicit def long2BigIntRational(x: Long): Rational[BigInt] = Rational(BigInt(x), 1)
}
