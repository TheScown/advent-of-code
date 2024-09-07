package space.scown.adventofcode
package lib

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions
import scala.math.Integral.Implicits.infixIntegralOps

case class Complex[T](re: T, im: T)(implicit n: Integral[T]) {

  val isReal: Boolean = im == n.zero

  val isImaginary: Boolean = re == n.zero

  def conjugate: Complex[T] = Complex(re, -im)

  def +(x: Complex[T]): Complex[T] = Complex(this.re + x.re, this.im + x.im)
  def -(x: Complex[T]): Complex[T] = Complex(this.re - x.re, this.im - x.im)
  def *(x: Complex[T]): Complex[T] = Complex(this.re * x.re - this.im * x.im, this.im * x.re + this.re * x.im)
  def /(x: Complex[T]): Complex[T] = {
    val conj = x.conjugate
    val d = this * conj
    val q = x * conj
    Complex(d.re / q.re, d.im / q.re)
  }
  def unary_-(): Complex[T] = Complex(-re, -im)

  def +(x: T): Complex[T] = this + Complex(x, n.zero)
  def :+(x: T): Complex[T] = this + Complex(x, n.zero)
  def *(x: T): Complex[T] = this * Complex(x, n.zero)
  def :*(x: T): Complex[T] = this * Complex(x, n.zero)

  /**
   * @param other The given Complex
   * @return Manhattan distance between this and the given Complex
   */
  def mh(other: Complex[T]): T = {
    val diff = this - other
    n.abs(diff.re.abs + diff.im.abs)
  }

  def to(end: Complex[T]): IndexedSeq[Complex[T]] = {
    NumericRange.inclusive(this.re, end.re, n.one)(n).flatMap { re =>
      NumericRange.inclusive(this.im, end.im, n.one)(n).map(im => Complex(re, im))
    }
  }
}

case object Complex {

  def ZERO[T](implicit n: Integral[T]): Complex[T] = Complex(n.zero, n.zero)
  def ONE[T](implicit n: Integral[T]): Complex[T] = Complex(n.one, n.zero)
  def I[T](implicit n: Integral[T]): Complex[T] = Complex(n.zero, n.one)

  def linearRange[T](start: Complex[T], end: Complex[T], step: Complex[T], inclusive: Boolean = false)(implicit n: Integral[T]): IndexedSeq[Complex[T]] = {
    if (inclusive) {
      if (start.re == end.re) {
        new NumericRange.Inclusive[T](start.im, end.im, step.im).map(im => Complex(start.re, im))
      }
      else if (start.im == end.im) {
        new NumericRange.Inclusive[T](start.re, end.re, step.re).map(re => Complex(re, start.im))
      }
      else throw new IllegalArgumentException(s"Non linear range $start to $end by $step")
    }
    else {
      if (start.re == end.re) {
        new NumericRange.Exclusive[T](start.im, end.im, step.im).map(im => Complex(start.re, im))
      }
      else if (start.im == end.im) {
        new NumericRange.Exclusive[T](start.re, end.re, step.re).map(re => Complex(re, start.im))
      }
      else throw new IllegalArgumentException(s"Non linear range $start to $end by $step")
    }
  }

}
