package space.scown.adventofcode
package lib

case class Vec4[T](x1: T, x2: T, x3: T, x4: T)(implicit n: Numeric[T]) {
  import n._
  def *(a: T): Vec4[T] = Vec4(a * x1, a * x2, a * x3, a * x4)
  def *:(a: T): Vec4[T] = this * a
  def dot(v2: Vec4[T]): T = x1 * v2.x1 + x2 * v2.x2 + x3 * v2.x3 + x4 * v2.x4

  def +(v2: Vec4[T]): Vec4[T] = Vec4(x1 + v2.x1, x2 + v2.x2, x3 + v2.x3, x4 + v2.x4)
  def -(v2: Vec4[T]): Vec4[T] = Vec4(x1 - v2.x1, x2 - v2.x2, x3 - v2.x3, x4 - v2.x4)
  def unary_- : Vec4[T] = -1.asInstanceOf[T] *: this

  def norm1: T = x1.abs + x2.abs + x3.abs + x4.abs
  def normMax: T = Seq(x1, x2, x3, x4).max

  def map[S](f: T => S)(implicit n: Numeric[S]): Vec4[S] = Vec4(
    f(x1),
    f(x2),
    f(x3),
    f(x4)
  )

  // TODO should this be some kind of Matrix?
  def map[S](v2: Vec4[T], v3: Vec4[T])(f: (T, T, T) => S)(implicit n: Numeric[S]): Vec4[S] = Vec4(
    f(x1, v2.x1, v3.x1),
    f(x2, v2.x2, v3.x2),
    f(x3, v2.x3, v3.x3),
    f(x4, v2.x4, v3.x4)
  )
}
