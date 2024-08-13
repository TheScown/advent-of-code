package space.scown.adventofcode
package lib

case class Vec3[T](x1: T, x2: T, x3: T)(implicit n: Numeric[T]) {
  import n._
  def *(a: T): Vec3[T] = Vec3(a * x1, a * x2, a * x3)
  def *:(a: T): Vec3[T] = this * a
  def dot(v2: Vec3[T]): T = x1 * v2.x1 + x2 * v2.x2 + x3 * v2.x3
  def x(v2: Vec3[T]): Vec3[T] = Vec3(
    x2 * v2.x3 - x3 * v2.x2,
    x3 * v2.x1 - x1 * v2.x3,
    x1 * v2.x2 - x2 * v2.x1,
  )

  def +(v2: Vec3[T]): Vec3[T] = Vec3(x1 + v2.x1, x2 + v2.x2, x3 + v2.x3)
  def -(v2: Vec3[T]): Vec3[T] = Vec3(x1 - v2.x1, x2 - v2.x2, x3 - v2.x3)
  def unary_- : Vec3[T] = -1.asInstanceOf[T] *: this

  def norm1: T = x1.abs + x2.abs + x3.abs
  def normMax: T = Seq(x1, x2, x3).max

  def map[S](f: T => S)(implicit n: Numeric[S]): Vec3[S] = Vec3(
    f(x1),
    f(x2),
    f(x3)
  )

  // TODO should this be some kind of Matrix?
  def map[S](v2: Vec3[T], v3: Vec3[T])(f: (T, T, T) => S)(implicit n: Numeric[S]): Vec3[S] = Vec3(
    f(x1, v2.x1, v3.x1),
    f(x2, v2.x2, v3.x2),
    f(x3, v2.x3, v3.x3)
  )
}
