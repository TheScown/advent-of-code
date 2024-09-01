package space.scown.adventofcode
package lib

case class Vec2[T](x1: T, x2: T)(implicit n: Numeric[T]) {
  import n._
  def *(a: T): Vec2[T] = Vec2(a * x1, a * x2)
  def *:(a: T): Vec2[T] = this * a
  def dot(v2: Vec2[T]): T = x1 * v2.x1 + x2 * v2.x2

  def +(v2: Vec2[T]): Vec2[T] = Vec2(x1 + v2.x1, x2 + v2.x2)
  def -(v2: Vec2[T]): Vec2[T] = Vec2(x1 - v2.x1, x2 - v2.x2)
  def unary_- : Vec2[T] = -1.asInstanceOf[T] *: this

  def norm1: T = x1.abs + x2.abs
  def normMax: T = Seq(x1, x2).max

  def map[S](f: T => S)(implicit n: Numeric[S]): Vec2[S] = Vec2(
    f(x1),
    f(x2)
  )

  // TODO should this be some kind of Matrix?
  def map[S](v2: Vec2[T], v3: Vec2[T])(f: (T, T, T) => S)(implicit n: Numeric[S]): Vec2[S] = Vec2(
    f(x1, v2.x1, v3.x1),
    f(x2, v2.x2, v3.x2)
  )
}
