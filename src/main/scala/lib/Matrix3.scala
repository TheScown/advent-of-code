package space.scown.adventofcode
package lib

case class Matrix3[T](col1: Vec3[T], col2: Vec3[T], col3: Vec3[T])(implicit n: Numeric[T]) {

  def *(v: Vec3[T]): Vec3[T] = {
    val transpose = this.transpose

    Vec3(
      transpose.col1 dot v,
      transpose.col2 dot v,
      transpose.col3 dot v,
    )
  }

  def transpose: Matrix3[T] = Matrix3(
    Vec3(col1(0), col2(0), col3(0)),
    Vec3(col1(1), col2(1), col3(1)),
    Vec3(col1(2), col2(2), col3(2)),
  )

  def apply(row: Int, col: Int): T = {
    if (row < 0 || row >=3 || col < 0 || col >= 3) {
      throw new IllegalArgumentException(s"Invalid arguments: row=$row, col=$col")
    }

    col match {
      case 0 => col1(row)
      case 1 => col2(row)
      case 2 => col3(row)
    }
  }

  def vectors: Vector[Vec3[T]] = Vector(col1, col2, col3)

}

case object Matrix3 {
  def IDENTITY[T](implicit n: Integral[T]) = Matrix3(
    Vec3(n.one, n.zero, n.zero),
    Vec3(n.zero, n.one, n.zero),
    Vec3(n.zero, n.zero, n.one)
  )
}
