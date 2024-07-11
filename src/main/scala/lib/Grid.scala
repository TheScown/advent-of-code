package space.scown.adventofcode
package lib

import scala.math.Integral.Implicits.infixIntegralOps

/**
 * Grid utility class
 *
 * The address (0, 0) is the top left hand corner of the grid
 *
 * @param values The grid vectors
 * @param wrapping True if the grid wraps around; false otherwise
 * @tparam T The type of value in the grid
 */
case class Grid[T](values: Vector[Vector[T]], wrapping: Boolean = false) {

  val rowLength: Int = values.head.size
  val columnLength: Int = values.size

  def apply(address: Complex): T = {
    values(-address.im.toInt)(address.re.toInt)
  }

  def updated(address: Complex, value: T): Grid[T] = {
    Grid(values.updated(-address.im.toInt, values(-address.im.toInt).updated(address.re.toInt, value)))
  }

  def updated(address: Complex, grid: Grid[T]): Grid[T] = {
    grid.zipWithIndex.map(p => (p._1, p._2 + address)).foldLeft(this) {(self, other) =>
      self.updated(other._2, other._1)
    }
  }

  def neighboursWithDiagonals(address: Complex): IndexedSeq[Complex] = {
    if (wrapping) {
      throw new UnsupportedOperationException("Wrapping grid not yet supported")
    }
    else for {
      i <- -1 to 1
      j <- -1 to 1
      if !(i == 0 && j == 0)
      if (address.re + i) >= 0 && (address.re + i) < rowLength
      if (address.im + j) <= 0 && (address.im + j) > -columnLength
    } yield Complex(address.re + i, address.im + j)
  }

  def neighbours(address: Complex): IndexedSeq[Complex] = {
    if (wrapping) {
      throw new UnsupportedOperationException("Wrapping grid not yet supported")
    }
    else for {
      i <- -1 to 1
      j <- -1 to 1
      if !(i == 0 && j == 0)
      if i == 0 || j == 0
      if (address.re + i) >= 0 && (address.re + i) < rowLength
      if (address.im + j) <= 0 && (address.im + j) > -columnLength
    } yield Complex(address.re + i, address.im + j)
  }

  def next(address: Complex, delta: Complex): Complex = {
    if (wrapping) {
      throw new UnsupportedOperationException("Wrapping grid not yet supported")
    }
    else {
      val possibleNext = address + delta
      if (possibleNext.re < 0 || possibleNext.re >= rowLength
        || possibleNext.im > 0 || possibleNext.im <= -columnLength) address
      else possibleNext
    }
  }

  def rotateRow(row: Int, by: Int): Grid[T] = {
    if (by == 0) return this

    val affectedRow = values(row)

    if (by < 0) {
      throw new UnsupportedOperationException("Rotate left not supported")
    }
    else {
      val toMove = affectedRow.slice(affectedRow.size - by, affectedRow.size)
      val newRow = toMove ++ affectedRow.slice(0, affectedRow.size - by)
      Grid(values.updated(row, newRow))
    }
  }

  def rotateColumn(column: Int, by: Int): Grid[T] = {
    if (by == 0) return this

    val affectedColumn = values.map(r => r(column))

    if (by < 0) {
      // Remember by is -ve
      val toMove = affectedColumn.slice(affectedColumn.size + by, affectedColumn.size)
      val newColumn = toMove ++ affectedColumn.slice(0, affectedColumn.size + by)
      Grid(values.zip(newColumn).map(p => p._1.updated(column, p._2)))
    } else {
      throw new UnsupportedOperationException("Rotate up not supported")
    }
  }

  def map[S](f: T => S): Grid[S] = {
    Grid(values.map(row => row.map(f)))
  }

  // The standard zipWithIndex assumes integer indices
  def zipWithIndex: Grid[(T, Complex)] = {
    Grid(values.zipWithIndex.map { case (row, im) =>
      row.zipWithIndex.map {
        case (v, re) => (v, Complex(re, -im))
      }
    })
  }

  def count(p: T => Boolean): Int = {
    values.map(row => row.count(p)).sum
  }

  def foldLeft[S](value: S)(f: (S, T) => S): S = {
    values.foldLeft(value) { (current, row) =>
      row.foldLeft(current)((current, t) => f(current, t))
    }
  }

  def indexOf(x: T): Option[Complex] = {
    zipWithIndex.values.flatten.find {
      case (v, _) => x == v
    }.map(_._2)
  }

  def size: Int = {
    rowLength * columnLength
  }

  override def toString: String = {
    val gridString = values.map(row => row.mkString(",")).mkString("\n")

    s"Grid(\n$gridString\n)"
  }
}
