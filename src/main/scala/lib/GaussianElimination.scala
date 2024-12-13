package space.scown.adventofcode
package lib

import scala.annotation.tailrec
import scala.language.implicitConversions

case object GaussianElimination {
  def solve[T](system: Vector[Vector[Rational[T]]])(implicit n: Integral[T]): Option[Vector[Rational[T]]] = {
    @tailrec
    def backSubstitute(system: Vector[Vector[Rational[T]]], index: Int, result: Vector[Rational[T]]): Vector[Rational[T]] = {
      if (index == -1) result
      else {
        val row = system(index)
        val products = row.slice(index + 1, row.size - 1)
          .zip(result.slice(index + 1, result.size))
          .map(p => p._1 * p._2)

        val knownSum = if (products.isEmpty) Rational.ZERO(n) else products.foldLeft(Rational.ZERO)(_ + _)

        backSubstitute(system, index - 1, result.updated(index, system(index).last - knownSum))
      }
    }

    @tailrec
    def helper(system: Vector[Vector[Rational[T]]], index: Int): Option[Vector[Rational[T]]] = {
      if (index == system.size) {
        // Do back substitution
        Some(backSubstitute(system, system.size - 1, Vector.fill(system.size)(Rational.ZERO)))
      }
      else {
        val possiblePivot = system(index)(index)
        val swappedCoefficients = if (possiblePivot != Rational.ZERO(n)) system else {
          (index + 1 until system.size).find(r => system(r)(index) != Rational.ZERO(n)) match {
            case None =>
              println("No solution")
              return None
            case Some(r) =>
              println(s"Swapping $r, $index")
              system.updated(index, system(r)).updated(r, system(index))
          }
        }
        val pivot = swappedCoefficients(index)(index)

        val dividedRow = swappedCoefficients(index).map(x => x / pivot)
        val rowProcessed = swappedCoefficients.updated(index, dividedRow)

        val remainingRowsProcessed = (index + 1 until system.size).foldLeft(rowProcessed) {(cfs, i) =>
          val multiplier = -cfs(i)(index)
          val updatedRow = cfs(index).map(_ * multiplier).zip(cfs(i)).map({
            case (a, b) => a + b
          })

          cfs.updated(i, updatedRow)
        }

        helper(remainingRowsProcessed, index + 1)
      }
    }

    helper(system, 0)
  }
}
