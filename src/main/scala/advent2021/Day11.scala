package space.scown.adventofcode
package advent2021

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Day11(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val grid = Grid(input.map(line => line.map(c => (c.asDigit, false)).toVector)).zipWithIndex

    val (_, result) = (0 until 100).foldLeft((grid, 0)) { case ((grid, flashCount), _) =>
      val (withoutFlashes, newFlashes) = runStep(grid)

      (withoutFlashes, flashCount + newFlashes)
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(line => line.map(c => (c.asDigit, false)).toVector)).zipWithIndex

    @tailrec
    def helper(grid: Grid[((Int, Boolean), Complex[Int])], stepCount: Int): Int = {
      val (withoutFlashes, newFlashes) = runStep(grid)

      if (newFlashes == grid.size) stepCount
      else helper(withoutFlashes, stepCount + 1)
    }

    val result = helper(grid, 1)

    println(s"Result 2: $result")
  }

  private def runStep(grid: Grid[((Int, Boolean), Complex[Int])]): (Grid[((Int, Boolean), Complex[Int])], Int) = {
    @tailrec
    def helper(queue: Queue[Complex[Int]], grid: Grid[((Int, Boolean), Complex[Int])]): Grid[((Int, Boolean), Complex[Int])] = {
      if (queue.isEmpty) grid
      else {
        val (next, nextQueue) = queue.dequeue

        val currentGridValue = grid(next)
        if (currentGridValue._1._2) {
          helper(nextQueue, grid)
        } else {
          val currentValue = currentGridValue._1._1
          val newValue = currentValue + 1
          val flashed = newValue > 9
          val updatedGrid = grid.updated(next, currentGridValue.copy(_1 = currentGridValue.copy(_1 = newValue, _2 = flashed)))

          if (flashed) {
            val neighbours = updatedGrid.neighboursWithDiagonals(next).map(updatedGrid.apply).filter(!_._1._2).map(_._2)

            val finalQueue = neighbours.foldLeft(nextQueue) {
              (queue, address) => queue.enqueue(address)
            }

            helper(finalQueue, updatedGrid)
          } else helper(nextQueue, updatedGrid)
        }
      }
    }

    val updatedCounts = grid.map { p =>
      p.copy(_1 = p._1.copy(_1 = p._1._1 + 1))
    }

    val startPoints = updatedCounts.filter(_._1._1 > 9).map(_._2).foldLeft(Queue[Complex[Int]]()) {
      (queue, address) => queue.enqueue(address)
    }

    val finalGrid = helper(startPoints, updatedCounts)

    val newFlashes = finalGrid.count(_._1._2)

    val withoutFlashes = finalGrid.map { p =>
      p.copy(_1 = p._1.copy(_1 = if (p._1._1 > 9) 0 else p._1._1, _2 = false))
    }

    (withoutFlashes, newFlashes)
  }

}

case object Day11 extends App {
  val input = Files.lines("2021/day11.txt")
  val problem = Day11(input)
  problem.solve1()
  problem.solve2()
}
