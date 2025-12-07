package space.scown.adventofcode
package advent2025.problems

import lib.{Complex, DFS, Files, Grid, Problem}

case class Day7(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))

    val start = grid.zipWithIndex.find(_._1 == 'S').get._2
    val direction = -Complex.I[Int]

    val result = DFS.reachable(start) { case (position, _) =>
      val next = grid.next(position, direction)

      grid(position) match {
        case '.' => Seq(next)
        case 'S' => Seq(next)
        case '^' => Seq(position - Complex.ONE, position + Complex.ONE[Int])
      }
    }.count(grid(_) == '^')

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))

    val start = grid.zipWithIndex.find(_._1 == 'S').get._2
    val direction = -Complex.I[Int]

    def helper(position: Complex[Int], cache: Map[Complex[Int], Long]): (Long, Map[Complex[Int], Long]) = {
      if (cache.contains(position)) (cache(position), cache)
      else {
        grid(position) match {
          case '^' =>
            val positionsToTest = Seq(position - Complex.ONE, position + Complex.ONE[Int])

            val (result, finalCache) = positionsToTest.foldLeft((0L, cache)) { case ((sum, cache), position) =>
              val (result, updatedCache) = helper(position, cache)

              (sum + result, updatedCache + (position -> result))
            }

            (result, finalCache + (position -> result))
          case _ =>
            val next = grid.next(position, direction)

            if (next == position) (1L, cache + (position -> 1))
            else {
              val (result, updatedCache) = helper(next, cache)
              (result, updatedCache + (position -> result))
            }
        }
      }
    }

    val (result, _) = helper(start, Map())

    println(s"Result 2: $result")
  }
}

case object Day7 extends App {
  val input = Files.lines("2025/day7.txt")
  val problem = Day7(input)
  problem.solve1()
  problem.solve2()
}
