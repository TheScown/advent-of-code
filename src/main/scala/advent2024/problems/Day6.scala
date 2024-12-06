package space.scown.adventofcode
package advent2024.problems

import lib.{Complex, Files, Grid, Problem, Timer}

import scala.annotation.tailrec

case class Day6(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val start = grid.indexOf('^').get
    val startDirection = Complex.I[Int]

    @tailrec
    def helper(position: Complex[Int], direction: Complex[Int], visited: Set[Complex[Int]]): Set[Complex[Int]] = {
      val next = grid.next(position, direction)

      if (next == position) visited
      else if (grid(next) == '#') helper(position, direction * -Complex.I[Int], visited)
      else helper(next, direction, visited + next)
    }

    val result = helper(start, startDirection, Set(start)).size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val start = grid.indexOf('^').get
    val startDirection = Complex.I[Int]

    @tailrec
    def helper(
      position: Complex[Int],
      direction: Complex[Int],
      visited: Set[(Complex[Int], Complex[Int])] = Set(),
      obstructions: Set[Complex[Int]] = Set(),
      testedForObstruction: Set[Complex[Int]] = Set()
    ): Set[Complex[Int]] = {
      val next = grid.next(position, direction)

      if (next == position)
        obstructions
      else {
        @tailrec
        def willIntersect(
          position: Complex[Int],
          direction: Complex[Int],
          visited: Set[(Complex[Int], Complex[Int])],
          grid: Grid[Char]
        ): Boolean = {
          val next = grid.next(position, direction)

          if (next == position) false
          else if (visited.contains((next, direction))) true
          else if (grid(next) == '#') willIntersect(position, direction * -Complex.I[Int], visited + ((position, direction)), grid)
          else willIntersect(next, direction, visited + ((position, direction)), grid)
        }

        val canPlaceObstruction = next != start && grid(next) != '#' && !testedForObstruction.contains(next) && willIntersect(
          position,
          direction,
          visited,
          grid.updated(next, '#')
        )

        val updatedObstructions = if (canPlaceObstruction) obstructions + next else obstructions

        if (grid(next) == '#') helper(position, direction * -Complex.I[Int], visited + ((position, direction)), updatedObstructions, testedForObstruction + next)
        else helper(next, direction, visited + ((position, direction)), updatedObstructions, testedForObstruction + next)
      }
    }

    val obstructions = helper(start, startDirection)
    val result = obstructions.size

    println(s"Result 2: $result")
  }

}

case object Day6 extends App {
  val input = Files.lines("2024/day6.txt")
  val problem = Day6(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}