package space.scown.adventofcode
package advent2017.problems

import lib.{DFS, Files, Grid, Problem, Timer}

import scala.annotation.tailrec

case class Day21(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse

    val result: Int = solve(instructions, 5)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse

    val result: Int = solve(instructions, 18)

    println(s"Result 2: $result")
  }

  private def solve(instructions: Vector[Instruction], numberOfIterations: Int): Int = {
    @tailrec
    def helper(grid: Grid[Char], iterationCount: Int): Int = {
      if (iterationCount == numberOfIterations) grid.count(_ == '#')
      else {
        val parts = if (grid.rowLength % 2 == 0) grid.grouped(2, 2) else grid.grouped(3, 3)

        val newGrid = Grid.flatten(parts.map { g =>
          val gridsToTest = DFS.reachable(g) { g =>
            Seq(
              g.rotateRight(),
              g.flipHorizontally()
            )
          }

          val matchingInstruction = instructions.find(i => gridsToTest.contains(i.matches))

          matchingInstruction.get.produces
        })

        helper(newGrid, iterationCount + 1)
      }
    }

    val initial = Grid(Vector(
      Vector('.', '#', '.'),
      Vector('.', '.', '#'),
      Vector('#', '#', '#'),
    ))

    val result = helper(initial, 0)
    result
  }

  def parse: Vector[Instruction] = {
    def stringToGrid(s: String) = {
      val rows = s.split("/").toVector
      Grid(rows.map(_.toVector))
    }

    input.map { line =>
      line.split(" => ") match {
        case Array(lhs, rhs) => Instruction(stringToGrid(lhs), stringToGrid(rhs), lhs, rhs)
      }
    }
  }

  case class Instruction(matches: Grid[Char], produces: Grid[Char], lhs: String, rhs: String)
}

case object Day21 extends App {
  val input = Files.lines("2017/day21.txt")
  val problem = Day21(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
