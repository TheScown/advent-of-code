package space.scown.adventofcode
package advent2024.problems

import lib.{Complex, Files, Grid, Problem}

case class Day14(input: Vector[String]) extends Problem {
  private val rows = 103
  private val columns = 101

  override def solve1(): Unit = {
    val (robots, gridWithRobots) = initialise(rows, columns)

    val (finalGrid, _) = simulate(100, robots, gridWithRobots)
    
    val removeCentre = finalGrid.zipWithIndex.map { case (set, i) =>
      if (i.re == finalGrid.centre.re || i.im == finalGrid.centre.im) Set()
      else set
    }
    
    val grouped = removeCentre.grouped(rows / 2 + 1, columns / 2 + 1)

    val result = grouped.map { grid =>
      grid.foldLeft(0) { (sum, set) =>
        sum + set.size
      }
    }.values.flatten.product

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (robots, gridWithRobots) = initialise(rows, columns)

    val result = 65 * 101 + 22

    simulate(result, robots, gridWithRobots, printAfter = result)

    println(s"Result 2: $result")
  }

  private def simulate(steps: Int, robots: Vector[Robot], gridWithRobots: Grid[Set[Robot]], printAfter: Int = -1): (Grid[Set[Robot]], Vector[Robot]) = {
    (1 to steps).foldLeft((gridWithRobots, robots)) { case ((grid, robots), step) =>
      val newRobots = robots.map { robot =>
        val next = grid.next(robot.p, robot.v)
        robot.copy(p = next)
      }

      val newGrid = robots.zip(newRobots).foldLeft(grid) { case (grid, (robot, newRobot)) =>
        grid.updated(robot.p, grid(robot.p) - robot).updated(newRobot.p, grid(newRobot.p) + newRobot)
      }

      if (printAfter != -1 && step >= printAfter) {
        println(step, newGrid.map { set =>
          if (set.nonEmpty) '#' else '.'
        })
      }

      (newGrid, newRobots)
    }
  }

  private def initialise(rows: Int, columns: Int): (Vector[Robot], Grid[Set[Robot]]) = {
    val robots = parse()
    val grid = Grid.of(rows, columns, Set[Robot](), wrapping = true)

    val gridWithRobots = robots.foldLeft(grid) { (grid, robot) =>
      grid.updated(robot.p, grid(robot.p) + robot)
    }
    (robots, gridWithRobots)
  }

  private def parse(): Vector[Robot] = {
    val pattern = "p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)".r

    input.zipWithIndex.map { case (line, index) => line match {
      case pattern(px, py, vx, vy) => Robot(index, Complex(px.toInt, -py.toInt), Complex(vx.toInt, -vy.toInt))
    } }
  }

  case class Robot(id: Int, p: Complex[Int], v: Complex[Int])
}

case object Day14 extends App {
  val input = Files.lines("2024/day14.txt")
  val problem = Day14(input)
  problem.solve1()
  problem.solve2()
}
