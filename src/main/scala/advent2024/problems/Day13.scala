package space.scown.adventofcode
package advent2024.problems

import lib.{Files, GaussianElimination, Problem, Rational}

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

case class Day13(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val machines = parse()

    val result = machines.map { machine =>
      val states = for {
        a <- 0 to 100
        b <- 0 to 100
      } yield State(machine, a, b)

      val solutions = states.filter(s => s.x == s.machine.px && s.y == s.machine.py)

      if (solutions.isEmpty) 0
      else solutions.minBy(_.cost).cost
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val machines = parse().map(m => m.copy(px = m.px + 10000000000000L, py = m.py + 10000000000000L))

    val result = machines.map { machine =>
      GaussianElimination.solve[Long](machine.equations)
    }
      .filter(_.isDefined)
      .map(_.get)
      .filter(row => row.forall(v => v > Rational.ZERO[Long] && v.isWhole))
      .map(row => 3 * row.head.d + row.last.d)
      .sum

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Machine] = {
    val buttonPattern = "Button ([AB]): X\\+(\\d+), Y\\+(\\d+)".r
    val prizePattern = "Prize: X=(\\d+), Y=(\\d+)".r

    @tailrec
    def helper(input: Vector[String], acc: Vector[Machine]): Vector[Machine] = {
      val (lines, rest) = input.span(_.nonEmpty)

      val machine = lines.foldLeft(Machine(0, 0, 0, 0, 0, 0)) { (machine, line) => line match {
        case buttonPattern("A", x, y) => machine.copy(ax = x.toInt, ay = y.toInt)
        case buttonPattern("B", x, y) => machine.copy(bx = x.toInt, by = y.toInt)
        case prizePattern(x, y) => machine.copy(px = x.toInt, py = y.toInt)
      } }

      if (rest.nonEmpty) helper(rest.tail, acc :+ machine)
      else acc :+ machine
    }

    helper(input, Vector())
  }

  case class State(machine: Machine, aPresses: Long, bPresses: Long) {
    val cost: Long = 3 * aPresses + bPresses
    val x: Long = aPresses * machine.ax + bPresses * machine.bx
    val y: Long = aPresses * machine.ay + bPresses * machine.by
  }

  case class Machine(ax: Long, ay: Long, bx: Long, by: Long, px: Long, py: Long) {
    def equations: Vector[Vector[Rational[Long]]] = {
      Vector(
        Vector(ax, bx, px),
        Vector(ay, by, py),
      )
    }
  }
}

case object Day13 extends App {
  val input = Files.lines("2024/day13.txt")
  val problem = Day13(input)
  problem.solve1()
  problem.solve2()
}
