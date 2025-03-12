package space.scown.adventofcode
package advent2024.problems

import lib.{BFS, Files, Problem, Timer}

case class Day7(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val equations = parse()

    val validEquations = equations.filter { equation =>
      def goal(equation: Equation) = {
        equation.values.size == 1 && equation.values.head == equation.testValue
      }

      BFS.solve(equation)(goal) { (equation, _) =>
        if (equation.values.size == 1) Seq()
        else {
          val lhs = equation.values.head
          val rhs = equation.values.tail.head

          val sum = lhs + rhs
          val product = lhs * rhs

          Seq(
            equation.copy(values = sum +: equation.values.tail.tail),
            equation.copy(values = product +: equation.values.tail.tail),
          ).filter(e => e.testValue >= e.values.head)
        }
      }.isDefined
    }

    val result = validEquations.map(_.testValue).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val equations = parse()

    val validEquations = equations.filter { equation =>
      def goal(equation: Equation) = {
        equation.values.size == 1 && equation.values.head == equation.testValue
      }

      BFS.solve(equation)(goal) { (equation, _) =>
        if (equation.values.size == 1) Seq()
        else {
          val lhs = equation.values.head
          val rhs = equation.values.tail.head

          val sum = lhs + rhs
          val product = lhs * rhs
          val concat = (lhs.toString + rhs.toString).toLong

          Seq(
            equation.copy(values = sum +: equation.values.tail.tail),
            equation.copy(values = product +: equation.values.tail.tail),
            equation.copy(values = concat +: equation.values.tail.tail),
          ).filter(e => e.testValue >= e.values.head)
        }
      }.isDefined
    }

    val result = validEquations.map(_.testValue).sum

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Equation] = {
    input.map { line =>
      val parts = line.split(": ")
      val testValue = parts(0).toLong
      val values = parts(1).split(" ").map(_.toLong).toVector
      Equation(testValue, values)
    }
  }

  private case class Equation(testValue: Long, values: Vector[Long])
}

case object Day7 extends App {
  val input = Files.lines("2024/day7.txt")
  val problem = Day7(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
