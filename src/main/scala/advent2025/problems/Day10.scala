package space.scown.adventofcode
package advent2025.problems

import lib.{BFS, Files, Problem}

import com.microsoft.z3.{IntExpr, IntNum}
import nitaii.z3.solver.Z3Utils._
import tools.aqua.turnkey.support.TurnKey

import scala.annotation.tailrec

case class Day10(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val machines = parse()

    val result = machines.map {
      case Machine(lights, buttons, _) =>
        val bestPresses = BFS.solve(Vector.fill(lights.size)(false))(_ == lights) { case (lights, _) =>
          buttons.map { button =>
            button.foldLeft(lights) { (acc, i) =>
              acc.updated(i, !acc(i))
            }
          }
        }

        bestPresses.get.steps
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val machines = parse()

    val solutions = machines.map {
      case Machine(_, buttons, joltages) => solve(buttons, joltages)
    }
    val result = solutions.sum

    println(s"Result 2: $result")
  }

  private def solve(buttons: Vector[Set[Int]], joltages: Vector[Int]): Int = {
    val variables: Seq[IntExpr] = buttons.indices.map { i =>
      Z(s"b$i")
    }

    val gtExpressions = variables.map(_ >= 0)

    val expressions = joltages.zipWithIndex.map { case (joltage, i) =>
      val requiredVariables: Seq[IntExpr] = buttons.zipWithIndex.filter { case (button, _) =>
        button.contains(i)
      }.map { case (_, bi) => Z(s"b$bi") }

      requiredVariables.reduce(_ + _) ~= joltage
    }

    val resultExpression = variables.reduce(_ + _) ~= Z("result")

    @tailrec
    def iterate(target: Int): Int = {
      val solver = newSolver
      gtExpressions.foreach(e => solver.add(e))
      expressions.foreach(e => solver.add(e))
      solver.add(resultExpression)
      solver.add(Z("result") < target)

      if (solver.check() == com.microsoft.z3.Status.SATISFIABLE) {
        val model = solver.getModel
        val value = model.eval(Z("result"), false).asInstanceOf[IntNum]
        iterate(value.getInt)
      }
      else if (target == Int.MaxValue) {
        throw new IllegalStateException("No solution < IntMax!")
      }
      else {
        target
      }
    }

    iterate(Int.MaxValue)
  }

  private def parse(): Vector[Machine] = {
    input.map { line =>
      val (lights, notLights) = line.span(_ != '(')
      val (buttons, joltages) = notLights.span(_ != '{')

      val lightsVector = lights.trim
        .replaceAll("[\\[\\]]", "")
        .toVector
        .map(_ == '#')

      val buttonsVector = buttons.trim
        .split(" ")
        .map { buttonString =>
          buttonString.replaceAll("[()]", "")
            .split(",")
            .map(_.toInt)
            .toSet
        }.toVector

      val joltageVector = joltages.replaceAll("[{}]", "")
        .split(",")
        .map(_.toInt)
        .toVector

      Machine(lightsVector, buttonsVector, joltageVector)
    }
  }

  private case class Machine(lights: Vector[Boolean], buttons: Vector[Set[Int]], joltages: Vector[Int])
}

case object Day10 extends App {
  val input = Files.lines("2025/day10.txt")
  val problem = Day10(input)
  problem.solve1()
  problem.solve2()
}
