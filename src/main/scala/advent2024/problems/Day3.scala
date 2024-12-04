package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Problem}

case class Day3(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val pattern = "mul\\(\\d{1,3},\\d{1,3}\\)".r
    val capturePattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r

    val muls = input.flatMap { line =>
      val matches = pattern.findAllIn(line).toVector

      matches.map {
        case capturePattern(x, y) => Mul(x.toInt, y.toInt)
      }
    }

    val result = muls.map(_.result).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val pattern = "do\\(\\)|don't\\(\\)|mul\\(\\d{1,3},\\d{1,3}\\)".r
    val capturePattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r

    val tokens = input.flatMap { line =>
      pattern.findAllIn(line).map {
        case "do()" => Do
        case "don't()" => Dont
        case capturePattern(x, y) => Mul(x.toInt, y.toInt)
      }
    }

    val result = tokens.foldLeft((0, true)) { case ((sum, enabled), token) => token match {
      case Do => (sum, true)
      case Dont => (sum, false)
      case mul@Mul(_, _) =>
        if (enabled) (sum + mul.result, enabled)
        else (sum, enabled)
    } }._1

    println(s"Result 2: $result")
  }

  sealed trait Token
  case object Do extends Token
  case object Dont extends Token

  case class Mul(x: Int, y: Int) extends Token {
    val result: Int = x * y
  }
}

case object Day3 extends App {
  val input = Files.lines("2024/day3.txt")
  val problem = Day3(input)
  problem.solve1()
  problem.solve2()
}
