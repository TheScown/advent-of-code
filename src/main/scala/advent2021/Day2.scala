package space.scown.adventofcode
package advent2021

import lib.{Complex, Files, Problem}

case class Day2(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()
    val initialPosition = Complex.ZERO[Int]

    val finalPosition = instructions.foldLeft(initialPosition) { case (position, (command, value)) =>
      command match {
        case "forward" => Complex(position.re + value, position.im)
        case "down" => Complex(position.re, position.im + value)
        case "up" => Complex(position.re, position.im - value)
      }
    }

    val result = finalPosition.re * finalPosition.im

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()
    val initialPosition = Complex.ZERO[Int]
    val initialAim = 0

    val (finalPosition, _) = instructions.foldLeft((initialPosition, initialAim)) { case ((position, aim), (command, value)) =>
      command match {
        case "forward" => (Complex(position.re + value, position.im + value * aim), aim)
        case "down" => (position, aim + value)
        case "up" => (position, aim - value)
      }
    }

    val result = finalPosition.re * finalPosition.im

    println(s"Result 2: $result")
  }

  private def parse(): Vector[(String, Int)] = {
    val pattern = "([a-z]+) (\\d+)".r

    input.map {
      case pattern(command, value) => (command, value.toInt)
    }
  }
}

case object Day2 extends App {
  val input = Files.lines("2021/day2.txt")
  val problem = Day2(input)
  problem.solve1()
  problem.solve2()
}
