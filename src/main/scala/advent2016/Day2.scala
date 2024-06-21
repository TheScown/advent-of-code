package space.scown.adventofcode
package advent2016

import lib.{Complex, Files, Grid, Problem}

import scala.math.Integral.Implicits.infixIntegralOps

case class Day2(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val keypad: Grid[Option[Int]] = Grid(Vector(
      Vector(Some(1), Some(2), Some(3)),
      Vector(Some(4), Some(5), Some(6)),
      Vector(Some(7), Some(8), Some(9)),
    ))

    val result = solve(keypad)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val keypad = Grid(Vector(
      Vector(None, None, Some(1), None, None),
      Vector(None, Some(2), Some(3), Some(4), None),
      Vector(Some(5), Some(6), Some(7), Some(8), Some(9)),
      Vector(None, Some(10), Some(11), Some(12), None),
      Vector(None, None, Some(13), None, None),
    ))

    val result = solve(keypad)

    println(s"Result 2: $result")
  }

  private def solve(keypad: Grid[Option[Int]]): String = {
    input.map { line =>
      val start = keypad.indexOf(Some(5)).get

      val end = line.foldLeft(start) { (position, c) =>
        val delta = c match {
          case 'R' => Complex.ONE
          case 'L' => -Complex.ONE
          case 'U' => Complex.I
          case 'D' => -Complex.I
        }

        val possibleNextPosition = keypad.next(position, delta)

        if (keypad(possibleNextPosition).isDefined) possibleNextPosition
        else position
      }

      keypad(end).get.toHexString.toUpperCase()
    }.mkString("")
  }
}

case object Day2 extends App {
  val input = Files.lines("2016/day2.txt")
  val problem = Day2(input)
  problem.solve1()
  problem.solve2()
}
