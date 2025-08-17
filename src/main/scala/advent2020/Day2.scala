package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

case class Day2(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val pattern = "(\\d+)-(\\d+) ([a-z]): ([a-z]+)".r

    val result = input.count {
      case pattern(startString, endString, characterString, password) =>
        val range = startString.toInt to endString.toInt
        val char = characterString.head

        val count = password.count(_ == char)
        range.contains(count)
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val pattern = "(\\d+)-(\\d+) ([a-z]): ([a-z]+)".r

    val result = input.count {
      case pattern(startString, endString, characterString, password) =>
        val position1 = startString.toInt - 1
        val position2 = endString.toInt - 1
        val char = characterString.head

        password.charAt(position1) == char ^ password.charAt(position2) == char
    }

    println(s"Result 2: $result")
  }
}

case object Day2 extends App {
  val input = Files.lines("2020/day2.txt")
  val problem = Day2(input)
  problem.solve1()
  problem.solve2()
}
