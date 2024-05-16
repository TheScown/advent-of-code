package space.scown.adventofcode
package problems

import lib.{Files, Problem}

case class Day2(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = input.map { s =>
      val parsed = s.split("x")
        .map(_.toInt)

      val sideAreas = (parsed match {
        case Array(a, b, c) => Vector(Vector(a, b), Vector(b, c), Vector(a, c))
      })
        .map(_.product)

      sideAreas.sum * 2 + sideAreas.min
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = input.map { s =>
      val parsed = s.split("x")
        .map(_.toInt)

      val perimeters = (parsed match {
        case Array(a, b, c) => Vector(Vector(a, b), Vector(b, c), Vector(a, c))
      })
        .map(x => x.sum * 2)

      perimeters.min + parsed.product
    }.sum

    println(s"Result 2: $result")
  }
}

case object Day2 extends App {
  val problem = Day2(Files.lines("2015/day2.txt"))

  problem.solve1()
  problem.solve2()
}
