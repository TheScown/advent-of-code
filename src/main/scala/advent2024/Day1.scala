package space.scown.adventofcode
package advent2024

import lib.{Files, Problem}

case class Day1(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (col1, col2) = columns()

    val result = col1.sorted.zip(col2.sorted).map { case (a, b) => (a - b).abs }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (col1, col2) = columns()

    val result = col1.map(x => x * col2.count(_ == x)).sum

    println(s"Result 2: $result")
  }

  private def columns(): (Vector[Int], Vector[Int]) = {
    val pairs = input.map(_.split("\\s+").toVector.map(_.toInt))
    val columns = pairs.transpose

    (columns.head, columns(1))
  }
}

case object Day1 extends App {
  val input = Files.lines("2024/day1.txt")
  val problem = Day1(input)
  problem.solve1()
  problem.solve2()
}
