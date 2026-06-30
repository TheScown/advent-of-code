package space.scown.adventofcode
package advent2021

import lib.{Complex, Files, Problem}

case class Day5(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val lines = parse()

    val relevantLines = lines.filter(line => line.isVertical || line.isHorizontal)

    val (_, intersections) = relevantLines.foldLeft((Set[Complex[Int]](), Set[Complex[Int]]())) {
      case ((seen, intersections), line) =>
        val rangeSet = Complex.linearRange(
          line.start,
          line.end,
          if (line.isVertical) Complex.I[Int] else Complex.ONE[Int],
          inclusive = true
        ).toSet

        (seen ++ rangeSet, intersections ++ (seen intersect rangeSet))
    }

    val result = intersections.size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val lines = parse()

    val (_, intersections) = lines.foldLeft((Set[Complex[Int]](), Set[Complex[Int]]())) {
      case ((seen, intersections), line) =>
        val step = if (line.isVertical) Complex.I[Int]
        else if (line.isHorizontal) Complex.ONE[Int]
        else if (line.end.im >= line.start.im) Complex(1, 1)
        else Complex(1, -1)

        val rangeSet = Complex.linearRange(
          line.start,
          line.end,
          step,
          inclusive = true
        ).toSet

        (seen ++ rangeSet, intersections ++ (seen intersect rangeSet))
    }

    val result = intersections.size

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Line] = {
    val pattern = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r

    input.map {
      case pattern(x1s, y1s, x2s, y2s) =>
        val (x1, y1, x2, y2) = (x1s.toInt, y1s.toInt, x2s.toInt, y2s.toInt)
        val xCompare = x1.compareTo(x2)
        val yCompare = y1.compareTo(y2)

        if (xCompare < 0) Line(Complex(x1, y1), Complex(x2, y2))
        else if (xCompare > 0) Line(Complex(x2, y2), Complex(x1, y1))
        else if (yCompare < 0) Line(Complex(x1, y1), Complex(x2, y2))
        else Line(Complex(x2, y2), Complex(x1, y1))
    }
  }

  private case class Line(start: Complex[Int], end: Complex[Int]) {
    val isVertical: Boolean = start.re == end.re
    val isHorizontal: Boolean = start.im == end.im
  }

}

case object Day5 extends App {
  val input = Files.lines("2021/day5.txt")
  val problem = Day5(input)
  problem.solve1()
  problem.solve2()
}
