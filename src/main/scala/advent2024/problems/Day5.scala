package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Problem}

case class Day5(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (greaterThan, reports) = parse()

    val validReports = reports.filter { report =>
      val sortedReport = report.sorted(comparator(greaterThan))
      report == sortedReport
    }

    val result = validReports.map(report => report(report.size / 2)).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (greaterThan, reports) = parse()

    val sortedReports = reports.map(_.sorted(comparator(greaterThan)))

    val sortedInvalidReports = reports.zip(sortedReports).filter(p => p._1 != p._2).map(_._2)

    val result = sortedInvalidReports.map(report => report(report.size / 2)).sum

    println(s"Result 2: $result")
  }

  private def parse(): (Map[Int, Set[Int]], Vector[Vector[Int]]) = {
    val (mappings, reports) = input.span(_.nonEmpty)

    val greaterThan = mappings.map { line =>
      val parts = line.split("\\|")
      parts(0).toInt -> parts(1).toInt
    }.groupMap(_._1)(_._2).map(p => (p._1, p._2.toSet))

    (greaterThan, reports.tail.map(_.split(",").map(_.toInt).toVector))
  }

  def comparator(greaterThan: Map[Int, Set[Int]]): Ordering[Int] = {
    (a: Int, b: Int) =>
      if (a == b) 0
      else if (greaterThan.getOrElse(a, Set()).contains(b)) -1
      else if (greaterThan.getOrElse(b, Set()).contains(a)) 1
      else throw new IllegalStateException(s"No rules for $a,$b")
  }
}

case object Day5 extends App {
  val input = Files.lines("2024/day5.txt")
  val problem = Day5(input)
  problem.solve1()
  problem.solve2()
}
