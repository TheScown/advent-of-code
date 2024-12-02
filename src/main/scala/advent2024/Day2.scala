package space.scown.adventofcode
package advent2024

import lib.{Files, Problem}

case class Day2(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val reports = parse

    val result = reports.count { report =>
      isSafe(report)
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val reports = parse

    val (safe, unsafe) = reports.partition { report =>
      isSafe(report)
    }

    val safeEnough = unsafe.count { report =>
      report.indices
        .map { i =>
          report.slice(0, i) ++ report.slice(i + 1, report.size)
        }
        .exists(isSafe)
    }

    val result = safe.size + safeEnough

    println(s"Result 2: $result")
  }

  private def parse: Vector[Vector[Int]] = {
    input.map(_.split(" ").map(_.toInt).toVector)
  }

  private def isSafe(report: Vector[Int]) = {
    val pairs = report.sliding(2).toVector
    val diffs = pairs.map(p => p.head - p(1))

    val sameSign = diffs.forall(diff => diff.sign == diffs.head.sign)
    val small = diffs.forall(diff => diff.abs <= 3 && diff.abs >= 1)
    sameSign && small
  }
}

case object Day2 extends App {
  val input = Files.lines("2024/day2.txt")
  val problem = Day2(input)
  problem.solve1()
  problem.solve2()
}
