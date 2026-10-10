package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day17(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (_, _, yMin, _) = parse()

    // At t = 2u + 1, we're back at 0 and v = -u, if that's greater than yMin we'll overshoot
    val lastValidU = -yMin - 1

    val result = lastValidU * (lastValidU + 1) / 2

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (xMin, xMax, yMin, yMax) = parse()

    val uxMin = Math.ceil((-1 + Math.sqrt(1 + 8 * xMin)) / 2).toInt
    val uxMax = xMax
    val uyMin = yMin
    val uyMax = -yMin - 1

    val pairs = for {
      ux <- uxMin to uxMax
      uy <- uyMin to uyMax
    } yield (ux, uy)

    val result = pairs.count { case (ux, uy) =>
      @tailrec
      def helper(t: Int): Boolean = {
        val sy = uy * t - (t * (t - 1)) / 2
        val sx = if (t <= ux) ux * t - (t * (t - 1)) / 2 else ux * (ux + 1) / 2

        if (sx >= xMin && sx <= xMax && sy >= yMin && sy <= yMax) true
        else if (sx > xMax || sy < yMin) false
        else helper(t + 1)
      }

      helper(1)
    }

    println(s"Result 2: $result")
  }

  private def parse(): (Int, Int, Int, Int) = {
    val pattern = "target area: x=(\\d+)..(\\d+), y=(-\\d+)..(-\\d+)".r

    input.head match {
      case pattern(xMin, xMax, yMin, yMax) => (xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt)
    }
  }
}

case object Day17 extends App {
  val input = Files.lines("2021/day17.txt")
  val problem = Day17(input)
  problem.solve1()
  problem.solve2()
}
