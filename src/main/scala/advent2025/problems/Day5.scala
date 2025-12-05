package space.scown.adventofcode
package advent2025.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day5(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (ranges, ingredients) = parse()

    val result = ingredients.count { ingredient =>
      ranges.exists(range => range.contains(ingredient))
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (ranges, _) = parse()

    val sortedRanges = ranges.sorted

    @tailrec
    def helper(
      remainingRanges: Vector[JumboInclusiveRange],
      acc: Vector[JumboInclusiveRange]
    ): Vector[JumboInclusiveRange] = {
      if (remainingRanges.isEmpty) acc
      else {
        val next = remainingRanges.head
        val (mergeable, rest) = remainingRanges.tail.span(_.overlaps(next))

        if (mergeable.nonEmpty) {
          val merged = (next +: mergeable).reduce(_ union _)
          helper(merged +: rest, acc)
        } else {
          helper(rest, acc :+ next)
        }
      }
    }

    val mergedRanges = helper(sortedRanges, Vector())
    val result = mergedRanges.map(_.size).sum

    println(s"Result 2: $result")
  }

  private def parse(): (Vector[JumboInclusiveRange], Vector[Long]) = {
    val (rangeLines, rest) = input.span(_.nonEmpty)
    val ingredients = rest.tail.map(_.toLong)

    val ranges = rangeLines.map { line =>
      val parts = line.split("-")

      JumboInclusiveRange(parts(0).toLong, parts(1).toLong)
    }

    (ranges, ingredients)
  }

  private case class JumboInclusiveRange(start: Long, end: Long) extends Ordered[JumboInclusiveRange] {
    val size: Long = end - start + 1

    def contains(x: Long): Boolean = x >= start && x <= end

    def union(other: JumboInclusiveRange): JumboInclusiveRange = {
      JumboInclusiveRange(math.min(start, other.start), math.max(end, other.end))
    }

    def overlaps(other: JumboInclusiveRange): Boolean = {
      contains(other.start) || contains(other.end) || other.contains(start) || other.contains(end)
    }

    override def compare(that: JumboInclusiveRange): Int = start.compare(that.start)
  }
}

case object Day5 extends App {
  val input = Files.lines("2025/day5.txt")
  val problem = Day5(input)
  problem.solve1()
  problem.solve2()
}
