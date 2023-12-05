package space.scown.advent2023
package problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day5(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val seedLine = lines.head
    val seeds = seedLine.replace("seeds:", "").trim().split(" ").map(_.toLong)

    val mappings = parseMappings()

    val result = seeds.map { seed =>
      mappings.foldLeft(seed) { (input, mapping) =>
        mapping.ranges.find(r => input >= r.start && input < r.start + r.length) match {
          case Some(Range(start, _, destination)) => destination + (input - start)
          case None => input
        }
      }
    }.min

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val seedLine = lines.head
    val seeds = seedLine.replace("seeds:", "").trim().split(" ").map(_.toLong)
      .grouped(2).map(a => {
        (a(0), a(1))
      })

    val mappings = parseMappings()

    val result = seeds.flatMap { seedPair =>
      mappings.foldLeft(Seq(seedPair)) { (inputRanges, mapping) =>
        inputRanges.flatMap {
          case (inputStart, inputLength) =>
            def helper(inputStart: Long, inputLength: Long, ranges: Vector[Range], acc: Seq[(Long, Long)]): Seq[(Long, Long)] = {
              if (ranges.isEmpty) {
                // Any stray length needs adding on
                return acc :+ (inputStart, inputLength)
              }

              val currentRange = ranges.head

              if (inputStart >= currentRange.start) {
                if (inputStart >= currentRange.start + currentRange.length) {
                  // This range is entirely too early, move on
                  return helper(inputStart, inputLength, ranges.tail, acc)
                }

                // The current range is relevant
                if (inputStart + inputLength < currentRange.start + currentRange.length) {
                  // We fall entirely within this range, we're done
                  val destination = currentRange.destination + inputStart - currentRange.start
                  return acc :+ (destination, inputLength)
                }

                // The current range is too small for the entirety of the input, use what we can, and proceed to next range
                helper(
                  // Start from the end of this range next go
                  currentRange.start + currentRange.length,
                  inputStart + inputLength - (currentRange.start + currentRange.length),
                  ranges.tail,
                  acc :+ (inputStart, currentRange.start + currentRange.length - inputStart)
                )
              }
              else {
                // Input starts earlier than range
                if (inputStart + inputLength < currentRange.start) {
                  // This range falls after the input, we're done
                  return acc :+ (inputStart, inputLength)
                }

                // Input starts before the range but continues into it
                // Map the first part to itself and retry with a reduced input range
                helper(
                  currentRange.start,
                  inputStart + inputLength - currentRange.start,
                  ranges,
                  acc :+ (inputStart, currentRange.start - inputStart)
                )
              }
            }

            helper(inputStart, inputLength, mapping.ranges, Seq())
        }
      }
    }.map(p => p._1).min

    println(s"Result 2: $result")
  }

  private def parseMappings(): Vector[Mapping] = {
    val nonEmptyLines = lines.tail
      .filterNot(line => line.isEmpty)
      .toList

    @tailrec
    def helper(lines: List[String], currentMapping: Option[Mapping], mappings: Vector[Mapping]): Vector[Mapping] = {
      if (lines.isEmpty) {
        val lastMapping = currentMapping.get
        return mappings :+ lastMapping.copy(ranges = lastMapping.ranges.sorted)
      }

      val nextLine = lines.head

      if (!nextLine.head.isDigit) {
        val parts = nextLine.split(" ")(0).split("-to-")
        val nextMapping = Mapping(parts(0), parts(1), Vector())
        helper(lines.tail, Some(nextMapping), currentMapping match {
          case Some(m) => mappings :+ m.copy(ranges = m.ranges.sorted)
          case None => mappings
        })
      }
      else {
        val parts = nextLine.split(" ").map(_.trim).map(_.toLong)
        val destination = parts(0)
        val start = parts(1)
        val length = parts(2)
        val range = Range(start, length, destination)
        helper(
          lines.tail,
          currentMapping match {
            case Some(m) => Some(m.copy(ranges = m.ranges :+ range))
            case None => throw new IllegalArgumentException("No current mapping for range")
          },
          mappings
        )
      }
    }

    helper(nonEmptyLines, None, Vector())
  }
}

case class Mapping(source: String, target: String, ranges: Vector[Range])

case class Range(start: Long, length: Long, destination: Long) extends Ordered[Range] {
  override def compare(that: Range): Int = start.compareTo(that.start)
}

object Day5 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day5.txt")
    Day5(value).solve1()
    time(Day5(value).solve2)
  }

}
