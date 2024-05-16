package space.scown.adventofcode
package advent2023.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day8(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val directions = lines.head

    val mapping = parseMap

    @tailrec
    def helper(remainingDirections: String, currentLocation: String, distance: Int): Int = {
      if (remainingDirections.isEmpty) helper(directions, currentLocation, distance)
      else if (currentLocation == "ZZZ") distance
      else {
        val nextLocation = getNextLocation(mapping, remainingDirections, currentLocation)
        helper(remainingDirections.tail, nextLocation, distance + 1)
      }
    }

    val result = helper(directions, "AAA", 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val directions = lines.head

    val mapping = parseMap

    @tailrec
    def singlePathHelper(remainingDirections: String, currentLocation: String, distance: Int): (Int, String, String) = {
      if (remainingDirections.isEmpty) singlePathHelper(directions, currentLocation, distance)
      else if (currentLocation.last == 'Z') {
        (distance, currentLocation, remainingDirections)
      }
      else {
        val nextLocation = getNextLocation(mapping, remainingDirections, currentLocation)
        singlePathHelper(remainingDirections.tail, nextLocation, distance + 1)
      }
    }

    val initialResults = mapping.keySet.filter(k => k.last == 'A').map(k => singlePathHelper(directions, k, 0))

    println(s"Initial results: ${initialResults.map(r => s"${r._1}").mkString(",")}")

    val result = initialResults.map {
      case (x, _, _) => x.toLong
    }.reduce(lcm)

    println(s"Result 2: $result")
  }

  private def getNextLocation(mapping: Map[String, Pair], remainingDirections: String, currentLocation: String) = {
    val nextLocations = mapping(currentLocation)
    val nextStep = remainingDirections.head
    val nextLocation = if (nextStep == 'L') nextLocations.left else nextLocations.right
    nextLocation
  }

  private def parseMap = {
    Map.from(for (
      line <- lines.tail if line.nonEmpty
    ) yield {
      val parts = line.split(" = ")
      val key = parts(0)
      val lr = parts(1).replaceAll("[()]", "").split(", ")
      key -> Pair(left = lr(0), right = lr(1))
    })
  }

  private def lcm(x: Long, y: Long) = {
    x * (y / gcd(x, y))
  }

  private def gcd(x: Long, y: Long): Long = {
    @tailrec
    def helper(a: Long, b: Long): Long = {
      if (b == 0) a
      else helper(b, a % b)
    }

    helper(Math.max(x, y), Math.min(x, y))
  }
}

case class Pair(left: String, right: String)

object Day8 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("2023/day8.txt")
    Day8(value).solve1()
    Day8(value).solve2()
  }

}
