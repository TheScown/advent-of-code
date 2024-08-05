package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day6(input: String) extends Problem {
  override def solve1(): Unit = {
    val buckets = input.split("\\s+").map(_.toInt).toVector.zipWithIndex

    val (result, _) = findRepeat(buckets, 0, Set())

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val buckets = input.split("\\s+").map(_.toInt).toVector.zipWithIndex

    val (_, startOfCycle) = findRepeat(buckets, 0, Set())
    val (cycleLength, _) = findRepeat(startOfCycle, 0, Set())

    println(s"Result 2: $cycleLength")
  }

  @tailrec
  private def findRepeat(buckets: Vector[(Int, Int)], count: Int, seen: Set[Vector[(Int, Int)]]): (Int, Vector[(Int, Int)]) = {
    if (seen.contains(buckets)) (count, buckets)
    else {
      val (toDistribute, fromBucket) = buckets.maxBy(_._1)
      val updatedBuckets = (fromBucket + 1 to fromBucket + toDistribute).foldLeft(buckets) {
        (buckets, receiver) => {
          val address = receiver % buckets.size
          buckets.updated(address, buckets(address).copy(_1 = buckets(address)._1 + 1))
        }
      }

      findRepeat(updatedBuckets.updated(fromBucket, buckets(fromBucket).copy(_1 = 0)), count + 1, seen + buckets)
    }
  }
}

case object Day6 extends App {
  val input = Files.lines("2017/day6.txt").head
  val problem = Day6(input)
  problem.solve1()
  problem.solve2()
}
