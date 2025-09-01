package space.scown.adventofcode
package advent2020

import lib.BFS.PathState
import lib.{BFS, Files, Problem}

import scala.annotation.tailrec

case class Day10(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val adapters = input.map(_.toInt).sorted
    val adaptersWithWall = 0 +: adapters
    val adaptersWithWallAndDevice = adaptersWithWall :+ adapters.last + 3
    val sizeOneCount = adaptersWithWallAndDevice.sliding(2).count(v => (v.head - v.last).abs == 1)
    val sizeThreeCount = adaptersWithWallAndDevice.sliding(2).count(v => (v.head - v.last).abs == 3)
    val result = sizeOneCount * sizeThreeCount

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val adapters = input.map(_.toInt).sorted
    val adaptersWithWall = 0 +: adapters
    val adaptersWithWallAndDevice = adaptersWithWall :+ adapters.last + 3

    val bottlenecks = adaptersWithWallAndDevice.sliding(2).filter {
      case Vector(adapter, next) =>
        next - adapter == 3
    }.map(_.head).toSet

    @tailrec
    def helper(remainingItems: Vector[Int], total: Long): Long = {
      if (remainingItems.isEmpty) total
      else {
        val cluster = remainingItems.takeWhile(!bottlenecks.contains(_))
        val fullCluster = if (remainingItems.size > cluster.size) cluster :+ remainingItems(cluster.size) else cluster

        val paths = BFS.reachable(Vector(fullCluster.head)) { path =>
          val currentPosition = path.last
          if (currentPosition == fullCluster.last) Seq()
          else {
            val range = currentPosition + 1 to currentPosition + 3
            val possibleDestinations = fullCluster.filter(range.contains)

            possibleDestinations.map(path :+ _)
          }
        }.filter {
          case PathState(value, _) => value.last == fullCluster.last
        }

        helper(remainingItems.drop(fullCluster.size), total * paths.size)
      }
    }

    val result = helper(adaptersWithWallAndDevice, 1)

    println(s"Result 2: $result")
  }
}

case object Day10 extends App {
  val input = Files.lines("2020/day10.txt")
  val problem = Day10(input)
  problem.solve1()
  problem.solve2()
}
