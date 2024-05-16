package space.scown.adventofcode
package advent2023.problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

case class Day10(grid: Vector[Vector[Char]]) extends Problem {
  override def solve1(): Unit = {
    val connectedPipes = connectedPipeAddresses(startingAddress)
    val nextAddress = connectedPipes(0)
    val previousAddress = connectedPipes(1)

    @tailrec
    def helper(slow: (Int, Int), fast: (Int, Int), previousSlow: (Int, Int), previousFast: (Int, Int), length: Int): Int = {
      if (length > 0 && fast == startingAddress || fast == nextAddress) length
      else {
        val nextSlow = connectedPipeAddresses(slow).find(a => a != previousSlow).get
        val nextFast = connectedPipeAddresses(fast).find(a => a != previousFast).get
        val nextNextFast = connectedPipeAddresses(nextFast).find(a => a != fast).get

        helper(nextSlow, nextNextFast, slow, nextFast, length + 1)
      }
    }

    val result = helper(startingAddress, startingAddress, previousAddress, previousAddress, 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val connectedPipes = connectedPipeAddresses(startingAddress)
    val nextAddress = connectedPipes(0)
    val previousAddress = connectedPipes(1)

    @tailrec
    def helper(slow: (Int, Int),previousSlow: (Int, Int), pipeAddresses: Set[(Int, Int)], adjacentAddresses: Set[(Int, Int)]): (Set[(Int, Int)], Set[(Int, Int)]) = {
      if (pipeAddresses.nonEmpty && slow == startingAddress) (pipeAddresses, adjacentAddresses)
      else {
        val nextSlow = connectedPipeAddresses(slow).find(a => a != previousSlow).get
        val adjacent = adjacentCells(slow).toSet

        helper(nextSlow, slow, pipeAddresses + slow, adjacentAddresses ++ adjacent)
      }
    }

    val (pipeAddresses, adjacentAddresses) = helper(startingAddress, previousAddress, Set(), Set())

    val searchStartPoints = adjacentAddresses.removedAll(pipeAddresses)

    @tailrec
    def searchHelper(seen: Set[(Int, Int)], queue: SortedSet[(Int, Int)]): Option[Set[(Int, Int)]] = {
      if (queue.isEmpty) return Some(seen)

      val address = queue.head
      val adjacent = adjacentCells(address)

      // If we hit the edge of the map this is one that's not enclosed by the pipe
      if (adjacent.size != 4) return None

      val toTest = adjacent.toSet.removedAll(pipeAddresses).removedAll(seen)

      searchHelper(seen + address, queue.tail ++ toTest)
    }

    val resultSet = searchStartPoints.foldLeft(SortedSet[(Int, Int)]()) { (seen, startPoint) =>
//      println(s"Searching from $startPoint")
      if (seen.contains(startPoint)) {
//        println("Already seen")
        seen
      }
      else {
        searchHelper(Set(), SortedSet(startPoint)) match {
          case Some(newValues) =>
//            println(s"Found: ${newValues.size} new points")
            seen ++ newValues
          case None =>
//            println("Reached edge of map")
            seen
        }
      }
    }

    val doesStartingAddressGoSouth = pipeAddresses.contains((startingAddress._1 + 1, startingAddress._2))
    val crossingTiles = Set('|', '7', 'F') ++ (if (doesStartingAddressGoSouth) Set('S') else Set())

    val insideLoop = resultSet.filter(a => {
      val (row, column) = a
      val count = (0 until column).count(c => {
        // In this particular input S is filling in for |
        pipeAddresses.contains((row, c)) && crossingTiles.contains(grid(row)(c))
      })
      count % 2 == 1
    } )

    println(s"Result 2: ${insideLoop.size}")
  }

  def picksShoelace(): Unit = {
    val connectedPipes = connectedPipeAddresses(startingAddress)
    val previousAddress = connectedPipes(1)

    @tailrec
    def helper(slow: (Int, Int), previousSlow: (Int, Int), pipeAddresses: Vector[(Int, Int)]): Vector[(Int, Int)] = {
      if (pipeAddresses.nonEmpty && slow == startingAddress) pipeAddresses
      else {
        val nextSlow = connectedPipeAddresses(slow).find(a => a != previousSlow).get

        helper(nextSlow, slow, pipeAddresses :+ slow)
      }
    }

    val pipeAddresses = helper(startingAddress, previousAddress, Vector())

    // Shoelace formula: the sum of the determinants of pairs of points in order is twice the area
    val doubleArea = pipeAddresses.zip(pipeAddresses.tail :+ pipeAddresses.head).map { pair =>
      val ((r1, c1), (r2, c2)) = pair
      r1 * c2 - r2 * c1
    }.sum

    // Pick's theorem: A = i + b / 2 - 1. A is half double area and b is pipeAddresses.size
    val result = (doubleArea + 2 - pipeAddresses.size) / 2

    println(s"Result 2: $result")
  }

  private def adjacentCells(address: (Int, Int)) = {
    val (row, column) = address

    Vector(
      (row - 1, column),
      (row + 1, column),
      (row, column - 1),
      (row, column + 1)
    ).filterNot {
      case (i, j) => i < 0 || i >= grid.size || j < 0 || j >= grid(0).size
    }
  }

  private val startingAddress: (Int, Int) = {
    val matchingRow = grid.find(row => row.indexOf('S') != -1).get
    val column = matchingRow.indexOf('S')
    val row = grid.indexOf(matchingRow)

    (row, column)
  }

  private def connectedPipeAddresses(address: (Int, Int)): Vector[(Int, Int)] = {
    val (row, column) = address
    val currentPipe = grid(row)(column)

    val result = adjacentCells(address).filter {
      case (i, j) =>
        val possiblyConnectedPipe = grid(i)(j)

        currentPipe match {
          case '-' =>
            if (j == column - 1) Set('-', 'L', 'F', 'S').contains(possiblyConnectedPipe)
            else if (j == column + 1) Set('-', 'J', '7', 'S').contains(possiblyConnectedPipe)
            else false
          case '|' =>
            if (i == row - 1) Set('|', '7', 'F', 'S').contains(possiblyConnectedPipe)
            else if (i == row + 1) Set('|', 'J', 'L', 'S').contains(possiblyConnectedPipe)
            else false
          case '7' =>
            if (i == row + 1) Set('|', 'L', 'J', 'S').contains(possiblyConnectedPipe)
            else if (j == column - 1) Set('-', 'F', 'L', 'S').contains(possiblyConnectedPipe)
            else false
          case 'F' =>
            if (i == row + 1) Set('|', 'J', 'L', 'S').contains(possiblyConnectedPipe)
            else if (j == column + 1) Set('-', 'J', '7', 'S').contains(possiblyConnectedPipe)
            else false
          case 'L' =>
            if (i == row - 1) Set('|', 'F', '7', 'S').contains(possiblyConnectedPipe)
            else if (j == column + 1) Set('-', 'J', '7', 'S').contains(possiblyConnectedPipe)
            else false
          case 'J' =>
            if (i == row - 1) Set('|', 'F', '7', 'S').contains(possiblyConnectedPipe)
            else if (j == column - 1) Set('-', 'L', 'F', 'S').contains(possiblyConnectedPipe)
            else false
          case 'S' =>
            if (i == row - 1) Set('|', 'F', '7').contains(possiblyConnectedPipe)
            else if (i == row + 1) Set('|', 'L', 'J').contains(possiblyConnectedPipe)
            else if (j == column - 1) Set('-', 'L', 'F').contains(possiblyConnectedPipe)
            else if (j == column + 1) Set('-', 'J', '7').contains(possiblyConnectedPipe)
            else false
          case _ => throw new IllegalStateException(s"Invalid currentPipe $address, $currentPipe")
        }
    }

    if (result.size != 2) throw new IllegalStateException(s"Wrong connections $address, $currentPipe, $result")

    result
  }
}

object Day10 {
  def main(args: Array[String]): Unit = {
    val value = Files.grid("2023/day10.txt")
    Day10(value).solve1()
    time(Day10(value).picksShoelace)
//    time(Day10(value).solve2)
  }

}
