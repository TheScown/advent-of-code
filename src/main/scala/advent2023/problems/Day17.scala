package space.scown.adventofcode
package advent2023.problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.collection.mutable

case class Day17(charGrid: Vector[Vector[Char]]) extends Problem {
  override def solve1(): Unit = {
    val grid = charGrid.map(row => row.map(c => c.asDigit))

    val queue = new mutable.PriorityQueue[Day17QueueEntry]()
    val target = (grid.size - 1, grid(0).size - 1)

    @tailrec
    def helper(seen: Map[((Int, Int), Vector[(Int, Int)]), Int]): Day17QueueEntry = {
      queue.dequeue() match {
        case entry@Day17QueueEntry(address@(row, column), score, history) =>
          val relevantHistory = if (history.size >= 3) history.takeRight(3) else history

//          println(s"$address, $score")
          if (address == target) entry
          else if (seen.contains((address, relevantHistory))) helper(seen)
          else {
            val newAddresses = Vector(
              (row - 1, column),
              (row + 1, column),
              (row, column - 1),
              (row, column + 1)
            )
              .filterNot({
                case (r, c) => r < 0 || r >= grid.size || c < 0 || c >= grid(0).size
              })
              .filterNot(address => history.nonEmpty && address == history.last)
              .filterNot({
                case (r, c) =>
                  // println(s"($r,$c), $history")
                  relevantHistory.size >= 3 && relevantHistory.forall({
                    case (rh, ch) => rh == r || ch == c
                  })
              })

            newAddresses
              .foreach({
                case (r, c) =>
                  val newHistory = history :+ address

                  queue.enqueue(Day17QueueEntry((r, c), grid(r)(c) + score, newHistory))
              })

            helper(seen + ((address, relevantHistory) -> score))
          }
      }
    }

    queue.enqueue(Day17QueueEntry((0, 0), 0, Vector()))

    val result = helper(Map())

//    println(s"${result.history}")
    println(s"Result 1: ${result.score}")
  }

  override def solve2(): Unit = {
    val grid = charGrid.map(row => row.map(c => c.asDigit))

    val queue = new mutable.PriorityQueue[Day17QueueEntry]()
    val target = (grid.size - 1, grid(0).size - 1)

    @tailrec
    def helper(seen: Map[((Int, Int), Vector[(Int, Int)]), Int]): Day17QueueEntry = {
      queue.dequeue() match {
        case entry@Day17QueueEntry(address@(row, column), score, history) =>
          val relevantHistory = if (history.size >= 10) history.takeRight(10) else history

//          println(s"$address, $score")
          if (address == target) entry
          else if (seen.contains((address, relevantHistory))) helper(seen)
          else {
            val newAddresses = Vector(
              (row - 1, column),
              (row + 1, column),
              (row, column - 1),
              (row, column + 1)
            )
              .filterNot({
                case (r, c) => r < 0 || r >= grid.size || c < 0 || c >= grid(0).size
              })
              .filterNot(address => history.nonEmpty && address == history.last)
              .filter({
                case (r, c) =>
                  if (relevantHistory.isEmpty) true
                  else {
                    val sameDirectionHistory = relevantHistory.reverse.takeWhile({
                      case (rh, ch) => rh == row || ch == column
                    })

                    if (sameDirectionHistory.size >= 4) true
                    else r == row && row == relevantHistory(relevantHistory.size - 1)._1 || c == column && column == relevantHistory(relevantHistory.size - 1)._2
                  }
              })
              .filterNot({
                case (r, c) =>
                  relevantHistory.size >= 10 && relevantHistory.forall({
                    case (rh, ch) => rh == r || ch == c
                  })
              })

            newAddresses
              .foreach({
                case (r, c) =>
                  val newHistory = history :+ address

                  queue.enqueue(Day17QueueEntry((r, c), grid(r)(c) + score, newHistory))
              })

            helper(seen + ((address, relevantHistory) -> score))
          }
      }
    }

    queue.enqueue(Day17QueueEntry((0, 0), 0, Vector()))

    val result = helper(Map())

//    println(s"${result.history}")
    println(s"Result 2: ${result.score}")
  }
}

private case class Day17QueueEntry(address: (Int, Int), score: Int, history: Vector[(Int, Int)]) extends Ordered[Day17QueueEntry] {
  override def compare(that: Day17QueueEntry): Int = that.score - this.score
}

object Day17 {
  def main(args: Array[String]): Unit = {
    val value = Files.grid("2023/day17.txt")
    time(() => Day17(value).solve1())
    time(() => Day17(value).solve2())
  }

}
