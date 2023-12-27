package space.scown.advent2023
package problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, SortedSet}

case class Day21(initialGrid: Vector[Vector[Char]]) extends Problem {
  override def solve1(): Unit = {
    val grid = initialGrid.updated(startingAddress._1, initialGrid(startingAddress._1).updated(startingAddress._2, '.'))
    val targetDistance = 64

    @tailrec
    def helper(queue: Queue[Day21QueueEntry], even: Set[(Int, Int)], odd: Set[(Int, Int)], seen: Set[Day21QueueEntry]): Int = {
      if (queue.isEmpty)
        even.size
      else {
        val (nextEntry, nextQueue) = queue.dequeue
        nextEntry match {
          case Day21QueueEntry(address@(row, column), distance) =>
            val evenDistance = distance % 2 == 0

            if (evenDistance && odd.contains(address) || !evenDistance && even.contains(address)) {
              helper(nextQueue, even, odd, seen)
            }
            else {
              val newEntries = Vector(
                (row - 1, column),
                (row + 1, column),
                (row, column - 1),
                (row, column + 1)
              )
                .filterNot(_ => distance == targetDistance)
                .filterNot({
                  case (r, c) => r < 0 || r >= grid.size || c < 0 || c >= grid(0).size
                })
                .filterNot({
                  case (r, c) => grid(r)(c) == '#'
                })
                .filterNot(address => evenDistance && odd.contains(address) || !evenDistance && even.contains(address))
                .map(address => Day21QueueEntry(address, distance + 1))
                .filterNot(entry => seen.contains(entry))

              val newQueue = newEntries.foldLeft(nextQueue)((queue, entry) => queue.enqueue(entry))
              val newEven = if (evenDistance) even + address else even
              val newOdd = if (!evenDistance) odd + address else odd

              helper(newQueue, newEven, newOdd, seen ++ newEntries)
            }
        }
      }
    }

    val result = helper(Queue(Day21QueueEntry(startingAddress, 0)), SortedSet(), SortedSet(), Set())

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = initialGrid.updated(startingAddress._1, initialGrid(startingAddress._1).updated(startingAddress._2, '.'))

    @tailrec
    def helper(grid: Vector[Vector[Char]], queue: Queue[Day21QueueEntry], even: Set[(Int, Int)], odd: Set[(Int, Int)], seen: Set[Day21QueueEntry], targetDistance: Int): Int = {
      if (queue.isEmpty)
        if (targetDistance % 2 == 0) even.size else odd.size
      else {
        val (nextEntry, nextQueue) = queue.dequeue
        nextEntry match {
          case Day21QueueEntry(address@(row, column), distance) =>
//            println(s"$distance,${queue.size}")
            val evenDistance = distance % 2 == 0

            if (evenDistance && odd.contains(address) || !evenDistance && even.contains(address)) {
              helper(grid, nextQueue, even, odd, seen, targetDistance)
            }
            else {
              val newEntries = Vector(
                (row - 1, column),
                (row + 1, column),
                (row, column - 1),
                (row, column + 1)
              )
                .filterNot(_ => distance == targetDistance)
                .filterNot({
                  case (r, c) => r < 0 || r >= grid.size || c < 0 || c >= grid(0).size
                })
                .filterNot({
                  case (r, c) => grid(r)(c) == '#'
                })
                .filterNot(address => evenDistance && odd.contains(address) || !evenDistance && even.contains(address))
                .map(address => Day21QueueEntry(address, distance + 1))
                .filterNot(entry => seen.contains(entry))

              val newQueue = newEntries.foldLeft(nextQueue)((queue, entry) => queue.enqueue(entry))
              val newEven = if (evenDistance) even + address else even
              val newOdd = if (!evenDistance) odd + address else odd

              helper(grid, newQueue, newEven, newOdd, seen ++ newEntries, targetDistance)
            }
        }
      }
    }

    val initialStep = grid.size - startingAddress._1 - 1
    val subsequentStep = grid.size

    def calculate(factor: Int): LazyList[Long] = {
      val expandedGrid = expandGrid(grid, factor)
      val expandedStart = (
        startingAddress._1 + factor * initialGrid.size,
        startingAddress._2 + factor * initialGrid(0).size
      )

      val steps = initialStep + factor * subsequentStep

      helper(
        expandedGrid,
        Queue(Day21QueueEntry(expandedStart, 0)),
        SortedSet(),
        SortedSet(),
        Set(),
        steps
      ).toLong #:: calculate(factor + 1)
    }

    val stream = calculate(0)
    val depth = predictionDepth(stream, 0)

    val x = ((26501365 - initialStep) / subsequentStep)

    // x is the number of 131 steps to take, so while we have 4 terms, one of them doesn't count
    val result = predict(stream.take(depth + 1).toVector, x - depth);

    println(s"Result 2: ${result.last}")
  }

  @tailrec
  private def predictionDepth(currentValues: LazyList[Long], depth: Int): Int = {
    if (currentValues.head == 0) depth
    else {
      predictionDepth(differences(currentValues), depth + 1)
    }
  }

  private def predict(currentValues: Vector[Long], n: Int): Vector[Long] = {
    if (currentValues.forall(x => x == 0)) Vector.fill(n)(0)
    else {
      val nextDifferences = predict(differences(currentValues), n)

      nextDifferences.foldLeft(Vector(currentValues.last))((acc, d) => acc :+ (acc.last + d)).tail
    }
  }

  private def differences(values: Vector[Long]): Vector[Long] = {
    values.zip(values.tail).map {
      case (i, j) => j - i
    }
  }

  private def differences(values: LazyList[Long]): LazyList[Long] = {
    values.zip(values.tail).map {
      case (i, j) => j - i
    }
  }

  private def expandGrid(grid: Vector[Vector[Char]], factor: Int): Vector[Vector[Char]] = {
    val range = 0 until 2 * factor

    val expandedRows = grid.map { r =>
      range.foldLeft(r)((acc, _) => acc ++ r)
    }

    range.foldLeft(expandedRows)((acc, _) => acc ++ expandedRows)
  }

  private val startingAddress: (Int, Int) = {
    val matchingRow = initialGrid.find(row => row.indexOf('S') != -1).get
    val column = matchingRow.indexOf('S')
    val row = initialGrid.indexOf(matchingRow)

    (row, column)
  }
}

private case class Day21QueueEntry(address: (Int, Int), distance: Int)

object Day21 {
  def main(args: Array[String]): Unit = {
    val value = Files.grid("day21.txt")
    time(() => Day21(value).solve1())
    time(() => Day21(value).solve2())
  }

}
