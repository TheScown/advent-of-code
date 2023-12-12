package space.scown.advent2023
package problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

case class Day12(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = solve1(lines)

    println(s"Result 1: $result")
  }

  private def solve1(lines: Vector[String]): Int = {
    lines.map { line =>
      val parts = line.split(" ")
      val springs = parts(0).split("").map(_.head).toList
      val brokenSets = parts(1).split(",").map(_.toInt).toList

      @tailrec
      def helper(queue: Queue[State], count: Int): Int = {
        if (queue.isEmpty) return count

        val (next, remainingQueue) = queue.dequeue
        val springs = next.springs
        val brokenSets = next.brokenSets
        val previous = next.previous

        if (springs.isEmpty) {
          if (brokenSets.isEmpty || brokenSets.size == 1 && brokenSets.head == 0) {
            //            println(currentString)
            helper(remainingQueue, count + 1)
          }
          else helper(remainingQueue, count)
        }
        else {
          val current = springs.head

          current match {
            case '.' =>
              if (brokenSets.isEmpty) helper(remainingQueue.enqueue(State(springs.tail, brokenSets, current)), count)
              else {
                val nextBrokenSet = brokenSets.head
                if (nextBrokenSet == 0) helper(remainingQueue.enqueue(State(springs.tail, brokenSets.tail, current)), count)
                else if (previous == '.') helper(remainingQueue.enqueue(State(springs.tail, brokenSets, current)), count)
                else helper(remainingQueue, count)
              }
            case '#' =>
              if (brokenSets.isEmpty) helper(remainingQueue, count)
              else {
                val nextBrokenSet = brokenSets.head
                if (nextBrokenSet == 0) helper(remainingQueue, count)
                else helper(remainingQueue.enqueue(State(springs.tail, nextBrokenSet - 1 :: brokenSets.tail, current)), count)
              }
            case '?' =>
              if (brokenSets.isEmpty) helper(remainingQueue.enqueue(State(springs.tail, brokenSets, '.')), count)
              else {
                val nextBrokenSet = brokenSets.head
                if (nextBrokenSet == 0) {
                  helper(
                    remainingQueue.enqueue(State(springs.tail, brokenSets.tail, '.')),
                    count
                  )
                }
                else if (previous == '#') {
                  helper(remainingQueue.enqueue(State(springs.tail, nextBrokenSet - 1 :: brokenSets.tail, '#')), count)
                }
                else {
                  helper(
                    remainingQueue
                      .enqueue(State(springs.tail, brokenSets, '.'))
                      .enqueue(State(springs.tail, nextBrokenSet - 1 :: brokenSets.tail, '#')),
                    count
                  )
                }
              }
          }
        }
      }

      val result = helper(Queue(State(springs, brokenSets, '.')), 0)

      result
    }.sum
  }

  override def solve2(): Unit = {
    val expandedLines = lines.map { line =>
      val parts = line.split(" ")
      val springs = parts(0)
      val brokenSets = parts(1)

      val expandedStrings = Vector.fill(5)(springs).mkString("?")
      val expandedBroken = Vector.fill(5)(brokenSets).mkString(",")

      s"$expandedStrings $expandedBroken"
    }

    val result: Long = solve2Fast(expandedLines)

    println(s"Result 2: $result")
  }

  private def solve2Fast(expandedLines: Vector[String]): Long = {
    expandedLines.map { line =>
      val parts = line.split(" ")
      val springs = parts(0).split("").map(_.head).toList
      val brokenSets = parts(1).split(",").map(_.toInt).toList

//      val cache = mutable.Map[(List[Char], List[Int], Int), Int]()

      val cache = mutable.Map[(List[Char], List[Int], Int), Long]()

      def helper(springs: List[Char], brokenSets: List[Int], brokenCount: Int): Long = {
        val cacheKey = (springs, brokenSets, brokenCount)

        if (cache.contains(cacheKey)) {
//          println(s"Cache hit, $cacheKey")
          return cache(cacheKey)
        }

        if (springs.isEmpty) {
//          println(s"$brokenSets,$brokenCount")
          if ((brokenSets.isEmpty && brokenCount == 0) || (brokenSets.size == 1 && brokenSets.head == brokenCount)) {
            cache.put(cacheKey, 1)
            1
          }
          else {
            cache.put(cacheKey, 0)
            0
          }
        }
        else {
          val remainingBrokenSets = if (brokenSets.isEmpty) Nil else brokenSets.tail

          springs.head match {
            case '.' =>
              val result = if (brokenCount == 0) helper(springs.tail, brokenSets, 0)
              else if (brokenSets.nonEmpty && brokenSets.head == brokenCount) helper(springs.tail, remainingBrokenSets, 0)
              else 0

              cache.put(cacheKey, result)
              result
            case '#' =>
              val result = helper(springs.tail, brokenSets, brokenCount + 1)
              cache.put(cacheKey, result)
              result
            case '?' =>
              val ifBrokenResult = helper(springs.tail, brokenSets, brokenCount + 1)

              val ifWorkingResult = if (brokenCount == 0) helper(springs.tail, brokenSets, 0)
              else if (brokenSets.nonEmpty && brokenSets.head == brokenCount) helper(springs.tail, remainingBrokenSets, 0)
              else 0

              val total = ifBrokenResult + ifWorkingResult
              cache.put(cacheKey, total)
              total
          }
        }
      }

      val result = helper(springs, brokenSets, 0)

//      println(s"RESULT $line, $result")

      result
    }.sum
  }
}

case class State(springs: List[Char], brokenSets: List[Int], previous: Char)

object Day12 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day12.txt")
    Day12(value).solve1()
    time(() => Day12(value).solve2())
  }

}
