package space.scown.adventofcode
package advent2020

import lib.{Files, Problem, Timer}

case class Day23(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val initialState = input.head.toVector.map(_.asDigit)

    val finalMapList = simulate(initialState, 100, 9)

    val sequence = finalMapList.cycleTo(1).tail.toVector.dropRight(1)

    val result = sequence.mkString("")

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val initialState = input.head.toVector.map(_.asDigit) ++ (10 to 1_000_000)

    val finalMapList = simulate(initialState, 10_000_000, 1_000_000)

    val result = finalMapList(1).toLong * finalMapList(finalMapList(1)).toLong

    println(s"Result 2: $result")
  }

  private def simulate(initialState: Vector[Int], iterations: Int, maxCup: Int, debug: Boolean = false): CircularMapList[Int] = {
    val index = initialState.zip(initialState.tail :+ initialState.head).toMap
    val initialCup = initialState.head

    (0 until iterations).foldLeft(CircularMapList(index, initialCup, debug = debug)) { (state, i) =>
      val currentCup = state.head
      val tail = state.tail
      val (pickedUp, rest) = tail.splitAt(3)

      val insertAfter = {
        val basicRange = (currentCup - 1) to Math.max(1, currentCup - 4) by -1

        val fullRange = if (basicRange.size == 4) basicRange
        else {
          val remainingRange = maxCup to (maxCup - 3 + basicRange.size) by -1
          basicRange ++ remainingRange
        }

        fullRange.find(!pickedUp.contains(_)).get
      }

      val next = state.insert(currentCup, rest.head).splice(pickedUp.head, pickedUp.last, insertAfter).tail
      next
    }
  }

  case class CircularMapList[T](private val underlyingMap: Map[T, T], override val head: T, debug: Boolean = false) extends Iterable[T] {
    override def iterator: Iterator[T] = MapListIterator()

    override def tail: CircularMapList[T] = CircularMapList(underlyingMap, underlyingMap(head), debug)

    override def size: Int = underlyingMap.size

    override def splitAt(n: Int): (Vector[T], CircularMapList[T]) = {
      val dropped = take(n).toVector

      (dropped, CircularMapList(underlyingMap, this(dropped.last), debug))
    }

    def cycleTo(newHead: T): CircularMapList[T] = CircularMapList(underlyingMap, newHead, debug)

    def insert(value: T, next: T) = CircularMapList(underlyingMap + (value -> next), head, debug)

    def splice(first: T, last: T, at: T): CircularMapList[T] = {
      val next = underlyingMap(at)

      val newMap = underlyingMap + (at -> first) + (last -> next)
      CircularMapList(
        newMap,
        head,
        debug
      )
    }

    def apply(index: T): T = underlyingMap(index)

    private case class MapListIterator() extends Iterator[T] {
      private var current = head
      private var initial = true

      override def hasNext: Boolean = {
        initial || current != head
      }

      override def next(): T = {
        initial = false
        val next = current
        current = underlyingMap(current)
        next
      }
    }
  }
}

case object Day23 extends App {
  val input = Files.lines("2020/day23.txt")
  val problem = Day23(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
