package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day19(input: String) extends Problem {
  override def solve1(): Unit = {
    val initialElfCount = input.toInt

    @tailrec
    def helper(elfCount: Int, firstElf: Int, presentCount: Int, lastPresentCount: Int): Int = {
      if (elfCount == 1) firstElf
      else {
        val nextElfCount = elfCount / 2
        val nextPresentCount = presentCount * 2
        val nextFirstElf = if (elfCount % 2 == 0) firstElf else firstElf + nextPresentCount
        val nextLastPresentCount = if (elfCount % 2 == 0) {
          nextPresentCount + lastPresentCount
        } else firstElf + nextPresentCount

        helper(nextElfCount, nextFirstElf, nextPresentCount, nextLastPresentCount)
      }
    }

    val result = helper(initialElfCount, 1, 1, 1)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val initialElfCount = input.toInt
    val initialElves = Vector.fill(initialElfCount)(1).zipWithIndex.map(p => Node(p._2 + 1, p._1, null, null))

    initialElves.zip(initialElves.tail :+ initialElves.head).foreach { case (a, b) =>
      a.next = b
      b.prev = a
    }

    @tailrec
    def advance(node: Node, n: Int): Node = {
      if (n == 0) node
      else advance(node.next, n - 1)
    }

    @tailrec
    def helper(current: Node, middle: Node, count: Int): Int = {
      if (count == 1) current.id
      else {
        val stolen = middle.presents
        current.presents += stolen
        val nextMiddle = if (count % 2 == 0) middle.remove() else middle.remove().next
        helper(current.next, nextMiddle, count - 1)
      }
    }

    val result = helper(initialElves.head, advance(initialElves.head, initialElfCount / 2), initialElfCount)

    println(s"Result 2: $result")
  }

  case class Node(id: Int, var presents: Int, var next: Node, var prev: Node) {

    def remove(): Node = {
      next.prev = prev
      prev.next = next
      next
    }

    // Override to prevent stack overflow in the debugger
    override def toString: String = s"Node($id,$presents,next=${next.id},prev=${prev.id})"

  }
}

case object Day19 extends App {
  val input = Files.lines("2016/day19.txt").head
  val problem = Day19(input)
  problem.solve1()
  problem.solve2()
}
