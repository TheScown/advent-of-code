package space.scown.adventofcode
package advent2016.problems

import lib.{Crypto, Files, Integers, Problem, Timer}

import scala.annotation.tailrec

case class Day14(input: String) extends Problem {
  override def solve1(): Unit = {
    val result = solve(Integers.naturalNumbers, Map(), Vector())(Crypto.md5)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = solve(Integers.naturalNumbers, Map(), Vector()) { s =>
      (0 until 2017).foldLeft(s)((s,_) => Crypto.md5(s))
    }

    println(s"Result 2: $result")
  }

  @tailrec
  private def solve(integers: LazyList[Int], tracker: Map[Int, Char], found: Vector[Int])(hasher: String => String): Int = {
    val pattern = "(.)\\1\\1".r

    if (found.size >= 64) found.sorted.apply(63)
    else integers match {
      case head #:: tail =>
        val hash = hasher(input + head)
        val isKey = pattern.findFirstMatchIn(hash) match {
          case Some(m) => Some(m.group(1).head)
          case _ => None
        }
        val validatesKey = tracker
          .filter { case (i, c) => head < i + 1000 && hash.contains(c.toString * 5) }

        val validatedKeys = validatesKey.keys

        val expired = tracker.filter { case (i, _) => head >= i + 1000 }.keys

        isKey match {
          case Some(c) =>
            solve(tail, tracker -- validatedKeys -- expired + (head -> c), found ++ validatedKeys)(hasher)
          case None => solve(tail, tracker -- validatedKeys -- expired, found ++ validatedKeys)(hasher)
        }
    }
  }
}

case object Day14 extends App {
  val input = Files.lines("2016/day14.txt").head
  val problem = Day14(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
