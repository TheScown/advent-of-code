package space.scown.adventofcode
package advent2017.problems

import advent2017.knothash.KnotHash
import lib.{Complex, DFS, Files, Grid, Problem}

case class Day14(input: String) extends Problem {
  override def solve1(): Unit = {
    val result = binaryStrings
      .map(_.count(_ == '1'))
      .sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val booleans = binaryStrings
      .map(s => s.map(_ == '1').toVector).toVector

    val grid = Grid(booleans)

    val result = grid.zipWithIndex.foldLeft((0, Set[Complex[Int]]())) { (acc, value) =>
      val (flag, address) = value
      val (count, visited) = acc

      if (!flag || visited.contains(address)) acc
      else {
        val reachable = DFS.reachable(address) { (current, _) =>
          grid.neighbours(current).filter(a => grid(a))
        }

        (count + 1, visited ++ reachable)
      }
    }._1

    println(s"Result 2: $result")
  }

  private def binaryStrings = {
    (0 to 127)
      .map(i => s"$input-$i")
      .map(KnotHash.knotHash)
      .map { hash =>
        hash.map { c =>
          val binary = Character.digit(c, 16).toBinaryString
          if (binary.length < 4) "0" * (4 - binary.length) + binary
          else binary
        }.mkString("")
      }
  }
}

case object Day14 extends App {
  val input = Files.lines("2017/day14.txt").head
  val problem = Day14(input)
  problem.solve1()
  problem.solve2()
}

