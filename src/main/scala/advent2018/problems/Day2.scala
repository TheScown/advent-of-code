package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem}

case class Day2(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val groups = input.map { s =>
      s.toVector.groupBy(identity).map(p => (p._1, p._2.size))
    }

    val twos = groups.count { map =>
      map.exists { case (_, count) => count == 2}
    }

    val threes = groups.count { map =>
      map.exists { case (_, count) => count == 3}
    }

    val result = twos * threes

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val pairs = input.combinations(2)

    val correctPair = pairs.find {
      case Vector(a, b) =>
        val differentLetters = a.zip(b).count { case (c1, c2) => c1 != c2 }
        differentLetters == 1
    }.get

    val result = correctPair match {
      case Vector(a, b) => a.zip(b).filter(p => p._1 == p._2).map(_._1).mkString("")
    }

    println(s"Result 2: $result")
  }
}

case object Day2 extends App {
  val input = Files.lines("2018/day2.txt")
  val problem = Day2(input)
  problem.solve1()
  problem.solve2()
}
