package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem, UnionFind, Vec4}

case class Day25(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val points = parse()

    val unionFind = connectComponents(points)

    val result = unionFind.componentCount

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    println("Result 2: Trigger the Underflow!")
  }

  private def connectComponents(points: Vector[Vec4[Int]]) = {
    val unionFind = UnionFind(points.size)

    for {
      i <- points.indices
      j <- i until points.size
    } {
      val p1 = points(i)
      val p2 = points(j)
      val mh = (p1 - p2).norm1

      if (mh <= 3) {
        unionFind.union(i, j)
      }
    }

    unionFind
  }

  private def parse(): Vector[Vec4[Int]] = {
    input.map { line =>
      val parts = line.split(",").map(_.toInt)
      Vec4(parts(0), parts(1), parts(2), parts(3))
    }
  }
}

case object Day25 extends App {
  val input = Files.lines("2018/day25.txt")
  val problem = Day25(input)
  problem.solve1()
  problem.solve2()
}
