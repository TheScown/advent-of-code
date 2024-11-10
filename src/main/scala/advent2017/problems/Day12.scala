package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem, UnionFind}

case class Day12(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()
    val unionFind: UnionFind = connectComponents(instructions)

    val result = unionFind.size(0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()
    val unionFind: UnionFind = connectComponents(instructions)

    val result = unionFind.componentCount

    println(s"Result 2: $result")
  }

  private def connectComponents(instructions: Vector[(Int, Set[Int])]) = {
    val unionFind = UnionFind(instructions.size)

    instructions.foreach {
      case (head, tail) => tail.foreach(c => unionFind.union(head, c))
    }
    unionFind
  }

  def parse(): Vector[(Int, Set[Int])] = {
    input.map { line =>
      val parts = line.split("<->")
      val head  = parts(0).trim.toInt
      val tail = parts(1).split(",").map(_.trim.toInt).toSet
      (head, tail)
    }
  }
}

case object Day12 extends App {
  val input = Files.lines("2017/day12.txt")
  val problem = Day12(input)
  problem.solve1()
  problem.solve2()
}
