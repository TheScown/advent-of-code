package space.scown.adventofcode
package advent2025.problems

import lib.{Files, Problem, UnionFind}

case class Day8(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val junctionBoxes = parse()

    val connections = possibleConnections(junctionBoxes).take(1000)

    val unionFind = UnionFind(junctionBoxes.size)

    connections.foreach { case (jb1, jb2, _) => unionFind.union(jb1.id, jb2.id) }

    val uniqueCircuits = junctionBoxes.indices.map(unionFind.find).toSet.toVector

    val result = uniqueCircuits.map(unionFind.size(_)).sorted.reverse.take(3).product

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val junctionBoxes = parse()

    val connections = possibleConnections(junctionBoxes)

    val unionFind = UnionFind(junctionBoxes.size)

    val requiredConnections = connections.takeWhile { case (jb1, jb2, _) =>
      unionFind.union(jb1.id, jb2.id)
      unionFind.componentCount != 1
    }.size

    val lastRequiredConnection = connections(requiredConnections)
    val result = lastRequiredConnection._1.position._1 * lastRequiredConnection._2.position._1

    println(s"Result 2: $result")
  }

  private def possibleConnections(junctionBoxes: Vector[JunctionBox]): Vector[(JunctionBox, JunctionBox, Long)] = {
    junctionBoxes.combinations(2)
      .map { pair =>
        val (a, b) = (pair.head, pair.last)

        (a, b, squaredDistance(a, b))
      }
      .toVector
      .sortBy(_._3)
  }

  private def squaredDistance(a: JunctionBox, b: JunctionBox): Long = {
    a match {
      case JunctionBox(_, (x1, y1, z1)) => b match {
        case JunctionBox(_, (x2, y2, z2)) =>
          (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)
      }
    }
  }

  private def parse(): Vector[JunctionBox] = {
    input.zipWithIndex.map { case (line, index) =>
      val parts = line.split(",").map(_.toLong)

      JunctionBox(index, (parts(0), parts(1), parts(2)))
    }
  }

  private case class JunctionBox(id: Int, position: (Long, Long, Long))
}

case object Day8 extends App {
  val input = Files.lines("2025/day8.txt")
  val problem = Day8(input)
  problem.solve1()
  problem.solve2()
}
