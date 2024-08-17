package space.scown.adventofcode
package advent2017.problems

import lib.{BFS, DFS, Files, Problem, Timer}

case class Day24(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val possibleBridges: Set[Bridge] = getPossibleBridges

    val result = possibleBridges.maxBy(_.strength).strength

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val possibleBridges = getPossibleBridges

    val result = possibleBridges.max(Ordering.by[Bridge, (Int, Int)](s => (s.partsUsed, s.strength))).strength

    println(s"Result 2: $result")
  }

  private def getPossibleBridges: Set[Bridge] = {
    val parts = parse()
    val partsIndex = parts.foldLeft(Map[Int, Set[(Int, Int)]]()) { (acc, part) =>
      val (l, r) = part
      acc + (l -> (acc.getOrElse(l, Set()) + part)) + (r -> (acc.getOrElse(r, Set()) + part))
    }

    BFS.reachable(Bridge(0, 0, 0, partsIndex)) {
      case Bridge(strength, port, partsUsed, remainingParts) =>
        val validParts = remainingParts.getOrElse(port, Set())

        validParts.toSeq.map {
          case p@(l, r) =>
            val withPartRemoved = remainingParts + (l -> (remainingParts.getOrElse(l, Set()) - p)) + (r -> (remainingParts.getOrElse(r, Set()) - p))
            Bridge(strength + l + r, if (l != port) l else r, partsUsed + 1, withPartRemoved)
        }
    }
  }

  def parse(): Set[(Int, Int)] = {
    val pattern = "(\\d+)/(\\d+)".r

    input.map {
      case pattern(l, r) => (l.toInt, r.toInt)
    }.toSet
  }

  private case class Bridge(strength: Int, nextPort: Int, partsUsed: Int, remainingParts: Map[Int, Set[(Int, Int)]])
}

case object Day24 extends App {
  val input = Files.lines("2017/day24.txt")
  val problem = Day24(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
