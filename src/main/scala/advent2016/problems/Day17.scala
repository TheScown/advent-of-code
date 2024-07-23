package space.scown.adventofcode
package advent2016.problems

import lib._

import scala.math.Integral.Implicits.infixIntegralOps

case class Day17(input: String) extends Problem {

  private val directions = Vector((Complex.I, 'U'), (-Complex.I, 'D'), (-Complex.ONE, 'L'), (Complex.ONE, 'R'))
  private val validChars = Set('b', 'c', 'd', 'e', 'f')

  override def solve1(): Unit = {
    val start = State(Complex.ZERO, Vector())
    def isComplete(state: State): Boolean = {
      state.address == Complex(3, -3)
    }

    val result = BFS.solve(start, isComplete)(validNeighbours).path.mkString("")

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val start = State(Complex.ZERO, Vector())

    val target = Complex(3, -3)

    val reachableStates = BFS.reachable(start) { state =>
      if (state.address == target) Seq()
      else {
        validNeighbours(state)
      }
    }

    val maxState = reachableStates.filter(_.address == target).maxBy(_.path.size)
    val result = maxState.path.size

    println(s"Result 2: $result")
  }

  private def validNeighbours(state: State): Seq[State] = state match {
    case State(address, path) =>
      val hash = Crypto.md5(input + path.mkString("")).take(4)
      hash.zip(directions)
        .filter { case (c, _) => validChars.contains(c) }
        .map { case (_, delta) => State(address + delta._1, path :+ delta._2) }
        .filter { case State(Complex(re, im), _) => re >= 0 && re < 4 && im <= 0 && im > -4 }
  }

  case class State(address: Complex, path: Vector[Char])
}

case object Day17 extends App {
  val input = Files.lines("2016/day17.txt").head
  val problem = Day17(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
