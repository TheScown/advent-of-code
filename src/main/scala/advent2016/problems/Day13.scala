package space.scown.adventofcode
package advent2016.problems

import lib._

import scala.math.BigInt.int2bigInt
import scala.math.Numeric.IntIsIntegral

case class Day13(input: String) extends Problem {
  private val favouriteNumber = input.toInt

  override def solve1(): Unit = {
    val target = Complex(31, 39)

    val start = State(Complex(1, 1), 0)

    val result = BFS.solve[State](start, _.address == target) {
      case State(address, moves) =>
        val neighbours = validNeighbours(address)

        neighbours.map(State(_, moves + 1))
    }.moves + 1

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val start = State(Complex(1, 1), 0)

    val result = DFS.reachable(start) {
      case State(address, moves) =>
        if (moves == 50) Seq()
        else validNeighbours(address).map(c => State(c, moves + 1))
    }.size

    println(s"Result 2: $result")
  }

  private def validNeighbours(address: Complex[Int]) = {
    Seq(Complex.ONE, -Complex.ONE, Complex.I, -Complex.I)
      .map(delta => address + delta)
      .filter { c =>
        c.re >= 0 && c.im >= 0 && !isWall(c)
      }
  }

  private def isWall(c: Complex[Int]): Boolean = c match {
    // (x*x + 3*x + 2*x*y + y + y*y) + favouriteNumber, wall if odd bit count
    case Complex(x, y) =>
      val number = x * x + 3 * x + 2 * x * y + y + y * y + favouriteNumber
      number.bitCount % 2 == 1
  }

  case class State(address: Complex[Int], moves: Int) {
    override def equals(obj: Any): Boolean = {
      if (!obj.isInstanceOf[State]) false
      else {
        val other = obj.asInstanceOf[State]

        address == other.address
      }
    }

    override def hashCode(): Int = address.hashCode()
  }
}

case object Day13 extends App {
  val input = Files.lines("2016/day13.txt").head
  val problem = Day13(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
