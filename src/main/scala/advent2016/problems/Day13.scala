package space.scown.adventofcode
package advent2016.problems

import lib.{Complex, Files, Problem, Timer}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.math.Integral.Implicits.infixIntegralOps

case class Day13(input: String) extends Problem {
  private val favouriteNumber = input.toInt

  override def solve1(): Unit = {
    val target = Complex(31, 39)

    @tailrec
    def helper(queue: Queue[State], cache: Set[Complex]): Int = {
      if (queue.isEmpty) throw new IllegalStateException("No states left")
      else {
        queue.dequeue match {
          case (State(address, moves), nextQueue) =>
            val neighbours = validNeighbours(address, cache)

            neighbours.find(_ == target) match {
              case Some(_) => moves + 1
              case None =>
                val updatedQueue = neighbours.foldLeft(nextQueue)((queue, n) => queue.enqueue(State(n, moves + 1)))
                helper(updatedQueue, cache ++ neighbours)
            }
        }
      }
    }

    val start = Complex(1, 1)

    val result = helper(Queue(State(start, 0)), Set(start))

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    @tailrec
    def helper(queue: Queue[State], cache: Set[Complex]): Int = {
      if (queue.isEmpty) cache.size
      else {
        queue.dequeue match {
          case (State(address, moves), nextQueue) =>
            val neighbours = if (moves == 50) Seq() else validNeighbours(address, cache)

            val updatedQueue = neighbours.foldLeft(nextQueue)((queue, n) => queue.enqueue(State(n, moves + 1)))
            helper(updatedQueue, cache ++ neighbours)
        }
      }
    }

    val start = Complex(1, 1)

    val result = helper(Queue(State(start, 0)), Set(start))

    println(s"Result 2: $result")
  }

  private def validNeighbours(address: Complex, cache: Set[Complex]) = {
    Seq(Complex.ONE, -Complex.ONE, Complex.I, -Complex.I)
      .map(delta => address + delta)
      .filter { c =>
        c.re >= 0 && c.im >= 0 && !cache.contains(c) && !isWall(c)
      }
  }

  private def isWall(c: Complex): Boolean = c match {
    // (x*x + 3*x + 2*x*y + y + y*y) + favouriteNumber, wall if odd bit count
    case Complex(x, y) =>
      val number = x * x + 3 * x + 2 * x * y + y + y * y + favouriteNumber
      number.bitCount % 2 == 1
  }

  case class State(address: Complex, moves: Int) {
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

case object Problem extends App {
  val input = Files.lines("2016/day13.txt").head
  val problem = Day13(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
