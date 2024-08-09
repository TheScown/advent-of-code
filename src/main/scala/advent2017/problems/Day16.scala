package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day16(input: String) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()
    val dancers = ('a' to 'p').toVector

    val result = run(instructions, dancers).mkString("")

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()
    val dancers = ('a' to 'p').toVector

    @tailrec
    def findCycle(dancers: Vector[Char], seen: Set[Vector[Char]], history: Vector[Vector[Char]]): (Int, Int, Vector[Char]) = {
      val next = run(instructions, dancers)

      if (seen.contains(next)) {
        val toStartOfCycle = history.indexOf(next)
        val cycleLength = history.size - toStartOfCycle

        (toStartOfCycle, cycleLength, next)
      }
      else findCycle(next, seen + next, history :+ next)
    }

    val (toStartOfCycle, cycleLength, finalDancers) = findCycle(dancers, Set(dancers), Vector(dancers))
    val runsRequired = 1_000_000_000 - toStartOfCycle
    val runsRequiredAfterCycles = runsRequired % cycleLength
    val result = (0 until runsRequiredAfterCycles).foldLeft(finalDancers)((dancers, _) => run(instructions, dancers)).mkString("")

    println(s"Result 2: $result")
  }

  private def run(instructions: List[Instruction], dancers: Vector[Char]): Vector[Char] = {
    instructions.foldLeft(dancers) { (dancers, instruction) =>
      instruction match {
        case Spin(x) =>
          val (before, after) = dancers.splitAt(dancers.size - x)
          after ++ before
        case Exchange(x, y) =>
          dancers.updated(x, dancers(y)).updated(y, dancers(x))
        case Partner(x, y) =>
          dancers.updated(dancers.indexOf(x), y).updated(dancers.indexOf(y), x)
      }
    }
  }

  def parse(): List[Instruction] = {
    val spin = "s(\\d+)".r
    val exchange = "x(\\d+)/(\\d+)".r
    val partner = "p(.)/(.)".r

    input.split(",").map {
      case spin(x) => Spin(x.toInt)
      case exchange(x, y) => Exchange(x.toInt, y.toInt)
      case partner(x, y) => Partner(x.head, y.head)
    }.toList
  }

  trait Instruction
  case class Spin(x: Int) extends Instruction
  case class Exchange(x: Int, y: Int) extends Instruction
  case class Partner(x: Char, y: Char) extends Instruction
}

case object Day16 extends App {
  val input = Files.lines("2017/day16.txt").head
  val problem = Day16(input)
  problem.solve1()
  problem.solve2()
}
