package space.scown.adventofcode2019
package problems

import intcode.{IntcodeComputer, IntcodeProgram}
import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day7(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val permutations = (0 until 5).permutations

    val result = permutations.map { phases =>
      @tailrec
      def helper(remainingPhases: IndexedSeq[Int], nextInput: Int = 0): Int = {
        if (remainingPhases.isEmpty) return nextInput

        val phase = remainingPhases.head

        val computer = IntcodeComputer(program)
        computer.execute(LazyList(phase, nextInput)).head match {
          case (_, Some(x)) => helper(remainingPhases.tail, x)
          case (_, None) => throw new IllegalStateException(s"No output for phase $phase, input $nextInput")
        }
      }

      helper(phases)
    }.max

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val permutations = (5 to 9).permutations

    val result = permutations.map { phases =>
      println(s"Phases $phases")

      val computers = phases.map { phase => (phase, IntcodeComputer(program)) }

      var lastOutputVar: () => LazyList[Int] = () => throw new IllegalStateException(s"First computer reading before init")
      val (firstPhase, firstComputer) = computers.head
      val firstInputStream = firstPhase #:: 0 #:: (() => {
        println("First computer getting additional input")
        lastOutputVar()
      })()
      val firstOutputStream = firstComputer.execute(firstInputStream).map(_._2).filter(_.isDefined).map(_.get)

      @tailrec
      def helper(remainingComputers: IndexedSeq[(Int, IntcodeComputer)], previousOutput: LazyList[Int]): LazyList[Int] = {
        if (remainingComputers.isEmpty) previousOutput
        else {
          val (phase, computer) = remainingComputers.head
          helper(remainingComputers.tail, computer.execute(phase #:: previousOutput).map(_._2).filter(_.isDefined).map(_.get))
        }
      }

      lastOutputVar = () => helper(computers.tail, firstOutputStream)
      lastOutputVar().last
    }.max

    println(s"Result 2: $result")
  }
}

object Day7 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day7.txt")
    Day7(value).solve1()
    Day7(value).solve2()
  }

}
