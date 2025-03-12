package space.scown.adventofcode
package advent2024.problems

import lib.{BFS, Files, Problem}

import scala.annotation.tailrec

case class Day17(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (registers, program) = parse()

    val output = run(0, registers, program, Vector())
    val result = output.mkString(",")

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (registers, program) = parse()

    val finalState = BFS.solve(State(0, Vector()))(_.outputs == program) {
      case (State(a, outputs), _) =>
        if (outputs.size > program.size) Seq()
        else {
          (0 until 8).flatMap { n =>
            val actualA = (a << 3) | n
            val outputForA = run(0, registers + ('A' -> actualA), program, Vector())

            if (outputForA == program.slice(program.size - outputForA.size, program.size)) Seq(State(actualA, outputForA))
            else Seq()
          }
        }
    }

    val result = finalState.get.value.a

    println(s"Result 2: $result")
  }

  case class State(a: Long, outputs: Vector[Int])

  @tailrec
  private def run(pc: Int, registers: Map[Char, Long], program: Vector[Int], outputs: Vector[Int]): Vector[Int] = {
    def comboOperand(o: Int): Long = o match {
      case 4 => registers('A')
      case 5 => registers('B')
      case 6 => registers('C')
      case x if x <= 3 => x
      case _ => throw new IllegalStateException(s"Unknown combo operand $o")
    }

    def dv(operand: Int) = {
      registers('A') / Math.pow(2, comboOperand(operand)).toLong
    }

    if (pc >= program.size) outputs
    else {
      val opcode = program(pc)
      val operand = program(pc + 1)

      opcode match {
        case 0 =>
          val result = dv(operand)
          run(pc + 2, registers + ('A' -> result), program, outputs)
        case 1 =>
          val result = registers('B') ^ operand
          run(pc + 2, registers + ('B' -> result), program, outputs)
        case 2 =>
          val result = comboOperand(operand) % 8
          run(pc + 2, registers + ('B' -> result), program, outputs)
        case 3 =>
          if (registers('A') == 0) run(pc + 2, registers, program, outputs)
          else run(operand, registers, program, outputs)
        case 4 =>
          val result = registers('B') ^ registers('C')
          run(pc + 2, registers + ('B' -> result), program, outputs)
        case 5 =>
          val result = comboOperand(operand) % 8
          run(pc + 2, registers, program, outputs :+ result.toInt)
        case 6 =>
          val result = dv(operand)
          run(pc + 2, registers + ('B' -> result), program, outputs)
        case 7 =>
          val result = dv(operand)
          run(pc + 2, registers + ('C' -> result), program, outputs)
      }
    }
  }

  private def parse(): (Map[Char, Long], Vector[Int]) = {
    val registerPattern = "Register ([ABC]): (\\d+)".r
    val programPattern = "Program: ([\\d,]+)".r

    input.filter(_.nonEmpty).foldLeft((Map[Char, Long](), Vector[Int]())) { case ((registers, program), line) => line match {
      case registerPattern(registerName, value) => (registers + (registerName.head -> value.toLong), program)
      case programPattern(programCsv) => (registers, programCsv.split(",").map(_.toInt).toVector)
    }}
  }
}

case object Day17 extends App {
  val input = Files.lines("2024/day17.txt")
  val problem = Day17(input)
  problem.solve1()
  problem.solve2()
}
