package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case class Day23(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = parse()
    val startState = Computer(0, 0, 0)

    val result: Int = solve(program, startState)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val program = parse()
    val startState = Computer(0, 1, 0)

    val result: Int = solve(program, startState)

    println(s"Result 2: $result")
  }

  private def solve(program: Vector[Instruction], startState: Computer) = {
    @tailrec
    def helper(computer: Computer): Int = {
      if (computer.pc < 0 || computer.pc >= program.size) computer.b
      else {
        helper(program(computer.pc).execute(computer))
      }
    }

    val result = helper(startState)
    result
  }

  def parse(): Vector[Instruction] = {
    input.map {
      line => Grammar.parse(Grammar.instruction, line) match {
        case Grammar.Success(instruction, _) => instruction
        case error@Grammar.Failure(_, _) => throw new IllegalArgumentException(s"$error")
        case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
      }
    }
  }

  case class Computer(pc: Int, a: Int, b: Int)

  case object Grammar extends RegexParsers {
    def instruction: Parser[Instruction] = hlf | tpl | inc | jmp | jie | jio
    private def hlf: Parser[Half] = ("hlf" ~> register) ^^ (c => Half(c))
    private def tpl: Parser[Triple] = ("tpl" ~> register) ^^ (c => Triple(c))
    private def inc: Parser[Increment] = ("inc" ~> register) ^^ (c => Increment(c))
    private def jmp: Parser[Jump] = ("jmp" ~> number) ^^ (v => Jump(v))
    private def jie: Parser[JumpIfEven] = ("jie" ~> (register ~ (", " ~> number))) ^^ {
      case register ~ number => JumpIfEven(register, number)
    }
    private def jio: Parser[JumpIfOne] = ("jio" ~> (register ~ (", " ~> number))) ^^ {
      case register ~ number => JumpIfOne(register, number)
    }

    private def register: Parser[Char] = "[ab]".r ^^ (s => s.head)
    private def number: Parser[Int] = "[+\\-]".r ~ "\\d+".r ^^ {
      case sign ~ value => (if (sign == "-") -1 else 1) * value.toInt
    }
  }

  trait Instruction {
    def execute(computer: Computer): Computer
  }

  case class Half(register: Char) extends Instruction {
    override def execute(computer: Computer): Computer = register match {
      case 'a' => computer.copy(a = computer.a / 2, pc = computer.pc + 1)
      case 'b' => computer.copy(b = computer.b / 2, pc = computer.pc + 1)
    }
  }

  case class Triple(register: Char) extends Instruction {
    override def execute(computer: Computer): Computer = register match {
      case 'a' => computer.copy(a = computer.a * 3, pc = computer.pc + 1)
      case 'b' => computer.copy(b = computer.b * 3, pc = computer.pc + 1)
    }
  }

  case class Increment(register: Char) extends Instruction {
    override def execute(computer: Computer): Computer = register match {
      case 'a' => computer.copy(a = computer.a + 1, pc = computer.pc + 1)
      case 'b' => computer.copy(b = computer.b + 1, pc = computer.pc + 1)
    }
  }

  case class Jump(offset: Int) extends Instruction {
    override def execute(computer: Computer): Computer = computer.copy(pc = computer.pc + offset)
  }

  case class JumpIfEven(register: Char, offset: Int) extends Instruction {
    override def execute(computer: Computer): Computer = {
      val registerValue = register match {
        case 'a' => computer.a
        case 'b' => computer.b
      }

      if (registerValue % 2 == 0) computer.copy(pc = computer.pc + offset)
      else computer.copy(pc = computer.pc + 1)
    }
  }

  case class JumpIfOne(register: Char, offset: Int) extends Instruction {
    override def execute(computer: Computer): Computer = {
      val registerValue = register match {
        case 'a' => computer.a
        case 'b' => computer.b
      }

      if (registerValue == 1) computer.copy(pc = computer.pc + offset)
      else computer.copy(pc = computer.pc + 1)
    }
  }
}

case object Day23 extends App {
  val input = Files.lines("2015/day23.txt")
  val problem = Day23(input)
  problem.solve1()
  problem.solve2()
}
