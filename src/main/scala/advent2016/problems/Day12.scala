package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case class Day12(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()

    val result = run(instructions, Map(('a', 0), ('b', 0), ('c', 0), ('d', 0)))

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()

    val result = run(instructions, Map(('a', 0), ('b', 0), ('c', 1), ('d', 0)))

    println(s"Result 2: $result")
  }

  def run(instructions: Vector[Instruction], registers: Map[Char, Int]): Int = {
    @tailrec
    def helper(pc: Int, registers: Map[Char, Int]): Int = {
      if (pc >= instructions.size) registers('a')
      else {
        instructions(pc) match {
          case Inc(Register(x)) => helper(pc + 1, registers + (x -> (registers(x) + 1)))
          case Dec(Register(x)) => helper(pc + 1, registers + (x -> (registers(x) - 1)))
          case Copy(Register(x), Register(y)) =>  helper(pc + 1, registers + (y -> registers(x)))
          case Copy(Value(x), Register(y)) =>  helper(pc + 1, registers + (y -> x))
          case Jnz(Register(x), Value(y)) =>
            if (registers(x) == 0) helper(pc + 1, registers)
            else helper(pc + y, registers)
          case Jnz(Value(x), Value(y)) =>
            if (x == 0) helper(pc + 1, registers)
            else helper(pc + y, registers)
        }
      }
    }

    helper(0, registers)
  }

  def parse(): Vector[Instruction] = {
    input.map { line =>
      AssembunnyGrammar.parse(AssembunnyGrammar.instruction, line) match {
        case AssembunnyGrammar.Success(instruction, _) => instruction
        case error@AssembunnyGrammar.Failure(_, _) => throw new IllegalArgumentException(s"$error")
        case AssembunnyGrammar.Error(msg, _) => throw new IllegalArgumentException(msg)
      }
    }
  }

  private case object AssembunnyGrammar extends RegexParsers {
    def instruction: Parser[Instruction] = cpy | inc | dec | jnz

    private def cpy: Parser[Copy] = ("cpy" ~> (source ~ register)) ^^ {
      case source ~ register => Copy(source, register)
    }

    private def inc: Parser[Inc] = ("inc" ~> register) ^^ (register => Inc(register))

    private def dec: Parser[Dec] = ("dec" ~> register) ^^ (register => Dec(register))

    private def jnz: Parser[Jnz] = ("jnz" ~> (source ~ number)) ^^ {
      case source ~ number => Jnz(source, number)
    }

    private def source = register | number
    private def register = "[abcd]".r ^^ (s => Register(s.head))
    private def number = "(-?\\d+)".r ^^ (s => Value(s.toInt))
  }

  trait Instruction
  private case class Copy(source: Source, dest: Register) extends Instruction
  private case class Inc(register: Register) extends Instruction
  private case class Dec(register: Register) extends Instruction
  private case class Jnz(source: Source, jump: Value) extends Instruction

  trait Source
  private case class Register(name: Char) extends Source
  case class Value(number: Int) extends Source
}

case object Day12 extends App {
  val input = Files.lines("2016/day12.txt")
  val problem = Day12(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
