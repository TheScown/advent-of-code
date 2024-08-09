package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

case class Day18(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()
    val registers = Map[Char, Long]()

    @tailrec
    def helper(pc: Long, registers: Map[Char, Long], lastSnd: Long): Long = {
      def read(x: Char): Long = registers.getOrElse(x, 0)

      def unwrap(source: Source) = {
        source match {
          case Value(x) => x
          case Register(x) => read(x)
        }
      }

      if (pc < 0 || pc >= instructions.size) throw new IllegalStateException("No more instructions")
      else {
        instructions(pc.toInt) match {
          case Snd(Register(x)) =>
            helper(pc + 1, registers, read(x))
          case Rcv(Register(x)) => if (read(x) != 0) lastSnd else helper(pc + 1, registers, lastSnd)
          case Set(Register(dest), source) =>
            val rhs = unwrap(source)
            helper(pc + 1, registers + (dest -> rhs), lastSnd)
          case Add(Register(dest), source) =>
            val rhs = unwrap(source)
            helper(pc + 1, registers + (dest -> (read(dest) + rhs)), lastSnd)
          case Mul(Register(dest), source) =>
            val rhs = unwrap(source)
            helper(pc + 1, registers + (dest -> (read(dest) * rhs)), lastSnd)
          case Mod(Register(dest), source) =>
            val rhs = unwrap(source)
            helper(pc + 1, registers + (dest -> (read(dest) % rhs)), lastSnd)
          case Jgz(condition, offset) =>
            val conditionValue = unwrap(condition)
            val offsetValue = unwrap(offset)

            if (conditionValue > 0) helper(pc + offsetValue, registers, lastSnd)
            else helper(pc + 1, registers, lastSnd)
        }
      }
    }

    val result = helper(0, registers, 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()
    val registers = Map[Char, Long]()

    @tailrec
    def helper(a: Computer, b: Computer): (Computer, Computer) = {
      a match {
        case Computer(_, pc, registers, queue, sendCount, _, _) =>
          def read(x: Char): Long = registers.getOrElse(x, 0)

          def unwrap(source: Source) = {
            source match {
              case Value(x) => x
              case Register(x) => read(x)
            }
          }

          if (pc < 0 || pc >= instructions.size) {
            if (b.terminated) {
              (a.copy(terminated = true), b)
            }
            else {
              helper(b, a.copy(terminated = true))
            }
          }
          else {
            instructions(pc.toInt) match {
              case Snd(Register(x)) =>
                helper(a.copy(pc = pc + 1, sendCount = sendCount + 1), b.copy(queue = b.queue :+ read(x)))
              case Rcv(Register(x)) =>
                if (queue.nonEmpty) {
                  helper(a.copy(pc = pc + 1, registers = registers + (x -> queue.head), queue = queue.tail, receiving = false), b)
                }
                else {
                  if (b.terminated || (b.receiving && b.queue.isEmpty)) {
                    (a.copy(terminated = true), b)
                  }
                  else {
                    helper(b, a.copy(receiving = true))
                  }
                }
              case Set(Register(dest), source) =>
                val rhs = unwrap(source)
                helper(a.copy(pc = pc + 1, registers = registers + (dest -> rhs)), b)
              case Add(Register(dest), source) =>
                val rhs = unwrap(source)
                helper(a.copy(pc = pc + 1, registers = registers + (dest -> (read(dest) + rhs))), b)
              case Mul(Register(dest), source) =>
                val rhs = unwrap(source)
                helper(a.copy(pc = pc + 1, registers = registers + (dest -> (read(dest) * rhs))), b)
              case Mod(Register(dest), source) =>
                val rhs = unwrap(source)
                helper(a.copy(pc = pc + 1, registers = registers + (dest -> (read(dest) % rhs))), b)
              case Jgz(condition, offset) =>
                val conditionValue = unwrap(condition)
                val offsetValue = unwrap(offset)

                if (conditionValue > 0) helper(a.copy(pc = pc + offsetValue), b)
                else helper(a.copy(pc = pc + 1), b)
            }
          }
      }
    }

    val finalComputers = helper(
      Computer(0, 0, registers, Vector(), 0, receiving = false, terminated = false),
      Computer(1, 0, registers + ('p' -> 1), Vector(), 0, receiving = false, terminated = false)
    )

    val computer1 = if (finalComputers._1.id == 1) finalComputers._1 else finalComputers._2
    val result = computer1.sendCount

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Instruction] = {
    input.map { line => Grammar.parse(Grammar.instruction, line) match {
      case Grammar.Success(instruction, _) => instruction
      case Grammar.NoSuccess.I(err, _) => throw new IllegalStateException(err)
    }}
  }

  case class Computer(id: Int, pc: Long, registers: Map[Char, Long], queue: Vector[Long], sendCount: Int, receiving: Boolean, terminated: Boolean)

  case object Grammar extends RegexParsers {

    def instruction: Parser[Instruction] = snd | rcv | set | add | mul | mod | jgz

    def snd: Parser[Snd] = ("snd" ~> register) ^^ (register => Snd(register))
    def rcv: Parser[Rcv] = ("rcv" ~> register) ^^ (register => Rcv(register))
    def set: Parser[Set] = ("set" ~> (register ~ source)) ^^ {
      case dest ~ source => Set(dest, source)
    }
    def add: Parser[Add] = ("add" ~> (register ~ source)) ^^ {
      case dest ~ source => Add(dest, source)
    }
    def mul: Parser[Mul] = ("mul" ~> (register ~ source)) ^^ {
      case dest ~ source => Mul(dest, source)
    }
    def mod: Parser[Mod] = ("mod" ~> (register ~ source)) ^^ {
      case dest ~ source => Mod(dest, source)
    }

    def jgz: Parser[Jgz] = ("jgz" ~> (source ~ source)) ^^ {
      case dest ~ source => Jgz(dest, source)
    }

    def source: Parser[Source] = register | value
    def register: Parser[Register] = "[a-z]".r ^^ (s => Register(s.head))
    def value: Parser[Value] = "0|-?[1-9][0-9]*".r ^^ (x => Value(x.toInt))
  }

  trait Instruction
  case class Snd(register: Register) extends Instruction
  case class Rcv(register: Register) extends Instruction
  case class Set(dest: Register, source: Source) extends Instruction
  case class Add(dest: Register, source: Source) extends Instruction
  case class Mul(dest: Register, source: Source) extends Instruction
  case class Mod(dest: Register, source: Source) extends Instruction
  case class Jgz(condition: Source, offset: Source) extends Instruction

  trait Source
  case class Register(name: Char) extends Source
  case class Value(value: Int) extends Source
}

case object Day18 extends App {
  val input = Files.lines("2017/day18.txt")
  val problem = Day18(input)
  problem.solve1()
  problem.solve2()
}
