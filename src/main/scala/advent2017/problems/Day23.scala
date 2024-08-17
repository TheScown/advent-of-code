package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem, Timer, VectorReader}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.parsing.combinator.{Parsers, RegexParsers}

case class Day23(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()
    val optimised = OptimizingGrammar.optimise(instructions)

    @tailrec
    def helper(pc: Int, registers: Map[Char, Long], mulCount: Int = 0): Int = {
      def unwrap(source: Source): Long = {
        source match {
          case Value(x) => x
          case Register(x) => registers(x)
        }
      }

      if (pc < 0 || pc >= optimised.size) mulCount
      else {
        optimised(pc) match {
          case Set(Register(x), source) =>
            val value = unwrap(source)
            helper(pc + 1, registers + (x -> value), mulCount)
          case Sub(Register(x), source) =>
            val value = unwrap(source)
            helper(pc + 1, registers + (x -> (registers(x) - value)), mulCount)
          case Mul(Register(x), source) =>
            val value = unwrap(source)
            helper(pc + 1, registers + (x -> (registers(x) * value)), mulCount + 1)
          case Jnz(conditionSource, offsetSource) =>
            val condition = unwrap(conditionSource)
            val offset = if (condition != 0) unwrap(offsetSource) else 1
            helper(pc + offset.toInt, registers, mulCount)
          case DDividesB(g, f, b, e, d) =>
            val always = registers + (g.name -> 0L) + (e.name -> registers(b.name))
            val newRegisters = if ((registers(d.name) * registers(e.name) - registers(b.name)) % registers(d.name) == 0) always + (f.name -> 0L) else always

            helper(pc + 9, newRegisters, mulCount + (registers(b.name).toInt - registers(e.name).toInt))
          case BIsComposite(g, f, b, e, d, initialE) =>
            val always = registers +
              (g.name -> 0L) +
              (e.name -> registers(b.name)) +
              (d.name -> registers(b.name))


            val setsF = (registers(d.name) until registers(b.name)).exists(d => ((d * initialE) - registers(b.name)) % d == 0)
            val newRegisters = if (setsF) always + (f.name -> 0L) else always

            helper(pc + 14, newRegisters, mulCount + (registers(b.name).toInt - initialE.toInt) * (registers(b.name).toInt - registers(d.name).toInt))
        }
      }
    }

    val registers = ('a' to 'h').map(_ -> 0L).toMap
    val result = helper(0, registers)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()
    val optimised = OptimizingGrammar.optimise(instructions)

    @tailrec
    def helper(pc: Int, registers: Map[Char, Long], instructionCount:Int = 0): Long = {
      def unwrap(source: Source): Long = {
        source match {
          case Value(x) => x
          case Register(x) => registers(x)
        }
      }

      if (pc < 0 || pc >= optimised.size) registers('h')
//      else if (instructionCount == 1000) registers('h')
      else {
        optimised(pc) match {
          case Set(Register(x), source) =>
            val value = unwrap(source)
            helper(pc + 1, registers + (x -> value), instructionCount + 1)
          case Sub(Register(x), source) =>
            val value = unwrap(source)
            helper(pc + 1, registers + (x -> (registers(x) - value)), instructionCount + 1 )
          case Mul(Register(x), source) =>
            val value = unwrap(source)
            helper(pc + 1, registers + (x -> (registers(x) * value)), instructionCount + 1)
          case Jnz(conditionSource, offsetSource) =>
            val condition = unwrap(conditionSource)
            val offset = if (condition != 0) unwrap(offsetSource) else 1
            helper(pc + offset.toInt, registers, instructionCount + 1)
          case DDividesB(g, f, b, e, d) =>
            val always = registers + (g.name -> 0L) + (e.name -> registers(b.name))
            val newRegisters = if ((registers(d.name) * registers(e.name) - registers(b.name)) % registers(d.name) == 0) always + (f.name -> 0L) else always

            helper(pc + 1, newRegisters, instructionCount + 1)
          case BIsComposite(g, f, b, e, d, initialE) =>
            val always = registers +
            (g.name -> 0L) +
            (e.name -> registers(b.name)) +
            (d.name -> registers(b.name))

            val setsF = (registers(d.name) until registers(b.name)).exists(d => ((d * initialE) - registers(b.name)) % d == 0)
            val newRegisters = if (setsF) always + (f.name -> 0L) else always

            helper(pc + 1, newRegisters, instructionCount + 1)
          case Noop() => helper(pc + 1, registers, instructionCount)
        }
      }
    }

    val registers = ('b' to 'h').map(_ -> 0L).toMap + ('a' -> 1L)
    val result = helper(0, registers)

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Instruction] = {
    input.map { line => Grammar.parse(Grammar.instruction, line) match {
      case Grammar.Success(instruction, _) => instruction
      case Grammar.NoSuccess.I(err, _) => throw new IllegalStateException(err)
    }}
  }

  case object Grammar extends RegexParsers {

    def instruction: Parser[Instruction] = set | sub | mul | jnz

    def set: Parser[Set] = ("set" ~> (register ~ source)) ^^ {
      case dest ~ source => Set(dest, source)
    }
    def sub: Parser[Sub] = ("sub" ~> (register ~ source)) ^^ {
      case dest ~ source => Sub(dest, source)
    }
    def mul: Parser[Mul] = ("mul" ~> (register ~ source)) ^^ {
      case dest ~ source => Mul(dest, source)
    }

    def jnz: Parser[Jnz] = ("jnz" ~> (source ~ source)) ^^ {
      case dest ~ source => Jnz(dest, source)
    }

    def source: Parser[Source] = register | value
    def register: Parser[Register] = "[a-z]".r ^^ (s => Register(s.head))
    def value: Parser[Value] = "0|-?[1-9][0-9]*".r ^^ (x => Value(x.toInt))
  }

  case object OptimizingGrammar extends Parsers {
    type Elem = Instruction

    def instructions: Parser[List[Instruction]] = instruction*

    def instruction: Parser[Instruction] = slowMul | slowSub | set | sub | mul | jnz

    def slowMul: Parser[BIsComposite] = set ~ slowSub ~ sub ~ set ~ sub ~ jnz ^? {
      case Set(Register(e1), Value(x)) ~ DDividesB(Register(g1), Register(f), Register(b1), Register(e2), Register(d1))
        ~ Sub(Register(d2), Value(-1)) ~ Set(Register(g2), Register(d3)) ~ Sub(Register(g3), Register(b2))
        ~ Jnz(Register(g4), Value(-13)) if e1 == e2 && g1 == g2 && g2 == g3 && g3 == g4 && d1 == d2 && d2 == d3 && b1 == b2
        => BIsComposite(Register(g1), Register(f), Register(b1), Register(e1), Register(d1), x.toLong)
    }

    def slowSub: Parser[DDividesB] = set ~ mul ~ sub ~ jnz ~ set ~ sub ~ set ~ sub ~ jnz ^? {
      case Set(Register(g1), Register(d)) ~ Mul(Register(g2), Register(e1)) ~ Sub(Register(g3), Register(b1)) ~ Jnz(Register(g4), Value(2))
        ~ Set(Register(f), Value(0)) ~ Sub(Register(e2), Value(-1)) ~ Set(Register(g5), Register(e3)) ~ Sub(Register(g6), Register(b2))
        ~ Jnz(Register(g7), Value(-8)) if g1 == g2 && g2 == g3 && g3 == g4 && g4 == g5 && g5 == g6 && g6 == g7 && e1 == e2 && e2 == e3 && b1 == b2
        => DDividesB(Register(g1), Register(f), Register(b1), Register(e1), Register(d))
    }

    def set: Parser[Set] = (input: Input) => if (input.atEnd) Failure("At end", input) else input.first match {
      case i@Set(_, _) => Success(i, input.rest)
      case _ => Failure("Not an inc", input)
    }

    def sub: Parser[Sub] = (input: Input) => if (input.atEnd) Failure("At end", input) else input.first match {
      case i@Sub(_, _) => Success(i, input.rest)
      case _ => Failure("Not a dec", input)
    }

    def mul: Parser[Mul] = (input: Input) => if (input.atEnd) Failure("At end", input) else input.first match {
      case i@Mul(_, _) => Success(i, input.rest)
      case _ => Failure("Not a dec", input)
    }

    def jnz: Parser[Jnz] = (input: Input) => if (input.atEnd) Failure("At end", input) else input.first match {
      case i@Jnz(_, _) => Success(i, input.rest)
      case _ => Failure("Not a jnz", input)
    }

    def optimise(instructions: Vector[Instruction]): Vector[Instruction] = {
      val newInstructions = OptimizingGrammar.instructions(VectorReader(instructions)) match {
        case OptimizingGrammar.Success(is, _) => is.toVector
        case error@OptimizingGrammar.Failure(_, _) => throw new IllegalArgumentException(s"$error")
        case OptimizingGrammar.Error(msg, _) => throw new IllegalArgumentException(msg)
      }

      @tailrec
      def noopPad(instructions: Vector[Instruction], from: Int = 0): Vector[Instruction] = {
        val toPad = instructions.indexWhere({
          case DDividesB(_, _, _, _, _) => true
          case BIsComposite(_, _, _, _, _, _) => true
          case _ => false
        }, from)

        if (toPad == -1) instructions
        else {
          val (before, after) = instructions.splitAt(toPad + 1)
          val paddingRequired = instructions(toPad) match {
            case DDividesB(_,_,_,_,_) => 8
            case BIsComposite(_, _, _, _, _, _) => 13
          }
          if (after.isEmpty || (after.nonEmpty && !after.head.isInstanceOf[Noop])) {
            noopPad(before ++ Vector.fill(paddingRequired)(Noop()) ++ after, toPad + 1)
          }
          else noopPad(before ++ after, toPad + 1)
        }
      }

      noopPad(newInstructions)
    }
  }

  trait Instruction
  case class Set(dest: Register, source: Source) extends Instruction
  case class Sub(dest: Register, source: Source) extends Instruction
  case class Mul(dest: Register, source: Source) extends Instruction
  case class Jnz(condition: Source, offset: Source) extends Instruction

  case class Noop() extends Instruction
  case class DDividesB(g: Register, f: Register, b: Register, e: Register, d: Register) extends Instruction
  case class BIsComposite(g: Register, f: Register, b: Register, e: Register, d: Register, initialE: Long) extends Instruction

  trait Source
  case class Register(name: Char) extends Source
  case class Value(value: Int) extends Source
}

case object Day23 extends App {
  val input = Files.lines("2017/day23.txt")
  val problem = Day23(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
