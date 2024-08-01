package space.scown.adventofcode
package advent2016.assembunny

import lib.VectorReader

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.parsing.combinator.{Parsers, RegexParsers}

case object Grammar extends RegexParsers {
  def instruction: Parser[Instruction] = cpy | inc | dec | jnz | tgl | out

  private def cpy: Parser[Copy] = ("cpy" ~> (source ~ register)) ^^ {
    case source ~ register => Copy(source, register)
  }

  private def inc: Parser[Inc] = ("inc" ~> register) ^^ (register => Inc(register))

  private def dec: Parser[Dec] = ("dec" ~> register) ^^ (register => Dec(register))

  private def jnz: Parser[Jnz] = ("jnz" ~> (source ~ source)) ^^ {
    case source ~ number => Jnz(source, number)
  }

  private def tgl: Parser[Tgl] = ("tgl" ~> register) ^^ (register => Tgl(register))

  private def out: Parser[Out] = ("out" ~> source) ^^ (source => Out(source))

  private def source: Parser[Source] = register | number
  private def register: Parser[Register] = "[abcd]".r ^^ (s => Register(s.head))
  private def number: Parser[Value] = "(-?\\d+)".r ^^ (s => Value(s.toInt))
}

case object OptimizingGrammar extends Parsers {
  type Elem = Instruction

  def instructions: Parser[List[Instruction]] = instruction*

  def instruction: Parser[Instruction] = multiply | add | primitive

  def multiply: Parser[Multiply] = (add ~ dec ~ jnz) ^? {
    case Add(src@Register(_), dest@Register(_)) ~ Dec(multiplier@Register(x)) ~ Jnz(Register(z), Value(-5)) if z == x => Multiply(src, multiplier, dest)
  }

  def add: Parser[Add] = (inc ~ dec ~ jnz) ^? {
    case Inc(dest@Register(_)) ~ Dec(src@Register(x)) ~ Jnz(Register(z), Value(-2)) if z == x => Add(src, dest)
  } | (dec ~ inc ~ jnz) ^? {
    case Dec(src@Register(x)) ~ Inc(dest@Register(_)) ~ Jnz(Register(z), Value(-2)) if z == x => Add(src, dest)
  }

  def inc: Parser[Inc] = (input: Input) => if (input.atEnd) Failure("At end", input) else input.first match {
    case i@Inc(_) => Success(i, input.rest)
    case _ => Failure("Not an inc", input)
  }

  def dec: Parser[Dec] = (input: Input) => if (input.atEnd) Failure("At end", input) else input.first match {
    case i@Dec(_) => Success(i, input.rest)
    case _ => Failure("Not a dec", input)
  }

  def jnz: Parser[Jnz] = (input: Input) => if (input.atEnd) Failure("At end", input) else input.first match {
    case i@Jnz(_, _) => Success(i, input.rest)
    case _ => Failure("Not a jnz", input)
  }

  // Any instruction we don't need to identify as its own thing
  def primitive: Parser[Instruction] = (input: Input) => if (input.atEnd) Failure("At end", input) else Success(input.first, input.rest)

  def optimise(instructions: Vector[Instruction]): Vector[Instruction] = {
    val newInstructions = OptimizingGrammar.instructions(VectorReader(instructions)) match {
      case OptimizingGrammar.Success(is, _) => is.toVector
      case error@OptimizingGrammar.Failure(_, _) => throw new IllegalArgumentException(s"$error")
      case OptimizingGrammar.Error(msg, _) => throw new IllegalArgumentException(msg)
    }

    @tailrec
    def noopPad(instructions: Vector[Instruction], from: Int = 0): Vector[Instruction] = {
      val toPad = instructions.indexWhere({
        case Multiply(_, _, _) => true
        case Add(_, _) => true
        case _ => false
      }, from)

      if (toPad == -1) instructions
      else {
        val (before, after) = instructions.splitAt(toPad + 1)
        val paddingRequired = instructions(toPad) match {
          case Multiply(_,_,_) => 4
          case Add(_, _) => 2
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

