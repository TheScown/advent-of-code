package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem}

import scala.util.parsing.combinator.RegexParsers

case class Day8(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()

    val finalRegisters = instructions.foldLeft(Map[String, Int]()) { (registers, instruction) =>
      instruction match {
        case Instruction(operation, condition) =>
          val shouldExecute = evaluateCondition(registers, condition)

          if (shouldExecute) {
            operation match {
              case Inc(register, value) => registers + (register -> (registers.getOrElse(register, 0) + value))
              case Dec(register, value) => registers + (register -> (registers.getOrElse(register, 0) - value))
            }
          }
          else registers
      }
    }

    val result = finalRegisters.maxBy(_._2)._2

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()

    val result = instructions.foldLeft((Map[String, Int](), 0)) { (p, instruction) =>
      val (registers, max) = p

      instruction match {
        case Instruction(operation, condition) =>
          val shouldExecute = evaluateCondition(registers, condition)

          if (shouldExecute) {
            (operation match {
              case Inc(register, value) => register -> (registers.getOrElse(register, 0) + value)
              case Dec(register, value) => register -> (registers.getOrElse(register, 0) - value)
            }) match {
              case update@(_, newValue) =>
                if (newValue > max) (registers + update, newValue)
                else (registers + update, max)
            }
          }
          else (registers, max)
      }
    }._2

    println(s"Result 2: $result")
  }

  private def evaluateCondition(registers: Map[String, Int], condition: Condition) = {
    condition match {
      case Lt(register, value) => registers.getOrElse(register, 0) < value
      case Gt(register, value) => registers.getOrElse(register, 0) > value
      case Lte(register, value) => registers.getOrElse(register, 0) <= value
      case Gte(register, value) => registers.getOrElse(register, 0) >= value
      case Eq(register, value) => registers.getOrElse(register, 0) == value
      case Neq(register, value) => registers.getOrElse(register, 0) != value
    }
  }

  private def parse(): Vector[Instruction] = {
    input.map { line =>
      Grammar.parse(Grammar.instruction, line) match {
        case Grammar.Success(result, _) => result
        case Grammar.NoSuccess.I(s, _) => throw new IllegalStateException(s"Grammar error: $s")
      }
    }
  }

  case object Grammar extends RegexParsers {

    def instruction: Parser[Instruction] = (operation ~ ("if" ~> condition)) ^^ {
      case operation ~ condition => Instruction(operation, condition)
    }

    def operation: Parser[Operation] = inc | dec

    def inc: Parser[Inc] = (register ~ "inc" ~ number) ^^ {
      case register ~ "inc" ~ value => Inc(register, value)
    }

    def dec: Parser[Dec] = (register ~ "dec" ~ number) ^^ {
      case register ~ "dec" ~ value => Dec(register, value)
    }

    def condition: Parser[Condition] = lt | gt | lte | gte | eq | neq

    private def lt: Parser[Lt] = (register ~ "<" ~ number) ^^ {
      case register ~ "<" ~ value => Lt(register, value)
    }

    private def gt: Parser[Gt] = (register ~ ">" ~ number) ^^ {
      case register ~ ">" ~ value => Gt(register, value)
    }

    private def lte: Parser[Lte] = (register ~ "<=" ~ number) ^^ {
      case register ~ "<=" ~ value => Lte(register, value)
    }

    private def gte: Parser[Gte] = (register ~ ">=" ~ number) ^^ {
      case register ~ ">=" ~ value => Gte(register, value)
    }

    private def eq: Parser[Eq] = (register ~ "==" ~ number) ^^ {
      case register ~ "==" ~ value => Eq(register, value)
    }

    private def neq: Parser[Neq] = (register ~ "!=" ~ number) ^^ {
      case register ~ "!=" ~ value => Neq(register, value)
    }

    private def number: Parser[Int] = "0|-?[1-9][0-9]*".r ^^ (s => s.toInt)
    private def register: Parser[String] = "[a-z]+".r
  }

  case class Instruction(operation: Operation, condition: Condition)

  sealed trait Operation
  case class Inc(register: String, value: Int) extends Operation
  case class Dec(register: String, value: Int) extends Operation

  sealed trait Condition
  private case class Lt(register: String, value: Int) extends Condition
  private case class Gt(register: String, value: Int) extends Condition
  private case class Lte(register: String, value: Int) extends Condition
  private case class Gte(register: String, value: Int) extends Condition
  private case class Eq(register: String, value: Int) extends Condition
  private case class Neq(register: String, value: Int) extends Condition
}

case object Day8 extends App {
  val input = Files.lines("2017/day8.txt")
  val problem = Day8(input)
  problem.solve1()
  problem.solve2()
}
