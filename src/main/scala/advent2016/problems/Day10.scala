package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

case class Day10(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse

    @tailrec
    def helper(instructions: Vector[Instruction], bots: Map[Int, Bot] = Map(), outputs: Map[Int, Vector[Int]] = Map()): Int = {
      if (instructions.isEmpty) throw new IllegalStateException("No more instructions")
      else {
        val (valid, invalid) = instructions.partition(i => canProcess(i, bots))

        val (updatedBots, updatedOutputs, resultOption) = processInstructions(bots, outputs, valid)

        if (resultOption.isDefined) resultOption.get
        else helper(invalid, updatedBots, updatedOutputs)
      }
    }

    val result = helper(instructions)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse

    @tailrec
    def helper(instructions: Vector[Instruction], bots: Map[Int, Bot] = Map(), outputs: Map[Int, Vector[Int]] = Map()): Map[Int, Vector[Int]] = {
      if (instructions.isEmpty) outputs
      else {
        val (valid, invalid) = instructions.partition(i => canProcess(i, bots))

        val (updatedBots, updatedOutputs, _) = processInstructions(bots, outputs, valid)

        helper(invalid, updatedBots, updatedOutputs)
      }
    }

    val outputs = helper(instructions)

    val result = outputs(0).head * outputs(1).head * outputs(2).head

    println(s"Result 2: $result")
  }

  private def processInstructions(bots: Map[Int, Bot], outputs: Map[Int, Vector[Int]], valid: Vector[Instruction]) = {
    valid.foldLeft((bots, outputs, None.asInstanceOf[Option[Int]])) {
      case ((bots, outputs, resultOption), instruction) =>
        instruction match {
          case Value(value, botId) =>
            val updatedBots = bots + (botId -> bots.getOrElse(botId, Bot()).give(value))
            (updatedBots, outputs, resultOption)
          case Give(source, low, high) =>
            val sourceBot = bots(source)
            val lowValue = sourceBot.values.min
            val highValue = sourceBot.values.max

            val newResultOption = if (lowValue == 17 && highValue == 61) Some(source) else resultOption

            Seq((low, lowValue), (high, highValue)).foldLeft((bots, outputs, newResultOption)) { case ((bots, outputs, resultOption), (destination, value)) =>
              destination match {
                case BotDestination(botId) =>
                  val updatedBots = bots + (botId -> bots.getOrElse(botId, Bot()).give(value))
                  (updatedBots, outputs, resultOption)
                case OutputDestination(outputId) =>
                  val updatedOutputs = outputs + (outputId -> outputs.getOrElse(outputId, Vector()).appended(value))
                  (bots, updatedOutputs, resultOption)
              }
            }
        }
    }
  }

  private def canProcess(instruction: Instruction, bots: Map[Int, Bot]): Boolean = instruction match {
    case Value(_, _) => true
    case Give(botId, _, _) => bots.getOrElse(botId, Bot()).values.size == 2
  }

  private def parse: Vector[Instruction] = {
    input.map {
      line => Grammar.parse(Grammar.instruction, line) match {
        case Grammar.Success(instruction, _) => instruction
        case error@Grammar.Failure(_, _) => throw new IllegalArgumentException(s"$error")
        case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
      }
    }
  }

  object Grammar extends RegexParsers {
    def instruction: Parser[Instruction] = value | give

    private def value: Parser[Value] = ("value" ~> number ~ ("goes to bot" ~> number)) ^^ {
      case value ~ botId => Value(value.toInt, botId.toInt)
    }

    private def give: Parser[Give] = ("bot" ~> number ~ ("gives low to" ~> destination ~ ("and high to" ~> destination))) ^^ {
      case source ~ (low ~ high) => Give(source.toInt, low, high)
    }

    private def destination: Parser[Destination] = botDestination | outputDestination
    private def botDestination: Parser[BotDestination] = "bot" ~> number ^^ (botId => BotDestination(botId.toInt))
    private def outputDestination: Parser[OutputDestination] = "output" ~> number ^^ (botId => OutputDestination(botId.toInt))

    private def number = "\\d+".r
  }

  trait Instruction
  private case class Value(value: Int, bot: Int) extends Instruction
  private case class Give(botId: Int, low: Destination, high: Destination) extends Instruction

  private trait Destination
  private case class BotDestination(botId: Int) extends Destination
  private case class OutputDestination(outputId: Int) extends Destination

  case class Bot(values: Vector[Int] = Vector()) {
    def give(i: Int): Bot = this.copy(values = values.appended(i))
  }
}

case object Day10 extends App {
  val input = Files.lines("2016/day10.txt")
  val problem = Day10(input)
  problem.solve1()
  problem.solve2()
}
