package space.scown.adventofcode
package advent2021

import lib.{BFS, Files, Problem}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class Day24(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = parse()
    val validSerialNumbers: Set[(State, Long)] = solve()

    val result = validSerialNumbers.map(_._2).max

    if (!simulate(program, result.toString)) {
      println("BAD RESULT")
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val program = parse()
    val validSerialNumbers: Set[(State, Long)] = solve()

    val result = validSerialNumbers.map(_._2).min

    if (!simulate(program, result.toString)) {
      println("BAD RESULT")
    }

    println(s"Result 2: $result")
  }

  private def solve(): Set[(State, Long)] = {
    val divPattern = "div z (\\d+)".r
    val v1Pattern = "add x (-?\\d+)".r
    val v2Pattern = "add y (-?\\d+)".r

    val divisors = select(divPattern)
    val v1s = select(v1Pattern)
    val v2s = select(v2Pattern).grouped(3).flatMap(vs => Seq(vs.last)).toVector

    val initialState = State(Vector[Expression](), Vector.fill(14)(None), 0)

    val minW = 1
    val maxW = 9

    val finalStates = BFS.reachable(initialState) { case State(expressions, assignments, index) =>
      if (index == 14)
        Seq()
      else {
        val divisor = divisors(index)
        val v1 = v1s(index)
        val v2 = v2s(index)

        val dividedExpressions = if (divisor == 1) expressions else {
          expressions.filter(_.powerOf26 > 0).map(e => e.copy(powerOf26 = e.powerOf26 - 1))
        }

        val trailingExpression = expressions.find(_.powerOf26 == 0).getOrElse(Expression(0, 0, 0, powerOf26 = 0, index = 0))
        val xr = Range.inclusive(trailingExpression.minW + v1, trailingExpression.maxW + v1)

        val (x0, x1) = (minW to maxW).partition(n => xr.contains(n))

        val x1Discontinuous = x1.zipWithIndex.find(p => x1.indices.contains(p._2 + 1) && x1(p._2 + 1) != p._1 + 1)
        val x1Parts = if (x1Discontinuous.isDefined) {
          val splitAt = x1Discontinuous.get._2 + 1
          val (left, right) = x1.splitAt(splitAt)
          Seq(left, right)
        } else Seq(x1)

        val x0es = if (x0.isEmpty) Seq() else x0.map { w =>
          val previousW = w - v1
          val newAssignments = assignments.updated(index, Some(w)).updated(trailingExpression.index, Some(previousW - trailingExpression.v2))
          val newTrailingExpression = trailingExpression.copy(minW = previousW, maxW = previousW)
          val updatedExpressions = if (divisor == 1) dividedExpressions.filter(_.powerOf26 > 0) :+ newTrailingExpression else dividedExpressions

          State(updatedExpressions, newAssignments, index + 1)
        }

        val x1es = x1Parts.map { x1Part =>
          val y = 26

          val multipliedExpressions = if (y == 1) dividedExpressions else {
            dividedExpressions.map(e => e.copy(powerOf26 = e.powerOf26 + 1))
          } :+ Expression(minW = x1Part.min + v2, maxW = x1Part.max + v2, v2, 0, index)

          State(multipliedExpressions, assignments, index + 1)
        }

        x0es ++ x1es
      }
    }.filter(_.steps == 14)

    val validFinalStates = finalStates.filter(_.value.expressions.isEmpty)

    validFinalStates.map(_.value).map { state =>
      val validValues = (0 until 14).map { i =>
        state.assignments(i) match {
          case Some(x) => x
        }
      }

      (state, validValues.mkString("").toLong)
    }
  }

  private def select(pattern: Regex): Vector[Int] = {
    input.map {
      case pattern(value) => Some(value.toInt)
      case _ => None
    }.filter(_.isDefined).map(_.get)
  }

  private def parse(): Vector[Instruction] = {
    input.map { line =>
      Grammar.parse(Grammar.instruction, line) match {
        case Grammar.Success(result, _) => result
      }
    }
  }

  private def simulate(program: Vector[Instruction], serial: String): Boolean = {
    @tailrec
    def helper(variables: Map[Variable, Long], index: Int, remainingInstructions: Vector[Instruction]): Boolean = {
      if (remainingInstructions.isEmpty) {
        val result = variables(Variable('z'))
        result == 0
      }
      else {
        val instruction = remainingInstructions.head
        instruction match {
          case Inp(variable) => helper(variables + (variable -> serial(index).asDigit), index + 1, remainingInstructions.tail)
          case Add(variable, Number(x)) => helper(variables + (variable -> (variables(variable) + x)), index, remainingInstructions.tail)
          case Add(variable, v@Variable(_)) => helper(variables + (variable -> (variables(variable) + variables(v))), index, remainingInstructions.tail)
          case Mul(variable, Number(x)) => helper(variables + (variable -> (variables(variable) * x)), index, remainingInstructions.tail)
          case Mul(variable, v@Variable(_)) => helper(variables + (variable -> (variables(variable) * variables(v))), index, remainingInstructions.tail)
          case Div(variable, Number(x)) => helper(variables + (variable -> (variables(variable) / x)), index, remainingInstructions.tail)
          case Div(variable, v@Variable(_)) => helper(variables + (variable -> (variables(variable) / variables(v))), index, remainingInstructions.tail)
          case Mod(variable, Number(x)) => helper(variables + (variable -> (variables(variable) % x)), index, remainingInstructions.tail)
          case Mod(variable, v@Variable(_)) => helper(variables + (variable -> (variables(variable) % variables(v))), index, remainingInstructions.tail)
          case Eql(variable, Number(x)) => helper(variables + (variable -> (if (variables(variable) == x) 1 else 0)), index, remainingInstructions.tail)
          case Eql(variable, v@Variable(_)) => helper(variables + (variable -> (if (variables(variable) == variables(v)) 1 else 0)), index, remainingInstructions.tail)
        }
      }
    }

    helper(Map(
      Variable('w') -> 0L,
      Variable('x') -> 0L,
      Variable('y') -> 0L,
      Variable('z') -> 0L
    ), 0, program)
  }

  private case class State(expressions: Vector[Expression], assignments: Vector[Option[Int]], index: Int)
  private case class Expression(minW: Int, maxW: Int, v2: Int, powerOf26: Int, index: Int)

  private sealed trait Value
  private case class Number(value: Long) extends Value
  private case class Variable(name: Char) extends Value

  private sealed trait Instruction
  private case class Inp(variable: Variable) extends Instruction
  private case class Add(variable: Variable, arg: Value) extends Instruction
  private case class Mul(variable: Variable, arg: Value) extends Instruction
  private case class Div(variable: Variable, arg: Value) extends Instruction
  private case class Mod(variable: Variable, arg: Value) extends Instruction
  private case class Eql(variable: Variable, arg: Value) extends Instruction

  private case object Grammar extends RegexParsers {
    def instruction: Parser[Instruction] = inp | add | mul | div | mod | eql

    def inp: Parser[Inp] = "inp" ~> variable ^^ (v => Inp(v))
    def add: Parser[Add] = "add" ~> (variable ~ value) ^^ {
      case v ~ x => Add(v, x)
    }
    def mul: Parser[Mul] = "mul" ~> (variable ~ value) ^^ {
      case v ~ x => Mul(v, x)
    }
    def div: Parser[Div] = "div" ~> (variable ~ value) ^^ {
      case v ~ x => Div(v, x)
    }
    def mod: Parser[Mod] = "mod" ~> (variable ~ value) ^^ {
      case v ~ x => Mod(v, x)
    }
    def eql: Parser[Eql] = "eql" ~> (variable ~ value) ^^ {
      case v ~ x => Eql(v, x)
    }

    def value: Parser[Value] = number | variable
    def number: Parser[Number] = "-?\\d+".r ^^ (s => Number(s.toLong))
    def variable: Parser[Variable] = "[wxyz]".r ^^ (s => Variable(s.head))
  }
}

case object Day24 extends App {
  val input = Files.lines("2021/day24.txt")
  val problem = Day24(input)
  problem.solve1()
  problem.solve2()
}

