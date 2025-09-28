package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case class Day18(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val tokenSequences = input.map {
      line =>
        Grammar.parseAll(Grammar.tokens, line) match {
          case Grammar.Success(result, _) => result.toVector
          case Grammar.NoSuccess.I(msg, next) =>
            throw new IllegalArgumentException(s"$msg,$next")
        }
    }

    def evaluate(tokens: Vector[Token]): Long = {
      tokens match {
        case Vector(Value(result)) => result
        case lhs +: op +: rhs +: rest =>
          val lhsValue = lhs match {
            case Parens(tokens) => evaluate(tokens)
            case Value(i) => i
          }

          val rhsValue = rhs match {
            case Parens(tokens) => evaluate(tokens)
            case Value(i) => i
          }

          val result = op match {
            case Add => lhsValue + rhsValue
            case Multiply => lhsValue * rhsValue
          }

          evaluate(Value(result) +: rest)
      }
    }

    val results = tokenSequences.map(tokens => evaluate(tokens))

    val result = results.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val tokenSequences = input.map {
      line =>
        ExpressionGrammar.parseAll(ExpressionGrammar.expression, line) match {
          case ExpressionGrammar.Success(result, _) => result
          case ExpressionGrammar.NoSuccess.I(msg, next) =>
            throw new IllegalArgumentException(s"$msg,$next")
        }
    }

    def evaluate(expression: Expression): Long = expression match {
      case ValueExpression(x) => x
      case AddExpression(lhs, rhs) => evaluate(lhs) + evaluate(rhs)
      case MultiplyExpression(lhs, rhs) => evaluate(lhs) * evaluate(rhs)
    }

    val results = tokenSequences.map(tokens => evaluate(tokens))

    val result = results.sum

    println(s"Result 2: $result")
  }

  private sealed trait Token
  private case object Add extends Token
  private case object Multiply extends Token
  private case class Value(i: Long) extends Token
  private case class Parens(tokens: Vector[Token]) extends Token

  private sealed trait Expression
  private case class MultiplyExpression(lhs: Expression, rhs: Expression) extends Expression
  private case class AddExpression(lhs: Expression, rhs: Expression) extends Expression
  private case class ValueExpression(value: Long) extends Expression

  private case object Grammar extends RegexParsers {
    def tokens: Parser[List[Token]] = token *

    def token: Parser[Token] = add | multiply | value | parens

    def parens: Parser[Token] = ("(" ~> tokens <~ ")") ^^ (tokens => Parens(tokens.toVector))

    def value: Parser[Token] = "\\d+".r ^^ (s => Value(s.toLong))

    def add: Parser[Token] = "+" ^^ (_ => Add)

    def multiply: Parser[Token] = "*" ^^ (_ => Multiply)
  }

  private case object ExpressionGrammar extends RegexParsers {
    def expression: Parser[Expression] = multiply

    def multiply: Parser[Expression] = (add ~ rep("*" ~> add)) ^^ {
      case lhs ~ Nil => lhs
      case lhs ~ list => list.foldLeft(lhs) { (acc, expr) =>
        MultiplyExpression(acc, expr)
      }
    }

    def add: Parser[Expression] = (primary ~ rep("+" ~> primary)) ^^ {
      case lhs ~ Nil => lhs
      case lhs ~ list => list.foldLeft(lhs) { (acc, expr) =>
        AddExpression(acc, expr)
      }
    }

    def primary: Parser[Expression] = parens | value
    def parens: Parser[Expression] = "(" ~> expression <~ ")"
    def value: Parser[Expression] = "\\d+".r ^^ (s => ValueExpression(s.toLong))
  }
}

case object Day18 extends App {
  val input = Files.lines("2020/day18.txt")
  val problem = Day18(input)
  problem.solve1()
  problem.solve2()
}
