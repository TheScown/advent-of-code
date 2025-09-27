package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

import scala.annotation.tailrec
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
        Grammar.parseAll(Grammar.tokens, line) match {
          case Grammar.Success(result, _) => result.toVector
          case Grammar.NoSuccess.I(msg, next) =>
            throw new IllegalArgumentException(s"$msg,$next")
        }
    }

    def evaluate(tokens: Vector[Token]): Long = {
      val withEvaluatedParens = evaluateParens(tokens)
      val withEvaluatedSums = evaluateSums(withEvaluatedParens, Vector())
      evaluateProducts(withEvaluatedSums)
    }

    def evaluateParens(tokens: Vector[Token]): Vector[Token] = {
      tokens.map {
        case Parens(tokens) => Value(evaluate(tokens))
        case v@Value(_) => v
        case Add => Add
        case Multiply => Multiply
      }
    }

    @tailrec
    def evaluateSums(tokens: Vector[Token], intermediate: Vector[Token]): Vector[Token] = {
      tokens match {
        case Vector(v@Value(_)) => intermediate :+ v
        case lhs +: op +: rhs +: rest => op match {
          case Multiply => evaluateSums(rhs +: rest, intermediate :+ lhs :+ op)
          case Add =>
            val lhsValue = lhs match {
              case Value(i) => i
            }

            val rhsValue = rhs match {
              case Value(i) => i
            }

            val result = lhsValue + rhsValue

            evaluateSums(Value(result) +: rest, intermediate)
        }
      }
    }

    def evaluateProducts(tokens: Vector[Token]): Long = {
      tokens.map{
        case Value(x) => x
        case _ => 1
      }.product
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


  private case object Grammar extends RegexParsers {
    def tokens: Parser[List[Token]] = token *

    def token: Parser[Token] = add | multiply | value | parens

    def parens: Parser[Token] = ("(" ~> tokens <~ ")") ^^ (tokens => Parens(tokens.toVector))

    def value: Parser[Token] = "\\d+".r ^^ (s => Value(s.toLong))

    def add: Parser[Token] = "+" ^^ (_ => Add)

    def multiply: Parser[Token] = "*" ^^ (_ => Multiply)
  }
}

case object Day18 extends App {
  val input = Files.lines("2020/day18.txt")
  val problem = Day18(input)
  problem.solve1()
  problem.solve2()
}
