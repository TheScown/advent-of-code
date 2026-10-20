package space.scown.adventofcode
package advent2021

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

case class Day18(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val numbers = parse()

    val sum = numbers.reduce(add)

    val result = sum.magnitude

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val numbers = parse()

    val result = numbers.combinations(2).map { case Vector(a, b) =>
      math.max(add(a, b).magnitude, add(b, a).magnitude)
    }.max

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Pair] = {
    input.filter(_.nonEmpty).map { line =>
      Grammar.parse(Grammar.pair, line)
    }.map(_.get)
  }

  private def add(lhs: SnailfishNumber, rhs: SnailfishNumber): SnailfishNumber = {
    val sum = Pair(lhs, rhs)

    @tailrec
    def reduce(input: SnailfishNumber): SnailfishNumber = {
      def parentMap(input: SnailfishNumber): Map[SnailfishNumber, Pair] = {
        def helper(current: SnailfishNumber, map: Map[SnailfishNumber, Pair]): Map[SnailfishNumber, Pair] = {
          current match {
            case RegularNumber(_) => map
            case p@Pair(lhs, rhs) =>
              val updated = map + (lhs -> p) + (rhs -> p)
              helper(rhs, helper(lhs, updated))
          }
        }

        helper(input, Map())
      }

      @tailrec
      def replace(current: SnailfishNumber, replacement: SnailfishNumber, parents: Map[SnailfishNumber, Pair]): SnailfishNumber = {
        parents.get(current) match {
          case Some(parent) =>
            val newParent = if (parent.lhs == current) parent.copy(lhs = replacement) else parent.copy(rhs = replacement)
            replace(parent, newParent, parents)
          case None => replacement
        }
      }

      def explode(root: SnailfishNumber): SnailfishNumber = {
        val parents = parentMap(root)

        def findDeepValue(current: SnailfishNumber, depth: Int): Option[Pair] = {
          current match {
            case RegularNumber(_) => None
            case p@Pair(lhs, rhs) =>
              if (depth > 4) Some(p)
              else {
                findDeepValue(lhs, depth + 1) match {
                  case None => findDeepValue(rhs, depth + 1)
                  case some => some
                }
              }
          }
        }

        @tailrec
        def findLeft(current: SnailfishNumber): Option[RegularNumber] = {
          @tailrec
          def descend(s: SnailfishNumber): RegularNumber = s match {
            case Pair(_, rhs) => descend(rhs)
            case r@RegularNumber(_) => r
          }

          parents.get(current) match {
            case Some(parent) =>
              if (parent.rhs == current) Some(descend(parent.lhs))
              else findLeft(parent)
            case None => None
          }
        }

        @tailrec
        def findRight(current: SnailfishNumber): Option[RegularNumber] = {
          @tailrec
          def descend(s: SnailfishNumber): RegularNumber = s match {
            case Pair(lhs, _) => descend(lhs)
            case r@RegularNumber(_) => r
          }

          parents.get(current) match {
            case Some(parent) =>
              if (parent.lhs == current) Some(descend(parent.rhs))
              else findRight(parent)
            case None => None
          }
        }

        findDeepValue(root, 1) match {
          case None => root
          case Some(p@Pair(RegularNumber(l), RegularNumber(r))) =>
            val left = findLeft(p)
            val right = findRight(p)

            val (afterLeft, afterLeftParents) = left match {
              case Some(rn@RegularNumber(n)) =>
                val newRoot = replace(rn, RegularNumber(n + l), parents)
                (newRoot, parentMap(newRoot))
              case None => (root, parents)
            }

            val (_, afterRightParents) = right match {
              case Some(rn@RegularNumber(n)) =>
                val newRoot = replace(rn, RegularNumber(n + r), afterLeftParents)
                (newRoot, parentMap(newRoot))
              case None => (afterLeft, afterLeftParents)
            }

            replace(p, RegularNumber(0), afterRightParents)
          case Some(p) => throw new IllegalStateException(s"Invalid deep pair: $p")
        }
      }

      def split(root: SnailfishNumber): SnailfishNumber = {
        val parents = parentMap(root)

        def findBigValue(current: SnailfishNumber): Option[RegularNumber] = {
          current match {
            case rn@RegularNumber(n) if n > 9 => Some(rn)
            case RegularNumber(_) => None
            case Pair(lhs, rhs) =>
              findBigValue(lhs) match {
                case None => findBigValue(rhs)
                case some => some
              }
          }
        }

        findBigValue(root) match {
          case None => root
          case Some(rn@RegularNumber(n)) => replace(
            rn,
            Pair(RegularNumber(n / 2), RegularNumber(n / 2 + n % 2)),
            parents
          )
        }
      }

      val exploded = explode(input)

      if (exploded != input) reduce(exploded)
      else {
        val splitted = split(exploded)

        if (splitted != exploded) reduce(splitted)
        else splitted
      }
    }

    reduce(sum)
  }

  private sealed trait SnailfishNumber {
    def magnitude: Int
    override def equals(obj: Any): Boolean = super.equals(obj)
    override def hashCode(): Int = super.hashCode()
  }
  private case class RegularNumber(value: Int) extends SnailfishNumber {
    override def magnitude: Int = value
  }
  private case class Pair(lhs: SnailfishNumber, rhs: SnailfishNumber) extends SnailfishNumber {
    override def magnitude: Int = 3 * lhs.magnitude + 2 * rhs.magnitude
  }

  private case object Grammar extends RegexParsers {
    def snailfishNumber: Parser[SnailfishNumber] = pair | number

    def pair: Parser[Pair] = "[" ~> snailfishNumber ~ ("," ~> snailfishNumber <~ "]") ^^ {
      case lhs ~ rhs => Pair(lhs, rhs)
    }

    def number: Parser[RegularNumber] = "\\d+".r ^^ (s => RegularNumber(s.toInt))
  }
}

case object Day18 extends App {
  val input = Files.lines("2021/day18.txt")
  val problem = Day18(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
