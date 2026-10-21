package space.scown.adventofcode
package advent2021

import lib.{Files, Problem, Timer, Zipper}

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

case class Day18(input: Vector[String]) extends Problem {
  type SnailfishNumber = Zipper.Tree[Int]

  override def solve1(): Unit = {
    val numbers = parse()

    val sum = numbers.reduce(add)

    val result = magnitude(sum)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val numbers = parse()

    val result = numbers.combinations(2).map { case Vector(a, b) =>
      math.max(magnitude(add(a, b)), magnitude(add(b, a)))
    }.max

    println(s"Result 2: $result")
  }

  private def parse(): Vector[SnailfishNumber] = {
    input.filter(_.nonEmpty).map { line =>
      Grammar.parse(Grammar.pair, line)
    }.map(_.get)
  }

  private def magnitude(tree: SnailfishNumber): Int = tree match {
    case Zipper.Leaf(x) => x
    case Zipper.Fork(l, r) => 3 * magnitude(l) + 2 * magnitude(r)
  }

  private def add(lhs: SnailfishNumber, rhs: SnailfishNumber): SnailfishNumber = {
    val sum = Zipper.Fork(lhs, rhs)

    @tailrec
    def reduce(input: SnailfishNumber): SnailfishNumber = {
      def explode(root: SnailfishNumber): SnailfishNumber = {
        def findDeepValue(current: Zipper.Location[Int], depth: Int): Option[Zipper.Location[Int]] = {
          current match {
            case Zipper.Location(Zipper.Leaf(_), _) => None
            case loc@Zipper.Location(Zipper.Fork(_, _), _) =>
              if (depth > 4) Some(loc)
              else {
                findDeepValue(Zipper.left(current), depth + 1) match {
                  case None => findDeepValue(Zipper.right(current), depth + 1)
                  case some => some
                }
              }
          }
        }

        @tailrec
        def findLeft(
          current: Zipper.Location[Int],
          acc: List[Zipper.Location[Int] => Zipper.Location[Int]] = Nil
        ): Option[(Zipper.Location[Int], List[Zipper.Location[Int] => Zipper.Location[Int]])] = {
          @tailrec
          def descend(
            loc: Zipper.Location[Int],
            acc: List[Zipper.Location[Int] => Zipper.Location[Int]]
          ): (Zipper.Location[Int], List[Zipper.Location[Int] => Zipper.Location[Int]]) = loc match {
            case Zipper.Location(Zipper.Fork(_, _), _) => descend(Zipper.right(loc), Zipper.up[Int] _ :: acc)
            case Zipper.Location(Zipper.Leaf(_), _) => (loc, acc)
          }

          current match {
            case Zipper.Location(_, Zipper.Top()) => None
            case loc@Zipper.Location(_, Zipper.L(_, _)) => findLeft(Zipper.up(loc), Zipper.left[Int] _ :: acc)
            case loc@Zipper.Location(_, Zipper.R(_, _)) => Some(descend(Zipper.left(Zipper.up(loc)), Zipper.up[Int] _ :: Zipper.right[Int] _ :: acc))
          }
        }

        @tailrec
        def findRight(
          current: Zipper.Location[Int],
          acc: List[Zipper.Location[Int] => Zipper.Location[Int]] = Nil
        ): Option[(Zipper.Location[Int], List[Zipper.Location[Int] => Zipper.Location[Int]])] = {
          @tailrec
          def descend(
            loc: Zipper.Location[Int],
            acc: List[Zipper.Location[Int] => Zipper.Location[Int]]
          ): (Zipper.Location[Int], List[Zipper.Location[Int] => Zipper.Location[Int]]) = loc match {
            case Zipper.Location(Zipper.Fork(_, _), _) => descend(Zipper.left(loc), Zipper.up[Int] _ :: acc)
            case Zipper.Location(Zipper.Leaf(_), _) => (loc, acc)
          }

          current match {
            case Zipper.Location(_, Zipper.Top()) => None
            case loc@Zipper.Location(_, Zipper.R(_, _)) => findRight(Zipper.up(loc), Zipper.right[Int] _ :: acc)
            case loc@Zipper.Location(_, Zipper.L(_, _)) => Some(descend(Zipper.right(Zipper.up(loc)), Zipper.up[Int] _ :: Zipper.left[Int] _ :: acc))
          }
        }

        val rootLocation = Zipper.top(root)

        findDeepValue(rootLocation, 1) match {
          case None => root
          case Some(deepLocation@Zipper.Location(Zipper.Fork(Zipper.Leaf(l), Zipper.Leaf(r)), _)) =>
            val left = findLeft(deepLocation)

            val deepLocationAfterLeft = left match {
              case Some((leftLocation@Zipper.Location(Zipper.Leaf(n), _), pathBack)) =>
                val newLocation = Zipper.modify(leftLocation, (_: SnailfishNumber) => Zipper.Leaf(n + l))
                pathBack.foldLeft(newLocation)((loc, f) => f(loc))
              case None => deepLocation
            }

            val right = findRight(deepLocationAfterLeft)

            val deepLocationAfterRight = right match {
              case Some((rightLocation@Zipper.Location(Zipper.Leaf(n), _), pathBack)) =>
                val newLocation = Zipper.modify(rightLocation, (_: SnailfishNumber) => Zipper.Leaf(n + r))
                pathBack.foldLeft(newLocation)((loc, f) => f(loc))
              case None => deepLocationAfterLeft
            }

            val replacedWithZero = Zipper.modify(deepLocationAfterRight, (_: SnailfishNumber) => Zipper.Leaf(0))
            Zipper.upmost(replacedWithZero).tree
          case Some(p) => throw new IllegalStateException(s"Invalid deep pair: $p")
        }
      }

      def split(root: SnailfishNumber): SnailfishNumber = {
        def findBigValue(current: Zipper.Location[Int]): Option[Zipper.Location[Int]] = {
          current match {
            case loc@Zipper.Location(Zipper.Leaf(n), _) if n > 9 =>
              Some(loc)
            case Zipper.Location(Zipper.Leaf(_), _) => None
            case Zipper.Location(Zipper.Fork(_, _), _) =>
              findBigValue(Zipper.left(current)) match {
                case None => findBigValue(Zipper.right(current))
                case some => some
              }
          }
        }

        findBigValue(Zipper.top(root)) match {
          case None => root
          case Some(bigLocation@Zipper.Location(Zipper.Leaf(n), _)) => Zipper.upmost(Zipper.modify(
            bigLocation,
            (_: SnailfishNumber) => Zipper.Fork(Zipper.Leaf(n / 2), Zipper.Leaf(n / 2 + n % 2))
          )).tree
        }
      }

      val exploded = explode(input)

      if (exploded ne input) reduce(exploded)
      else {
        val splitted = split(exploded)

        if (splitted ne exploded) reduce(splitted)
        else splitted
      }
    }

    reduce(sum)
  }

  private case object Grammar extends RegexParsers {
    def snailfishNumber: Parser[SnailfishNumber] = pair | number

    def pair: Parser[SnailfishNumber] = "[" ~> snailfishNumber ~ ("," ~> snailfishNumber <~ "]") ^^ {
      case lhs ~ rhs => Zipper.Fork(lhs, rhs)
    }

    def number: Parser[SnailfishNumber] = "\\d+".r ^^ (s => Zipper.Leaf(s.toInt))
  }
}

case object Day18 extends App {
  val input = Files.lines("2021/day18.txt")
  val problem = Day18(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
