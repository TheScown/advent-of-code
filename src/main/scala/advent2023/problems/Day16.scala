package space.scown.adventofcode
package advent2023.problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

case class Day16(grid: Vector[Vector[Char]]) extends Problem {

  override def solve1(): Unit = {
    val resultSet = search(Vector(((0, 0), Right)), Set())
    val positionSet = resultSet.map(p => p._1)
    val result = positionSet.size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val startingPositions = Vector(Up, Right, Down, Left).flatMap(_.startingPositions(grid.size, grid(0).size))

    val result = startingPositions.par.map { input =>
      val resultSet = search(Vector(input), Set())
      val positionSet = resultSet.map(p => p._1)
      positionSet.size
    }.max

    println(s"Result 2: $result")
  }

  @tailrec
  private def search(positionsAndDirections: Vector[((Int, Int), Direction)], seen: Set[((Int, Int), Direction)]): Set[((Int, Int), Direction)] = {
    if (positionsAndDirections.isEmpty) seen
    else {
      val nextMoves = positionsAndDirections.flatMap { positionAndDirection =>
        val (position, direction) = positionAndDirection
        val (row, column) = position
        val c = grid(row)(column)
        val nextDirections = direction(c)
        val nextPositions = nextDirections.map(_ + position)
        val nextMoves = nextPositions.zip(nextDirections)

        nextMoves.filterNot { p =>
          val ((row, column), _) = p
          if (row < 0 || row >= grid.size || column < 0 || column >= grid(0).size) true
          else seen.contains(p)
        }
      }

      search(nextMoves, seen ++ positionsAndDirections)
    }
  }
}

sealed trait Direction {
  def apply(c: Char): Seq[Direction]

  def +(p: (Int, Int)): (Int, Int)

  def startingPositions(rows: Int, columns: Int): IndexedSeq[((Int, Int), Direction)]

}

case object Right extends Direction {
  override def apply(c: Char): Seq[Direction] = c match {
    case '/' => Vector(Up)
    case '\\' => Vector(Down)
    case '|' => Vector(Up, Down)
    case _ => Vector(Right)
  }

  override def +(p: (Int, Int)): (Int, Int) = (p._1, p._2 + 1)

  override def startingPositions(rows: Int, columns: Int): IndexedSeq[((Int, Int), Direction)] = {
    0 until rows map { i => ((i, 0), Right) }
  }
}
case object Left extends Direction {
  override def apply(c: Char): Seq[Direction] = c match {
    case '/' => Vector(Down)
    case '\\' => Vector(Up)
    case '|' => Vector(Up, Down)
    case _ => Vector(Left)
  }

  override def +(p: (Int, Int)): (Int, Int) = (p._1, p._2 - 1)

  override def startingPositions(rows: Int, columns: Int): IndexedSeq[((Int, Int), Direction)] = {
    0 until rows map { i => ((i, columns - 1), Left) }
  }
}
case object Down extends Direction {
  override def apply(c: Char): Seq[Direction] = c match {
    case '/' => Vector(Left)
    case '\\' => Vector(Right)
    case '-' => Vector(Left, Right)
    case _ => Vector(Down)
  }

  override def +(p: (Int, Int)): (Int, Int) = (p._1 + 1, p._2)

  override def startingPositions(rows: Int, columns: Int): IndexedSeq[((Int, Int), Direction)] = {
    0 until columns map { i => ((0, i), Down) }
  }
}
case object Up extends Direction {
  override def apply(c: Char): Seq[Direction] = c match {
    case '/' => Vector(Right)
    case '\\' => Vector(Left)
    case '-' => Vector(Left, Right)
    case _ => Vector(Up)
  }

  override def +(p: (Int, Int)): (Int, Int) = (p._1 - 1, p._2)

  override def startingPositions(rows: Int, columns: Int): IndexedSeq[((Int, Int), Direction)] = {
    0 until columns map { i => ((rows - 1, i), Up) }
  }
}

object Day16 {
  def main(args: Array[String]): Unit = {
    val value = Files.grid("2023/day16.txt")
    time(() => Day16(value).solve1())
    time(() => Day16(value).solve2())
  }

}
