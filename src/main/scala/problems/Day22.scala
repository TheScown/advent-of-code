package space.scown.advent2023
package problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day22(lines: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val bricks = parse

    val (settledBricks, _, supportedBy) = settle(bricks, Vector(), Map(), Map())

    val critical = settledBricks.filter(b => supportedBy(b).size == 1).map(b => supportedBy(b).head).toSet

    val canDisintegrate = settledBricks.filterNot(b => critical.contains(b))
    val result = canDisintegrate.size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val bricks = parse

    val (settledBricks, _, supportedBy) = settle(bricks, Vector(), Map(), Map())

    val supportsMap = settledBricks.foldLeft(Map[Brick, Set[Brick]]()) { (acc, brick) =>
      val supportsForBrick = supportedBy(brick).toVector

      val currentSets = supportsForBrick.map(b => acc.getOrElse(b, Set()) + brick)
      val pairs = supportsForBrick.zip(currentSets)

      acc ++ pairs
    }

    val critical = settledBricks.filter(b => supportedBy(b).size == 1).map(b => supportedBy(b).head).toSet

    @tailrec
    def supportHelper(testSet: Set[Brick], acc: Set[Brick]): Set[Brick] = {
      if (testSet.isEmpty) acc
      else {
        val supportedByTestSet = testSet.flatMap { b => supportsMap.getOrElse(b, Set()) }
        val willFall = supportedByTestSet.filter { b =>
          val supports = supportedBy.getOrElse(b, Set())
          supports.forall(b => testSet.contains(b) || acc.contains(b))
        }
        supportHelper(willFall, acc ++ willFall)
      }
    }

    val falls = critical.toVector.map { b => supportHelper(Set(b), Set()) }
    val result = falls.map(_.size).sum

    println(s"Result 2: $result")
  }

  private def parse: Vector[Brick] = {
    lines.zipWithIndex.map { p =>
      val (line, index) = p
      val brickStrings = line.split("~")
      val cubes = brickStrings.map { csv =>
        val coordinates = csv.split(",").map(_.toInt)
        (coordinates(0), coordinates(1), coordinates(2))
      }

      Brick(index, cubes(0), cubes(1))
    }.sorted
  }

  @tailrec
  private def settle(remainingBricks: Vector[Brick], settledBricks: Vector[Brick], index: Map[(Int, Int, Int), Brick], supportedBy: Map[Brick, Set[Brick]]): (Vector[Brick], Map[(Int, Int, Int), Brick], Map[Brick, Set[Brick]]) = {
    if (remainingBricks.isEmpty) (settledBricks, index, supportedBy)
    else {
      val next = remainingBricks.head
      val coordinates = next.coordinates

      if (next.grounded) {
        settle(
          remainingBricks.tail,
          settledBricks :+ next,
          index ++ coordinates.map(c => c -> next),
          supportedBy + (next -> Set())
        )
      }
      else {
        val newZ = (next.bottom  until 1 by -1).find { currentZ =>
          if (next.vertical) {
            val (x, y, _) = coordinates.head
            val testCoordinate = (x, y, currentZ - 1)
            index.contains(testCoordinate)
          }
          else {
            coordinates.exists { c =>
              val (x, y, _) = c
              val testCoordinate = (x, y, currentZ - 1)
              index.contains(testCoordinate)
            }
          }
        }

        newZ match {
          case None =>
            // Brick hits the floor
            val updatedBrick = next.withZ(1)

            settle(
              remainingBricks.tail,
              settledBricks :+ updatedBrick,
              index ++ updatedBrick.coordinates.map(c => c -> updatedBrick),
              supportedBy + (updatedBrick -> Set())
            )
          case Some(newZ) =>
            val updatedBrick = next.withZ(newZ)
            if (updatedBrick.vertical) {
              val (x, y, z) = updatedBrick.bottomCoordinate
              val support = index((x, y, z - 1))

              settle(
                remainingBricks.tail,
                settledBricks :+ updatedBrick,
                index ++ updatedBrick.coordinates.map(c => c -> updatedBrick),
                supportedBy + (updatedBrick -> Set(support))
              )
            }
            else {
              val supports = updatedBrick.coordinates.map(c => (c._1, c._2, c._3 - 1)).filter(c => index.contains(c)).map(index(_))
              settle(
                remainingBricks.tail,
                settledBricks :+ updatedBrick,
                index ++ updatedBrick.coordinates.map(c => c -> updatedBrick),
                supportedBy + (updatedBrick -> supports.toSet)
              )
            }
        }
      }
    }
  }
}

case class Brick(id: Int, c1: (Int, Int, Int), c2: (Int, Int, Int)) extends Ordered[Brick] {
  override def compare(that: Brick): Int = this.bottom - that.bottom

  def grounded: Boolean = bottom == 1

  def bottom: Int = Math.min(c1._3, c2._3)
  def bottomCoordinate: (Int, Int, Int) = if (c1._3 <= c2._3) c1 else c2

  def vertical: Boolean = c1._3 != c2._3

  def coordinates: IndexedSeq[(Int, Int, Int)] = {
    if (c1._1 != c2._1) (Math.min(c1._1, c2._1) to Math.max(c1._1, c2._1)).map(x => (x, c1._2, c1._3))
    else if (c1._2 != c2._2) (Math.min(c1._2, c2._2) to Math.max(c1._2, c2._2)).map(y => (c1._1, y, c1._3))
    else (Math.min(c1._3, c2._3) to Math.max(c1._3, c2._3)).map(z => (c1._1, c1._2, z))
  }

  def withZ(z: Int): Brick = {
    if (vertical) {
      if (c1._3 <= c2._3) copy(c1 = (c1._1, c1._2, z), c2 = (c2._1, c2._2, c2._3 - c1._3 + z))
      else copy(c2 = (c2._1, c2._2, z), c1 = (c1._1, c1._2, c1._3 - c2._3 + z))
    }
    else {
      val newCoordinates = coordinates.map { c =>
        val (x, y, _) = c
        (x, y, z)
      }
      copy(c1 = newCoordinates.head, c2 = newCoordinates.last)
    }
  }

}

object Day22 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day22.txt")
    time(() => Day22(value).solve1())
    time(() => Day22(value).solve2())
  }

}
