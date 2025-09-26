package space.scown.adventofcode
package advent2020

import lib.{Files, Grid, Problem}

case class Day17(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))

    val initialMap = grid.zipWithIndex.foldLeft(Map[(Int, Int, Int), Boolean]()) { case (map, (char, index)) =>
      map + ((index.re, index.im, 0) -> isActive(char))
    }

    def neighbourCoordinates(x: Int, y: Int, z: Int): Seq[(Int, Int, Int)] = {
      for {
        k <- -1 to 1
        i <- -1 to 1
        j <- -1 to 1
        if !(i == 0 && j == 0 && k == 0)
      } yield (x + i, y + j, z + k)
    }

    val finalMap = (0 until 6).foldLeft(initialMap) { case (map, _) =>
      val activeEntries = map.filter(_._2).keys
      val minX = activeEntries.minBy(_._1)._1
      val maxX = activeEntries.maxBy(_._1)._1
      val minY = activeEntries.minBy(_._2)._2
      val maxY = activeEntries.maxBy(_._2)._2
      val minZ = activeEntries.minBy(_._3)._3
      val maxZ = activeEntries.maxBy(_._3)._3

      val updatedMap = (for {
        i <- minX - 1 to maxX + 1
        j <- minY - 1 to maxY + 1
        k <- minZ - 1 to maxZ + 1
      } yield {
        val currentValue = map.getOrElse((i, j, k), false)
        val neighbours = neighbourCoordinates(i, j, k)
        val neighbourValues = neighbours.map(map.getOrElse(_, false))
        val activeNeighbours = neighbourValues.count(b => b)

        if (currentValue) {
          ((i, j, k), activeNeighbours == 2 || activeNeighbours == 3)
        }
        else {
          ((i, j, k), activeNeighbours == 3)
        }
      }).toMap

      updatedMap
    }

    val result = finalMap.count { _._2 }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))

    val initialMap = grid.zipWithIndex.foldLeft(Map[(Int, Int, Int, Int), Boolean]()) { case (map, (char, index)) =>
      map + ((index.re, index.im, 0, 0) -> isActive(char))
    }

    def neighbourCoordinates(x: Int, y: Int, z: Int, w: Int): Seq[(Int, Int, Int, Int)] = {
      for {
        k <- -1 to 1
        l <- -1 to 1
        i <- -1 to 1
        j <- -1 to 1
        if !(i == 0 && j == 0 && k == 0 && l == 0)
      } yield (x + i, y + j, z + k, w + l)
    }

    val finalMap = (0 until 6).foldLeft(initialMap) { case (map, _) =>
      val activeEntries = map.filter(_._2).keys
      val minX = activeEntries.minBy(_._1)._1
      val maxX = activeEntries.maxBy(_._1)._1
      val minY = activeEntries.minBy(_._2)._2
      val maxY = activeEntries.maxBy(_._2)._2
      val minZ = activeEntries.minBy(_._3)._3
      val maxZ = activeEntries.maxBy(_._3)._3
      val minW = activeEntries.minBy(_._4)._4
      val maxW = activeEntries.maxBy(_._4)._4

      val updatedMap = (for {
        i <- minX - 1 to maxX + 1
        j <- minY - 1 to maxY + 1
        k <- minZ - 1 to maxZ + 1
        l <- minW - 1 to maxW + 1
      } yield {
        val currentValue = map.getOrElse((i, j, k, l), false)
        val neighbours = neighbourCoordinates(i, j, k, l)
        val neighbourValues = neighbours.map(map.getOrElse(_, false))
        val activeNeighbours = neighbourValues.count(b => b)

        if (currentValue) {
          ((i, j, k, l), activeNeighbours == 2 || activeNeighbours == 3)
        }
        else {
          ((i, j, k, l), activeNeighbours == 3)
        }
      }).toMap

      updatedMap
    }

    val result = finalMap.count { _._2 }

    println(s"Result 2: $result")
  }

  private def isActive(c: Char): Boolean = c == '#'
}

case object Day17 extends App {
  val input = Files.lines("2020/day17.txt")
  val problem = Day17(input)
  problem.solve1()
  problem.solve2()
}
