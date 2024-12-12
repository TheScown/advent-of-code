package space.scown.adventofcode
package advent2024.problems

import lib.{DFS, Files, Grid, Problem}

case class Day12(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val plantGrid = Grid(input.map(_.toVector))
    val initialRegionGrid = Grid.of[Option[Int]](plantGrid.columnLength, plantGrid.rowLength, None)

    val (finalRegionGrid, regionCount) = countRegions(plantGrid, initialRegionGrid)

    val result = (0 until regionCount).map { r =>
      val matchingAddresses = finalRegionGrid.map(_.get).zipWithIndex.filter { case (i, _) => i == r }.map(_._2)
      val area = matchingAddresses.size
      val perimeter = matchingAddresses.map { i =>
        4 - finalRegionGrid.neighbours(i).map(finalRegionGrid.apply).count(_.get == r)
      }.sum

      area * perimeter
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val plantGrid = Grid(input.map(_.toVector))
    val initialRegionGrid = Grid.of[Option[Int]](plantGrid.columnLength, plantGrid.rowLength, None)

    val (finalRegionGrid, regionCount) = countRegions(plantGrid, initialRegionGrid)

    val result = (0 until regionCount).map { r =>
      val addressesInRegion = finalRegionGrid.map(_.get).zipWithIndex.filter { case (i, _) => i == r }.map(_._2)
      val addressesInRegionSet = addressesInRegion.toSet
      val area = addressesInRegion.size

      val perimeterComponents = addressesInRegion.map { i =>
        val neighbours = finalRegionGrid.neighbours(i)
        val neighboursInRegion = neighbours.filter(finalRegionGrid(_).get == r)

        // Count corners
        neighboursInRegion.size match {
          // If all neighbours are outside the region, we have four corners
          case 0 => 4
          // If 3 neighbours are outside the region, we have two corners (the side within the region has the other two
          case 1 => 2
          case 2 =>
            // If the sides are adjacent, it's a corner. If they are opposite, it's not
            val delta = neighboursInRegion.head - neighboursInRegion(1)

            if (delta.isImaginary || delta.isReal) 0
            else {
              // There might be a concave corner
              val diffs = neighboursInRegion.map(i - _)
              val possibleCorner = i - diffs.sum

              if (addressesInRegionSet.contains(possibleCorner)) 1
              else 2
            }
          case 3 =>
            // If only one neighbour is outside, there might be up to two concave corners
            (neighboursInRegion :+ neighboursInRegion.head).sliding(2).toVector.count { p =>
              val diffs = p.map(i - _)
              val possibleCorner = i - diffs.sum
              !addressesInRegionSet.contains(possibleCorner)
            }
          case 4 =>
            // An interior point might be the inside of up to four concave corners – check it's diagonal neighbours
            val diagonals = finalRegionGrid.neighboursWithDiagonals(i).filterNot(neighbours.contains).toSet

            (diagonals diff addressesInRegionSet).size
        }
      }
      val perimeter = perimeterComponents.sum

      area * perimeter
    }.sum

    println(s"Result 2: $result")
  }

  private def countRegions(plantGrid: Grid[Char], initialRegionGrid: Grid[Option[Int]]): (Grid[Option[Int]], Int) = {
    plantGrid.zipWithIndex.foldLeft((initialRegionGrid, 0)) { case ((regionGrid, counter), (plantType, index)) =>
      if (regionGrid(index).isDefined) (regionGrid, counter)
      else {
        val reachable = DFS.reachable(index) { (i, _) =>
          plantGrid.neighbours(i).filter(plantGrid(_) == plantType)
        }

        (reachable.foldLeft(regionGrid) { (regionGrid, i) =>
          regionGrid.updated(i, Some(counter))
        }, counter + 1)
      }
    }
  }
}

case object Day12 extends App {
  val input = Files.lines("2024/day12.txt")
  val problem = Day12(input)
  problem.solve1()
  problem.solve2()
}
