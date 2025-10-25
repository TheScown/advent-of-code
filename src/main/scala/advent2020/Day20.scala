package space.scown.adventofcode
package advent2020

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec

case class Day20(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val tiles = parse()
    val tileGrid = assembleTileGrid(tiles)

    val topLeft = tileGrid(Complex.ZERO).id
    val topRight = tileGrid(Complex(tileGrid.rowLength - 1, 0)).id
    val bottomLeft = tileGrid(Complex(0, -(tileGrid.columnLength - 1))).id
    val bottomRight = tileGrid(Complex(tileGrid.rowLength - 1, -(tileGrid.columnLength - 1))).id

    val result = topLeft * topRight * bottomLeft * bottomRight

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val tiles = parse()
    val tileGrid = assembleTileGrid(tiles).map(_.tile)
    val tilesWithoutBorders = tileGrid.map(tile => tile.slice(Complex(1, -1), tile.rowLength - 2, tile.columnLength - 2))

    val finalImage = Grid.flatten(tilesWithoutBorders)
    val finalImagePermutations = getTilePermutations(0, finalImage).map(_.tile)

    val seaMonster = Grid(Vector(
      "                  # ".toVector,
      "#    ##    ##    ###".toVector,
      " #  #  #  #  #  #   ".toVector,
    ))

    val seaMonsterCost = seaMonster.count(_ == '#')

    val roughness = finalImagePermutations.map { image =>
      val seaMonsterCount = image.indices.count { index =>
        if (
          index.re + seaMonster.rowLength > image.rowLength ||
            -index.im + seaMonster.columnLength > image.columnLength
        ) false
        else {
          val possibleMonster = image.slice(index, seaMonster.rowLength, seaMonster.columnLength)

          possibleMonster.zipWithIndex.forall { case (c, index) =>
            seaMonster(index) match {
              case ' ' => true
              case '#' => c == '#'
            }
          }
        }
      }

      if (seaMonsterCount == 0) -1
      else image.count(_ == '#') - seaMonsterCount * seaMonsterCost
    }

    val result = roughness.find(_ > 0).get

    println(s"Result 2: $result")
  }

  private def assembleTileGrid(tiles: Map[Long, Vector[Tile]]): Grid[Tile] = {
    val gridSize = Math.sqrt(tiles.size).toInt
    val emptyGrid = Grid.of[Option[Tile]](gridSize, gridSize, None)

    val (finalGrid, _) = emptyGrid.indices.foldLeft((emptyGrid, tiles)) { case ((acc, tiles), index) => {
      if (index == Complex.ZERO[Int]) {
        val cornerTiles = identifyCornerTiles(tiles)
        val initialCornerTilePermutations = cornerTiles.head._2
        val topLeftCornerTile = initialCornerTilePermutations.map { tile =>
          val otherTiles = (tiles - tile.id).values

          val aboveOptions = findTilesAbove(otherTiles, tile)
          val belowOptions = findTilesBelow(otherTiles, tile)
          val leftOptions = findTilesLeft(otherTiles, tile)
          val rightOptions = findTilesRight(otherTiles, tile)

          (tile, aboveOptions, belowOptions, leftOptions, rightOptions)
        }.find { case (_, aboveOptions, belowOptions, leftOptions, rightOptions) =>
          aboveOptions.isEmpty && leftOptions.isEmpty && belowOptions.nonEmpty && rightOptions.nonEmpty
        }.get

        (acc.updated(index, Some(topLeftCornerTile._1)), tiles - topLeftCornerTile._1.id)
      }
      else {
        val needsAbove = index.im < 0
        val needsLeft = index.re > 0

        val nextTile = tiles.map { case (_, tilePermutations) =>
          tilePermutations.find { tile =>
            val matchesLeft = if (needsLeft) {
              val left = acc(index - Complex.ONE).get
              tile.tile.firstColumn == left.tile.lastColumn
            } else true

            val matchesAbove = if (needsAbove) {
              val above = acc(index + Complex.I[Int]).get
              tile.tile.firstRow == above.tile.lastRow
            } else true

            matchesAbove && matchesLeft
          }
        }.find(_.isDefined).get

        (acc.updated(index, nextTile), tiles - nextTile.get.id)
      }
    }}

    finalGrid.map(_.get)
  }

  private def identifyCornerTiles(tiles: Map[Long, Vector[Tile]]): Map[Long, Vector[Tile]] = {
    tiles.filter { case (id, tilePermutations) =>
      val otherTiles = (tiles - id).values
      val testTiles = tilePermutations.filter(_.rotations == 0)

      val aboveOptions = testTiles.map { testTile =>
        findTilesAbove(otherTiles, testTile)
      }

      val belowOptions = testTiles.map { testTile =>
        findTilesBelow(otherTiles, testTile)
      }

      val leftOptions = testTiles.map { testTile =>
        findTilesLeft(otherTiles, testTile)
      }

      val rightOptions = testTiles.map { testTile =>
        findTilesRight(otherTiles, testTile)
      }

      testTiles.indices.exists { i =>
        val aboves = aboveOptions(i)
        val belows = belowOptions(i)
        val lefts = leftOptions(i)
        val rights = rightOptions(i)

        (aboves.nonEmpty && belows.isEmpty && lefts.nonEmpty && rights.isEmpty) ||
          (aboves.nonEmpty && belows.isEmpty && lefts.isEmpty && rights.nonEmpty) ||
          (aboves.isEmpty && belows.nonEmpty && lefts.nonEmpty && rights.isEmpty) ||
          (aboves.isEmpty && belows.nonEmpty && lefts.isEmpty && rights.nonEmpty)
      }
    }
  }

  private def findTilesAbove(otherTiles: Iterable[Vector[Tile]], testTile: Tile): Vector[Tile] = {
    val grid = testTile.tile

    otherTiles.flatMap { otherTilePermutations =>
      otherTilePermutations.filter(otp => otp.tile.lastRow == grid.firstRow)
    }.toVector
  }

  private def findTilesBelow(otherTiles: Iterable[Vector[Tile]], testTile: Tile): Vector[Tile] = {
    val grid = testTile.tile

    otherTiles.flatMap { otherTilePermutations =>
      otherTilePermutations.filter(otp => otp.tile.firstRow == grid.lastRow)
    }.toVector
  }

  private def findTilesLeft(otherTiles: Iterable[Vector[Tile]], testTile: Tile): Vector[Tile] = {
    val grid = testTile.tile

    otherTiles.flatMap { otherTilePermutations =>
      otherTilePermutations.filter(otp => otp.tile.lastColumn == grid.firstColumn)
    }.toVector
  }

  private def findTilesRight(otherTiles: Iterable[Vector[Tile]], testTile: Tile) = {
    val grid = testTile.tile

    otherTiles.flatMap { otherTilePermutations =>
      otherTilePermutations.filter(otp => otp.tile.firstColumn == grid.lastColumn)
    }.toVector
  }

  private def parse(): Map[Long, Vector[Tile]] = {
    val idPattern = "Tile (\\d+):".r

    @tailrec
    def helper(acc: Map[Long, Vector[Tile]], remainingLines: Vector[String]): Map[Long, Vector[Tile]] = {
      val (tileLines, rest) = remainingLines.span(_.nonEmpty)

      val id = tileLines.head match {
        case idPattern(id) => id.toLong
      }

      val baseTile = Grid(tileLines.tail.map(_.toVector))

      val tilePermutations = getTilePermutations(id, baseTile)

      val updatedAcc = acc + (id -> tilePermutations.toVector)

      if (rest.nonEmpty) helper(updatedAcc, rest.tail)
      else updatedAcc
    }

    helper(Map(), input)
  }

  private def getTilePermutations(id: Long, baseTile: Grid[Char]): IndexedSeq[Tile] = {
    for {
      rotations <- 0 to 3
      flipped <- Seq(false, true)
    } yield {
      val flippedTile = if (flipped) baseTile.flipVertically() else baseTile
      val rotatedTile = (0 until rotations).foldLeft(flippedTile)((tile, _) => tile.rotateRight())
      Tile(id, rotatedTile, rotations, flipped)
    }
  }

  private case class Tile(id: Long, tile: Grid[Char], rotations: Int, flipped: Boolean)
}

case object Day20 extends App {
  val input = Files.lines("2020/day20.txt")
  val problem = Day20(input)
  problem.solve1()
  problem.solve2()
}
