package space.scown.adventofcode
package advent2025.problems

import lib.{Complex, Files, Problem}

case class Day9(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val tiles = parse()
    val combinations = tiles.combinations(2)

    val areas = combinations.map {
      case Vector(a, b) =>
        val width = (a.re - b.re).abs + 1
        val height = (a.im - b.im).abs + 1
        width * height
    }

    val result = areas.max

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val redTiles = parse()
    val combinations = redTiles.combinations(2)

    val areas = combinations.map {
      case Vector(a, b) =>
        val width = (a.re - b.re).abs + 1
        val height = (a.im - b.im).abs + 1
        (a, b, width * height)
    }.toVector.sortBy(-_._3)

    val polygonEdges = redTiles.sliding(2).map {
      case Vector(a, b) => (a, b, a.re == b.re)
    }.toVector

    val (verticalEdges, horizontalEdges) = polygonEdges.partition(_._3)

    val tileSet = redTiles.toSet

    val result = areas.find { case (a, b, area) =>
      val x = area
      val nonRedCorners = Seq(
        Complex(a.re, b.im),
        Complex(b.re, a.im),
      )

      // Both the non-red corners must be inside the polygon
      nonRedCorners.forall {
        tileToTest =>
          val aa = area
          val (a1, b1) = (a, b)
          // Red tiles are trivially in the shape
          if (tileSet.contains(tileToTest)) {
            true
          }
          // Tiles on the edge of the polygon are considered part of the shape
          else if (isOnEdge(tileToTest, polygonEdges)) {
            true
          }
          else {
            // Head north from the tile, counting the edges crossed
            // Odd means in the shape, even means outside
            val im = tileToTest.im

            val horizontalEdgesToTest = horizontalEdges.filter(_._1.im > im)

            // Detecting a crossed horizontal edge is easy
            val crossedEdges = horizontalEdgesToTest.count {
              case (a, b, _) =>
                tileToTest.re > a.re && tileToTest.re < b.re || tileToTest.re < a.re && tileToTest.re > b.re
            }

            // We only count vertical edges if one of the ends is concave (S or Z shape locally)
            val crossedVerticalEdges = verticalEdges.count {
              case (a, b, _) =>
                if (tileToTest.re != a.re) false
                else if (a.im < tileToTest.im || b.im < tileToTest.im) false
                else {
                  val aNeighbours = (
                    Complex(a.re - 1, a.im),
                    Complex(a.re + 1, a.im),
                  )

                  val bNeighbours = (
                    Complex(b.re - 1, b.im),
                    Complex(b.re + 1, b.im),
                  )

                  isOnEdge(aNeighbours._1, polygonEdges) && isOnEdge(bNeighbours._2, polygonEdges) ||
                    isOnEdge(aNeighbours._2, polygonEdges) && isOnEdge(bNeighbours._1, polygonEdges)
                }
            }

            (crossedEdges + crossedVerticalEdges) % 2 != 0
          }
      }

      // TODO check that the entirety of the rectangle edges are inside the polygon
      // If a rectangle edge is crossed by a polygon edge, it isn't valid
      // If a rectangle edge contains a red tile, check the tiles before and after it – both must be in the shape

//      val minRe = math.min(a.re, b.re)
//      val maxRe = math.max(a.re, b.re)
//      val minIm = math.min(a.im, b.im)
//      val maxIm = math.max(a.im, b.im)
//
//      val redTilesInRectangle = redTiles.filter { tile =>
//        (tile.re > minRe && tile.re < maxRe) && (tile.im > minIm && tile.im < maxIm)
//      }
//
//      redTilesInRectangle.size == 0
    }.get._3

    // < 4647960552
    println(s"Result 2: $result")
  }

  private def isOnEdge(tile: Complex[Long], edges: Vector[(Complex[Long], Complex[Long], Boolean)]): Boolean = {
    edges.exists {
      case (a, b, isVertical) =>
        if (isVertical) {
          val re = a.re

          tile.re == re && (tile.im >= a.im && tile.im <= b.im || tile.im <= a.im && tile.im >= b.im)
        }
        else {
          val im = a.im

          tile.im == im && (tile.re >= a.re && tile.re <= b.re || tile.re <= a.re && tile.re >= b.re)
        }
    }
  }

  private def parse(): Vector[Complex[Long]] = {
    input.map { line =>
      val parts = line.split(",")
      Complex(parts(0).toLong, -parts(1).toLong)
    }
  }
}

case object Day9 extends App {
  val input = Files.lines("2025/day9.txt")
  val problem = Day9(input)
  problem.solve1()
  problem.solve2()
}
