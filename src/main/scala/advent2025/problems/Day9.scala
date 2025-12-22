package space.scown.adventofcode
package advent2025.problems

import lib.{Complex, Files, Grid, Problem, Timer}

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

    val polygonEdges = (redTiles :+ redTiles.head).sliding(2).map {
      case Vector(a, b) => (a, b, a.re == b.re)
    }.toVector

    val (verticalEdges, horizontalEdges) = polygonEdges.partition(_._3)

    val tileSet = redTiles.toSet

    val bestRectangle = areas.find { case (a, b, _) =>
      val (newCorner1, newCorner2) = (Complex(a.re, b.im), Complex(b.re, a.im))

      if (!isTileInPolygon(newCorner1, polygonEdges, tileSet) || !isTileInPolygon(newCorner2, polygonEdges, tileSet)) false
      else {
        val edges = Seq(
          a,
          Complex(a.re, b.im),
          b,
          Complex(b.re, a.im),
          a
        ).sliding(2).toVector

        edges.forall {
          case Seq(a, b) =>
            val edgeIsVertical = a.re == b.re
            val perpendicularEdgesToTest = if (edgeIsVertical) horizontalEdges else verticalEdges

            // If a rectangle edge is crossed by a polygon edge, it isn't valid
            val crossesAnEdge = perpendicularEdgesToTest.exists { case (c, d, _) =>
              if (edgeIsVertical) {
                val minRe = math.min(c.re, d.re)
                val maxRe = math.max(c.re, d.re)
                val im = c.im

                ((im < a.im && im > b.im) || (im > a.im && im < b.im)) && (a.re > minRe && a.re < maxRe)
              }
              else {
                val minIm = math.min(c.im, d.im)
                val maxIm = math.max(c.im, d.im)
                val re = c.re

                ((re < a.re && re > b.re) || (re > a.re && re < b.re)) && (a.im > minIm && a.im < maxIm)
              }
            }

            !crossesAnEdge
        }
      }
    }.get

    val result = bestRectangle._3

    println(s"Result 2: $result")
  }

  private def isTileInPolygon(tileToTest: Complex[Long], polygonEdges: Vector[(Complex[Long], Complex[Long], Boolean)], tileSet: Set[Complex[Long]]): Boolean = {
    val (verticalEdges, horizontalEdges) = polygonEdges.partition(_._3)

    // Red tiles are trivially in the shape
    if (tileSet.contains(tileToTest)) true
    // Tiles on the edge of the polygon are considered part of the shape
    else if (isOnEdge(tileToTest, polygonEdges)) true
    else {
      // Head north from the tile, counting the edges crossed
      // Odd means in the shape, even means outside
      val im = tileToTest.im

      val horizontalEdgesToTest = horizontalEdges.filter(_._1.im > im)

      // Detecting a crossed horizontal edge is easy
      val crossedEdges = horizontalEdgesToTest.count {
        case (a, b, _) =>
          (tileToTest.re > a.re && tileToTest.re < b.re) || (tileToTest.re < a.re && tileToTest.re > b.re)
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

            (isOnEdge(aNeighbours._1, polygonEdges) && isOnEdge(bNeighbours._2, polygonEdges)) ||
              (isOnEdge(aNeighbours._2, polygonEdges) && isOnEdge(bNeighbours._1, polygonEdges))
          }
      }

      (crossedEdges + crossedVerticalEdges) % 2 != 0
    }
  }

  private def isOnEdge(tile: Complex[Long], edges: Vector[(Complex[Long], Complex[Long], Boolean)]): Boolean = {
    edges.exists {
      case (a, b, isVertical) =>
        if (isVertical) {
          val re = a.re

          tile.re == re && ((tile.im >= a.im && tile.im <= b.im) || (tile.im <= a.im && tile.im >= b.im))
        }
        else {
          val im = a.im

          tile.im == im && ((tile.re >= a.re && tile.re <= b.re) || (tile.re <= a.re && tile.re >= b.re))
        }
    }
  }

  private def parse(): Vector[Complex[Long]] = {
    input.map { line =>
      val parts = line.split(",")
      Complex(parts(0).toLong, -parts(1).toLong)
    }
  }

  private def visualize(point: Complex[Long], tileSet: Set[Complex[Long]], polygonEdges: Vector[(Complex[Long], Complex[Long], Boolean)]): Unit = {
    val visibility = 10
    val gridSize = 2 * visibility + 1

    val locality = Grid.of(gridSize, gridSize, '.').zipWithIndex.map { case (_, index) =>
      val offset = Complex(index.re.toLong - visibility.toLong, index.im.toLong + visibility.toLong)
      val tile = offset + point

      val result = if (tile == point && tileSet.contains(tile)) 'P'
      else if (tile == point) 'X'
      else if (tileSet.contains(tile)) 'R'
      else if (isTileInPolygon(tile, polygonEdges, tileSet)) '#'
      else '.'

      result
    }

    println(point)
    println(locality)
  }
}

case object Day9 extends App {
  val input = Files.lines("2025/day9.txt")
  val problem = Day9(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
