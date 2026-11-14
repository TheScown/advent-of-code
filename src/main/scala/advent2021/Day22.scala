package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

import scala.collection.immutable.NumericRange

case class Day22(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val cuboids = parse()

    val zoneOfInterest = Cuboid(-50, 50, -50, 50, -50, 50, on = false)

    val validCuboids = cuboids.filter(c => intersects(c, zoneOfInterest))

    val clampedCuboids = validCuboids.map { cuboid =>
      intersection(cuboid, zoneOfInterest)
    }

    val map = clampedCuboids.foldLeft(Map[(Long, Long, Long), Boolean]()) { (acc, cuboid) =>
      val updated = for {
        x <- cuboid.xRange
        y <- cuboid.yRange
        z <- cuboid.zRange
      } yield (x, y, z) -> cuboid.on

      (acc ++ updated).filter(_._2)
    }

    val result = map.count(_._2)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val cuboids = parse()

    val onCuboids = cuboids.foldLeft(Vector[Cuboid]()) { (existingRegions, newRegion) =>
      val intersectingCuboids = existingRegions.filter(r => intersects(newRegion, r))

      println(s"New Region: $newRegion")
      intersectingCuboids.foreach { c =>
        val intersectionC = intersection(newRegion, c)
        println(s"int $c = $intersectionC")

        if (intersectionC == newRegion || intersectionC == c) {
          println("FULL")
        }
      }
      println()

      existingRegions :+ newRegion
    }.filter(_.on)

    val result = onCuboids.map(_.volume).sum

    println(s"Result 2: $result")
  }

  private def intersects(a: Cuboid, b: Cuboid): Boolean = {
    a.minX <= b.maxX && a.maxX >= b.minX &&
      a.minY <= b.maxY && a.maxY >= b.minY &&
      a.minZ <= b.maxZ && a.maxZ >= b.minZ
  }

  private def intersection(a: Cuboid, b: Cuboid): Cuboid = {
    a.copy(
      minX = math.max(b.minX, a.minX), maxX = math.min(b.maxX, a.maxX),
      minY = math.max(b.minY, a.minY), maxY = math.min(b.maxY, a.maxY),
      minZ = math.max(b.minZ, a.minZ), maxZ = math.min(b.maxZ, a.maxZ)
    )
  }

  private def parse(): Vector[Cuboid] = {
    val pattern = "(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)".r

    input.map {
      case pattern(on, minX, maxX, minY, maxY, minZ, maxZ) => Cuboid(minX.toLong, maxX.toLong, minY.toLong, maxY.toLong, minZ.toLong, maxZ.toLong, on == "on")
    }
  }

  private sealed trait Volume {
    def volume: Long
  }

  private case class Cuboid(minX: Long, maxX: Long, minY: Long, maxY: Long, minZ: Long, maxZ: Long, on: Boolean) extends Volume {
    lazy val volume: Long = (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)
    val xRange: NumericRange.Inclusive[Long] = minX to maxX
    val yRange: NumericRange.Inclusive[Long] = minY to maxY
    val zRange: NumericRange.Inclusive[Long] = minZ to maxZ
  }

  private case class CuboidWithMissing(main: Cuboid, withMissing: Vector[Cuboid])
}

case object Day22 extends App {
  val input = Files.lines("2021/day22.txt")
  val problem = Day22(input)
  problem.solve1()
  problem.solve2()
}
