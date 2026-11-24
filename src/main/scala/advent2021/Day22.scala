package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

import scala.annotation.tailrec
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

    @tailrec
    def helper(onCuboids: Set[Cuboid], toTest: Vector[Cuboid]): Set[Cuboid] = {
      if (toTest.isEmpty) onCuboids
      else {
        val testCuboid = toTest.head

        onCuboids.find(possibleTarget => intersects(possibleTarget, testCuboid)) match {
          case Some(target) =>
            val intersectionCuboid = intersection(target, testCuboid)
            val restOfTarget = target - intersectionCuboid
            val cuboidsWithDecomposedTarget = (onCuboids - target) ++ restOfTarget
            val restOfTest = testCuboid - intersectionCuboid

            if (testCuboid.on) helper(cuboidsWithDecomposedTarget + intersectionCuboid, restOfTest ++ toTest.tail)
            else helper(cuboidsWithDecomposedTarget, restOfTest ++ toTest.tail)
          case None =>
            if (testCuboid.on) helper(onCuboids + testCuboid, toTest.tail)
            else helper(onCuboids, toTest.tail)
        }
      }
    }

    val onCuboids = helper(Set(), cuboids).toVector

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

  private case class Cuboid(minX: Long, maxX: Long, minY: Long, maxY: Long, minZ: Long, maxZ: Long, on: Boolean) {
    lazy val volume: Long = (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)
    val xRange: NumericRange.Inclusive[Long] = minX to maxX
    val yRange: NumericRange.Inclusive[Long] = minY to maxY
    val zRange: NumericRange.Inclusive[Long] = minZ to maxZ

    def -(c: Cuboid): Vector[Cuboid] = {
      if (!intersects(this, c)) {
        throw new IllegalStateException(s"Non-intersecting subtraction: $this - $c")
      }

      val (cc1, minXCut) = if (c.minX > minX) {
        val cutOff = this.copy(maxX = c.minX - 1)
        val restToTest = this.copy(minX = c.minX)
        (restToTest, Vector(cutOff))
      } else (this, Vector())

      val (cc2, maxXCut) = if (c.maxX < cc1.maxX) {
        val cutOff = cc1.copy(minX = c.maxX + 1)
        val restToTest = cc1.copy(maxX = c.maxX)
        (restToTest, Vector(cutOff))
      } else (cc1, Vector())

      val (cc3, minYCut) = if (c.minY > cc2.minY) {
        val cutOff = cc2.copy(maxY = c.minY - 1)
        val restToTest = cc2.copy(minY = c.minY)
        (restToTest, Vector(cutOff))
      } else (cc2, Vector())

      val (cc4, maxYCut) = if (c.maxY < cc3.maxY) {
        val cutOff = cc3.copy(minY = c.maxY + 1)
        val restToTest = cc3.copy(maxY = c.maxY)
        (restToTest, Vector(cutOff))
      } else (cc3, Vector())

      val (cc5, minZCut) = if (c.minZ > cc4.minZ) {
        val cutOff = cc4.copy(maxZ = c.minZ - 1)
        val restToTest = cc4.copy(minZ = c.minZ)
        (restToTest, Vector(cutOff))
      } else (cc4, Vector())

      val (_, maxZCut) = if (c.maxZ < cc5.maxZ) {
        val cutOff = cc5.copy(minZ = c.maxZ + 1)
        val restToTest = cc5.copy(maxZ = c.maxZ)
        (restToTest, Vector(cutOff))
      } else (cc5, Vector())

      minXCut ++ maxXCut ++ minYCut ++ maxYCut ++ minZCut ++ maxZCut
    }
  }

}

case object Day22 extends App {
  val input = Files.lines("2021/day22.txt")
  val problem = Day22(input)
  problem.solve1()
  problem.solve2()
}
