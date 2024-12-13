package space.scown.adventofcode
package advent2023.problems.day24

import lib.Timer.time
import lib.{Files, GaussianElimination, Problem, Rational}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.math.Fractional.Implicits.infixFractionalOps
import scala.math.Numeric.BigIntIsIntegral

case class Day24(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val hailstones = parse

    val testMin = 200000000000000L
    val testMax = 400000000000000L

    val intersections = for {
      i <- hailstones.indices
      j <- hailstones.indices if j > i
    } yield hailstones(i).intersects2(hailstones(j))

    val validIntersections = intersections
      .filter(i => i.isDefined).map(_.get)
      .filter({
        case (x, y, t1, t2) => t1 > 0 && t2 > 0 && x >= testMin && x <= testMax && y >= testMin && y <= testMax
      })

    val result = validIntersections.size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val hailstones = parse

    val solution = hailstones.combinations(3).map { hs =>
      println(s"Trying ${hs(0)}, ${hs(1)}, ${hs(2)}")
      val system = equations(hs(0), hs(1), hs(2))

      GaussianElimination.solve(system)
    }.find(_.isDefined).get.get

    println(s"$solution")

    val result = solution(0) + solution(1) + solution(2)

    println(s"Result 2: $result")
  }

  private def parse = {
    lines.map { line =>
      val parts = line.split("@")
      val ps = parts(0).trim().split(",\\s+").map(_.toLong)
      val vs = parts(1).trim().split(",\\s+").map(_.toLong)

      Hailstone(ps(0), ps(1), ps(2), vs(0), vs(1), vs(2))
    }
  }

  private def equations(h1: Hailstone, h2: Hailstone, h3: Hailstone): Vector[Vector[Rational[BigInt]]] = {
    Vector(
      Vector(h1.vy - h2.vy, h2.vx - h1.vx, 0, h2.py - h1.py, h1.px - h2.px, 0, (h2.py * h2.vx - h2.px * h2.vy) - (h1.py * h1.vx - h1.px * h1.vy)),
      Vector(h1.vz - h2.vz, 0, h2.vx - h1.vx, h2.pz - h1.pz, 0, h1.px - h2.px, (h2.pz * h2.vx - h2.px * h2.vz) - (h1.pz * h1.vx - h1.px * h1.vz)),
      Vector(0, h1.vz - h2.vz, h2.vy - h1.vy, 0, h2.pz - h1.pz, h1.py - h2.py, (h2.pz * h2.vy - h2.py * h2.vz) - (h1.pz * h1.vy - h1.py * h1.vz)),
      Vector(h1.vy - h3.vy, h3.vx - h1.vx, 0, h3.py - h1.py, h1.px - h3.px, 0, (h3.py * h3.vx - h3.px * h3.vy) - (h1.py * h1.vx - h1.px * h1.vy)),
      Vector(h1.vz - h3.vz, 0, h3.vx - h1.vx, h3.pz - h1.pz, 0, h1.px - h3.px, (h3.pz * h3.vx - h3.px * h3.vz) - (h1.pz * h1.vx - h1.px * h1.vz)),
      Vector(0, h1.vz - h3.vz, h3.vy - h1.vy, 0, h3.pz - h1.pz, h1.py - h3.py, (h3.pz * h3.vy - h3.py * h3.vz) - (h1.pz * h1.vy - h1.py * h1.vz)),
    )
  }
}

case class Hailstone(px: Long, py: Long, pz: Long, vx: Long, vy: Long, vz: Long) {
  def x(t: Long): Long = px + t * vx
  def y(t: Long): Long = py + t * vy
  def z(t: Long): Long = pz + t * vz

  def grad2: BigDecimal = BigDecimal(y(1) - y(0)) / BigDecimal(x(1) - x(0))
  def c2: BigDecimal = py - grad2 * px

  def tx(x: BigDecimal): BigDecimal = (x - px) / vx
  def ty(y: BigDecimal): BigDecimal = (y - py) / vy
  def tz(z: BigDecimal): BigDecimal = (z - pz) / vz

  def intersects2(that: Hailstone): Option[(BigDecimal, BigDecimal, BigDecimal, BigDecimal)] = {
    if (grad2 == that.grad2) None
    else {
      val x = (that.c2 - c2) / (grad2 - that.grad2)
      val y = grad2 * x + c2
      val t1 = tx(x)
      val t2 = that.tx(x)

      Some((x, y, t1, t2))
    }
  }
}

object Day24 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("2023/day24.txt")
    time(() => Day24(value).solve1())
    time(() => Day24(value).solve2())
  }

}
