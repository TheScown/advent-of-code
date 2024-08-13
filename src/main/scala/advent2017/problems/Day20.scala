package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem, Timer, Vec3}

import scala.Double.NaN

case class Day20(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val particles = parse()
    val t = 1000

    val result = particles.map(p => (p.s(t), p.id)).minBy(_._1.norm1)._2

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val particles = parse()
    val dParticles = particles.map(_.toDouble)
    val pairs = dParticles.combinations(2)

    def collisionTime(q1: DParticle, q2: DParticle): Option[(Long, DParticle, DParticle)] = (q1, q2) match {
      case (DParticle(_, p1, u1, a1), DParticle(_, p2, u2, a2)) =>
        val a = (a2 - a1).map(_ / 2)
        val b = (u2 + a2.map(_ / 2)) - (u1 + a1.map(_ / 2))
        val c = p2 - p1

        def solve(multiplier: Double)(a: Double, b: Double, c: Double): Double = {
          if (a == 0) {
            // Linear system
            if (b == 0) NaN
            else -c/b
          }
          else {
            val discriminant = b * b - 4 * a * c

            if (discriminant < 0) NaN
            else {
              ((-b) + (multiplier * Math.sqrt(discriminant))) / (2 * a)
            }
          }
        }

        val t1 = a.map(b, c)(solve(1))
        val t2 = a.map(b, c)(solve(-1))

        val tx1s = Set(t1.x1, t2.x1)
        val tx2s = Set(t1.x2, t2.x2)
        val tx3s = Set(t1.x3, t2.x3)

        val validTs = tx1s.intersect(tx2s).intersect(tx3s).filter(_.isWhole).filter(_ > 0)

        if (validTs.isEmpty) None
        else Some((validTs.min.toLong, q1, q2))
    }

    val collisionEvents = pairs.map(v => collisionTime(v(0), v(1))).filter(_.isDefined).map(_.get).toVector.sortBy(_._1)
    // This is over-simplified, but it turns out no particles collide at multiple points in time
    val result = particles.size - collisionEvents.flatMap(t => Seq(t._2, t._3)).toSet.size

    println(s"Result 2: $result")

  }

  def parse(): Vector[Particle] = {
    val number = "(0|-?[1-9][0-9]*)"
    val pattern = s"p=<$number,$number,$number>, v=<$number,$number,$number>, a=<$number,$number,$number>".r

    input.zipWithIndex.map {
      case (pattern(p1, p2, p3, v1, v2, v3, a1, a2, a3), id) => Particle(
        id,
        Vec3(p1.toLong, p2.toLong, p3.toLong),
        Vec3(v1.toLong, v2.toLong, v3.toLong),
        Vec3(a1.toLong, a2.toLong, a3.toLong)
      )
    }
  }

  case class Particle(id: Int, p: Vec3[Long], u: Vec3[Long], a: Vec3[Long]) {
    def v(t: Long): Vec3[Long] = {
      val result = u + (a * t)
      result
    }
    def s(t: Long): Vec3[Long] = p + (u * t) + a * ((t * (t + 1)) / 2)

    def toDouble: DParticle = DParticle(id, p.map(_.toDouble), u.map(_.toDouble), a.map(_.toDouble))
  }

  case class DParticle(id: Int, p: Vec3[Double], u: Vec3[Double], a: Vec3[Double])
}

case object Day20 extends App {
  val input = Files.lines("2017/day20.txt")
  val problem = Day20(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}