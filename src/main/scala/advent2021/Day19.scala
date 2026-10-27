package space.scown.adventofcode
package advent2021

import lib.{Files, Matrix3, Problem, Timer, Vec3}

import scala.annotation.tailrec

case class Day19(input: Vector[String]) extends Problem {
  private val STANDARD_BASIS = Matrix3.IDENTITY[Int]

  override def solve1(): Unit = {
    val scanners = parse()

    val (knownScanners, unknownScanners) = scanners.partition(_.position.isDefined)

    val mappedScanners = mapScanners(knownScanners, unknownScanners, knownScanners)

    val beacons = mappedScanners.flatMap(_.absoluteReadings).toSet

    val result = beacons.size
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val scanners = parse()

    val (knownScanners, unknownScanners) = scanners.partition(_.position.isDefined)

    val mappedScanners = mapScanners(knownScanners, unknownScanners, knownScanners)

    val scannerLocations = mappedScanners.map(_.position.get)

    val result = scannerLocations.combinations(2).map {
      case Vector(a, b) => (a - b).norm1
    }.max
    println(s"Result 2: $result")
  }

  @tailrec
  private def mapScanners(
    knownScanners: Vector[Scanner],
    unknownScanners: Vector[Scanner],
    newlyKnownScanners: Vector[Scanner]
  ): Vector[Scanner] = {
    if (unknownScanners.isEmpty) knownScanners
    else {
      val scannerPairs = for {
        known <- newlyKnownScanners
        unknown <- unknownScanners
        // The twelve overlapping beacons will each have the same distances to the 11 other beacons, and they count twice
        if (known.signature intersect unknown.signature).size >= 12 * 11 / 2
      } yield (known, unknown)

      val readingPairs = (for {
        (known, unknown) <- scannerPairs
        knownReading <- known.readings
        unknownReading <- unknown.readings
        // The overlapping beacons appear in both scanners, thus a beacon in both places will match at least 11 other beacons
        if (known.beaconSignatures(knownReading) intersect unknown.beaconSignatures(unknownReading)).size >= 11
      } yield (known.basis.get * knownReading + known.position.get, unknownReading, unknown))
        .groupBy(_._3)

      // Find a basis such that all matching pairs give the same scanner position
      val validPossibleScannerLocations = for {
        (scanner, pairs) <- readingPairs
        basis <- allBases
        mappedPairs = pairs.map { case (knownReading, unknownReading, _) =>
          knownReading - (basis * unknownReading)
        }
        if mappedPairs.toSet.size == 1
      } yield (scanner, basis, mappedPairs.head)

      val updatedScanners = validPossibleScannerLocations.map {
        case (scanner, basis, position) => scanner.copy(position = Some(position), basis = Some(basis))
      }.toVector

      val updatedScannerIds = updatedScanners.map(_.id).toSet

      mapScanners(
        knownScanners ++ updatedScanners,
        unknownScanners.filterNot(s => updatedScannerIds.contains(s.id)),
        updatedScanners
      )
    }
  }

  private def parse(): Vector[Scanner] = {
    @tailrec
    def helper(remainingInput: Vector[String], acc: Vector[Scanner]): Vector[Scanner] = {
      if (remainingInput.isEmpty) acc
      else {
        val (scannerLines, rest) = remainingInput.span(_.nonEmpty)

        val idPattern = "--- scanner (\\d+) ---".r
        val id = scannerLines.head match {
          case idPattern(id) => id.toInt
        }

        val readings = scannerLines.tail.map { line =>
          val values = line.split(",").map(_.toInt)
          Vec3(values(0), values(1), values(2))
        }

        val scanner = Scanner(
          id,
          if (id == 0) Some(Vec3(0, 0, 0)) else None,
          if (id == 0) Some(STANDARD_BASIS) else None,
          readings
        )

        val updatedAcc = acc :+ scanner

        if (rest.nonEmpty) helper(rest.tail, updatedAcc)
        else updatedAcc
      }
    }

    helper(input, Vector())
  }

  private case class Scanner(
    id: Int,
    position: Option[Vec3[Int]],
    basis: Option[Matrix3[Int]],
    readings: Vector[Vec3[Int]]
  ) {
    lazy val absoluteReadings: Vector[Vec3[Int]] = {
      readings.map(reading => {
        (basis.get * reading) + position.get
      })
    }

    lazy val beaconSignatures: Map[Vec3[Int], Set[Int]] = {
      readings.map { reading =>
        val distances = for {
          other <- readings
          if other != reading
        } yield (reading - other).norm1
        reading -> distances.toSet
      }.toMap
    }

    lazy val signature: Set[Int] = {
      beaconSignatures.values.flatten.toSet
    }
  }

  private val allBases = for {
    e1 <- STANDARD_BASIS.vectors
    e2 <- STANDARD_BASIS.vectors
    if e1 != e2
    e1d <- Seq(1, -1)
    e2d <- Seq(1, -1)
  } yield Matrix3(
    e1 * e1d,
    e2 * e2d,
    (e1 * e1d) x (e2 * e2d)
  )
}

case object Day19 extends App {
  val input = Files.lines("2021/day19.txt")
  val problem = Day19(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
