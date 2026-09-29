package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case class Day16(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val rootPacket = parseRootPacket()

    def versionSum(packet: Packet): Int = packet match {
      case Value(version, _) => version
      case Operator(version, _, packets) => version + packets.map(versionSum).sum
    }

    val result = versionSum(rootPacket)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val rootPacket = parseRootPacket()

    def computePacket(packet: Packet): Long = packet match {
      case Value(_, value) => value
      case Operator(_, operation, packets) => operation match {
        case 0 => packets.map(computePacket).sum
        case 1 => packets.map(computePacket).product
        case 2 => packets.map(computePacket).min
        case 3 => packets.map(computePacket).max
        case 5 => packets match {
          case Vector(a, b) => if (computePacket(a) > computePacket(b)) 1 else 0
        }
        case 6 => packets match {
          case Vector(a, b) => if (computePacket(a) < computePacket(b)) 1 else 0
        }
        case 7 => packets match {
          case Vector(a, b) => if (computePacket(a) == computePacket(b)) 1 else 0
        }
      }
    }

    val result = computePacket(rootPacket)

    println(s"Result 2: $result")
  }

  private def parseRootPacket() = {
    val bits = parseBits()

    Grammar.parse(Grammar.packet, bits).get
  }

  private def parseBits(): String = {
    input.head.flatMap { c =>
      val i = Integer.valueOf(c.toString, 16)
      val b = Integer.toBinaryString(i)
      s"${"0" * (4 - b.length)}$b"
    }.mkString
  }

  private sealed trait Packet {
    def version: Int
    def packetType: Int
  }
  private case class Value(version: Int, value: Long) extends Packet {
    override val packetType = 4
  }
  private case class Operator(version: Int, packetType: Int, packets: Vector[Packet]) extends Packet

  private case object Grammar extends RegexParsers {
    def packet: Parser[Packet] = intPacket | operatorPacket

    private def intPacket: Parser[Value] = bits(3) ~ "100" ~ (nonTerminalIntGroup*) ~ terminalIntGroup ^^ {
      case version ~ _ ~ nonTerminals ~ terminal => Value(
        bits2Int(version),
        bits2Long(nonTerminals.mkString + terminal)
      )
    }

    private def operatorPacket: Parser[Operator] = bits(3) ~ bits(3) ~ (bitPackets | countedPackets) ^^ {
      case version ~ typeString ~ packets => Operator(
        bits2Int(version),
        bits2Int(typeString),
        packets.toVector
      )
    }

    private def bitPackets: Parser[Seq[Packet]] = "0" ~ fixedBitsToPackets ^^ {
      case _ ~ packets => packets
    }

    private def countedPackets: Parser[Seq[Packet]] = "1" ~ packetCountToPackets ^^ {
      case _ ~ packets => packets
    }

    private def fixedBitsToPackets: Parser[Seq[Packet]] = {
      (in: Input) => {
        parse(bits(15), in) match {
          case Success(sizeBits, next) =>
            val size = bits2Int(sizeBits)
            parse(bits(size), next) match {
              case Success(packetBits, rest) =>
                parse(rep(packet), packetBits) match {
                  case Success(packets, _) => Success(packets, rest)
                }
            }
        }
      }
    }

    private def packetCountToPackets: Parser[Seq[Packet]] = {
      (in: Input) => {
        parse(bits(11), in) match {
          case Success(bits, next) =>
            val packetCount = bits2Int(bits)
            parse(repN(packetCount, packet), next)
        }
      }
    }

    private def nonTerminalIntGroup: Parser[String] = "1" ~> bits(4)
    private def terminalIntGroup: Parser[String] = "0" ~> bits(4)

    private def bits(n: Int): Parser[String] = repN(n, bit) ^^ (bs => bs.mkString)
    private def bit: Parser[String] = "0" | "1"
  }

  private def bits2Int(s: String) = {
    Integer.valueOf(s, 2)
  }

  private def bits2Long(s: String) = {
    java.lang.Long.valueOf(s, 2)
  }
}

case object Day16 extends App {
  val input = Files.lines("2021/day16.txt")
  val problem = Day16(input)
  problem.solve1()
  problem.solve2()
}
