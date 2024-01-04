package space.scown.adventofcode2019
package problems

import intcode.{IntcodeComputer, IntcodeProgram, Output, RequiresInput}
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

case class Day23(lines: Vector[String]) extends Problem {
  private val ips: NumericRange.Inclusive[Long] = 0L to 49L

  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)

    val computers = ips.map(ip => IntcodeComputer(program).execute() match {
      case RequiresInput(_, continue) => (ip, continue(ip))
    })

    @tailrec
    def runNetwork(computers: Seq[(Long, Output)], nextComputers: Seq[(Long, Output)], inboxes: Map[Long, Seq[Packet]]): Packet = {
      if (computers.isEmpty) {
        val targetPackets = inboxes(255);

        if (targetPackets.nonEmpty) targetPackets.head
        else runNetwork(nextComputers, Vector(), inboxes)
      } else {
        computers.head match {
          case (ip, output@RequiresInput(outputs, continue)) =>
            val nextPackets = inboxes(ip)

//            println(s"Outputs from $ip, $outputs")
//            println(s"Packets for $ip, $nextPackets")

            val nextComputer: Output = if (nextPackets.nonEmpty) {
              nextPackets.foldLeft(output.asInstanceOf[Output]) {(output, packet) => output match {
                case RequiresInput(_, continue) => continue(packet.x) match {
                  case RequiresInput(_, continue) => continue(packet.y)
                }
              }}
            }
            else {
              continue(-1)
            }

            val mapWithPacketsConsumed = inboxes + (ip -> Vector())
            val updatedInboxes = outputs.grouped(3).map(p => (p.head, Packet(p.head, p(1), p(2)))).foldLeft(mapWithPacketsConsumed) { (map, pair) =>
              val (address, packet) = pair
              map + (address -> (map(address) :+ packet))
            }

            runNetwork(computers.tail, nextComputers :+ (ip, nextComputer), updatedInboxes)
        }
      }
    }

    val result = runNetwork(computers, Vector(), Map.from(ips.map(ip => (ip, Vector()))) + (255L -> Vector()))

    // Should be 24106
    println(s"Result 1: ${result.y}")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)

    val computers = ips.map(ip => IntcodeComputer(program).execute() match {
      case RequiresInput(_, continue) => (ip, continue(ip))
    })

    @tailrec
    def runNetwork(computers: Seq[(Long, Output)], nextComputers: Seq[(Long, Output)], inboxes: Map[Long, Seq[Packet]], idleCount: Int, packetsSentByNat: Seq[Packet]): Packet = {
      if (computers.isEmpty) {
        val targetPackets = inboxes(255);
        println(s"IDLE COUNT: $idleCount")

        if (targetPackets.nonEmpty && idleCount == 50)  {
          println("IDLE")
          if (packetsSentByNat.size > 2 && packetsSentByNat.last == packetsSentByNat(packetsSentByNat.size - 2)) packetsSentByNat.last
          else {
            val packetToSend = targetPackets.last
            println(s"Sending $packetToSend")
            runNetwork(nextComputers, Vector(), inboxes + (0L -> inboxes(0).appended(packetToSend)), 0, packetsSentByNat :+ packetToSend)
          }
        }
        else {
          println("No NAT packets")
          runNetwork(nextComputers, Vector(), inboxes, 0, packetsSentByNat)
        }
      } else {
        computers.head match {
          case (ip, output@RequiresInput(outputs, continue)) =>
            val nextPackets = inboxes(ip)

//            println(s"Outputs from $ip, $outputs")
//            println(s"Packets for $ip, $nextPackets")

            val nextComputer: Output = if (nextPackets.nonEmpty) {
              nextPackets.foldLeft(output.asInstanceOf[Output]) {(output, packet) => output match {
                case RequiresInput(_, continue) => continue(packet.x) match {
                  case RequiresInput(_, continue) => continue(packet.y)
                }
              }}
            }
            else {
              continue(-1)
            }

            val mapWithPacketsConsumed = inboxes + (ip -> Vector())
            val updatedInboxes = outputs.grouped(3).map(p => (p.head, Packet(p.head, p(1), p(2)))).foldLeft(mapWithPacketsConsumed) { (map, pair) =>
              val (address, packet) = pair
              map + (address -> (map.getOrElse(address, Vector()) :+ packet))
            }

            val newIdle = if (nextPackets.isEmpty) idleCount + 1 else idleCount

            runNetwork(computers.tail, nextComputers :+ (ip, nextComputer), updatedInboxes, newIdle, packetsSentByNat)
        }
      }
    }

    val result = runNetwork(computers, Vector(), Map.from(ips.map(ip => (ip, Vector()))) + (255L -> Vector()), 0, Vector())

    println(s"Result 2: ${result.y}")
  }
}

case class Packet(address: Long, x: Long, y: Long)

object Day23 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day23.txt")
    Day23(value).solve1()
    Day23(value).solve2()
  }

}