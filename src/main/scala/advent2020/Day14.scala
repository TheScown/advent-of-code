package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

import java.lang.Long.parseLong

case class Day14(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()

    val finalState = instructions.foldLeft(State(0, 0, Map())) {
      case (State(zeroesMask, onesMask, memory), instruction) => instruction match {
        case Mask(mask) =>
          val zeroesMask = parseLong(mask.replaceAll("X", "0"), 2)
          val onesMask = parseLong(mask.replaceAll("X", "1"), 2)

          State(zeroesMask, onesMask, memory)
        case Mem(address, value) =>
          val adjustedValue = (value & onesMask) | zeroesMask

          State(zeroesMask, onesMask, memory + (address -> adjustedValue))
      }
    }

    val result = finalState.memory.values.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()

    val (_, finalMemory) = instructions.foldLeft(("", Map[Long, Long]())) {
      case ((mask, memory), instruction) => instruction match {
        case Mask(newMask) => (newMask, memory)
        case Mem(address, value) =>
          val addressString = BigInt(address).toString(2).reverse.padTo(36, '0').reverse

          val updatedAddressString = addressString.zip(mask).map {
            case (aBit, maskBit) => maskBit match {
              case 'X' => 'X'
              case '1' => '1'
              case '0' => aBit
            }
          }

          val indexesToChange = updatedAddressString.zipWithIndex.filter(_._1 == 'X').map(_._2)

          val allAddresses = indexesToChange.foldLeft(Vector(updatedAddressString)) { case (addresses, index) =>
            addresses.flatMap(address => Seq(
              address.updated(index, '0'),
              address.updated(index, '1'),
            ))
          }.map(address => parseLong(address.mkString(""), 2))

          val finalMemory = allAddresses.foldLeft(memory) { (memory, address) =>
            memory + (address -> value)
          }

          (mask, finalMemory)
      }
    }

    val result = finalMemory.values.sum

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Instruction] = {
    val maskPattern = "mask = ([01X]+)".r
    val memPattern = "mem\\[(\\d+)] = (\\d+)".r

    input.map {
      case maskPattern(mask) => Mask(mask)
      case memPattern(address, value) => Mem(address.toLong, value.toLong)
    }
  }

  case class State(zeroesMask: Long, onesMask: Long, memory: Map[Long, Long])

  trait Instruction
  case class Mem(address: Long, value: Long) extends Instruction
  case class Mask(mask: String) extends Instruction
}

case object Day14 extends App {
  val input = Files.lines("2020/day14.txt")
  val problem = Day14(input)
  problem.solve1()
  problem.solve2()
}
