package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day9(input: String) extends Problem {
  override def solve1(): Unit = {
    val digits = input.map(_.asDigit).toVector
    val blocks = digits.grouped(2).zipWithIndex.map{ case (pair, id) =>
      BlockGroup(id, pair.head, if (pair.tail.nonEmpty) Vector.fill(pair(1))(None) else Vector())
    }.toVector

    @tailrec
    def defrag(blockIndex: Int, reallocateIndex: Int, blocks: Vector[BlockGroup]): Vector[BlockGroup] = {
      if (blockIndex == reallocateIndex) blocks
      else {
        val source = blocks(reallocateIndex)
        val dest = blocks(blockIndex)

        val (updatedDest, updatedSource) =
          if (dest.remainingCapacity > source.usedSize) (dest.take(source.id, source.usedSize), BlockGroup(source.id, 0, Vector()))
          else (dest.take(source.id, dest.remainingCapacity), source.copy(usedSize = source.usedSize - dest.remainingCapacity))

        val updatedBlocks = blocks
          .updated(blockIndex, updatedDest)
          .updated(reallocateIndex, updatedSource)

        if (updatedSource.usedSize > 0) defrag(blockIndex + 1, reallocateIndex, updatedBlocks)
        else defrag(blockIndex, reallocateIndex - 1, updatedBlocks)
      }
    }

    val defragged = defrag(0, blocks.size - 1, blocks)

    val result = checksum(defragged)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val digits = input.map(_.asDigit).toVector
    val blocks = digits.grouped(2).zipWithIndex.map{ case (pair, id) =>
      BlockGroup(id, pair.head, if (pair.tail.nonEmpty) Vector.fill(pair(1))(None) else Vector())
    }.toVector

    val defragged = blocks.zipWithIndex.reverse.foldLeft(blocks) { case (newBlocks, (block, i)) =>
      val firstBlockIndex = newBlocks.zipWithIndex.indexWhere { case (dest, destIndex) =>
        destIndex < i && dest.remainingCapacity >= block.usedSize
      }

      if (firstBlockIndex == -1) newBlocks
      else {
        val destBlock = newBlocks(firstBlockIndex)
        val updatedBlock = destBlock.take(block.id, block.usedSize)

        newBlocks.updated(firstBlockIndex, updatedBlock).updated(i, newBlocks(i).copy(id = 0))
      }
    }

    val result = checksum(defragged)

    println(s"Result 2: $result")
  }

  private def checksum(defragged: Vector[BlockGroup]) = {
    defragged.foldLeft((0L, 0L)) { case ((sum, offset), block) =>
      (sum + block.checksum(offset), offset + block.totalSize)
    }._1
  }

  private case class BlockGroup(id: Long, usedSize: Int, free: Vector[Option[Long]]) {
    def totalSize: Long = usedSize + free.size
    def remainingCapacity: Int = free.size - free.count(_.isDefined)

    def take(newId: Long, quantity: Int): BlockGroup = {
      val (notFree, stillFree) = free.span(_.isDefined)

      if (quantity > stillFree.size)
        throw new IllegalArgumentException(s"Insufficient capacity: required $quantity but was ${stillFree.size}")
      else copy(
        free = notFree ++ Vector.fill(quantity)(Some(newId)) ++ Vector.fill(stillFree.size - quantity)(None)
      )
    }

    def checksum(offset: Long): Long = {
      val ownFileChecksum = (usedSize * offset + usedSize * (usedSize - 1) / 2) * id

      val freeChecksum = free.filter(_.isDefined).zipWithIndex.map {
        case (data, index) => data.get * (index + offset + usedSize)
      }.sum

      ownFileChecksum + freeChecksum
    }
  }
}

case object Day9 extends App {
  val input = Files.lines("2024/day9.txt").head
  val problem = Day9(input)
  problem.solve1()
  problem.solve2()
}
