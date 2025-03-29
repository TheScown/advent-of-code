package space.scown.adventofcode
package intcodeassembler

import java.io.{BufferedWriter, FileWriter}
import scala.io.Source
import scala.util.Using
import scala.util.parsing.input.{PagedSeq, PagedSeqReader}

case class Assembler(pagedSeq: PagedSeq[Char]) {
  def assembled: List[Long] = {
    val instructions = parse()

    val (labelMap, _) = instructions.foldLeft[(Map[String, Long], Long)]((Map(), 0)) {
      case ((labels, line), instruction) =>
        instruction match {
          case Label(label) =>
            if (labels.contains(label)) throw new IllegalArgumentException(s"Duplicate label $label")
            else (labels + (label -> line), line)
          case instruction => (labels, line + instruction.size)
        }
    }

    def mapParameter(parameter: Parameter): Parameter = parameter match {
      case p@ValueParameter(_, _) => p
      case LabelParameter(label, mode, offset) => ValueParameter(labelMap(label) + offset, mode)
    }

    instructions.flatMap {
      case Add(dest, a, b) =>
        Vector(
          1 + 100 * a.mode.value + 1000 * b.mode.value + 10000 * dest.mode.value,
          mapParameter(a).value,
          mapParameter(b).value,
          mapParameter(dest).value
        )
      case Mul(dest, a, b) =>
        Vector(
          2 + 100 * a.mode.value + 1000 * b.mode.value + 10000 * dest.mode.value,
          mapParameter(a).value,
          mapParameter(b).value,
          mapParameter(dest).value
        )
      case In(dest) =>
        Vector(
          3 + 100 * dest.mode.value,
          mapParameter(dest).value
        )
      case Out(src) =>
        Vector(
          4 + 100 * src.mode.value,
          mapParameter(src).value
        )
      case Jnz(test, dest) =>
        Vector(
          5 + 100 * test.mode.value + 1000 * dest.mode.value,
          mapParameter(test).value,
          mapParameter(dest).value
        )
      case Jez(test, dest) =>
        Vector(
          6 + 100 * test.mode.value + 1000 * dest.mode.value,
          mapParameter(test).value,
          mapParameter(dest).value
        )
      case Lt(dest, a, b) =>
        Vector(
          7 + 100 * a.mode.value + 1000 * b.mode.value + 10000 * dest.mode.value,
          mapParameter(a).value,
          mapParameter(b).value,
          mapParameter(dest).value
        )
      case Eq(dest, a, b) =>
        Vector(
          8 + 100 * a.mode.value + 1000 * b.mode.value + 10000 * dest.mode.value,
          mapParameter(a).value,
          mapParameter(b).value,
          mapParameter(dest).value
        )
      case Rb(value) =>
        Vector(
          9 + 100 * value.mode.value,
          mapParameter(value).value
        )
      case End() => Vector(99)
      case Label(_) => Vector()
      case Value(x) => Vector(x)
    }
  }

  private def parse(): List[Instruction] = {
    Grammar.parseAll[List[Instruction]](Grammar.lines, new PagedSeqReader(pagedSeq)) match {
      case Grammar.Success(result, _) => result
      case Grammar.NoSuccess.I(error, next) =>
        throw new IllegalArgumentException(error)
    }
  }
}

case object Assembler {
  def main(args: Array[String]): Unit = {
    val src = args.head
    val dest = args(1)

    Using(Source.fromFile(src)) { source =>
      try {
        val reader = source.bufferedReader()
        val output = Assembler(PagedSeq.fromReader(reader)).assembled

        Using(new BufferedWriter(new FileWriter(dest))) { writer =>
          writer.write(output.mkString(","))
          writer.newLine()
        }
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }
  }
}
