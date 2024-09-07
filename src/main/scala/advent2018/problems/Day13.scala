package space.scown.adventofcode
package advent2018.problems

import lib.Gui.getPanelWindow
import lib._

import javax.swing.WindowConstants
import scala.annotation.tailrec
import scala.math.Numeric.Implicits.infixNumericOps

case class Day13(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = parse()
    val cartLocations = grid.zipWithIndex.filter(p => p._1 == '<' | p._1 == '>' | p._1 == 'v' | p._1 == '^')
    val carts = cartLocations.map { case (c, position) =>
      Cart(
        position,
        c match {
          case '<' => -Complex.ONE
          case '>' => Complex.ONE
          case '^' => Complex.I
          case 'v' => -Complex.I
        },
        Complex.I
      )
    }.toVector

    val cleanGrid = grid.map {
      case '<' => '-'
      case '>' => '-'
      case '^' => '|'
      case 'v' => '|'
      case c => c
    }

//    val panel = new CartGridPanel(cleanGrid, 1500, 1500)
//    val frame = getPanelWindow(panel, 1500, 1500)
//    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)

    @tailrec
    def turnHelper(carts: Vector[Cart]): Complex = {
      val sortedCarts = carts.sorted { (x: Cart, y: Cart) =>
        if (x.position.im < y.position.im) 1
        else if (x.position.im > y.position.im) -1
        else {
          if (x.position.re < y.position.re) -1
          else if (x.position.im > y.position.im) 1
          else 0
        }
      }

      @tailrec
      def cartHelper(remainingCarts: Vector[Cart], acc: Vector[Cart]): Either[Vector[Cart], Complex] = {
        if (remainingCarts.isEmpty) Left(acc)
        else {
          val cart = remainingCarts.head
          val newPosition = cart.position + cart.direction
          val nextTrack = cleanGrid(newPosition)

          val newCart = nextTrack match {
            case '\\' => Cart(
              newPosition,
              if (cart.direction.isReal) cart.direction * -Complex.I else cart.direction * Complex.I,
              cart.nextDirection
            )
            case '/' => Cart(
              newPosition,
              if (cart.direction.isReal) cart.direction * Complex.I else cart.direction * -Complex.I,
              cart.nextDirection
            )
            case '+' => Cart(newPosition, cart.direction * cart.nextDirection, cart.nextDirection match {
              case Complex.I => Complex.ONE
              case Complex.ONE => -Complex.I
              case Complex(re, im) if re == 0 && im == -1 => Complex.I
            })
            case _ => cart.copy(position = newPosition)
          }

          val toCollide = (remainingCarts.tail :+ newCart) ++ acc
          val collision = toCollide.groupBy(_.position).find(_._2.size > 1)

//          panel.setGrid(cleanGrid, toCollide, collision.map(_._1))
//          panel.repaint()

          if (collision.isDefined) Right(collision.get._1)
          else cartHelper(remainingCarts.tail, acc :+ newCart)
        }
      }

      cartHelper(sortedCarts, Vector()) match {
        case Left(carts) =>
          turnHelper(carts)
        case Right(collision) => collision
      }
    }

    val result = turnHelper(carts)
    println(s"Result 1: ${result.re},${-result.im}")
  }

  override def solve2(): Unit = {
    val grid = parse()
    val cartLocations = grid.zipWithIndex.filter(p => p._1 == '<' | p._1 == '>' | p._1 == 'v' | p._1 == '^')
    val carts = cartLocations.map { case (c, position) =>
      Cart(
        position,
        c match {
          case '<' => -Complex.ONE
          case '>' => Complex.ONE
          case '^' => Complex.I
          case 'v' => -Complex.I
        },
        Complex.I
      )
    }.toVector

    val cleanGrid = grid.map {
      case '<' => '-'
      case '>' => '-'
      case '^' => '|'
      case 'v' => '|'
      case c => c
    }

    @tailrec
    def turnHelper(carts: Vector[Cart]): Complex = {
      if (carts.size == 1) carts.head.position
      else {
        val sortedCarts = carts.sorted { (x: Cart, y: Cart) =>
          if (x.position.im < y.position.im) 1
          else if (x.position.im > y.position.im) -1
          else {
            if (x.position.re < y.position.re) -1
            else if (x.position.im > y.position.im) 1
            else 0
          }
        }

        @tailrec
        def cartHelper(remainingCarts: Vector[Cart], acc: Vector[Cart]): Vector[Cart] = {
          if (remainingCarts.isEmpty) acc
          else {
            val cart = remainingCarts.head
            val newPosition = cart.position + cart.direction
            val nextTrack = cleanGrid(newPosition)

            val newCart = nextTrack match {
              case '\\' => Cart(
                newPosition,
                if (cart.direction.isReal) cart.direction * -Complex.I else cart.direction * Complex.I,
                cart.nextDirection
              )
              case '/' => Cart(
                newPosition,
                if (cart.direction.isReal) cart.direction * Complex.I else cart.direction * -Complex.I,
                cart.nextDirection
              )
              case '+' => Cart(newPosition, cart.direction * cart.nextDirection, cart.nextDirection match {
                case Complex.I => Complex.ONE
                case Complex.ONE => -Complex.I
                case Complex(re, im) if re == 0 && im == -1 => Complex.I
              })
              case _ => cart.copy(position = newPosition)
            }

            val toCollide = (remainingCarts.tail :+ newCart) ++ acc
            val collision = toCollide.groupBy(_.position).find(_._2.size > 1)

            if (collision.isDefined) cartHelper(remainingCarts.tail.filterNot(collision.get._2.contains), acc.filterNot(collision.get._2.contains))
            else cartHelper(remainingCarts.tail, acc :+ newCart)
          }
        }

        turnHelper(cartHelper(sortedCarts, Vector()))
      }
    }

    val result = turnHelper(carts)
    println(s"Result 2: ${result.re},${-result.im}")
  }

  private def parse(): Grid[Char] = {
    val maxLineLength = input.maxBy(_.length).length

    val paddedLines = input.map { line =>
      if (line.length == maxLineLength) line
      else line + (" " * (maxLineLength - line.length))
    }

    Grid(paddedLines.map(_.toVector))
  }

  private case class Cart(position: Complex, direction: Complex, nextDirection: Complex)

  private class CartGridPanel(grid: Grid[Char], width: Int, height: Int) extends GridPanel[Char](grid, width, height) {
    def setGrid(grid: Grid[Char], carts: Vector[Cart], collision: Option[Complex]): Unit = {
      val updatedGrid = carts.foldLeft(grid) { (grid, cart) =>
        grid.updated(cart.position, cart.direction match {
          case Complex(re, im) if re == -1 && im == 0 => '<'
          case Complex.ONE => '>'
          case Complex.I => '^'
          case Complex(re, im) if re == 0 && im == -1 => 'v'
        })
      }

      if(collision.isDefined) setGrid(updatedGrid.updated(collision.get, 'X'))
      else setGrid(updatedGrid)
    }
  }
}

case object Day13 extends App {
  val input = Files.lines("2018/day13.txt")
  val problem = Day13(input)
  problem.solve1()
  problem.solve2()
}
