package space.scown.adventofcode
package lib

import java.awt.{Color, Dimension, Font, Graphics, GridBagLayout}
import javax.swing.JPanel

class GridPanel[T](var grid: Grid[T], width: Int, height: Int) extends JPanel() {

  setPreferredSize(new Dimension(width, height))
  setLayout(new GridBagLayout())

  def setGrid(grid: Grid[T]): Unit = {
    this.grid = grid
    this.repaint()
  }

  override def paintComponent(g: Graphics): Unit = {
    g.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12))
    g.setColor(Color.BLACK)

    val rowStrings = grid.values.map(row => row.mkString(""))
    val stringHeight = getGraphics.getFontMetrics().getHeight

    rowStrings.zipWithIndex.foreach { case (rs, i) =>
      g.drawString(rs, 0, i * stringHeight + stringHeight)
    }
  }

}
