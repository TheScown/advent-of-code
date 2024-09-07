package space.scown.adventofcode
package lib

import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Dimension, FlowLayout, Image}
import javax.swing._

case object Gui {
  def renderImage(image: BufferedImage, scaleFactor: Int = 10): Unit = {
    val width = image.getWidth()
    val height = image.getHeight()

    val frame = new JFrame()
    frame.setVisible(true)
    // Scale the image up so it's legible
    val icon = new ImageIcon(image.getScaledInstance(width * scaleFactor, height * scaleFactor, Image.SCALE_DEFAULT))
    frame.setLayout(new BorderLayout())
    // Need to allow height for the title bar
    val insets = frame.getInsets
    frame.setSize(width * scaleFactor, height * scaleFactor + insets.top + insets.bottom)
    val label = new JLabel()
    label.setIcon(icon)
    frame.getContentPane.add(label)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  }

  def getPanelWindow(panel: JPanel, width: Int, height: Int): JFrame = {
    val frame = new JFrame()
    frame.setVisible(true)
    // Allow for title bar
    frame.setLayout(new FlowLayout())
    frame.setSize(width, height + frame.getInsets.top)
    frame.setLocationRelativeTo(null)
    frame.getContentPane.add(panel)
    frame.pack()

    frame
  }
}
