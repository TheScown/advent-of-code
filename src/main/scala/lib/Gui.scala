package space.scown.adventofcode
package lib

import java.awt.{FlowLayout, Image}
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel, WindowConstants}

case object Gui {
  def renderImage(image: BufferedImage, scaleFactor: Int = 10): Unit = {
    val width = image.getWidth()
    val height = image.getHeight()

    val frame = new JFrame()
    // Scale the image up so it's legible
    val icon = new ImageIcon(image.getScaledInstance(width * scaleFactor, height * scaleFactor, Image.SCALE_DEFAULT))
    frame.setLayout(new FlowLayout())
    // Need to allow height for the title bar
    frame.setSize(width * scaleFactor, height * scaleFactor + 50)
    val label = new JLabel()
    label.setIcon(icon)
    frame.add(label)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  }
}
