package TimingMazes
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.collection.mutable.ArrayBuffer

/**
  * Created by ast15tbl on 06/04/17.
  */
object ImageReader {
  /**
    * Läser en maze från en bild som uppfyller samma krav som http://hereandabove.com/maze/mazeorig.form.html
    * Dessa bilder har en vit ram, alltså en stig runt hela laburinten. Därför bortses det yttersta lagret från
    * i denna algoritm.
    * @param file En bildfil innehållande laburinten med en pixel per vägg- respektive stig-element
    * @return returnerar en matris av booleaner där true representerar stig och false representerar vägg.
    */
  def fromImage(file: File): Vector[Vector[Boolean]] = {
    val image: BufferedImage = ImageIO.read(file)
    val result: ArrayBuffer[Vector[Boolean]] = ArrayBuffer.empty

    for (y <- 1 until image.getHeight - 1) {
      val row : ArrayBuffer[Boolean] = ArrayBuffer.empty

      for (x <- 1 until image.getWidth - 1) {
        val pixel = image.getRGB(x, y)  //Pixeldata
        val red = (pixel >> 16) & 0xff
        val green = (pixel >> 8) & 0xff
        val blue = pixel & 0xff

        val isPath = red == 255 && green == 255 && blue == 255
        row.append(isPath)
      }
      result.append(row.toVector)
    }
    result.toVector
  }

  def main(args: Array[String]): Unit = {
    val file = new File("C:/Users/Tobbe/Desktop/maze10x10.png")
    val matrix = fromImage(file)
    for(y <- matrix.indices){
      for (x <- matrix(y).indices) {
        print({if (matrix(y)(x)) 1 else 0})
      }
      println()
    }
  }
}
