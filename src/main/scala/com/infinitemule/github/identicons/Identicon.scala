/*
 * GitHub Identicons
 */
package com.infinitemule.github.identicons

import org.apache.commons.codec.digest.DigestUtils
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.IOException
import java.io.File
import javax.imageio.ImageIO


object Identicon {
    
  object Display {    
    val backgroundColor = new Color(240, 240, 240)
    val width      = 84
    val saturation = 0.45f
    val value      = 0.80f
  }
  
  def hash(s: String) = DigestUtils.md5Hex(s)

  /**
   * This is using GitHub's method to lay out the pixels,
   * but I still don't know which hash they are using.
   * 
   * 
   * - Get a hash
   * - Convert each hex digit to an integer
   * - Take the first 15 hex digits of the integer hash 
   * - Map mod 2 over them to get odd/even, odd being true (false, true, false ...)
   * - Group them by 5 so that you have a 5x3 matrix
   * - Transpose the matrix and reverse the rows so that the
   *   first index is the (2,0) position.
   * - Take the first two bits of a row, reverse them, and
   *   add them to the end.  (true, false, true) -> (true, false, true, false, true)
   */
  def create(username: String): Identicon = {
           
    val ints = hash(username).toCharArray().toList
      .map { b => Integer.parseInt(b.toString, 16) }
    
    val glyph = ints
      .take(15)      
      .map { h  => (h % 2) == 1 }
      .grouped(5).toList
      .transpose
      .map { xs => xs.reverse ::: xs.tail }
                
    val color = createColor(ints.take(3))
        
    Identicon(glyph, color)
  }  
  
  
  /**
   * 
   * I have no idea how GitHub determines the color 
   * but this will keep the colors within a nice visible
   * range (i.e. colors that are not close to all white or all black)
   * 
   * - Take a color in RGB 0-255
   * - Covert it to HSB so that we can get the hue
   * - Create a new color with a fixed saturation and value
   * 
   * TODO Make the saturation and value either customizable
   *      or within a range based on the hash.
   */
  def createColor(rgb: List[Int]): Color = {       
    
    val hsb = Color.RGBtoHSB(rgb(0), rgb(1), rgb(2), null)
    
    new Color(Color.HSBtoRGB(
        hsb(0), 
        Identicon.Display.saturation, 
        Identicon.Display.value))
                
  }
      
    
}


/**
 *
 */
case class Identicon(val glyph: List[List[Boolean]], val color: Color) {
  
  
  /**
   * Gets the color in hex RGB format.  Useful
   * if you want to draw it in HTML.
   */
  def hexColor(): String = {
     "%02x%02x%02x".format(
            color.getRed(), 
            color.getGreen(), 
            color.getBlue())    
  }
  
  
  /**
   * Creates a BufferedImage version of an identicon
   */ 
  def createImage(): BufferedImage = {
    
    val width = Identicon.Display.width
    
    val img = new BufferedImage(width * 5, width * 5, BufferedImage.TYPE_INT_RGB)
    val gfx = img.createGraphics()

    gfx.setColor(Identicon.Display.backgroundColor)
    gfx.fillRect(0, 0, img.getWidth(), img.getHeight())    
    gfx.setColor(color)
    
    /*
     * - Index both rows
     * - Filter only rows with pixel turned on
     * - Map coordinates to the on pixels
     * - Flatten the list to get a list of coords for
     *   each on pixel (i.e ((0,1), (0,2), (0,3), (1,0), ...
     */
    val onPixels =  glyph.map { _.zipWithIndex } .zipWithIndex
     .map { case(row, y) => 
       row.filter { case(bit, x) => bit } 
          .map    { case(bit, x) => (x, y) } 
     } 
     .flatten

    onPixels.map { case(x, y) => 
      gfx.fillRect(x * width, y * width, width, width) 
    } 
    
    img
  }

  
  /**
   * Saves an identicon to a PNG file.
   */
  def saveImage(filename: String) = {
    
    try {
      ImageIO.write(createImage(), "png", new File(filename))
    } 
    catch { 
      case e: IOException =>
        throw new RuntimeException("Error saving image.", e)
    }

  }
    
    
  /**
   * Prints an identicon to the console
   */
  def print() = {
        
    glyph.foreach { row =>      
      println(row.map { x => if(x) "X" else " " }
                 .mkString(""))      
    }
    
    println("COLOR: #" + hexColor)
    
  }
}
