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

  /**
   * Still trying to figure out how GitHub does this but I am
   * using the all the elements in the has but the last one,
   * this gives me 15 numbers to work with (5x3)
   * 
   * - Get a hash
   * - Divide into groups of two hex digits ("f3", "87" ...)
   * - Parse to Int and map mod 2 over them to get odd/even (0, 1, 0 ...)
   * - Group them into threes, drop the last digit (5 lists of three digits)
   * - Take the last two numbers, reverse them and add them to the front of 
   *   the list (0, 0, 1) => (1, 0, 0, 0, 1)
   */
  def create(username: String): Identicon = {
           
    val hash = DigestUtils.sha1Hex(username)
      .grouped(2)
      .toList
      .map { Integer.parseInt(_, 16) }
    
    val glyph = hash        
        .map { _ % 2 }
        .grouped(3)
        .take(5)
        .map { xs => xs(2) :: xs(1) :: xs }
        .toList
            
    val color = createColor(hash.take(3))
        
    Identicon(glyph, color)
  }  
  
  
  /**
   * 
   * I have no idea how GitHub determines the color 
   * but this will keep the colors within a nice visible
   * range (i.e. colors that are close to all white or all black)
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
    
    new Color(Color.HSBtoRGB(hsb(0), 0.45f, 0.80f))
                
  }
      
    
}

case class Identicon(val glyph: List[List[Int]], val color: Color) {
  
  
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
    
    val img = new BufferedImage(420, 420, BufferedImage.TYPE_INT_RGB)
    val gfx = img.createGraphics()

    gfx.setColor(new Color(240, 240, 240))
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
     .map { case(row, i) => 
       row.filter { case(bit, j) => bit == 1 } 
       .map { case(bit, j) => (j, i) } 
     } 
     .flatten

    onPixels.map { case(x, y) => gfx.fillRect(x * 84, y * 84, 84, 84) } 
    
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
      println(row.map { x => if(x == 1) "X" else " " }.mkString(""))      
    }
    
    println("COLOR: " + hexColor)
    
  }
}
