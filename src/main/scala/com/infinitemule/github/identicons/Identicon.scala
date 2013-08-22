package com.infinitemule.github.identicons

import org.apache.commons.codec.digest.DigestUtils
import java.awt.Color

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
  
  
  def print(username: String) = {
    
    val identicon = create(username)
    
    identicon.glyph.foreach { row =>
      println(row.map { x => if(x == 1) "X" else " " }.mkString(""))      
    }
    
    println("COLOR: " + identicon.color)
    
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
   * - Return it as a hex string
   * 
   * TODO Make the saturation and value either customizable
   *      or within a range based on the hash.
   */
  def createColor(rgb: List[Int]): String = {       
    
    val hsb = Color.RGBtoHSB(rgb(0), rgb(1), rgb(2), null)
    
    val color = new Color(Color.HSBtoRGB(hsb(0), 0.45f, 0.80f))
        
    "%02x%02x%02x".format(color.getRed(), color.getGreen(), color.getBlue())
    
  }
    
}

case class Identicon(val glyph: List[List[Int]], val color: String)
