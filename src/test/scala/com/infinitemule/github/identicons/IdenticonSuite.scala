/*
 * GitHub Identicons
 */
package com.infinitemule.github.identicons

import org.scalatest.FunSuite

/**
 * This is difficult to test since you don't know
 * what you are going to get because it's based on 
 * a hash value. 
 */
class IdenticonSuite extends FunSuite {

  test("Creating an Identicon should result in a 5 x 5 matrix of booleans") {
    
    val identicon = Identicon.create("test")
    
    assert(identicon.glyph.size === 5)
    assert(identicon.glyph(1).size === 5)
    
  }
  
  /**
   * Note this will fail if a different hash algo is used
   */
  test("Using test as a username should create the spicified identicon") {
    
    val identicon = Identicon.create("test")
    
    assert(identicon.glyph(0)(0))
    assert(identicon.glyph(0)(2))
    assert(identicon.glyph(0)(4))
    
    assert(identicon.glyph(1)(0))
    assert(identicon.glyph(1)(2))
    assert(identicon.glyph(1)(4))

    assert(identicon.glyph(2)(2))

    assert(identicon.glyph(3)(0))
    assert(identicon.glyph(3)(4))

    assert(identicon.glyph(4)(0))
    assert(identicon.glyph(4)(1))
    assert(identicon.glyph(4)(2))
    assert(identicon.glyph(4)(3))
    assert(identicon.glyph(4)(4))

    assert(identicon.color.getRed()   === 204)
    assert(identicon.color.getGreen() === 112)
    assert(identicon.color.getBlue()  === 179)
    
  }
  
}