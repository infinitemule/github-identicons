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
  
  
}