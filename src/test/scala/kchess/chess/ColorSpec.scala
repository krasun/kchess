package kchess.chess

import org.scalatest.FlatSpec

class ColorSpec extends FlatSpec {

  "Color values" should "be only white and black" in {
    assert(Color.values === List(White(), Black()))
  }

  "String representation of White()" should "be White" in {
    assert(White().toString === "White")
  }

  "String representation of Black()" should "be Black" in {
    assert(Black().toString === "Black")
  }

  "Opposite of White()" should "be Black()" in {
    assert(White().opposite === Black())
  }

  "Opposite of Black()" should "be White()" in {
    assert(Black().opposite === White())
  }
}