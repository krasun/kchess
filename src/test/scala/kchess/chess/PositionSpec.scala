package kchess.chess

import org.scalatest.FlatSpec

class PositionSpec extends FlatSpec {
  "Add tuple" should "increment position" in {
    assert((Position.A1 + (1, 1)).get === Position.B2)
  }

  "Add tuple" should "return nothing for invalid position" in {
    assert((Position.A1 + (-1, -1)).isEmpty)
  }

  "Apply" should "return Position by string" in {
    assert(Position('a', '1') === Position.A1)
  }

  "A1" should "have string representation as A1" in {
    assert(Position.A1.toString === "a1")
  }

  "H8" should "have string representation as H8" in {
    assert(Position.H8.toString === "h8")
  }

  "Up" should "inrement row by 1 (A1.up == A2)" in {
    assert(Position.A1.up().get === Position.A2)
  }

  "Down" should "decrement row by 1 (A2.down == A1)" in {
    assert(Position.A1.up().get === Position.A2)
  }

  "Right" should "inrement column by 1 (A1.right == B1)" in {
    assert(Position.A1.right().get === Position.B1)
  }

  "Left" should "decrement column by 1 (B1.left == A1)" in {
    assert(Position.B1.left().get === Position.A1)
  }

  "Up" should "be None for impossible position" in {
    assert(Position.H8.up().isEmpty)
  }

  "Down" should "be None for impossible position" in {
    assert(Position.H1.down().isEmpty)
  }

  "Left" should "be None for impossible position" in {
    assert(Position.A1.left().isEmpty)
  }

  "Right" should "be None for impossible position" in {
    assert(Position.H1.right().isEmpty)
  }

  "Up right" should "inrement row and column by 1 (A1.upRight == B2)" in {
    assert(Position.A1.upRight().get === Position.B2)
  }

  "Up right" should "be None for impossible position" in {
    assert(Position.H1.upRight().isEmpty)
  }

  "Up left" should "inrement row by 1 and decrement column by 1 (B1.upLeft == A2)" in {
    assert(Position.B1.upLeft().get === Position.A2)
  }

  "Up left" should "be None for impossible position" in {
    assert(Position.A1.upLeft().isEmpty)
  }

  "Down left" should "decrement row and column by 1 (B8.downLeft == A7)" in {
    assert(Position.B8.downLeft().get === Position.A7)
  }

  "Down left" should "be None for impossible position" in {
    assert(Position.A8.downLeft().isEmpty)
  }

  "Down right" should "decrement row by 1 and increment column by 1 (B8.downRight == C7)" in {
    assert(Position.B8.downRight().get === Position.C7)
  }

  "Down right" should "be None for impossible position" in {
    assert(Position.H8.downRight().isEmpty)
  }
}