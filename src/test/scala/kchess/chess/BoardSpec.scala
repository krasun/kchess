package kchess.chess

import org.scalatest.FlatSpec

import scala.util.{Failure, Success}

class BoardSpec extends FlatSpec {

  val standardBoard = Board.standard

  "Standard board" should "have standard position of pieces" in {
    assert(
      standardBoard === Board(
        Map(
          Position.A2 -> Pawn(White()),
          Position.B2 -> Pawn(White()),
          Position.C2 -> Pawn(White()),
          Position.D2 -> Pawn(White()),
          Position.E2 -> Pawn(White()),
          Position.F2 -> Pawn(White()),
          Position.G2 -> Pawn(White()),
          Position.H2 -> Pawn(White()),

          Position.A1 -> Rook(White()),
          Position.B1 -> Knight(White()),
          Position.C1 -> Bishop(White()),
          Position.D1 -> Queen(White()),
          Position.E1 -> King(White()),
          Position.F1 -> Bishop(White()),
          Position.G1 -> Knight(White()),
          Position.H1 -> Rook(White()),

          Position.A7 -> Pawn(Black()),
          Position.B7 -> Pawn(Black()),
          Position.C7 -> Pawn(Black()),
          Position.D7 -> Pawn(Black()),
          Position.E7 -> Pawn(Black()),
          Position.F7 -> Pawn(Black()),
          Position.G7 -> Pawn(Black()),
          Position.H7 -> Pawn(Black()),

          Position.A8 -> Rook(Black()),
          Position.B8 -> Knight(Black()),
          Position.C8 -> Bishop(Black()),
          Position.D8 -> Queen(Black()),
          Position.E8 -> King(Black()),
          Position.F8 -> Bishop(Black()),
          Position.G8 -> Knight(Black()),
          Position.H8 -> Rook(Black())
        )
      )
    )
  }

  "At" should "return some(piece) at specified position" in {
    assert(standardBoard.at(Position.E2) === Some(Pawn(White())))
  }

  "At" should "return none at specified position" in {
    assert(standardBoard.at(Position.E2.up().get) === None)
  }

  "Drop" should "drop piece at specified position" in {
    val updatedBoard = standardBoard.drop(Position.E2)

    assert(updatedBoard.at(Position.E2) === None)
  }

  "Move" should "move piece successfully" in {
    val Success(updatedBoard) = standardBoard.move(Position.E2, Position.E2.up().get)

    assert(updatedBoard.at(Position.E2) === None)
    assert(updatedBoard.at(Position.E2.up().get) === Some(Pawn(White())))
  }

  "Move" should "fail if there is no piece at specified position" in {
    val Failure(exception) = standardBoard.move(Position.E2.up().get, Position.E2)

    assert(exception.getMessage === "There is no piece at e3.")
  }

  "White king position" should "is E1" in {
    assert(standardBoard.kingPosition(White()) === Some(Position.E1))
  }

  "Black king position" should "is E8" in {
    assert(standardBoard.kingPosition(Black()) === Some(Position.E8))
  }

  "Color of white" should "return pieces of white color" in {
    val board = Board(
      Map(
        Position.E1 -> Pawn(White()),
        Position.H8 -> Knight(Black()),
        Position.B7 -> Pawn(Black()),
        Position.A2 -> Knight(White())
      )
    )

    assert(
      board.ofColor(White()) ===
        Map(
          Position.E1 -> Pawn(White()),
          Position.A2 -> Knight(White())
        )
    )
  }

  "Color of black" should "return pieces of black color" in {
    val board = Board(
      Map(
        Position.E1 -> Pawn(White()),
        Position.H8 -> Knight(Black()),
        Position.B7 -> Pawn(Black()),
        Position.A2 -> Knight(White())
      )
    )

    assert(
      board.ofColor(Black()) ===
        Map(
          Position.H8 -> Knight(Black()),
          Position.B7 -> Pawn(Black())
        )
    )
  }
}