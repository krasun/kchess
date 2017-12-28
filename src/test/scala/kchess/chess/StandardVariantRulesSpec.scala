package kchess.chess

import org.scalatest.FlatSpec

import scala.util.{Failure, Success}

class StandardVariantRulesSpec extends FlatSpec {

  val standardBoard = Board.standard
  val history = History()

  "White Pawn on E2" should "have E3 as available move" in {
    val Success(result) = StandardVariantRules.checkMove(standardBoard, Position.E2, Position.E3, history)

    assert(result === CheckResult(Pawn(White()), None))
  }

  "White Pawn on E2 (standard board)" should "have E4 as available move" in {
    val Success(result) = StandardVariantRules.checkMove(standardBoard, Position.E2, Position.E4, history)

    assert(result === CheckResult(Pawn(White()), None))
  }

  "White Pawn on E2 (standard board)" should "not have E1 as available move" in {
    val Failure(exception) = StandardVariantRules.checkMove(standardBoard, Position.E2, Position.E1, history)

    assert(exception.getMessage === "Invalid move! Available positions: e3, e4.")
  }

  "White Pawn on E2 (standard board)" should "not have F3 as available move" in {
    val Failure(exception) = StandardVariantRules.checkMove(standardBoard, Position.E2, Position.F3, history)

    assert(exception.getMessage === "Invalid move! Available positions: e3, e4.")
  }

  "White Pawn on E2 (standard board)" should "not have D3 as available move" in {
    val Failure(exception) = StandardVariantRules.checkMove(standardBoard, Position.E2, Position.F3, history)

    assert(exception.getMessage === "Invalid move! Available positions: e3, e4.")
  }

  "White Pawn on E2 (empty board)" should "not have E1 as available move" in {
    val board = Board(Map(Position.E2 -> Pawn(White()), Position.A1 -> King(White()), Position.H8 -> King(White())))

    val Failure(exception) = StandardVariantRules.checkMove(board, Position.E2, Position.E1, history)

    assert(exception.getMessage === "Invalid move! Available positions: e3, e4.")
  }

  "Black Pawn on E2 (empty board)" should "not have E1 as available move" in {
    val board = Board(Map(Position.E2 -> Pawn(White()), Position.A1 -> King(White()), Position.H8 -> King(White())))

    val Failure(exception) = StandardVariantRules.checkMove(board, Position.E2, Position.E1, history)

    assert(exception.getMessage === "Invalid move! Available positions: e3, e4.")
  }

  "White Knight on B1 (standard board)" should "have C3 as available move" in {
    val Success(result) = StandardVariantRules.checkMove(standardBoard, Position.B1, Position.C3, history)

    assert(result === CheckResult(Knight(White()), None))
  }

  "White Knight on B1 (standard board)" should "have A3 as available move" in {
    val Success(result) = StandardVariantRules.checkMove(standardBoard, Position.B1, Position.A3, history)

    assert(result === CheckResult(Knight(White()), None))
  }

  "White Knight" should "capture black piece" in {
    val Success(updatedBoard) = standardBoard.move(Position.E7, Position.C3)

    val Success(result) = StandardVariantRules.checkMove(updatedBoard, Position.B1, Position.C3, history)

    assert(result === CheckResult(Knight(White()), None))
  }

  "White Bishop on C1 (standard board)" should "not have available moves" in {
    val Failure(exception) = StandardVariantRules.checkMove(standardBoard, Position.C1, Position.C2, history)

    assert(exception.getMessage === "Invalid move!")
  }

  "White Bishop on F1" should "have E2 as available diagonal move" in {
    val Success(updatedBoard) = standardBoard.move(Position.E2, Position.E4)

    val Success(result) = StandardVariantRules.checkMove(updatedBoard, Position.F1, Position.E2, history)

    assert(result === CheckResult(Bishop(White()), None))
  }

  "White Bishop on F1" should "have D3 as available diagonal move" in {
    val Success(updatedBoard) = standardBoard.move(Position.E2, Position.E4)

    val Success(result) = StandardVariantRules.checkMove(updatedBoard, Position.F1, Position.D3, history)

    assert(result === CheckResult(Bishop(White()), None))
  }

  "White Bishop on F1" should "have C4 as available diagonal move" in {
    val Success(updatedBoard) = standardBoard.move(Position.E2, Position.E4)

    val Success(result) = StandardVariantRules.checkMove(updatedBoard, Position.F1, Position.C4, history)

    assert(result === CheckResult(Bishop(White()), None))
  }

  "White Queen on D1 (standard board)" should "not have D2 as available move" in {
    val Failure(exception) = StandardVariantRules.checkMove(standardBoard, Position.D1, Position.D2, history)

    assert(exception.getMessage === "Invalid move!")
  }

  "White Queen on D1 (standard board)" should "not have C2 as available move" in {
    val Failure(exception) = StandardVariantRules.checkMove(standardBoard, Position.D1, Position.C2, history)

    assert(exception.getMessage === "Invalid move!")
  }

  "White Queen on D1" should "have E2 as available diagonal move" in {
    val Success(updatedBoard) = standardBoard.move(Position.E2, Position.E4)

    val Success(result) = StandardVariantRules.checkMove(updatedBoard, Position.D1, Position.E2, history)

    assert(result === CheckResult(Queen(White()), None))
  }

  "White Queen on D1" should "have F3 as available diagonal move" in {
    val Success(updatedBoard) = standardBoard.move(Position.E2, Position.E4)

    val Success(result) = StandardVariantRules.checkMove(updatedBoard, Position.D1, Position.F3, history)

    assert(result === CheckResult(Queen(White()), None))
  }

  "White Queen on empty board" should "have available diagonal, horizontal and vertical positions" in {
    val board = Board(Map(Position.D4 -> Queen(White())))

    val availablePositions = StandardVariantRules.availableMoves(board, Queen(White()), Position.D4, history).map(_._3).toSet

    assert(availablePositions === Set(
      Position.D1,
      Position.D2,
      Position.D3,
      Position.D5,
      Position.D6,
      Position.D7,
      Position.D8,

      Position.A4,
      Position.B4,
      Position.C4,
      Position.E4,
      Position.F4,
      Position.G4,
      Position.H4,

      Position.A1,
      Position.B2,
      Position.C3,
      Position.E5,
      Position.F6,
      Position.G7,
      Position.H8,

      Position.A7,
      Position.B6,
      Position.C5,
      Position.E3,
      Position.F2,
      Position.G1
    ))
  }

  "White Queen" should "not go through obstacles" in {
    val board = Board(Map(
      Position.D4 -> Queen(White()),
      Position.D6 -> Pawn(White()),
      Position.D2 -> Pawn(White()),
      Position.C4 -> Pawn(White()),
      Position.G4 -> Pawn(White()),
      Position.B2 -> Pawn(White()),
      Position.G7 -> Pawn(White()),
      Position.B6 -> Pawn(White()),
      Position.F2 -> Pawn(White())
    ))

    val availablePositions = StandardVariantRules.availableMoves(board, Queen(White()), Position.D4, history).map(_._3).toSet

    assert(availablePositions === Set(
      Position.D3,
      Position.F6,
      Position.E3,
      Position.F4,
      Position.E4,
      Position.D5,
      Position.E5,
      Position.C5,
      Position.C3
    ))
  }

  "White Queen" should "attack obstacles, not King" in {
    val board = Board(Map(
      Position.D4 -> Queen(White()),
      Position.D6 -> Pawn(Black()),
      Position.D2 -> Pawn(Black()),
      Position.C4 -> Pawn(Black()),
      Position.G4 -> Pawn(Black()),
      Position.B2 -> King(Black()),
      Position.G7 -> Pawn(Black()),
      Position.B6 -> Pawn(Black()),
      Position.F2 -> Pawn(Black())
    ))

    val availablePositions = StandardVariantRules.availableMoves(board, Queen(White()), Position.D4, history).map(_._3).toSet

    assert(availablePositions === Set(
      Position.D3,
      Position.F6,
      Position.E3,
      Position.F4,
      Position.E4,
      Position.D5,
      Position.E5,
      Position.C5,
      Position.C3,
      Position.D6,
      Position.D2,
      Position.C4,
      Position.G4,
      Position.G7,
      Position.B6,
      Position.F2
    ))
  }

  "White Rook on empty board" should "have available horizontal and vertical positions" in {
    val board = Board(Map(Position.D4 -> Rook(White())))

    val availablePositions = StandardVariantRules.availableMoves(board, Rook(White()), Position.D4, history).map(_._3).toSet

    assert(availablePositions === Set(
      Position.D1,
      Position.D2,
      Position.D3,
      Position.D5,
      Position.D6,
      Position.D7,
      Position.D8,

      Position.A4,
      Position.B4,
      Position.C4,
      Position.E4,
      Position.F4,
      Position.G4,
      Position.H4
    ))
  }

  "White Rook" should "not go through obstacles" in {
    val board = Board(Map(
      Position.D4 -> Rook(White()),
      Position.D6 -> Pawn(White()),
      Position.D2 -> Pawn(White()),
      Position.C4 -> Pawn(White()),
      Position.G4 -> Pawn(White()),
      Position.B2 -> Pawn(White()),
      Position.G7 -> Pawn(White()),
      Position.B6 -> Pawn(White()),
      Position.F2 -> Pawn(White())
    ))

    val availablePositions = StandardVariantRules.availableMoves(board, Rook(White()), Position.D4, history).map(_._3).toSet

    assert(availablePositions === Set(
      Position.D3,
      Position.D5,
      Position.E4,
      Position.F4
    ))
  }

  "White Rook" should "attack obstacles, not King" in {
    val board = Board(Map(
      Position.D4 -> Rook(White()),
      Position.D6 -> Pawn(Black()),
      Position.D2 -> Pawn(Black()),
      Position.C4 -> Pawn(Black()),
      Position.G4 -> Pawn(Black()),
      Position.B2 -> King(Black()),
      Position.G7 -> Pawn(Black()),
      Position.B6 -> Pawn(Black()),
      Position.F2 -> Pawn(Black())
    ))

    val availablePositions = StandardVariantRules.availableMoves(board, Rook(White()), Position.D4, history).map(_._3).toSet

    assert(availablePositions === Set(
      Position.D3,
      Position.D6,
      Position.G4,
      Position.F4,
      Position.E4,
      Position.D5,
      Position.D2,
      Position.C4
    ))
  }

  // @todo test cases for each type of figures
  // - pawn
  //  - en passant
  //  - promotion
  // - castles
  // - king
  // - checkmate
  // - check
  // - stalemate
}