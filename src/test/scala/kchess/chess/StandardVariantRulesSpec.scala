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

  // @todo test cases for each type of figures
  // - pawn
  //  - en passant
  //  - promotion
  // - castles
  // - checkmate
  // - check
  // - stalemate
}