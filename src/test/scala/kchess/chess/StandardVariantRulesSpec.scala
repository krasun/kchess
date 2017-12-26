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

  // @todo test cases for each type of figures
  // - pawn
  //  - en passant
  // - knight
  // - bishop
  // - castles
  // - checkmate
  // - check
  // - stalemate
}