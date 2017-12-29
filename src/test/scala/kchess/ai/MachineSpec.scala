package kchess.ai

import kchess.chess._
import org.scalatest.FlatSpec

import scala.util.Success

class MachineSpec extends FlatSpec {

  val humanPlayer = Player("Human")
  val machinePlayer = Player("Machine")

  "AI" should "capture pawn if there is no problem for it" in {
    val board = Board(Map(Position.A2 -> Pawn(White()), Position.E5 -> Pawn(White()), Position.D6 -> Pawn(Black())))
    val game = Game(board, humanPlayer, machinePlayer, History(List(Move(Pawn(White()), Position.E2, Position.E4))))

    val (from, to) = Machine.move(game, machinePlayer)

    assert((from, to) === (Position.D6, Position.E5))
  }

  "AI" should "move to center at start" in {
    val Success(updatedGame) = Game.standard(humanPlayer, machinePlayer).applyMove(Position.E2, Position.E4)

    val (from, to) = Machine.move(updatedGame, machinePlayer)

    assert((from, to) !== (Position.A7, Position.A6))
    assert((from, to) !== (Position.A7, Position.A5))
    assert((from, to) !== (Position.B7, Position.B6))
    assert((from, to) !== (Position.B7, Position.B5))
    assert((from, to) !== (Position.H7, Position.H6))
    assert((from, to) !== (Position.H7, Position.H5))
    assert((from, to) !== (Position.G7, Position.G6))
    assert((from, to) !== (Position.G7, Position.G5))
  }

  "AI" should "not allow to attack pawn without any reasons to do so" in {
    val board = Board(Map(Position.A2 -> Pawn(White()), Position.F4 -> Pawn(White()), Position.E5 -> Pawn(White()), Position.D7 -> Pawn(Black()), Position.A7 -> Pawn(Black())))
    val game = Game(board, humanPlayer, machinePlayer, History(List(Move(Pawn(White()), Position.E2, Position.E4))))

    val (from, to) = Machine.move(game, machinePlayer)

    assert((from, to) === (Position.A7, Position.A6))
  }

  "AI" should "not capture pawn by Knight, center is in high priority" in {
    val board = Board(Map(Position.A2 -> Pawn(White()), Position.E5 -> Pawn(White()), Position.C6 -> Knight(Black())))
    val game = Game(board, humanPlayer, machinePlayer, History(List(Move(Pawn(White()), Position.E2, Position.E4))))

    val (from, to) = Machine.move(game, machinePlayer)

    assert((from, to) === (Position.C6, Position.E7))
  }

  "AI" should "capture pawn if there is no problem for it (edge case - kills all White pieces)" in {
    val board = Board(Map(Position.E4 -> Pawn(White()), Position.D5 -> Pawn(Black())))
    val game = Game(board, humanPlayer, machinePlayer, History(List(Move(Pawn(White()), Position.E2, Position.E4))))

    val (from, to) = Machine.move(game, machinePlayer)

    assert((from, to) === (Position.D5, Position.E4))
  }

  "AI" should "prefer checkmate instead of material" in {
    val board = Board(Map(Position.G8 -> King(Black()), Position.A1 -> King(White()), Position.H3 -> Queen(White()), Position.C4 -> Rook(Black()), Position.B3 -> Rook(Black())))
    val game = Game(board, humanPlayer, machinePlayer, History(List(Move(King(White()), Position.B1, Position.A1))))

    val (from, to) = Machine.move(game, machinePlayer)

    assert((from, to) === (Position.C4, Position.A4))
  }

  "AI" should "avoid checkmate" in {
    val board = Board(Map(
      Position.A8 -> King(Black()),
      Position.A7 -> Pawn(Black()),
      Position.B7 -> Pawn(Black()),
      Position.D8 -> Rook(Black()),

      Position.C5 -> Queen(White()),
      Position.D5 -> Rook(White()),
      Position.H1 -> King(White())
    ))
    val game = Game(board, humanPlayer, machinePlayer, History(List(Move(King(White()), Position.H7, Position.H8))))

    val (from, to) = Machine.move(game, machinePlayer)

    assert((from, to) === (Position.D8, Position.E8))
  }
}