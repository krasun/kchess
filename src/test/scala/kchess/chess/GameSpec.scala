package kchess.chess

import org.scalatest.FlatSpec

class GameSpec extends FlatSpec {

  val whitePlayer = Player("Hawk")
  val blackPlayer = Player("Dove")
  val newGame = Game.standard(whitePlayer, blackPlayer)

  "Standard game" should "be new game with standard board" in {
    val expectedGame = Game(Board.standard, whitePlayer, blackPlayer, History())

    val actualGame = Game.standard(whitePlayer, blackPlayer)

    assert(actualGame === expectedGame)
  }

  "Game" should "not accept players with the same name" in {
    assertThrows[IllegalArgumentException] {
      Game(Board.standard, Player("test"), Player("test"), History())
    }
  }

  "Game" should "status" in {
    // @todo test promotion, draw?
  }

  "Game" should "have checkmate state" in {
    val board = Board(Map(
      Position.A1 -> King(White()),
      Position.C4 -> Bishop(Black()),
      Position.B4 -> Rook(Black()),
      Position.A4 -> Rook(Black())
    ))

    val game = Game(board, whitePlayer, blackPlayer, History(List()))

    assert(game.state === Checkmate(blackPlayer, whitePlayer, Black(), White()))
  }

  "Game" should "have stalemate state" in {
    val board = Board(Map(
      Position.A1 -> King(White()),
      Position.C4 -> Bishop(Black()),
      Position.B4 -> Rook(Black())
    ))

    val game = Game(board, whitePlayer, blackPlayer, History(List()))

    assert(game.state === Stalemate())
  }

  "Game" should "assign white color to white player" in {
    assert(newGame.colorOf(whitePlayer) === White())
    assert(newGame.playerOf(White()) === whitePlayer)
  }

  "Game" should "assign black color to black player" in {
    assert(newGame.colorOf(blackPlayer) === Black())
    assert(newGame.playerOf(Black()) === blackPlayer)
  }

  "Game" should "expect move of white player at start" in {
    assert(newGame.state === ExpectsMove(whitePlayer, White()))
  }

  "Current player" should "be white player at start" in {
    assert(newGame.currentPlayer === whitePlayer)
  }

  "Current player" should "be black player after move of white player" in {
    val nextGame = newGame.applyMove(Position.E2, Position.E2.up().get).get

    assert(nextGame.currentPlayer === blackPlayer)
  }

  "Game" should "expect move of black player after move of white player" in {
    val nextGame = newGame.applyMove(Position.E2, Position.E2.up().get).get

    assert(nextGame.state === ExpectsMove(blackPlayer, Black()))
  }
}