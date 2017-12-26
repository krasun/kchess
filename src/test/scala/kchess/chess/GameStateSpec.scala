package kchess.chess

import org.scalatest.FlatSpec

class GameStateSpec extends FlatSpec {
  "Game state" should "be over for checkmate" in {
    assert(Checkmate(Player("Winner"), Player("Loser"), White(), Black()).isGameOver)
  }

  "Game state" should "be over for stalemate" in {
    assert(Stalemate().isGameOver)
  }

  "Game state" should "not be over if expects move" in {
    assert(!ExpectsMove(Player("White"), White()).isGameOver)
  }

  "Game state" should "not be over if expects promotion" in {
    assert(!ExpectsPromotion(Player("White"), Position.H7, Position.H8).isGameOver)
  }
}