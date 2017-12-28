package kchess.app

import kchess.ai.Machine
import kchess.chess._

import scala.annotation.tailrec
import scala.util.{Failure, Success}

object App {

  // @todo draw by repeat of positions
  // @todo allow to draw, resign
  // @todo notification for check
  // @todo notification for resign, checkmate and so on
  // @todo play by network
  // @todo how to ask for draw, how to resign in this architecture of game loop?
  // @todo how to ask for pawn promotion
  // @todo render history in PGN notation
  // @todo start game with FEN or PGN
  // @todo implement https://en.wikipedia.org/wiki/Fifty-move_rule

  def main(args: Array[String]): Unit = {
    // could be received from arguments or loaded from database
    val whitePlayerName = "Blue Ocean"
    val blackPlayerName = "Developer"

    // could be loaded from database or initialized from scratch
    // to play with machine set isMachine = true for desired player
    val game = Game.standard(Player(whitePlayerName), Player(blackPlayerName))

    GameLoop.loop(game)
  }
}