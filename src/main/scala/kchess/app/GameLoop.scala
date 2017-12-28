package kchess.app

import kchess.ai.Machine
import kchess.chess._

import scala.annotation.tailrec
import scala.util.{Failure, Success}

object GameLoop {

  @tailrec
   def loop(game: Game): Unit = {
    game.state match {

      case ExpectsMove(player, color) =>

        GameView.renderBoard(game.board)

        // @todo implement game loop with AI machine
//        if (player.isMachine) {
//          GameView.askMachineToMove(player, color)
//
//          val (from, to) = Machine.move(game, player)
//          GameView.machineMoves((from, to))
//
//          game.applyMove(from ,to) match {
//            case Success(updatedGame) => loop(updatedGame)
//
//            case Failure(exception) =>
//              GameView.renderFailureMessage(exception.getMessage)
//              loop(game)
//          }
//        } else {

          GameView.askToMove(player, color) match {
            // move has been parsed successfully
            case Success((from, to)) =>
              game.applyMove(from ,to) match {
                case Success(updatedGame) => loop(updatedGame)

                case Failure(exception) =>
                  GameView.renderFailureMessage(exception.getMessage)
                  loop(game)
              }

            case Failure(exception) => exception match {
              case QuitGameException(message) =>
                GameView.renderAbortedGame(message)
              case _ =>
                GameView.renderFailureMessage(exception.getMessage)
                loop(game)
            }
//          }
        }
      case Stalemate() =>
        GameView.renderBoard(game.board)
        GameView.renderStalemateMessage(game)

      case Checkmate(winner, loser, winnerColor, loserColor) =>
        GameView.renderBoard(game.board)
        GameView.renderCheckmateMessage(winner, loser, winnerColor, loserColor)
    }
  }
}