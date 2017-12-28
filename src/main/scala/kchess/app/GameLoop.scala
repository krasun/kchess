package kchess.app

import kchess.ai.Machine
import kchess.chess._

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

object GameLoop {
  def configureMachineMove(game: Game, machinePlayer: Player): (Game, Player, Color) => Try[(Position, Position)] = {

    def machineMove(game: Game, player: Player, color: Color): Try[(Position, Position)] = {
      if (player == machinePlayer) {
        GameView.askMachineToMove(player, color)
        val (from, to) = Machine.move(game, player)
        GameView.machineMoves((from, to))

        Success((from, to))
      } else {
        GameView.askToMove(player, color)
      }
    }

    machineMove
  }

  def humanMove(game: Game, player: Player, color: Color): Try[(Position, Position)] = GameView.askToMove(player, color)

  @tailrec
   def loop(game: Game, playerMove: (Game, Player, Color) => Try[(Position, Position)]): Unit = {
    game.state match {

      case ExpectsMove(player, color) =>
        GameView.renderBoard(game.board)

        playerMove(game, player, color) match {
          case Success((from, to)) =>
            game.applyMove(from ,to) match {
              case Success(updatedGame) => loop(updatedGame, playerMove)

              case Failure(exception) =>
                GameView.renderFailureMessage(exception.getMessage)
                loop(game, playerMove)
            }

          case Failure(exception) => exception match {
            case QuitGameException(message) =>
              GameView.renderAbortedGame(message)
            case _ =>
              GameView.renderFailureMessage(exception.getMessage)
              loop(game, playerMove)
          }
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