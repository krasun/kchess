package kchess.app

import kchess.ai.Machine
import kchess.chess._

import scala.annotation.tailrec
import scala.util.{Failure, Success}

object App {

  def main(args: Array[String]): Unit = {
    // could be received from arguments or loaded from database
    val whitePlayerName = "Blue Ocean"
    val blackPlayerName = "Developer"

    // could be loaded from database or initialized from scratch
    val game = Game.standard(Player(whitePlayerName), Player(blackPlayerName))

    // @todo draw by repeat of positions
    // @todo allow to draw, resign
    // @todo notification for check
    // @todo notification for resign, checkmate and so on
    // @todo play by network
    // @todo how to ask for draw, how to resign in this architecture of game loop?
    // @todo how to ask for pawn promotion
    // @todo render history in PGN notation
    // @todo start game with FEN or PGN

    // @todo cover chessapi with tests for %100

    val machinePlayer = Player(blackPlayerName)
    withMachineLoop(game, machinePlayer)

//    sameComputerLoop(game)
  }

  def withMachineLoop(game: Game, machinePlayer: Player): Unit = {
    game.state match {

      case ExpectsMove(player, color) =>

        GameView.renderBoard(game.board)

        if (player == machinePlayer) {
          GameView.askMachineToMove(player, color)

          val (from, to) = Machine.move(game, machinePlayer)
          GameView.machineMoves((from, to))

          game.applyMove(from ,to) match {
            case Success(updatedGame) => withMachineLoop(updatedGame, machinePlayer)

            case Failure(exception) =>
              GameView.renderFailureMessage(exception.getMessage)
              withMachineLoop(game, machinePlayer)
          }
        } else {

          GameView.askToMove(player, color) match {
            // move has been parsed successfully
            case Success((from, to)) =>
              game.applyMove(from ,to) match {
                case Success(updatedGame) => withMachineLoop(updatedGame, machinePlayer)

                case Failure(exception) =>
                  GameView.renderFailureMessage(exception.getMessage)
                  withMachineLoop(game, machinePlayer)
              }

            case Failure(exception) => exception match {
              case QuitGameException(message) =>
                GameView.renderAbortedGame(message)
              case _ =>
                GameView.renderFailureMessage(exception.getMessage)
                withMachineLoop(game, machinePlayer)
            }
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

  @tailrec
  def sameComputerLoop(game: Game): Unit = {
    game.state match {

      case ExpectsMove(player, color) =>

        GameView.renderBoard(game.board)
        GameView.askToMove(player, color) match {
          // move has been parsed successfully
          case Success((from, to)) =>
            game.applyMove(from ,to) match {
              case Success(updatedGame) => sameComputerLoop(updatedGame)

              case Failure(exception) =>
                GameView.renderFailureMessage(exception.getMessage)
                sameComputerLoop(game)
            }

          case Failure(exception) => exception match {
            case QuitGameException(message) =>
              GameView.renderAbortedGame(message)
            case _ =>
              GameView.renderFailureMessage(exception.getMessage)
              sameComputerLoop(game)
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

