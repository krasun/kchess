package app

import kchess._

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
    // @todo play with AI
    // @todo play by network
    // @todo how to ask for draw, how to resign in this architecture of game loop?
    // @todo how to ask for pawn promotion
    // @todo render history in PGN notation
    // @todo start game with FEN or PGN
    // @todo network game
    // @todo cover with tests

    loop(game)
  }

  @tailrec
  def loop(game: Game): Unit = {
    game.state match {

      case ExpectsMove(player, color) =>

        GameView.renderBoard(game.board)
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

