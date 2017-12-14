import kchess._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object App {

  def main(args: Array[String]): Unit = {
    // could be received from arguments or loaded from database
    val whitePlayerName = "Karpov"
    val blackPlayerName = "Kasparov"

    // could be loaded from database or initialized from scratch
    val game = Game.standard(Player(whitePlayerName), Player(blackPlayerName))

    loop(game)
  }

  @tailrec
  def loop(game: Game): Unit = {
    game.state match {
      case ExpectsMove(player, color) =>

        GameView.renderBoard(game.board)

        // @todo how to ask for draw, how to resign in this architecture of game loop?
        // @todo how to ask for pawn promotion

        GameView.askToMove(player, color) match {
          // move has been parsed successfully
          case Success((from, to)) =>
            game.applyMove(from ,to) match {
              case Success(updatedGame) => {
                loop(updatedGame)
              }
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

      case GameOver() => GameView.renderFinishedGame(game)
    }
  }
}

