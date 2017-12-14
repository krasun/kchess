package kchess

import scala.util.{Try, Success, Failure}

case class Game(board: Board, whitePlayer: Player, blackPlayer: Player, history: History) {

  val colorOf = Map(
    whitePlayer -> Color.White,
    blackPlayer -> Color.Black
  )

  val playerOf = colorOf.map(_.swap)

  val currentPlayer: Player = playerOf(history.nextColor)

  val state: GameState = ExpectsMove(currentPlayer, colorOf(currentPlayer))

  def isOver: Boolean = state match {
    case GameOver() => true
    case _ => false
  }

  def applyMove(from: Position, to: Position): Try[Game] = Rules.checkMove(board, from, to, history) match {
    case Success(CheckResult(movedPiece, capturesAt)) =>

      val dropAt = capturesAt.getOrElse(to)
      board.drop(dropAt).move(from, to) match {
        case Success(updatedBoard) => Success(Game(updatedBoard, whitePlayer, blackPlayer, history :+ Move(movedPiece, from, to)))
        case Failure(exception) => Failure(exception)
      }

    case Failure(exception) => Failure(exception)
  }
}

object Game {
  def standard(whitePlayer: Player, blackPlayer: Player): Game = Game(Board.standard, whitePlayer, blackPlayer, History())
}
