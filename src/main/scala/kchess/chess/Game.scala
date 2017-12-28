package kchess.chess

import scala.util.{Try, Success, Failure}

case class Game(board: Board, whitePlayer: Player, blackPlayer: Player, history: History) {
  require(whitePlayer.name != blackPlayer.name, "Player names should be different.")

  val colorOf = Map(
    whitePlayer -> White(),
    blackPlayer -> Black()
  )

  val playerOf = Map[Color, Player](
    White() -> whitePlayer,
    Black() -> blackPlayer
  )

  val currentPlayer: Player = playerOf(history.nextColor)

  val state: GameState = {
    val checkmate = StandardVariantRules.isCheckmate(board, history)
    if (checkmate.nonEmpty) {
      val color = checkmate.get
      Checkmate(playerOf(color.opposite), playerOf(color), color.opposite, color)
    } else if (StandardVariantRules.isStalemate(board, history)) Stalemate()
    else ExpectsMove(currentPlayer, colorOf(currentPlayer))
  }

  def applyMove(from: Position, to: Position): Try[Game] = StandardVariantRules.checkMove(board, from, to, history) match {
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