package kchess

import scala.util.{Try, Success, Failure}

case class Game(board: Board, whitePlayer: Player, blackPlayer: Player) {

  val colorOf = Map(
    whitePlayer -> Color.White,
    blackPlayer -> Color.Black
  )

  def playerOf = colorOf.map(_.swap)

  def state: GameState = ExpectsMove(currentPlayer, colorOf(currentPlayer))

  def currentPlayer: Player = playerOf(board.currentColor)

  def isOver: Boolean = state match {
    case GameOver() => true
    case _ => false
  }

  def applyMove(move: String): Try[Game] = parseMove(move) match {
    case Success((from, to)) =>
      board.applyMove(from, to) match {
        case Success(updatedBoard) => Success(Game(updatedBoard, whitePlayer, blackPlayer))
        case Failure(exception) => Failure(exception)
      }
    case Failure(exception) => Failure(exception)
  }

  private val MovePattern = """([a-h])([1-8]) ([a-h])([1-8])""".r

  private def parseMove(move: String): Try[(Position, Position)] = move.toLowerCase match {
      case MovePattern(fromFile, fromRank, toFile, toRank) =>
        Success((Position(fromFile.head, fromRank.head), Position(toFile.head, toRank.head)))
      case _ =>
        Failure(new Exception(s"Invalid format of move! Expected move format is '$MovePattern', e.g. 'e2 e4'."))
  }
}

object Game {
  def standard(whitePlayer: Player, blackPlayer: Player): Game = Game(Board.standard, whitePlayer, blackPlayer)
}
