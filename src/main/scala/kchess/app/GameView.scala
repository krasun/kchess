package kchess.app

import kchess.chess._

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object GameView {
  def askMachineToMove(player: Player, color: Color): Unit = {
    val name = player.name
    val colorAsText = color.toString.toLowerCase
    printf(s"$name ($colorAsText), moves: ")
  }

  def machineMoves(move: (Position, Position)): Unit = {
    val (from, to) = move
    print(s"$from $to")
    println()
  }

  def askGameType(): Unit = {
    println()
    println("Game types:")

    println("  1. Human vs human (same machine);")
    println("  2. Play with the machine;")
    println("  3. Create network game;")
    println("  4. Join network game.")

    println()
    print("Enter game type:")
  }

  val QuitCommand = "quit"

  def renderFailureMessage(message: String): Unit = println(s"\033[31m$message")

  def renderAbortedGame(message: String): Unit = renderFailureMessage(message)

  def renderFinishedGame(game: Game): Unit = ???

  def askToMove(player: Player, color: Color): Try[(Position, Position)] = {
    val name = player.name
    val colorAsText = color.toString.toLowerCase
    printf(s"$name ($colorAsText), moves (type 'quit' to abort game): ")

    val input = StdIn.readLine()

    if (input == QuitCommand) Failure(QuitGameException("Game is quitted."))
    else parseMove(input) match {
      case Success(move) => Success(move)
      case Failure(exception) => Failure(exception)
    }
  }

  def pieceToChar(piece: Piece): Char = piece match {
    case Pawn(White()) => '♙'
    case Knight(White()) => '♘'
    case Bishop(White()) => '♗'
    case Rook(White()) => '♖'
    case Queen(White()) => '♕'
    case King(White()) => '♔'

    case Pawn(Black()) => '♟'
    case Knight(Black()) => '♞'
    case Bishop(Black()) => '♝'
    case Rook(Black()) => '♜'
    case Queen(Black()) => '♛'
    case King(Black()) => '♚'
  }

  def renderStalemateMessage(game: Game): Unit = {
    renderFailureMessage("Stalemate!")
  }

  def renderCheckmateMessage(winner: Player, loser: Player, winnerColor: Color, loserColor: Color): Unit = {
    val winnerName = winner.name
    val loserName = loser.name
    renderFailureMessage(s"Checkmate! $winnerName ($winnerColor) is the winner and $loserName ($loserColor) is the loser!")
  }

  def renderBoard(board: Board): Unit = {
    println()

    for (row <- 1 to 8) {
      print(" " + (8 - row + 1) + " ")
      for (colum <- 1 to 8) {
        val r = 8 - row + 1

        val pieceChar = board.at(Position(r, colum)).map(pieceToChar).getOrElse(' ')
        val isWhite = (row + colum) % 2 == 0
        if (isWhite) {
          print("\033[49m " + pieceChar + " \033[0m")
        } else {
          print("\033[43m " + pieceChar + " \033[0m")
        }
      }
      println()
    }

    print("   ")
    for (i <- 1 to 8) {
      print(" " + ('a' + i - 1).toChar  + " ")
    }

    println()
    println()
  }

  private val MovePattern = """([a-h])([1-8]) ([a-h])([1-8])""".r

  private def parseMove(move: String): Try[(Position, Position)] = move.toLowerCase match {
    case MovePattern(fromFile, fromRank, toFile, toRank) =>
      Success((Position(fromFile.head, fromRank.head), Position(toFile.head, toRank.head)))
    case _ =>
      Failure(new Exception(s"Invalid format of move! Expected move format is '$MovePattern', e.g. 'e2 e4'."))
  }
}
