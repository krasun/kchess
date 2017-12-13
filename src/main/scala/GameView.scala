import kchess._

import scala.io.StdIn

object GameView {

  val QuitCommand = "quit"

  def renderFailureMessage(message: String): Unit = println(s"\033[31m$message")

  def renderAbortedGame(): Unit = renderFailureMessage("Game is quitted.")

  def renderFinishedGame(game: Game): Unit = ???

  def askToMove(player: Player, color: Color.Value): Option[String] = {
    val name = player.name
    val colorAsText = color.toString.toLowerCase
    printf(s"$name ($colorAsText), moves (type 'quit' to abort game): ")

    val move = StdIn.readLine()
    if (move == QuitCommand) {
      None
    } else {
      Some(move)
    }
  }

  def pieceToChar(piece: Piece): Char = {
    piece match {
      // white
      case Pawn(Color.White) => '♙'
      case Knight(Color.White) => '♘'
      case Bishop(Color.White) => '♗'
      case Rook(Color.White) => '♖'
      case Queen(Color.White) => '♕'
      case King(Color.White) => '♔'
      // black
      case Pawn(Color.Black) => '♟'
      case Knight(Color.Black) => '♞'
      case Bishop(Color.Black) => '♝'
      case Rook(Color.Black) => '♜'
      case Queen(Color.Black) => '♛'
      case King(Color.Black) => '♚'
    }
  }

  def renderBoard(board: Board): Unit = {
    println()
    for (row <- 1 to 8) {
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
    println()
  }
}
