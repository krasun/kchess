package kchess.app

import kchess.chess._

import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}

object GameView {

  val HumanGameType = 1
  val MachineGameType = 2
  val NetworkServerGameType = 3
  val NetworkClientGameType = 4

  val QuitCommand = "quit"

  // @todo draw by repeat of positions
  // @todo allow to draw, resign
  // @todo notification for check
  // @todo notification for resign, checkmate and so on
  // @todo play by network
  // @todo how to ask for draw, how to resign in this architecture of game loop?
  // @todo how to ask for pawn promotion
  // @todo render history in PGN notation
  // @todo start game with FEN or PGN
  // @todo implement https://en.wikipedia.org/wiki/Fifty-move_rule

  def run(): Unit = {
    GameView.askGameType() match {
      case Success(gameType) => gameType match {
        case GameView.HumanGameType =>
          // could be received from arguments or loaded from database
          val whitePlayerName = "Player1"
          val blackPlayerName = "Player2"

          // could be loaded from database or initialized from scratch
          val game = Game.standard(Player(whitePlayerName), Player(blackPlayerName))

          GameLoop.loop(game, GameLoop.humanMove)

        case GameView.MachineGameType =>
          // could be received from arguments or loaded from database
          val whitePlayerName = "PlayerName"
          val blackPlayerName = "MachineName"

          val whitePlayer = Player(whitePlayerName)
          val blackPlayer = Player(blackPlayerName)

          // could be loaded from database or initialized from scratch
          val game = Game.standard(Player(whitePlayerName), Player(blackPlayerName))

          val machinePlayer = blackPlayer

          GameLoop.loop(game, GameLoop.configureMachineMove(game, machinePlayer))

        case GameView.NetworkServerGameType => ???
        case GameView.NetworkClientGameType => ???
      }
      case Failure(exception) => GameView.renderFailureMessage(exception.getMessage)
    }
  }

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

  def askGameType(): Try[Int] = {
    renderMenu()

    var input = StdIn.readLine()
    while (input != QuitCommand && !(input.matches("\\d+") && 1 <= input.toInt && input.toInt <= 4)) {

      renderMenu()
      input = StdIn.readLine()

      println(input)
    }

    if (input == QuitCommand) Failure(QuitGameException("Game is quitted."))
    else Success(input.toInt)
  }

  def renderMenu(): Unit = {
    println()
    println("Game types:")
    println()
    println("  1. Human vs human (same machine);")
    println("  2. Play with the machine;")
    println("  3. Create network game;")
    println("  4. Join network game.")

    println()
    print("Enter game type (type 'quit' to quit): ")
  }

  def renderFailureMessage(message: String): Unit = println(s"\u001b[31m$message")

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
          print("\u001b[49m " + pieceChar + " \u001b[0m")
        } else {
          print("\u001b[43m " + pieceChar + " \u001b[0m")
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