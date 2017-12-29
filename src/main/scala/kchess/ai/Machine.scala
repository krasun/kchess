package kchess.ai

import kchess.chess._

object Machine {

  private val PawnWhite = Array(
    Array(0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0),
    Array(5.0,  5.0,  5.0,  5.0,  5.0,  5.0,  5.0,  5.0),
    Array(1.0,  1.0,  2.0,  3.0,  3.0,  2.0,  1.0,  1.0),
    Array(0.5,  0.5,  1.0,  2.5,  2.5,  1.0,  0.5,  0.5),
    Array(0.0,  0.0,  0.0,  2.0,  2.0,  0.0,  0.0,  0.0),
    Array(0.5, -0.5, -1.0,  0.0,  0.0, -1.0, -0.5,  0.5),
    Array(0.5,  1.0, 1.0,  -2.0, -2.0,  1.0,  1.0,  0.5),
    Array(0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0)
  )

  private val PawnBlack = PawnWhite.reverse

  private val KnightWhite = Array(
    Array(-5.0, -4.0, -3.0, -3.0, -3.0, -3.0, -4.0, -5.0),
    Array(-4.0, -2.0,  0.0,  0.0,  0.0,  0.0, -2.0, -4.0),
    Array(-3.0,  0.0,  1.0,  1.5,  1.5,  1.0,  0.0, -3.0),
    Array(-3.0,  0.5,  1.5,  2.0,  2.0,  1.5,  0.5, -3.0),
    Array(-3.0,  0.0,  1.5,  2.0,  2.0,  1.5,  0.0, -3.0),
    Array(-3.0,  0.5,  1.0,  1.5,  1.5,  1.0,  0.5, -3.0),
    Array(-4.0, -2.0,  0.0,  0.5,  0.5,  0.0, -2.0, -4.0),
    Array(-5.0, -4.0, -3.0, -3.0, -3.0, -3.0, -4.0, -5.0)
  )

  private val KnightBlack = KnightWhite.reverse

  private val BishopWhite = Array(
    Array(-2.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -2.0),
    Array(-1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -1.0),
    Array(-1.0,  0.0,  0.5,  1.0,  1.0,  0.5,  0.0, -1.0),
    Array(-1.0,  0.5,  0.5,  1.0,  1.0,  0.5,  0.5, -1.0),
    Array(-1.0,  0.0,  1.0,  1.0,  1.0,  1.0,  0.0, -1.0),
    Array(-1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0, -1.0),
    Array(-1.0,  0.5,  0.0,  0.0,  0.0,  0.0,  0.5, -1.0),
    Array(-2.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -2.0)
  )

  private val BishopBlack = BishopWhite.reverse

  private val RookWhite = Array(
    Array(0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0),
    Array(0.5,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array(0.0,   0.0, 0.0,  0.5,  0.5,  0.0,  0.0,  0.0)
  )

  private val RookBlack = RookWhite.reverse

  private val QueenWhite = Array(
    Array(-2.0, -1.0, -1.0, -0.5, -0.5, -1.0, -1.0, -2.0),
    Array(-1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -1.0),
    Array(-1.0,  0.0,  0.5,  0.5,  0.5,  0.5,  0.0, -1.0),
    Array(-0.5,  0.0,  0.5,  0.5,  0.5,  0.5,  0.0, -0.5),
    Array(0.0,  0.0,  0.5,  0.5,  0.5,  0.5,  0.0, -0.5),
    Array(-1.0,  0.5,  0.5,  0.5,  0.5,  0.5,  0.0, -1.0),
    Array(-1.0,  0.0,  0.5,  0.0,  0.0,  0.0,  0.0, -1.0),
    Array(-2.0, -1.0, -1.0, -0.5, -0.5, -1.0, -1.0, -2.0)
  )

  private val QueenBlack = QueenWhite.reverse

  private val KingWhite = Array(
    Array(-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0),
    Array(-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0),
    Array(-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0),
    Array(-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0),
    Array(-2.0, -3.0, -3.0, -4.0, -4.0, -3.0, -3.0, -2.0),
    Array(-1.0, -2.0, -2.0, -2.0, -2.0, -2.0, -2.0, -1.0),
    Array(2.0,  2.0,  0.0,  0.0,  0.0,  0.0,  2.0,  2.0 ),
    Array(2.0,  3.0,  1.0,  0.0,  0.0,  1.0,  3.0,  2.0 )
  )

  private val KingBlack = KingWhite.reverse

  // https://medium.freecodecamp.org/simple-chess-ai-step-by-step-1d55a9266977

  // @todo alpha-beta pruning
  // @todo evaluation table/position
  // @todo how to evaluate promotion, stalemate
  // @todo add cache?

  def move(game: Game, machinePlayer: Player): (Position, Position) = {
    val machineColor = game.colorOf(machinePlayer)

    minimaxRoot(game, machineColor)
  }

  private def minimaxRoot(game: Game, color: Color, maximize: Boolean = true, depth: Int = 3): (Position, Position) = {
    val valuedMoves =
      for ((_, from, to, _) <- StandardVariantRules.availableMoves(game.board, color, game.history).par)
        // @todo how to solve .get case?
        yield game.applyMove(from, to).map(updatedGame => ((from, to), minimax(updatedGame, color.opposite, !maximize, depth - 1))).get

    val ((bestFrom, bestTo), _) = valuedMoves.maxBy(_._2)

    (bestFrom, bestTo)
  }

  private def minimax(game: Game, color: Color, maximize: Boolean, depth: Int): Double = {
    if (depth == 0 || StandardVariantRules.availableMoves(game.board, color, game.history).isEmpty) -gameValue(game, color)
    else {
      val values =
        for ((_, from, to, _) <- StandardVariantRules.availableMoves(game.board, color, game.history))
          // @todo how to solve .get case?
          yield game.applyMove(from, to).map(minimax(_, color.opposite, !maximize, depth - 1)).get

      if (maximize) values.max
      else values.min
    }
  }

  /** Game value should be positive for interesting color */
  private def gameValue(game: Game, color: Color): Double = {
    val checkmate = StandardVariantRules.isCheckmate(game.board, game.history) match {
      case Some(White()) => Double.NegativeInfinity
      case Some(Black()) => Double.PositiveInfinity
      case None => 0
    }

    val material = boardValue(game.board)

    material + checkmate
  }

  private def boardValue(board: Board): Double = board.pieces.map {
    case (position, piece) => pieceValue(piece, position)
  }.sum

  private def pieceValue(piece: Piece, position: Position): Double = {
    val x = 8 - position.row
    val y = position.column - 1

    piece match {
      case Pawn(White()) => 10 * PawnWhite(x)(y)
      case Bishop(White()) => 30 * BishopWhite(x)(y)
      case Knight(White()) => 30 * KnightWhite(x)(y)
      case Rook(White()) => 50 * RookWhite(x)(y)
      case Queen(White()) => 90 * QueenWhite(x)(y)
      case King(White()) => 900 * KingWhite(x)(y)

      case Pawn(Black()) => -10 * PawnBlack(x)(y)
      case Bishop(Black()) => -30 * BishopBlack(x)(y)
      case Knight(Black()) => -30 * KingBlack(x)(y)
      case Rook(Black()) => -50 * RookBlack(x)(y)
      case Queen(Black()) => -90 * QueenBlack(x)(y)
      case King(Black()) => -900 * KingBlack(x)(y)
    }
  }
}