package kchess

import scala.util.{Failure, Success, Try}

object Rules {

  case class PossibleMove(from: Position, to: Position, selectedPiece: Piece, capturesPiece: Option[Piece], capturesAt: Option[Position])

  private val BishopDirections = List(
    (+1, +1),
    (+1, -1),
    (-1, +1),
    (-1, -1)
  )

  private val RookDirections = List(
    (+1, 0),
    (-1, 0),
    (0, +1),
    (0, -1)
  )

  private val QueenDirections = RookDirections ++ BishopDirections

  private val KingDirections = QueenDirections

  def checkMove(board: Board, from: Position, to: Position, history: History): Try[CheckResult] = board.at(from) match {
    case Some(selectedPiece) =>
      if (selectedPiece.color != history.nextColor) {
        val expectsColor = history.nextColor
        Failure(new Exception(s"Expects move of $expectsColor piece!"))
      } else {

        val kingCaptureSafeMoves = captureKingSafeFilter(kingUnsafePossibleMoves(board, selectedPiece, from, history))
        val withoutOpenForCheckMoves = withoutChecksKingFilter(board, history, kingCaptureSafeMoves)

        withoutOpenForCheckMoves.find(_.to == to) match {
          case Some(possibleMove) =>

            Success(CheckResult(selectedPiece, possibleMove.capturesAt))

          case None =>
            val message =
              if (withoutOpenForCheckMoves.isEmpty) "Invalid move!"
              else {
                val possiblePosstionsAsText = withoutOpenForCheckMoves.map(_.to).mkString(", ")
                s"Invalid move! Available positions: $possiblePosstionsAsText."
              }
            Failure(new Exception(message))
        }
      }
    case None =>
      val fromAsString = from.toString
      Failure(new Exception(s"There is no piece at $fromAsString."))
  }

  private def applyMove(board: Board, history: History, possibleMove: PossibleMove): Try[(Board, History)] = {
    val dropAt = possibleMove.capturesAt.getOrElse(possibleMove.to)
    board.drop(dropAt).move(possibleMove.from, possibleMove.to) match {
      case Success(updatedBoard) => Success((updatedBoard, history :+ Move(possibleMove.selectedPiece, possibleMove.from, possibleMove.to)))
      case Failure(exception) => Failure(exception)
    }
  }

  def isKingAtCheck(board: Board, color: Color.Value, history: History): Boolean = {
    val kingPosition = board.kingPosition(color)

    // find at least one which can attack king
    board.ofColor(color.opposite).exists {
      case (enemyPosition, enemyPiece) => !enemyPiece.isInstanceOf[King] && attacksKing(board, enemyPiece, enemyPosition, kingPosition, history)
    }
  }

  private def isKingAtCheckmate(board: Board, color: Color.Value, history: History): Boolean = {
    ???
  }


  private def isStalemate(board: Board, color: Color.Value, history: History): Boolean = {
    ???
  }

  // it also filters moves if king is under check
  private def withoutChecksKingFilter(board: Board, history: History, possibleMoves: List[PossibleMove]): List[PossibleMove] = {
    def filterMoves(possibleMove: PossibleMove): Boolean = applyMove(board, history, possibleMove) match {
      case Success((updatedBoard, updatedHistory)) => !isKingAtCheck(updatedBoard, possibleMove.selectedPiece.color, updatedHistory)
      case _ => false
    }

    possibleMoves.filter(filterMoves)
  }

  private def attacksKing(board: Board, enemyPiece: Piece, enemyPosition: Position, kingPosition: Position, history: History): Boolean = {
    kingUnsafePossibleMoves(board, enemyPiece, enemyPosition, history).exists(_.to == kingPosition)
  }

  private def captureKingSafeFilter(possibleMoves: List[PossibleMove]): List[PossibleMove] = possibleMoves.filter(m => m.capturesPiece.isEmpty || !m.capturesPiece.get.isInstanceOf[King])

  // king unsafe moves means that any king after move can be under check or under checkmate
  // and also it means that piece can capture a king
  private def kingUnsafePossibleMoves(board: Board, selectedPiece: Piece, from: Position, history: History): List[PossibleMove] = {
    selectedPiece match {
      case Pawn(_) => kingUnsafePossibleMovesByPawn(board, selectedPiece, from, history)
      case Knight(_) => kingUnsafePossibleMovesByKnight(board, selectedPiece, from)
      case Bishop(_) => kingUnsafePossibleMovesByBishop(board, selectedPiece, from)
      case Rook(_) => kingUnsafePossibleMovesByRook(board, selectedPiece, from)
      case Queen(_) => kingUnsafePossibleMovesByQueen(board, selectedPiece, from)
      case King(_) => kingUnsafePossibleMovesByKing(board, selectedPiece, from)
    }
  }

  private def kingUnsafePossibleMovesByPawn(board: Board, selectedPiece: Piece, from: Position, history: History): List[PossibleMove] = {
    val (initialRow, direction) = selectedPiece.color match {
      case Color.White => (2, +1)
      case Color.Black => (7, -1)
    }

    val simpleStep: Position => (Boolean, Option[Piece], Option[Position]) = (p: Position) => (board.at(p).isEmpty, None, None)
    val doubleStep: Position => (Boolean, Option[Piece], Option[Position]) = (p: Position) => ((p.row - (2 * direction)) == initialRow && board.at(p).isEmpty && (p + (-direction, 0)).exists(board.at(_).isEmpty), None, None)
    val capturesPiece: Position => (Boolean, Option[Piece], Option[Position]) = (p: Position) => (board.at(p).isDefined && board.at(p).get.color != selectedPiece.color, board.at(p), Some(p))
    val enPassant: Position => (Boolean, Option[Piece], Option[Position]) = (p: Position) => history.lastOption match {
      case Some(Move(Pawn(color), _, Position(lastToRow, lastToColumn))) if color != selectedPiece.color && from.row == lastToRow && p.column == lastToColumn => (true, Some(Pawn(selectedPiece.color.opposite)), Some(Position(lastToRow, lastToColumn)))
      case _ => (false, None, None)
    }

    // @todo implement promotion

    val validations = List(
      (from + (direction, 0), simpleStep),
      (from + (2 * direction, 0), doubleStep),
      (from + (direction, +1), capturesPiece),
      (from + (direction, -1), capturesPiece),
      (from + (direction, +1), enPassant),
      (from + (direction, -1), enPassant)
    )

    for ((possiblePosition, validate) <- validations; to <- possiblePosition; (isValid, capturesPiece, capturesAt) = validate(to) if isValid)
      yield PossibleMove(from, to, selectedPiece, capturesPiece, capturesAt)
  }


  private def kingUnsafePossibleMovesByKnight(board: Board, selectedPiece: Piece, from: Position): List[PossibleMove] = {
    val possiblePositions = List(
      from.up().flatMap(_.upRight()),
      from.up().flatMap(_.upLeft()),

      from.down().flatMap(_.downLeft()),
      from.down().flatMap(_.downRight()),

      from.right().flatMap(_.upRight()),
      from.right().flatMap(_.downRight()),

      from.left().flatMap(_.upLeft()),
      from.left().flatMap(_.downLeft())
    ).filter(_.isDefined).map(_.get)

    for (possiblePosition <- possiblePositions; piece = board.at(possiblePosition) if piece.isEmpty || piece.get.color != selectedPiece.color)
      yield PossibleMove(from, possiblePosition, selectedPiece, piece, None)
  }

  private def kingUnsafePossibleMovesByBishop(board: Board, selectedPiece: Piece, from: Position): List[PossibleMove] =
    generateMovesByDirections(board, selectedPiece, from, BishopDirections)

  private def kingUnsafePossibleMovesByRook(board: Board, selectedPiece: Piece, from: Position): List[PossibleMove] =
    generateMovesByDirections(board, selectedPiece, from, RookDirections)

  private def kingUnsafePossibleMovesByQueen(board: Board, selectedPiece: Piece, from: Position): List[PossibleMove] =
    generateMovesByDirections(board, selectedPiece, from, QueenDirections)

  private def kingUnsafePossibleMovesByKing(board: Board, selectedPiece: Piece, from: Position): List[PossibleMove] = {
    val allMoves = for (kingDirection <- KingDirections; to <- from + kingDirection) yield to

    allMoves
      .filter(to => board.at(to).isEmpty || board.at(to).get.color != selectedPiece.color)
      .map(to => PossibleMove(from, to, selectedPiece, board.at(to), None))
  }

  private def generateMovesByDirections(board: Board, selectedPiece: Piece, from: Position, directions: List[(Int, Int)]): List[PossibleMove] =
    (for (direction <- directions) yield generateMovesByDirection(board, selectedPiece, from, direction)).flatten

  private def generateMovesByDirection(board: Board, selectedPiece: Piece, from: Position, direction: (Int, Int)): List[PossibleMove] = {
    def go(board: Board, targetPosition: Option[Position], passed: List[(Position, Option[Piece])]): List[(Position, Option[Piece])] = targetPosition match {
      case Some(to) => board.at(to) match {
        case Some(piece) if piece.color == selectedPiece.color => passed
        case Some(piece) if piece.color != selectedPiece.color => passed :+ (to, Some(piece))
        case None => go(board, to + direction, passed :+ (to, None))
      }
      case None => passed
    }

    go(board, from + direction, Nil).map {
      case (position, Some(piece)) => PossibleMove(from, position, selectedPiece, Some(piece), None)
      case (position, None) => PossibleMove(from, position, selectedPiece, None, None)
    }
  }
}
