package kchess.chess

import scala.util.{Try, Failure, Success}

case class Board(pieces: Map[Position, Piece]) {

  def ofColor(color: Color): Map[Position, Piece] = for ((position, piece) <- pieces if piece.color == color) yield position -> piece

  def kingPosition(color: Color): Position =
    (for ((position, piece) <- pieces if piece.color == color && piece.isInstanceOf[King]) yield position).head

  def at(position: Position): Option[Piece] = pieces.get(position)

  def drop(dropAt: Position): Board = Board(pieces - dropAt)

  def move(from: Position, to: Position): Try[Board] = at(from) match {
    case Some(piece) =>
      Success(Board((pieces - from) + (to -> piece)))
    case None =>
      val fromAsString = from.toString
      Failure(new Exception(s"There is no piece at $fromAsString."))
  }
}

object Board {
  def standard: Board = Board(
    Map(
      Position.A2 -> Pawn(White()),
      Position.B2 -> Pawn(White()),
      Position.C2 -> Pawn(White()),
      Position.D2 -> Pawn(White()),
      Position.E2 -> Pawn(White()),
      Position.F2 -> Pawn(White()),
      Position.G2 -> Pawn(White()),
      Position.H2 -> Pawn(White()),

      Position.A1 -> Rook(White()),
      Position.B1 -> Knight(White()),
      Position.C1 -> Bishop(White()),
      Position.D1 -> Queen(White()),
      Position.E1 -> King(White()),
      Position.F1 -> Bishop(White()),
      Position.G1 -> Knight(White()),
      Position.H1 -> Rook(White()),

      Position.A7 -> Pawn(Black()),
      Position.B7 -> Pawn(Black()),
      Position.C7 -> Pawn(Black()),
      Position.D7 -> Pawn(Black()),
      Position.E7 -> Pawn(Black()),
      Position.F7 -> Pawn(Black()),
      Position.G7 -> Pawn(Black()),
      Position.H7 -> Pawn(Black()),

      Position.A8 -> Rook(Black()),
      Position.B8 -> Knight(Black()),
      Position.C8 -> Bishop(Black()),
      Position.D8 -> Queen(Black()),
      Position.E8 -> King(Black()),
      Position.F8 -> Bishop(Black()),
      Position.G8 -> Knight(Black()),
      Position.H8 -> Rook(Black())
    )
  )
}
