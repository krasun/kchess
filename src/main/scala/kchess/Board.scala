package kchess

import scala.util.{Try, Failure, Success}

case class Board(pieces: Map[Position, Piece]) {

  def ofColor(color: Color.Value): Map[Position, Piece] = for ((position, piece) <- pieces if piece.color == color) yield position -> piece

  def kingPosition(color: Color.Value): Position =
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
      Position.A2 -> Pawn(Color.White),
      Position.B2 -> Pawn(Color.White),
      Position.C2 -> Pawn(Color.White),
      Position.D2 -> Pawn(Color.White),
      Position.E2 -> Pawn(Color.White),
      Position.F2 -> Pawn(Color.White),
      Position.G2 -> Pawn(Color.White),
      Position.H2 -> Pawn(Color.White),

      Position.A1 -> Rook(Color.White),
      Position.B1 -> Knight(Color.White),
      Position.C1 -> Bishop(Color.White),
      Position.D1 -> Queen(Color.White),
      Position.E1 -> King(Color.White),
      Position.F1 -> Knight(Color.White),
      Position.G1 -> Bishop(Color.White),
      Position.H1 -> Rook(Color.White),

      Position.A7 -> Pawn(Color.Black),
      Position.B7 -> Pawn(Color.Black),
      Position.C7 -> Pawn(Color.Black),
      Position.D7 -> Pawn(Color.Black),
      Position.E7 -> Pawn(Color.Black),
      Position.F7 -> Pawn(Color.Black),
      Position.G7 -> Pawn(Color.Black),
      Position.H7 -> Pawn(Color.Black),

      Position.A8 -> Rook(Color.Black),
      Position.B8 -> Knight(Color.Black),
      Position.C8 -> Bishop(Color.Black),
      Position.D8 -> Queen(Color.Black),
      Position.E8 -> King(Color.Black),
      Position.F8 -> Knight(Color.Black),
      Position.G8 -> Bishop(Color.Black),
      Position.H8 -> Rook(Color.Black)
    )
  )
}
