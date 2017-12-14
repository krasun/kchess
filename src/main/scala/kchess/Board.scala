package kchess

import scala.util.{Try, Failure, Success}

case class Board(pieces: Map[Position, Piece], history: History) {

  val currentColor: Color.Value =
    history
      .lastOption
      .map(m => m.piece.color.opposite)
      .getOrElse(Color.White)

  def ofColor(color: Color.Value): Map[Position, Piece] = for ((position, piece) <- pieces if piece.color == color) yield position -> piece

  def kingPosition(color: Color.Value): Position =
    (for ((position, piece) <- pieces if piece.color == color && piece.isInstanceOf[King]) yield position).head

  def at(position: Position): Option[Piece] = pieces.get(position)

  def applyMove(from: Position, to: Position): Try[Board] = pieces.get(from) match {
    case Some(selectedPiece) =>
      if (selectedPiece.color != currentColor) {
        val expectsColor = currentColor.toString.toLowerCase
        Failure(new Exception(s"Expects move of $expectsColor piece!"))
      } else {
        Rules.checkMove(this, selectedPiece, from, to) match {
          case Success(CheckResult(capturesAt)) =>

            val dropAt = capturesAt.getOrElse(to)
            val updatedPieces = ((pieces - from) - dropAt) + (to -> selectedPiece)

            Success(Board(updatedPieces, history :+ Move(selectedPiece, from, to)))

          case Failure(exception) => Failure(exception)
        }
      }
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
    ),
    History()
  )
}
