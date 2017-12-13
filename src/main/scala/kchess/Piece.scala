package kchess

sealed trait Piece {
  def color: Color.Value
}

case class Pawn(color: Color.Value) extends Piece

case class Knight(color: Color.Value) extends Piece

case class Bishop(color: Color.Value) extends Piece

case class Rook(color: Color.Value) extends Piece

case class Queen(color: Color.Value) extends Piece

case class King(color: Color.Value) extends Piece