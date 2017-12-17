package kchess.chess

sealed trait Piece {
  def color: Color
}

case class Pawn(color: Color) extends Piece

case class Knight(color: Color) extends Piece

case class Bishop(color: Color) extends Piece

case class Rook(color: Color) extends Piece

case class Queen(color: Color) extends Piece

case class King(color: Color) extends Piece