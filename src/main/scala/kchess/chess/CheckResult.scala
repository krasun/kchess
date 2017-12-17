package kchess.chess

case class CheckResult(movedPiece: Piece, capturesAt: Option[Position])