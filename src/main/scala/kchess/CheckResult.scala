package kchess

case class CheckResult(movedPiece: Piece, capturesAt: Option[Position])