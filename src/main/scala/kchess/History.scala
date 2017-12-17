package kchess

case class History(moves: List[Move]) {
  def lastOption: Option[Move] = moves.lastOption

  def :+ (move: Move): History = History(moves :+ move)

  def nextColor: Color = lastOption.map(m => m.movedPiece.color.opposite).getOrElse(White())
}

object History {
  def apply(): History = History(List())
}