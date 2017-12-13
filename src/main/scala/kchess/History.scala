package kchess

case class History(moves: List[Move]) {

  def lastOption: Option[Move] = moves.lastOption

  def :+ (move: Move): History = History(moves :+ move)
}

object History {
  def apply(): History = History(List())
}