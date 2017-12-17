package kchess

sealed trait GameState {
  def isGameOver: Boolean = this match {
    case Stalemate() => true
    case Checkmate(_, _, _, _) => true
    case _ => false
  }
}

case class ExpectsMove(player: Player, color: Color) extends GameState

case class ExpectsPromotion(player: Player, from: Position, to: Position)

case class Checkmate(winner: Player, loser: Player, winnerColor: Color, loserColor: Color) extends GameState

case class Stalemate() extends GameState