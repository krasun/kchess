package kchess

sealed trait GameState

case class ExpectsMove(player: Player, color: Color.Value) extends GameState

case class ExpectsPromotion(player: Player, from: Position, to: Position)

case class GameOver() extends GameState