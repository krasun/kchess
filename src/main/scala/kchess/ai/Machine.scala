package kchess.ai

import kchess.chess._

object Machine {

  // @todo more intellectual algorithm
  def move(game: Game, machinePlayer: Player): (Position, Position) = {
    val machineColor = game.colorOf(machinePlayer)

    Rules.availableMoves(game.board, machineColor, game.history).map {
      case (selectedPiece, from, to, capturesPiece) => (from, to)
    }.head
  }
}