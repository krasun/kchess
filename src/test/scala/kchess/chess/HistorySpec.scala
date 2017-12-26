package kchess.chess

import org.scalatest.FlatSpec

class HistorySpec extends FlatSpec {

  val newHistory = History()

  "New history" should "have empty list of moves" in {
    assert(History().moves === Nil)
  }

  ":+" should "add move to history" in {
    val updatedHistory = newHistory :+ Move(Pawn(White()), Position.E2, Position.E2.up().get)

    assert(updatedHistory.moves.head === Move(Pawn(White()), Position.E2, Position.E2.up().get))
  }

  "lastOption" should "return last move as Some(move)" in {
    val updatedHistory = newHistory :+ Move(Pawn(White()), Position.E2, Position.E2.up().get)

    assert(updatedHistory.lastOption === Some(Move(Pawn(White()), Position.E2, Position.E2.up().get)))
  }

  "newHistory.lastOption" should "return last move as None" in {
    assert(newHistory.lastOption === None)
  }

  "New history" should "return White as next color" in {
    assert(History().nextColor === White())
  }

  "History with last move of White" should "return Black as next color" in {
    val updatedHistory = History(List(Move(Pawn(White()), Position.E2, Position.E2.up().get)))

    assert(updatedHistory.nextColor === Black())
  }
}