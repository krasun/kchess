package kchess.chess

case class Position(row: Int, column: Int) {
  def file: String = ('a' + column - 1).toChar.toString
  def rank: String = row.toString

  def up(): Option[Position] =
    if (isValid(row + 1)) Some(Position(row + 1, column))
    else None

  def down(): Option[Position] =
    if (isValid(row - 1)) Some(Position(row - 1, column))
    else None

  def left(): Option[Position] =
    if (isValid(column - 1)) Some(Position(row, column - 1))
    else None

  def right(): Option[Position] =
    if (isValid(column + 1)) Some(Position(row, column + 1))
    else None

  def upLeft(): Option[Position] = up().flatMap(_.left())

  def upRight(): Option[Position] = up().flatMap(_.right())

  def downLeft(): Option[Position] = down().flatMap(_.left())

  def downRight(): Option[Position] = down().flatMap(_.right())

  def +(delta: (Int, Int)): Option[Position] = {
    val (rowDelta, columnDelta) = delta
    val (newRow, newColumn) = (row + rowDelta, column + columnDelta)

    if (isValid(newRow) && isValid(newColumn)) {
      Some(Position(newRow, newColumn))
    } else {
      None
    }
  }

  private def isValid(index: Int): Boolean = 1 <= index && index <= 8

  override def toString: String = file + rank
}

object Position {
  def apply(file: Char, rank: Char): Position = new Position(rank - '0', file - 'a' + 1)

  val A1 = Position(1, 1)
  val B1 = Position(1, 2)
  val C1 = Position(1, 3)
  val D1 = Position(1, 4)
  val E1 = Position(1, 5)
  val F1 = Position(1, 6)
  val G1 = Position(1, 7)
  val H1 = Position(1, 8)

  val A2 = Position(2, 1)
  val B2 = Position(2, 2)
  val C2 = Position(2, 3)
  val D2 = Position(2, 4)
  val E2 = Position(2, 5)
  val F2 = Position(2, 6)
  val G2 = Position(2, 7)
  val H2 = Position(2, 8)

  val A7 = Position(7, 1)
  val B7 = Position(7, 2)
  val C7 = Position(7, 3)
  val D7 = Position(7, 4)
  val E7 = Position(7, 5)
  val F7 = Position(7, 6)
  val G7 = Position(7, 7)
  val H7 = Position(7, 8)

  val A8 = Position(8, 1)
  val B8 = Position(8, 2)
  val C8 = Position(8, 3)
  val D8 = Position(8, 4)
  val E8 = Position(8, 5)
  val F8 = Position(8, 6)
  val G8 = Position(8, 7)
  val H8 = Position(8, 8)
}