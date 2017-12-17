package kchess

trait Color {
  def opposite: Color = this match {
    case White() => Black()
    case Black() => White()
  }

  override def toString: String = this match {
    case Black() => "Black"
    case White() => "White"
  }
}

case class White() extends Color
case class Black() extends Color

object Color {
  def values: List[Color] = List(White(), Black())
}