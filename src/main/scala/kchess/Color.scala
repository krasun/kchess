package kchess

object Color extends Enumeration {
  val White, Black = Value

  implicit class OverridedValue(val value:Value) {
    def opposite: Color.Value = if (value == Color.White) Color.Black else Color.White
  }
}