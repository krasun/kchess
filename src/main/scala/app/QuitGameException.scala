package app

case class QuitGameException(message: String) extends Exception(message)