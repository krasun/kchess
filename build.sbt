name := "kchess"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalacOptions ++= Seq(
  "-deprecation"
)

coverageMinimum := 80

coverageFailOnMinimum := true

coverageHighlighting := true