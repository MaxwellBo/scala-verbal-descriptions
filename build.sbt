name := "scala-verbal-descriptions"
version := "0.1.0"
organization := "Maxwell Bo"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalameta" %% "scalameta" % "3.3.1"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"

val scalaTestVersion = "3.0.5"

libraryDependencies += "org.scalactic" %% "scalactic" % scalaTestVersion
libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
