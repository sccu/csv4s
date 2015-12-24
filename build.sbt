organization := "name.sccu"

name := "csv4s"

version := "0.1.5"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.10.0", "2.11.0")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "latest.integration" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)
