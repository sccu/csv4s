organization := "name.sccu"

name := "csv4s"

version := "0.1.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "latest.integration" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)
