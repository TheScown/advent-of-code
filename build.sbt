ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2015",
    idePackagePrefix := Some("space.scown.adventofcode")
  )
