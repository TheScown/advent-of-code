ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

libraryDependencies += "org.json4s" %% "json4s-native" % "4.0.7"
libraryDependencies += "commons-codec" % "commons-codec" % "1.17.0"
libraryDependencies += "tools.aqua" % "z3-turnkey" % "4.14.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code",
    idePackagePrefix := Some("space.scown.adventofcode")
  )
