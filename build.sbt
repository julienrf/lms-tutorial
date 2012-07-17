name := "lms-tutorial"

resolvers += ScalaToolsSnapshots

scalaVersion := "2.10.0-M1-virtualized"

libraryDependencies += "EPFL" %% "lms" % "0.2"

scalacOptions += "-Yvirtualize"