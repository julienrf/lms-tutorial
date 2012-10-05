name := "lms-tutorial"

resolvers += ScalaToolsSnapshots

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.0-M7"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

scalacOptions += "-Yvirtualize"