name := "IJPlugins"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "net.imagej" % "ij" % "1.49v"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

fork := true

enablePlugins(SbtImageJ)