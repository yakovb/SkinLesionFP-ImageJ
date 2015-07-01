name := "IJPlugins"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "net.imagej" % "ij" % "1.49v"

libraryDependencies += "org.spire-math" %% "spire" % "0.10.1"

fork := true

enablePlugins(SbtImageJ)