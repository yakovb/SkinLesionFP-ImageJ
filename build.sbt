name := "IJPlugins"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "net.imagej" % "ij" % "1.49v"

libraryDependencies += "org.spire-math" %% "spire" % "0.10.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.5"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

fork := true

enablePlugins(SbtImageJ)