import AssemblyKeys._

name := "Troggie"

version := "1.0"

scalaVersion := "2.9.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"pircbot" % "pircbot" % "1.5.0" withSources(),
	"com.typesafe.akka" % "akka-actor" % "2.0" withSources(),
	"org.xerial" % "sqlite-jdbc" % "3.7.2" withSources(),
	"joda-time" % "joda-time" % "1.6.2" withSources(),
	"org.scalaquery" % "scalaquery_2.9.0-1" % "0.9.5" withSources(),
	"junit" % "junit" % "4.10" % "test" withSources(),
	"org.scalatest" %% "scalatest" % "1.7.1" % "test" withSources(),
	"org.mockito" % "mockito-all" % "1.9.0" % "test" withSources(),
	"com.typesafe.akka" % "akka-testkit" % "2.0" % "test" withSources(),
	"org.scala-tools.testing" %% "specs" % "1.6.9" % "test" withSources()
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

seq(assemblySettings: _*)

jarName in assembly := "Troggie.jar"