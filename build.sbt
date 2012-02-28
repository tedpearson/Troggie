name := "Troggie"

version := "1.0"

scalaVersion := "2.9.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"pircbot" % "pircbot" % "1.5.0" withSources(),
	"com.typesafe.akka" % "akka-actor" % "2.0-RC2" withSources(),
	"org.xerial" % "sqlite-jdbc" % "3.7.2" withSources(),
	"joda-time" % "joda-time" % "1.6.2" withSources()
)

scalacOptions ++= Seq("-unchecked", "-deprecation")