name := "java-less"

description := "Thin language based on Java"

scalaVersion := "2.11.6"

///////////////////////////////////////////////////////////////////////////////////////////////////

resolvers += "Uqbar Central" at "http://uqbar-wiki.org/mvn/releases"

lazy val cacao = FDProject(
	"org.uqbar" %% "voodoo" % "latest.integration",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
	"org.scalatest" %% "scalatest" % "2.2.4" % "test",
	"org.uqbar" %% "parser-test" % "latest.integration" % "test"
)

///////////////////////////////////////////////////////////////////////////////////////////////////

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"