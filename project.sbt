name := "java-less"

description := "Thin language based on Java"

scalaVersion := "2.11.6"

///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val cacao = FDProject(
	"org.uqbar" %% "voodoo" % "1.3.3",
	"org.uqbar" %% "identity-map" % "latest.integration",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
	"org.scala-lang" % "scala-reflect" % "2.11.6",
	"org.scalatest" %% "scalatest" % "2.2.4" % "test",
	"org.uqbar" %% "parser-test" % "latest.integration" % "test"
)

///////////////////////////////////////////////////////////////////////////////////////////////////

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"