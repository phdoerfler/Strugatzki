name := "strugatzki"

version := "0.14-SNAPSHOT"

organization := "de.sciss"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "0.30-SNAPSHOT",
   "com.github.scopt" %% "scopt" % "1.1.2",
   "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"
)

retrieveManaged := true

scalacOptions += "-deprecation"
