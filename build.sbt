name := "strugatzki"

version := "0.16"

organization := "de.sciss"

scalaVersion := "2.9.1"

description := "Algorithms for extracting audio features and matching audio file similarities"

homepage := Some( url( "https://github.com/Sciss/Strugatzki" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "0.32",
   "com.github.scopt" %% "scopt" % "1.1.2",
   "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// ---- publishing ----

publishTo <<= version { (v: String) =>
   Some( "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/".+(
      if( v.endsWith( "-SNAPSHOT")) "snapshots/" else "releases/"
   ))
}

pomExtra :=
<licenses>
  <license>
    <name>GPL v2+</name>
    <url>http://www.gnu.org/licenses/gpl-2.0.txt</url>
    <distribution>repo</distribution>
  </license>
</licenses>

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
