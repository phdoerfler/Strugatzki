name := "strugatzki"

version := "0.17"

organization := "de.sciss"

scalaVersion := "2.9.1"

description := "Algorithms for extracting audio features and matching audio file similarities"

homepage := Some( url( "https://github.com/Sciss/Strugatzki" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "0.32",
   "com.github.scopt" %% "scopt" % "1.1.3",
   "org.scalatest" %% "scalatest" % "1.7.1" % "test"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// ---- publishing ----

publishMavenStyle := true

// publishTo <<= version { (v: String) =>
//    Some( "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/".+(
//       if( v.endsWith( "-SNAPSHOT")) "snapshots/" else "releases/"
//    ))
// }

publishTo <<= version { (v: String) =>
   Some( if( v.endsWith( "-SNAPSHOT" ))
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
   else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
   )
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra :=
<licenses>
  <license>
    <name>GPL v2+</name>
    <url>http://www.gnu.org/licenses/gpl-2.0.txt</url>
    <distribution>repo</distribution>
  </license>
</licenses>
<scm>
  <url>git@github.com:Sciss/Strugatzki.git</url>
  <connection>scm:git:git@github.com:Sciss/Strugatzki.git</connection>
</scm>
<developers>
   <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
   </developer>
</developers>

// credentials += Credentials( Path.userHome / ".ivy2" / ".credentials" )

// ---- ls.implicit.ly ----

seq( lsSettings :_* )

(LsKeys.tags in LsKeys.lsync) := Seq( "music-information-retrieval", "machine-learning", "music", "dsp", "feature-extraction" )

(LsKeys.ghUser in LsKeys.lsync) := Some( "Sciss" )

(LsKeys.ghRepo in LsKeys.lsync) := Some( "Strugatzki" )

// bug in ls -- doesn't find the licenses from global scope
(licenses in LsKeys.lsync) := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

