import AssemblyKeys._

name := "strugatzki"

version := "1.1.0"

organization := "de.sciss"

scalaVersion := "2.9.2"

description := "Algorithms for extracting audio features and matching audio file similarities"

homepage := Some( url( "https://github.com/Sciss/Strugatzki" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "1.1.+",
   "com.github.scopt" %% "scopt" % "2.1.0",
   "org.scalatest" %% "scalatest" % "1.8" % "test"
)

// libraryDependencies <+= scalaVersion { sv =>
//   val scopt = sv match {
//      case "2.10.0-RC2" => "com.github.scopt" %  "scopt_2.10.0-RC1"
//      case _ =>            "com.github.scopt" %% "scopt"
//   }
//   scopt % "2.1.0"
// }

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// ---- build info ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq( name, organization, version, scalaVersion, description,
   BuildInfoKey.map( homepage ) { case (k, opt) => k -> opt.get },
   BuildInfoKey.map( licenses ) { case (_, Seq( (lic, _) )) => "license" -> lic }
)

buildInfoPackage := "de.sciss.strugatzki"

// ---- publishing ----

publishMavenStyle := true

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

// ---- packaging ----

seq( assemblySettings: _* )

test in assembly := {}

// ---- ls.implicit.ly ----

seq( lsSettings :_* )

(LsKeys.tags in LsKeys.lsync) := Seq( "music-information-retrieval", "machine-learning", "music", "dsp", "feature-extraction" )

(LsKeys.ghUser in LsKeys.lsync) := Some( "Sciss" )

(LsKeys.ghRepo in LsKeys.lsync) := Some( "Strugatzki" )

// bug in ls -- doesn't find the licenses from global scope
(licenses in LsKeys.lsync) := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

