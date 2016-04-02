name               := "Strugatzki"

version            := "2.11.0"

organization       := "de.sciss"

scalaVersion       := "2.11.8"

crossScalaVersions := Seq("2.11.8", "2.10.6")

description        := "Algorithms for extracting audio features and matching audio file similarities"

homepage           := Some(url("https://github.com/Sciss/" + name.value))

licenses           := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

lazy val scalaColliderVersion = "1.18.1"
lazy val spanVersion          = "1.3.1"
lazy val paletteVersion       = "1.0.0"
lazy val fileUtilVersion      = "1.1.1"
lazy val scoptVersion         = "3.4.0"
lazy val scalaTestVersion     = "2.2.6"

libraryDependencies ++= Seq(
  "de.sciss"          %% "scalacollider"    % scalaColliderVersion,   // for the feature ugens
  "de.sciss"          %% "span"             % spanVersion,            // representation of time spans
  "de.sciss"          %  "intensitypalette" % paletteVersion,         // color scheme for self similarity
  "de.sciss"          %% "fileutil"         % fileUtilVersion,        // easy path compositions
  "com.github.scopt"  %% "scopt"            % scoptVersion,           // parsing command line options
  "org.scalatest"     %% "scalatest"        % scalaTestVersion % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

// ---- build info ----

enablePlugins(BuildInfoPlugin)

buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
  BuildInfoKey.map(homepage) { case (k, opt)             => k -> opt.get },
  BuildInfoKey.map(licenses) { case (_, Seq( (lic, _) )) => "license" -> lic }
)

buildInfoPackage := "de.sciss.strugatzki"

// ---- publishing ----

publishMavenStyle := true

publishTo :=
  Some(if (isSnapshot.value)
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := { val n = name.value
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}

// ---- packaging ----

test    in assembly := ()
target  in assembly := baseDirectory.value
jarName in assembly := s"${name.value}.jar"

// ---- ls.implicit.ly ----

// seq(lsSettings :_*)
// (LsKeys.tags   in LsKeys.lsync) := Seq("music-information-retrieval", "machine-learning", "music", "dsp", "feature-extraction")
// (LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")
// (LsKeys.ghRepo in LsKeys.lsync) := Some(name.value)
