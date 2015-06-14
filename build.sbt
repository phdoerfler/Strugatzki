name               := "Strugatzki"

version            := "2.10.0-SNAPSHOT"

organization       := "de.sciss"

scalaVersion       := "2.11.6"
crossScalaVersions := Seq("2.11.6", "2.10.5")

description        := "Algorithms for extracting audio features and matching audio file similarities"
homepage           := Some(url("https://github.com/Sciss/" + name.value))
licenses           := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

lazy val scalaColliderVersion = "1.17.2"
lazy val spanVersion          = "1.4.0-SNAPSHOT"
lazy val paletteVersion       = "1.0.0"
lazy val fileUtilVersion      = "1.1.1"
lazy val scoptVersion         = "3.3.0"
lazy val scalaTestVersion     = "2.2.4"

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

assemblyJarName in assembly := s"${name.value}.jar"

// ---- ls.implicit.ly ----

// seq(lsSettings :_*)
// (LsKeys.tags   in LsKeys.lsync) := Seq("music-information-retrieval", "machine-learning", "music", "dsp", "feature-extraction")
// (LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")
// (LsKeys.ghRepo in LsKeys.lsync) := Some(name.value)
