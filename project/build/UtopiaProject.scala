import sbt._

class UtopiaProject( info: ProjectInfo ) extends DefaultProject( info ) {
   val soundProcesses      = "de.sciss" %% "soundprocesses" % "0.23"
   val scalaColliderSwing  = "de.sciss" %% "scalacolliderswing" % "0.27"
   val fscapeJobs          = "de.sciss" %% "fscapejobs" % "0.14"
   val scopt               = "com.github.scopt" %% "scopt" % "1.1.1"
   val scalaAudioWidgets   = "de.sciss" %% "scalaaudiowidgets" % "0.10-SNAPSHOT"
   val sonogram            = "de.sciss" %% "sonogramoverview" % "0.16"

   val repo1               = "Clojars Repository" at "http://clojars.org/repo" // this is needed for ScalaInterpreterPane
}