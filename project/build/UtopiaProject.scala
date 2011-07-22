import sbt._

class UtopiaProject( info: ProjectInfo ) extends DefaultProject( info ) {
   val soundProcesses      = "de.sciss" %% "soundprocesses" % "0.23"
   val fscapeJobs          = "de.sciss" %% "fscapejobs" % "0.14"
}