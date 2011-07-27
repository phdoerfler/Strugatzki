import sbt._

class StrugatzkiProject( info: ProjectInfo ) extends DefaultProject( info ) {
   val scalaCollider = "de.sciss" %% "scalacollider" % "0.24"
   val scopt         = "com.github.scopt" %% "scopt" % "1.1.1"
}