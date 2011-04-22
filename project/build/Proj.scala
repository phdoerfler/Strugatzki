
import xml._
import sbt.{ FileUtilities => FU, _}

/**
 *    @version 0.12, 21-Jul-10
 */
class InterPlayProject( info: ProjectInfo ) extends DefaultProject( info ) {
   val soundProcesses      = "de.sciss" %% "soundprocesses" % "0.22"  // XXX tmp
   val fscapeJobs          = "de.sciss" %% "fscapejobs" % "0.13"

   val ccstmRepo           = "CCSTM Release Repository at PPL" at "http://ppl.stanford.edu/ccstm/repo-releases"
   val ccstmSnap           = "CCSTM Snapshot Repository at PPL" at "http://ppl.stanford.edu/ccstm/repo-snapshots"
}
