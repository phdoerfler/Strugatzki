## Strugatzki

### statement

Some algorithms for searching for sounds similar to other sounds. They are employed in my piece "Leere Null".

(C)opyright 2011-2012 by Hanns Holger Rutz. All rights reserved. It is released under the [GNU General Public License](http://github.com/Sciss/SonogramOverview/blob/master/licenses/Strugatzki-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

### requirements / installation

Builds with sbt 0.11 against Scala 2.9.1 Depends on ScalaCollider and scopt.

### creating an IntelliJ IDEA project

If you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "0.11.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "Strugatzki"
    > gen-idea
