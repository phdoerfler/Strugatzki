/*
 *  Processor.scala
 *  (Strugatzki)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.strugatzki
package aux

import java.io.File
import de.sciss.synth.io.{AudioFileSpec, SampleFormat, AudioFileType, AudioFile}
import actors.Actor

trait Processor {
   // note: using a type arg with processor crashes scalac -- do not ask __WHY__ !
   protected val companion : ProcessorCompanion

   protected final def createTempAudioFile( id: String, numChannels: Int ) : AudioFile = {
      val file = createTempFile( "corr_" + id, ".aif" )
      AudioFile.openWrite( file, AudioFileSpec( AudioFileType.IRCAM, SampleFormat.Float, numChannels, 44100 ))
   }

   /**
    * Subclasses may override this to perform further cleanup when the process is aborted.
    */
   protected def aborted() {}

   final def abort() { Act ! Abort }
   final def start() { Act.start() }

   protected /* private */ object Abort

   protected def observer: companion.Observer // ProcessorCompanion#Observer

   protected def Act: Actor

   protected object ProcT extends Thread {
      var aborted: Boolean = false
      private var lastProg = -1
      override def run() {
         Act ! (try {
            body()
         } catch {
            case e => companion.Failure( e )
         })
      }

      def progress( i: Int ) {
         if( i > lastProg ) {
            lastProg = i
            Act ! companion.Progress( i )
         }
      }
   }

   protected def body() : companion.Result

   protected final def checkAborted = ProcT.synchronized { ProcT.aborted }
   protected final def progress( f: Float ) { ProcT.progress( (f * 100 + 0.5f).toInt )}

   protected final def createTempFile( prefix: String, suffix: String ) : File = {
      val f = File.createTempFile( prefix, suffix, Strugatzki.tmpDir )
      f.deleteOnExit()
      f
   }
}