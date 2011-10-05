/*
 *  Processor.scala
 *  (Strugatzki)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
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
 *
 *
 *  Changelog:
 */

package de.sciss.strugatzki
package aux

import java.io.File
import de.sciss.synth.io.{AudioFileSpec, SampleFormat, AudioFileType, AudioFile}
import actors.Actor

trait Processor {
   protected val companion : ProcessorCompanion

   protected final def createTempAudioFile( id: String, numChannels: Int ) : AudioFile = {
      val file = File.createTempFile( "corr_" + id, ".aif" )
      file.deleteOnExit()
      AudioFile.openWrite( file, AudioFileSpec( AudioFileType.IRCAM, SampleFormat.Float, numChannels, 44100 ))
   }

   protected final def avg( b: Array[ Float ], off: Int, len: Int ) = {
      var sum = 0.0
      var i = off; val stop = off + len; while( i < stop ) {
         sum += b( i )
      i += 1 }
      (sum / len).toFloat
   }

   protected final def stat( mat: Array[ Array[ Float ]], frameOff: Int, frameLen: Int,
                             chanOff: Int, chanLen: Int ) : (Double, Double) = {
      val chanStop   = chanOff + chanLen
      val frameStop  = frameOff + frameLen
      var sum = 0.0
      var ch = chanOff; while( ch < chanStop ) {
         val cb = mat( ch )
         var i = frameOff; while( i < frameStop ) {
            sum += cb( i )
         i +=1 }
      ch += 1 }
      val matSize = frameLen * chanLen
      val mean = sum / matSize
      sum = 0.0
      ch = chanOff; while( ch < chanStop ) {
         val cb = mat( ch )
         var i = frameOff; while( i < frameStop ) {
            val d = cb( i ) - mean
            sum += d * d
         i +=1 }
      ch += 1 }
      val stddev = math.sqrt( sum / matSize )
      (mean, stddev)
   }

   protected final def normalize( normBuf: Array[ Array[ Float ]], b: Array[ Array[ Float ]], bOff: Int, bLen: Int ) {
      if( normBuf == null ) return
      var ch = 0; val numCh = b.length; while( ch < numCh ) {
         val cb   = b( ch )
         val cn   = normBuf( ch )
         val min  = cn( 0 )
         val max  = cn( 1 )
         val d    = max - min
         var i = bOff; val iStop = bOff + bLen; while( i < iStop ) {
            val f    = cb( i )
            // XXX should values be clipped to [0...1] or not?
            cb( i )  = (f - min) / d
         i += 1 }
      ch += 1 }
   }

   /**
    * Subclasses may override this to perform further cleanup when the process is aborted.
    */
   protected def aborted() {}

   final def abort() { Act ! Abort }
   final def start() { Act.start() }

   private object Abort

   protected def observer: companion.Observer

   private object Act extends Actor {
      def act() {
         ProcT.start()
         var result : /* companion. */ companion.Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.aborted = true
                  aborted()
               case res: /* companion. */ companion.Progress =>
                  observer( res )
               case res @ /* companion. */ companion.Aborted =>
                  result = res
               case res: /* companion. */ companion.Failure =>
                  result = res
               case res: /* companion. */ companion.Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }

   private object ProcT extends Thread {
      var aborted: Boolean = false
      private var lastProg = -1
      override def run() {
         Act ! (try {
            body()
         } catch {
            case e => /* companion. */ companion.Failure( e )
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
}