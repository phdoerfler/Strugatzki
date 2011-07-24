/*
 *  Processor.scala
 *  (Utopia)
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

package de.sciss.utopia

import de.sciss.synth.io.AudioFile
import java.io.File
import actors.Actor
import de.sciss.utopia.FeatureExtraction.Progress

trait ProcessorCompanion {
   var verbose = false

   protected lazy val tmpDir = new File( sys.props.getOrElse( "java.io.tmpdir", "/tmp" ))

   type Observer = PartialFunction[ ProgressOrResult, Unit ]
   type PayLoad

   sealed trait ProgressOrResult
   final case class Progress( percent: Int ) extends ProgressOrResult
   sealed trait Result extends ProgressOrResult
   case class Success( result: PayLoad ) extends Result
   final case class Failure( t: Throwable ) extends Result
   case object Aborted extends Result
}
//object Processor {
//
//}
trait Processor {
   protected val companion: ProcessorCompanion
   protected def observer: companion.Observer

   Act.start()

   def abort() { Act ! Abort }

   private object Abort

   private object Act extends Actor {
      def act() {
         ProcT.start()
         var result : companion.Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.aborted = true
               case res: companion.Progress =>
                  observer( res )
               case res @ companion.Aborted =>
                  result = res
               case res: companion.Failure =>
                  result = res
               case res: companion.Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }

   protected def body() : companion.Result

   private object ProcT extends Thread {
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
            Act ! Progress( i )
         }
      }
   }

   protected def checkAborted = ProcT.synchronized { ProcT.aborted }
   protected def progress( f: Float ) = ProcT.progress( (f * 100 + 0.5f).toInt )
}