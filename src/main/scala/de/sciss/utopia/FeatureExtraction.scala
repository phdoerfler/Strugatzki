/*
 *  FeatureExtraction.scala
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

import sys.error
import java.io.File
import actors.{DaemonActor, Future}
import de.sciss.synth.{SynthDef, Server, RevocableFuture}
import de.sciss.synth
import synth.io.AudioFile
import de.sciss.osc.OSCBundle

object FeatureExtraction {
   def apply( settings: Settings )( observer: PartialFunction[ ProgressOrResult ]) : FeatureExtraction = {
      new FeatureExtraction( settings, observer )
   }

   sealed trait SettingsLike {
      def audioInput : File
      def featureOutput : File
      def metaOutput : Option[ File ]
      def numCoeffs : Int
      def fftSize : Int
      def fftOverlap : Int
   }

   object SettingsBuilder { def apply = new SettingsBuilder }
   final class SettingsBuilder extends SettingsLike {
      var audioInput : File         = new File( "input.aif" )
      var featureOutput : File      = new File( sys.props.getOrElse( "java.io.tmp", "/tmp" ), "features.aif" )
      var metaOutput                = Option.empty[ File ]
      var numCoeffs : Int           = 13
      var fftSize : Int             = 1024
      var fftOverlap : Int          = 2

      def toSettings = Settings( audioInput, featureOutput, metaOutput, numCoeffs, fftSize, fftOverlap )
   }

   final case class Settings( audioInput : File, featureOutput : File, metaOutput : Option[ File ],
                              numCoeffs : Int, fftSize : Int, fftOverlap : Int )
   extends SettingsLike

   sealed trait ProgressOrResult
   final case class Progress( amount: Float ) extends ProgressOrResult
   sealed trait Result extends ProgressOrResult
   case object Success extends Result
   final case class Failure( t: Throwable ) extends Result
   case object Aborted extends Result
}
final class FeatureExtraction private ( val settings: FeatureExtraction.Settings,
                                        observer: PartialFunction[ FeatureExtraction.ProgressOrResult ]) {
   import FeatureExtraction._

   def abort() { Act ! Abort }

   private object Abort

   private object ProcT extends Thread {
      override def run() {
         try {
            procBody()
         } finally {

         }
      }
   }

   private def procBody() {
      import synth._
      import ugen._
      val s             = Server.dummy
      val spec          = AudioFile.readSpec( settings.audioInput )
      val stepSize      = settings.fftSize / settings.fftOverlap
      val coeffBufSize  = 1024
      val fftBufID      = 0
      val coeffBufID    = 1
      val df = SynthDef( "nrt" ) {
         val in         = Mix( In.ar( NumOutputBuses.ir, af.numChannels )) // XXX mono mix could be configurable
         val chain      = FFT( fftBufID, in, 1.0 / settings.fftOverlap )
         val coeffs     = MFCC.kr( chain, settings.numCoeffs )
         val rmax       = RunningMax.kr( coeffs, 0 )
         val rmin       = RunningMin.kr( coeffs, 0 )
//         Out.kr( 0, rmax :: rmin :: Nil )
         val trig       = Impulse.ar( SampleRate.ir / stepSize )
//         SendReply.ar( trig, coeffs, "/feat" )
         val phase      = Stepper.ar( trig, min = 0, max = coeffBufSize - 1 )
         BufWr.kr( coeffs, coeffBufID, phase, loop = 0, interp = 1 )
      }

//      val df2 = SynthDef( "snap" ) {
//         val coeffs = Latch.ar( K2A.ar( In.kr( 0, numCoeffs * 2 )), Impulse.ar( 0 ))
//         DiskOut.ar( 2, coeffs )
////   Line.ar( dur = ControlDur.ir, doneAction = freeSelf )
//      }

      val syn        = Synth( s )
      val fftBuf     = new Buffer( s, fftBufID )
      val coeffBuf   = new Buffer( s, coeffBufID )
      val inPath     = settings.audioInput.getAbsolutePath
      val outPath    = settings.featureOutput.getAbsolutePath
      val numFrames  = ((spec.numFrames + stepSize - 1) / stepSize) * stepSize
      val dur        = numFrames / spec.sampleRate
      val bndls      = OSCBundle.secs( 0.0,
         fftBuf.allocMsg( settings.fftSize ),
         coeffBuf.allocMsg( coeffBufSize, numCoeffs ),
         buf3.writeMsg( outPath, numFrames = 0, leaveOpen = true ),
         df.recvMsg,
         df2.recvMsg,
         syn.newMsg( df.name, s.rootNode )
      ) :: OSCBundle.secs( dur,
         syn.freeMsg,
         syn2.newMsg( df2.name, s.rootNode )
      ) :: OSCBundle.secs( dur + 32768.0/44100,
         syn2.freeMsg,
         buf3.closeMsg
      ) :: Nil

      val c = de.sciss.osc.OSCPacketCodec.default
      val sz = bndls.map( _.getEncodedSize( c )).max

      val f = File.createTempFile( "tmp", ".osc" )
      val raf = new RandomAccessFile( f, "rw" )
      val bb = java.nio.ByteBuffer.allocate( sz )
      val fch = raf.getChannel()
      bndls.foreach { bndl =>
         bndl.encode( c, bb )
         bb.flip
         raf.writeInt( bb.limit )
         fch.write( bb )
         bb.clear
      }
      raf.close

// run from cmdline:
      "./scsynth -i 1 -o 1 -N " + f.getAbsolutePath + " " + inPath + " /tmp/killme.aif 44100 AIFF int16"

      val afRes = AudioFile.openRead( outPath )
      val fb = afRes.frameBuffer( 1 )
      afRes.readFrames( fb )
      afRes.close
      fb.map(_(0))
   }
   }

   private object Act extends DaemonActor {
      start()
      def act() {
         var result : Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  result = Aborted
            }
         } andThen { observer( result )}
      }
   }
}