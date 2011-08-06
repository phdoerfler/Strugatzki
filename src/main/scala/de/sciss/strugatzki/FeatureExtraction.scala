/*
 *  FeatureExtraction.scala
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

import sys.error
import de.sciss.synth
import de.sciss.osc.{OSCPacketCodec, OSCBundle}
import java.io.{RandomAccessFile, File}
import java.nio.ByteBuffer
import synth.io.{AudioFileSpec, AudioFileType, SampleFormat, AudioFile}
import sys.process.{ProcessLogger, Process}
import actors.Actor
import xml.{NodeSeq, XML}

object FeatureExtraction {
   var verbose = false

   private lazy val tmpDir = new File( sys.props.getOrElse( "java.io.tmpdir", "/tmp" ))

   def apply( settings: Settings )( observer: PartialFunction[ ProgressOrResult, Unit ]) : FeatureExtraction = {
      new FeatureExtraction( settings, observer )
   }

   object ChannelsBehavior {
      def fromID( id: Int ) : ChannelsBehavior = id match {
         case 0 => Mix
         case 1 => First
         case 2 => Last
         case _ => throw new IllegalArgumentException( id.toString )
      }
      /** Input signal's channels are mixed together before taking the analysis */
      case object Mix extends ChannelsBehavior { val id = 0 }
      /** Just the first channel is used in the analysis (i.e. the left channel if the audio input is stereo */
      case object First extends ChannelsBehavior { val id = 1 }
      /** Just the last channel is used in the analysis (i.e. the right channel if the audio input is stereo */
      case object Last extends ChannelsBehavior { val id = 2 }
   }
   /** Defines how analysis data is taken from multi channel files */
   sealed trait ChannelsBehavior { def id: Int }

   sealed trait SettingsLike {
      def audioInput : File
      def featureOutput : File
      def metaOutput : Option[ File ]
      def numCoeffs : Int
      def fftSize : Int
      def fftOverlap : Int
      def channelsBehavior : ChannelsBehavior
   }

   object SettingsBuilder {
      def apply() : SettingsBuilder = new SettingsBuilder
      def apply( settings: Settings ) : SettingsBuilder = {
         val sb = new SettingsBuilder
         sb.read( settings )
         sb
      }
   }
   final class SettingsBuilder extends SettingsLike {
      var audioInput : File         = new File( "input.aif" )
      var featureOutput : File      = new File( tmpDir, "features.aif" )
      var metaOutput                = Option.empty[ File ]
      var numCoeffs : Int           = 13
      var fftSize : Int             = 1024
      var fftOverlap : Int          = 2
      var channelsBehavior : ChannelsBehavior = ChannelsBehavior.Mix

      def build = Settings( audioInput, featureOutput, metaOutput, numCoeffs, fftSize, fftOverlap, channelsBehavior )

      def read( settings: Settings ) {
         audioInput        = settings.audioInput
         featureOutput     = settings.featureOutput
         metaOutput        = settings.metaOutput
         numCoeffs         = settings.numCoeffs
         fftSize           = settings.fftSize
         fftOverlap        = settings.fftOverlap
         channelsBehavior  = settings.channelsBehavior
      }
   }

   object Settings {
      implicit def fromBuilder( sb: SettingsBuilder ) : Settings = sb.build
      def fromXMLFile( file: File ) : Settings = fromXML( XML.loadFile( file ))
      def fromXML( xml: NodeSeq ) : Settings = {
         val sb = new SettingsBuilder
         sb.audioInput     = new File( (xml \ "input").text )
         sb.featureOutput  = new File( (xml \ "output").text )
         sb.metaOutput     = {
            val e = (xml \ "meta").text
            if( e.isEmpty ) None else Some( new File( e ))
         }
         sb.numCoeffs      = (xml \ "numCoeffs").text.toInt
         sb.fftSize        = (xml \ "fftSize").text.toInt
         sb.fftOverlap     = (xml \ "fftOverlap").text.toInt
         sb.channelsBehavior = {
            val e = (xml \ "channels").text
            if( e.isEmpty ) ChannelsBehavior.Mix else ChannelsBehavior.fromID( e.toInt )
         }
         sb.build
      }
   }
   final case class Settings( audioInput : File, featureOutput : File, metaOutput : Option[ File ],
                              numCoeffs : Int, fftSize : Int, fftOverlap : Int, channelsBehavior: ChannelsBehavior )
   extends SettingsLike {
      def toXML =
<feature>
   <input>{audioInput.getPath}</input>
   <output>{featureOutput.getPath}</output>
   <meta>{metaOutput.map( _.getPath ).getOrElse( "" )}</meta>
   <numCoeffs>{numCoeffs}</numCoeffs>
   <fftSize>{fftSize}</fftSize>
   <fftOverlap>{fftOverlap}</fftOverlap>
   <channels>{channelsBehavior.id}</channels>
</feature>
   }

   sealed trait ProgressOrResult
   final case class Progress( percent: Int ) extends ProgressOrResult
   sealed trait Result extends ProgressOrResult
   case object Success extends Result
   final case class Failure( t: Throwable ) extends Result
   case object Aborted extends Result
}
final class FeatureExtraction private ( val settings: FeatureExtraction.Settings,
                                        observer: PartialFunction[ FeatureExtraction.ProgressOrResult, Unit ]) {
   import FeatureExtraction._

//   Act.start()

   def abort() { Act ! Abort }
   def start() { Act.start() }

   private object Abort

   private object ProcT extends Thread {
      var aborted: Boolean = false
      var p: Process = null

      override def run() {
         Act ! (try {
            if( procBody() ) Success else Aborted
         } catch {
            case e => Failure( e )
         })
      }

      private def procBody() : Boolean = {
         import synth._
         import ugen._

         val spec             = AudioFile.readSpec( settings.audioInput )
         val stepSize         = settings.fftSize / settings.fftOverlap
         val coeffRate        = spec.sampleRate / stepSize

         val so               = new ServerOptionsBuilder
         val oscF             = File.createTempFile( "tmp", ".osc" )
         val dummyOutput      = File.createTempFile( "tmp", ".aif" )
         so.inputBusChannels  = spec.numChannels
         so.sampleRate        = spec.sampleRate.toInt // coeffRate.toInt
         so.outputBusChannels = 1
         so.nrtCommandPath    = oscF.getAbsolutePath
         so.nrtInputPath      = Some( settings.audioInput.getAbsolutePath )
         so.nrtOutputPath     = dummyOutput.getAbsolutePath
         so.nrtHeaderFormat   = AudioFileType.AIFF
         so.nrtSampleFormat   = SampleFormat.Int16

         val s                = Server.dummy( "nrt", so.build )
         val coeffBufSize     = 888 // 1024
         val numCh            = settings.numCoeffs + 1
         val fftBufID         = 0
         val coeffBufID       = 1
         val fftWinType       = 1   // -1 rect, 0 sine, 1 hann
         val df = SynthDef( "nrt" ) {
            val in0        = In.ar( NumOutputBuses.ir, spec.numChannels )
            val in         = settings.channelsBehavior match {
               case ChannelsBehavior.Mix     => Mix( in0 )
               case ChannelsBehavior.First   => in0.outputs.head
               case ChannelsBehavior.Last    => in0.outputs.last
            }
            val chain      = FFT( fftBufID, in, 1.0 / settings.fftOverlap, fftWinType )
            val coeffs     = MFCC.kr( chain, settings.numCoeffs )
            val loud       = Loudness.kr( chain ) / 32
//            val rmax       = RunningMax.kr( coeffs, 0 )
//            val rmin       = RunningMin.kr( coeffs, 0 )
            val trig       = Impulse.kr( coeffRate ) // - Impulse.kr( 0 )
            val phaseHi    = coeffBufSize - 1
//            val phase      = Stepper.kr( trig, lo = 0, hi = phaseHi, resetVal = phaseHi )
            val phase      = Stepper.kr( trig, 0, 0, phaseHi, 1, phaseHi )
//            phase.poll(trig)
            BufWr.kr( loud +: coeffs.outputs, coeffBufID, phase )
         }

         val syn        = Synth( s )
         val fftBuf     = new Buffer( s, fftBufID )
         val coeffBuf   = new Buffer( s, coeffBufID )
         val numFFTs    = ((spec.numFrames + stepSize - 1) / stepSize).toInt // + 1
         val numWrites  = (numFFTs + coeffBufSize - 1) / coeffBufSize

         def tmpName( i: Int ) = new File( tmpDir, "feat" + i + ".aif" )

         val bufBndls = IndexedSeq.tabulate( numWrites ) { i =>
            val startFrame = i * coeffBufSize
            val stopFrame  = math.min( numFFTs, startFrame + coeffBufSize )
            val numFrames  = stopFrame - startFrame
            val msg        = coeffBuf.writeMsg( tmpName( i ).getAbsolutePath,
               AudioFileType.AIFF, SampleFormat.Float,
               if( i == 0 ) numFrames - 1 else numFrames, if( i == 0 ) 1 else 0, false )
//            val time       = (i + 1.5) * stepSize / coeffRate // spec.sampleRate

            // i don't know... in theory i should be writing the half a frame before or after,
            // but that causes trouble. so just write it exactly at the Stepper boundaries
            // and hope it scales up to long durations :-(
            val time       = (stopFrame - 0.0) / coeffRate
            OSCBundle.secs( time, msg )
         }

         val initBndl = OSCBundle.secs( 0.0,
            fftBuf.allocMsg( settings.fftSize ),
            coeffBuf.allocMsg( coeffBufSize, numCh ),
            df.recvMsg,
            syn.newMsg( df.name, s.rootNode )
         )

         val bndls   = initBndl +: bufBndls  // don't bother about n_free and b_free

         val c    = OSCPacketCodec.default
         val sz   = bndls.map( _.getEncodedSize( c )).max
         val raf  = new RandomAccessFile( oscF, "rw" )
         val bb   = ByteBuffer.allocate( sz )
         val fch  = raf.getChannel
         bndls.foreach { bndl =>
            bndl.encode( c, bb )
            bb.flip
            raf.writeInt( bb.limit )
            fch.write( bb )
            bb.clear
         }
         raf.close()

         val dur = OSCBundle.timetagToSecs( bufBndls.last.timetag )

         if( verbose ) println( "dur: " + dur.round(0.01) + "s ; numFFTs: " + numFFTs +
            "; numWrites: " + numWrites + "; coeffRate " + coeffRate.round(0.01) + " Hz" )

         val log = new ProcessLogger {
            var lastProg = -1
            def buffer[ T ]( f: => T ) : T = f  // ???
            def out( line: => String ) {
               if( line.startsWith( "nextOSCPacket" )) {
                  val time = line.substring( 14 ).toFloat
                  val prog = (time / dur * 80).toInt
                  if( prog != lastProg ) {
                     Act ! Progress( prog )  // up to 80%
                     lastProg = prog
                  }
               } else if( line != "start time 0" ) {
                  Console.out.println( line )
               }
            }
            def err( line: => String ) {
               Console.err.println( line )
            }
         }

         def shouldAbort : Boolean = this.synchronized { aborted }

         val proc = this.synchronized {
            if( shouldAbort ) return false
            val args = so.toNonRealtimeArgs
            if( verbose ) println( args.mkString( "cmd: ", " ", "" ))
            val pb = Process( args, Some( new File( so.programPath ).getParentFile ))
            p = pb.run( log )
            p
         }
         val res = proc.exitValue() // blocks
         this.synchronized {
            p = null
            if( shouldAbort ) return false
         }
         if( res != 0 ) throw new RuntimeException( "scsynth failed with exit code " + res )

         val afOutS     = AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, numCh, coeffRate )
         val afOut      = AudioFile.openWrite( settings.featureOutput, afOutS )
         for( i <- 0 until numWrites ) {
            if( shouldAbort ) return false
            val afIn    = AudioFile.openRead( tmpName( i ))
            val b       = afIn.frameBuffer( afIn.numFrames.toInt )
            val lasts   = new Array[ Float ]( afIn.numChannels )
            afIn.readFrames( b )
            afIn.close
            // deal with NaNs
            for( ch <- 0 until afIn.numChannels ) {
               val cb = b( ch )
               var last = lasts( ch )
               for( i <- 0 until cb.size ) {
                  val f = cb( i )
                  if( f.isNaN ) cb( i ) = last else last = f
               }
               lasts( ch ) = last
            }

            afOut.writeFrames( b )
            afIn.file.foreach( _.delete() )
            val prog = ((i + 1).toFloat / numWrites * 20).toInt + 80
            Act ! Progress( prog )
         }
         afOut.close

         settings.metaOutput.foreach { metaFile =>
            val xml = settings.toXML
            XML.save( metaFile.getAbsolutePath, xml, "UTF-8", true, null )
         }

         true
      }
   }

   private object Act extends Actor {
      def act() {
         ProcT.start()
         var result : Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.synchronized {
                     ProcT.aborted = true
                     if( ProcT.p != null ) ProcT.p.destroy()
                  }
               case res @ Progress( _ ) =>
                  observer( res )
               case res @ Aborted =>
                  result = res
               case res @ Failure( _ ) =>
                  result = res
               case res @ Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }
}