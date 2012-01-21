/*
 *  FeatureExtraction.scala
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

import de.sciss.synth
import de.sciss.osc.{PacketCodec, Bundle}
import java.io.{RandomAccessFile, File}
import java.nio.ByteBuffer
import synth.io.{AudioFileSpec, AudioFileType, SampleFormat, AudioFile}
import sys.process.{ProcessLogger, Process}
import xml.{NodeSeq, XML}
import actors.Actor

object FeatureExtraction extends aux.ProcessorCompanion {
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
      /**
       * The input audio file to extract the features from.
       */
      def audioInput : File

      /**
       * The output "audio" file to write the feature vectors to
       * (this will be in AIFF format).
       */
      def featureOutput : File

      /**
       * An optional file to which the extraction settings are
       * saved (in XML format).
       */
      def metaOutput : Option[ File ]

      /**
       * The number of MFCC used for the spectral feature.
       */
      def numCoeffs : Int

      /**
       * The FFT size used to calculate the feature vectors.
       */
      def fftSize : Int

      /**
       * The FFT overlap factor used to step from vector to vector.
       * This equals fftSize / stepSize, so a value of 2 means
       * the window step is half of the fft size (windows are 50% overlapping).
       */
      def fftOverlap : Int

      /**
       * The channel behaviour determines how to handle multichannel files.
       * Currently the feature vectors are calculated on a mono signal only.
       * This setting determines whether multiple channels in the input audio
       * file are mixed together, or if just the first or just the last
       * channel is used in the extraction process.
       */
      def channelsBehavior : ChannelsBehavior
   }

   object SettingsBuilder {
      def apply() : SettingsBuilder = new SettingsBuilder
      def apply( settings: Settings ) : SettingsBuilder = {
         val sb = SettingsBuilder()
         sb.read( settings )
         sb
      }
   }
   final class SettingsBuilder private () extends SettingsLike {
      /**
       * The audio input defaults to `input.aif` (relative path)
       */
      var audioInput : File         = new File( "input.aif" )
      /**
       * The feature vector output file defaults to a temporary file
       * beginning with `features` and having suffix `.aif`.
       *
       * @see  Strugatzki#tmpDir
       */
      var featureOutput : File      = File.createTempFile( "features", ".aif", Strugatzki.tmpDir )
      /**
       * The extraction meta data file option defaults to `None`
       */
      var metaOutput                = Option.empty[ File ]
      /**
       * The number of MFCC defaults to 13.
       */
      var numCoeffs : Int           = 13
      /**
       * The FFT size defaults to 1024
       */
      var fftSize : Int             = 1024
      /**
       * The FFT overlap defaults to 2
       */
      var fftOverlap : Int          = 2
      /**
       * The multichannel behaviour defaults to `Mix`.
       */
      var channelsBehavior : ChannelsBehavior = ChannelsBehavior.Mix

      def build : Settings = Settings( audioInput, featureOutput, metaOutput, numCoeffs, fftSize, fftOverlap, channelsBehavior )

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
         val sb = SettingsBuilder()
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

   type PayLoad = Unit
}
final class FeatureExtraction private ( val settings: FeatureExtraction.Settings,
                                        protected val observer: PartialFunction[ FeatureExtraction.ProgressOrResult, Unit ])
extends aux.Processor {
   import FeatureExtraction._

   protected val companion = FeatureExtraction

   private var scsynth: Process = null

   protected def body() : Result = {
      import synth._
      import ugen._

      val spec             = AudioFile.readSpec( settings.audioInput )
      val stepSize         = settings.fftSize / settings.fftOverlap
      val coeffRate        = spec.sampleRate / stepSize

      val so               = Server.Config()
      val oscF             = createTempFile( "tmp", ".osc" )
      val dummyOutput      = createTempFile( "tmp", ".aif" )
      so.inputBusChannels  = spec.numChannels
      so.sampleRate        = spec.sampleRate.toInt // coeffRate.toInt
      so.outputBusChannels = 1
      so.nrtCommandPath    = oscF.getAbsolutePath
      so.nrtInputPath      = Some( settings.audioInput.getAbsolutePath )
      so.nrtOutputPath     = dummyOutput.getAbsolutePath
      so.nrtHeaderFormat   = AudioFileType.AIFF
      so.nrtSampleFormat   = SampleFormat.Int16

      val s                = Server.dummy( "nrt", so.build )
      val coeffBufSize     = 1024   // 888
      val numCh            = settings.numCoeffs + 1
      val fftBufID         = 0
      val coeffBufID       = 1
      val fftWinType       = 1   // -1 rect, 0 sine, 1 hann
      val df = SynthDef( "nrt" ) {
//            val in0        = In.ar( NumOutputBuses.ir, spec.numChannels )
//            val in         = settings.channelsBehavior match {
//               case ChannelsBehavior.Mix     => Mix( in0 )
//               case ChannelsBehavior.First   => in0.outputs.head
//               case ChannelsBehavior.Last    => in0.outputs.last
//            }
         val chanOff    = NumOutputBuses.ir
         val in         = settings.channelsBehavior match {
            case ChannelsBehavior.Mix     => Mix( In.ar( chanOff, spec.numChannels ))
            case ChannelsBehavior.First   => In.ar( chanOff, 1 )
            case ChannelsBehavior.Last    => In.ar( chanOff + spec.numChannels - 1, 1 )
         }
         val chain      = FFT( fftBufID, in, 1.0 / settings.fftOverlap, fftWinType )
         val coeffs     = MFCC.kr( chain, settings.numCoeffs )
         val loud       = Loudness.kr( chain ) / 32
         val trig       = Impulse.kr( coeffRate ) // - Impulse.kr( 0 )
         val phaseHi    = coeffBufSize - 1
         val phase      = Stepper.kr( trig, 0, 0, phaseHi, 1, phaseHi )
//            BufWr.kr( loud +: coeffs.outputs, coeffBufID, phase )
         BufWr.kr( Flatten( Seq( loud, coeffs )), coeffBufID, phase )
      }

      val syn        = Synth( s )
      val fftBuf     = new Buffer( s, fftBufID )
      val coeffBuf   = new Buffer( s, coeffBufID )
      val numFFTs    = ((spec.numFrames + stepSize - 1) / stepSize).toInt // + 1
      val numWrites  = (numFFTs + coeffBufSize - 1) / coeffBufSize

//      def tmpName( i: Int ) = new File( Strugatzki.tmpDir, "feat" + i + ".aif" )

      val tmpNames = IndexedSeq.fill( numWrites )( createTempFile( "feat_part", ".aif" ))

      val bufBndls = IndexedSeq.tabulate( numWrites ) { i =>
         val startFrame = i * coeffBufSize
         val stopFrame  = math.min( numFFTs, startFrame + coeffBufSize )
         val numFrames  = stopFrame - startFrame
         val msg        = coeffBuf.writeMsg( tmpNames( i ).getAbsolutePath,
            AudioFileType.AIFF, SampleFormat.Float,
            if( i == 0 ) numFrames - 1 else numFrames, if( i == 0 ) 1 else 0, false )
//            val time       = (i + 1.5) * stepSize / coeffRate // spec.sampleRate

         // i don't know... in theory i should be writing the half a frame before or after,
         // but that causes trouble. so just write it exactly at the Stepper boundaries
         // and hope it scales up to long durations :-(
         val time       = (stopFrame - 0.0) / coeffRate
         Bundle.secs( time, msg )
      }

      val initBndl = Bundle.secs( 0.0,
         fftBuf.allocMsg( settings.fftSize ),
         coeffBuf.allocMsg( coeffBufSize, numCh ),
         df.recvMsg,
         syn.newMsg( df.name, s.rootNode )
      )

      val bndls   = initBndl +: bufBndls  // don't bother about n_free and b_free

      val c    = PacketCodec().scsynth().build
      val sz   = bndls.map( _.encodedSize( c )).max
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

//         val dur = Bundle.timetagToSecs( bufBndls.last.timetag )
//      val dur = bufBndls.last.timetag.toSecs
      val dur = bndls.last.timetag.toSecs

      if( verbose ) println( "dur: " + dur.round(0.01) + "s ; numFFTs: " + numFFTs +
         "; numWrites: " + numWrites + "; coeffRate " + coeffRate.round(0.01) + " Hz" )

      val log = new ProcessLogger {
         def buffer[ T ]( f: => T ) : T = f  // ???
         def out( line: => String ) {
            if( line.startsWith( "nextOSCPacket" )) {
               val time = line.substring( 14 ).toFloat
               val prog = time / dur * 0.8  // up to 80%
               progress( prog.toFloat )
            } else if( line != "start time 0" ) {
               Console.out.println( line )
            }
         }
         def err( line: => String ) {
            Console.err.println( line )
         }
      }

      val proc = this.synchronized {
         if( checkAborted ) return Aborted
         val args = so.toNonRealtimeArgs
         if( verbose ) println( args.mkString( "cmd: ", " ", "" ))
         val pb = Process( args, Some( new File( so.programPath ).getParentFile ))
         scsynth = pb.run( log )
         scsynth
      }
      val res = proc.exitValue() // blocks
      this.synchronized {
         scsynth = null
         if( checkAborted ) return Aborted
      }
      if( res != 0 ) throw new RuntimeException( "scsynth failed with exit code " + res )
      if( verbose ) println( "scsynth exited with code " + res )

      val afOutS     = AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, numCh, coeffRate )
      val afOut      = AudioFile.openWrite( settings.featureOutput, afOutS )
      for( i <- 0 until numWrites ) {
         if( checkAborted ) return Aborted
         val afIn    = AudioFile.openRead( tmpNames( i ))
         val b       = afIn.buffer( afIn.numFrames.toInt )
         val lasts   = new Array[ Float ]( afIn.numChannels )
         afIn.read( b )
         afIn.close()
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

         afOut.write( b )
         afIn.file.foreach( _.delete() )
         val prog = ((i + 1).toFloat / numWrites * 0.2f) + 0.8f
         progress( prog )
      }
      afOut.close()

      settings.metaOutput.foreach { metaFile =>
         val xml = settings.toXML
         XML.save( metaFile.getAbsolutePath, xml, "UTF-8", true, null )
      }

      Success( () )
   }

   override protected def aborted() {
      if( scsynth != null ) {
         scsynth.destroy()
         scsynth = null
      }
   }

   // SCALAC FUCKING CHOKES ON companion.Result

   val Act = new Actor {
      def act() {
         ProcT.start()
         var result : Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.aborted = true
                  aborted()
               case res: Progress =>
                  observer( res )
               case res @ Aborted =>
                  result = res
               case res: Failure =>
                  result = res
               case res: Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }
}