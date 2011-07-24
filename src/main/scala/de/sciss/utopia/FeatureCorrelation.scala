/*
 *  FeatureCorrelation.scala
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

import xml.XML
import de.sciss.synth.io.AudioFile
import collection.breakOut
import collection.immutable.{ SortedSet => ISortedSet }
import java.io.{RandomAccessFile, FilenameFilter, File}

object FeatureCorrelation extends ProcessorCompanion {

   type PayLoad = Match

   final case class Match( file: File, punchOut: Long, punchLen: Long, punchIn: Long )

   def apply( settings: Settings )( observer: Observer ) : FeatureCorrelation = {
      new FeatureCorrelation( settings, observer )
   }

   /** where temporal weight is between 0 (just spectral corr) and 1 (just temporal corr) */
   final case class Punch( span: Span, temporalWeight: Float = 0.5f )

   sealed trait SettingsLike {
      def databaseFolder : File
      def metaInput: File
      /** the span in the audio input serving for correlation to find the punch in material */
      def punchIn: Punch
      /** the span in the audio input serving for correlation to find the punch out material */
      def punchOut : Option[ Punch ]
      /** minimum length of the material to punch in */
      def minPunch: Long
      /** maximum length of the material to punch in */
      def maxPunch: Long
      def normalize : Boolean
   }

   final class SettingsBuilder extends SettingsLike {
      var databaseFolder   = new File( Utopia.defaultDir )
      var metaInput        = new File( "input_feat.xml" )
      var punchIn          = Punch( Span( 0L, 44100L ), 0.5f )
      var punchOut         = Option.empty[ Punch ]
      var minPunch         = 22050L
      var maxPunch         = 88200L
      var normalize        = true

      def build = Settings( databaseFolder, metaInput, punchIn, punchOut, minPunch, maxPunch, normalize )
   }

   object Settings {
      implicit def fromBuilder( sb: SettingsBuilder ) : Settings = sb.build
   }
   final case class Settings( databaseFolder: File, metaInput: File, punchIn: Punch, punchOut: Option[ Punch ],
                              minPunch: Long, maxPunch: Long, normalize: Boolean )
   extends SettingsLike

   private case class Sample( idx: Int, measure: Float ) extends Ordered[ Sample ] {
       def compare( that: Sample ) : Int = idx.compare( that.idx )
   }
   private val sampleOrd = Ordering.ordered[ Sample ]
}
final class FeatureCorrelation private ( settings: FeatureCorrelation.Settings,
                                         protected val observer: FeatureCorrelation.Observer ) extends Processor {
   protected val companion = FeatureCorrelation
   import companion._

   protected def body() : Result = {
      import FeatureExtraction.{ Settings => ExtrSettings }

      val extrIn     = ExtrSettings.fromXML( XML.loadFile( settings.metaInput ))
      val stepSize   = extrIn.fftSize / extrIn.fftOverlap

      def fullToFeat( n: Long ) = ((n + (stepSize >> 1)) / stepSize).toInt

      // collect all valid database files from the folder
      val punchMetas = settings.databaseFolder.listFiles( new FilenameFilter {
         def accept( dir: File, name: String ) = name.endsWith( "_feat.xml" )
      }).toSet - settings.metaInput

      // collect all database entries which match the input resolution
      // (for simplicity, we ignore the fact that the sample rates could differ)
      val extrDBs: IndexedSeq[ ExtrSettings ] = punchMetas.map( file => {
         val e = ExtrSettings.fromXML( XML.loadFile( file ))
         if( (e.numCoeffs == extrIn.numCoeffs) && (e.fftSize / e.fftOverlap == stepSize) ) Some( e ) else None
      })( breakOut ).collect { case Some( e ) => e }

      val normBuf = if( settings.normalize ) {
         val afNorm = AudioFile.openRead( new File( settings.databaseFolder, Utopia.NORMALIZE_NAME ))
         require( (afNorm.numChannels == extrIn.numCoeffs + 1) && afNorm.numFrames == 2L )
         val b = afNorm.frameBuffer( 2 )
         afNorm.readFrames( b )
         Some( b )
      } else None

      val afIn       = AudioFile.openRead( extrIn.featureOutput )

      def readInBuffer( punch: Punch ) : (Array[ Float ], Array[ Array[ Float ]]) = {
         val start   = fullToFeat( punch.span.start )
         val stop    = fullToFeat( punch.span.stop )
         val b       = afIn.frameBuffer( stop - start )
         afIn.seekFrame( start )
         afIn.readFrames( b )
         normBuf.foreach { n =>
            for( ch <- 0 until b.length ) {
               val cb   = b( ch )
               val cn   = n( ch )
               val min  = cn( 0 )
               val max  = cn( 1 )
               val d    = max - min
               for( i <- 0 until cb.length ) {
                  val f    = cb( i )
                  // XXX should values be clipped to [0...1] or not?
                  cb( i )  = (f - min) / d
               }
            }
         }
         (b( 0 ), b.drop( 1 ))
      }

      // Outline of Algorithm:
      // - read input feature in-span and out-span
      // - optionally normalize
      val (tempBufInIn, specBufInIn) = readInBuffer( settings.punchIn )
      val bufInOut = settings.punchOut.map( readInBuffer( _ ))
      afIn.close

      val punchInLen = tempBufInIn.length

      // - for each span:
      extrDBs foreach { extrDB =>
         val afExtr     = AudioFile.openRead( extrDB.featureOutput )
         //   - create a temp file
         //   - write the sliding xcorr to that file
         // A simple optimization could be to not begin writing the
         // temp file unless a punch-in correlation is found which is better
         // than the previous best match. This could also trigger
         // the punch-out measurement which could thus offset at
         // first_punch_in + min_punch_len
         var tmpFile    = Option.empty[ RandomAccessFile ]
         val b          = afExtr.frameBuffer( punchInLen )
         var left       = afExtr.numFrames
         var readSz     = punchInLen   // read full buffer in first round
         var readOff    = 0
         // - go through in-span file + for each sample check span in out-span file
         // - thus determine best match
         while( left > 0 ) {
            val chunkLen   = math.min( left, readSz ).toInt
            afExtr.readFrames( b, readOff, chunkLen )
            left   -= chunkLen
            readOff = (readOff + chunkLen) % punchInLen
            readSz  = 1 // read single frames in successive round (and rotate buffer)
         }
      }

      Aborted // XXX TODO
   }

   private def stat( mat: Array[ Array[ Float ]], frameOff: Int, frameLen: Int, chanOff: Int, chanLen: Int ) : (Double, Double) = {
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

   private def correlate( a: Array[ Array[ Float ]], aMean: Double, aStdDev: Double, aFrameOff: Int,
                          b: Array[ Array[ Float ]], bFrameOff: Int,
                          frameLen: Int, chanOff: Int, chanLen: Int ) : Float = {
      var sum = 0.0
//      val (amean, astddev) = stat( a, aFrameOff, frameLen, chanOff, chanLen )
      val (bMean, bStdDev) = stat( b, bFrameOff, frameLen, chanOff, chanLen )
      val aAdd = -aMean
      val aMul = 1.0 / aStdDev
      val bAdd = -bMean
      val bMul = 1.0 / bStdDev

      val chanStop   = chanOff + chanLen
      var ch = chanOff; while( ch < chanStop ) {
         val ca = a( ch )
         val cb = b( ch )
         var i = 0; while( i < frameLen ) {
            sum += ((ca( i + aFrameOff ) + aAdd) * aMul)  * ((cb( i + bFrameOff ) + bAdd) * bMul)
         i += 1 }
      ch += 1 }
      val matSize = frameLen * chanLen
      (sum / (matSize - 1)).toFloat
   }

   private def similarityAnalysis( anaClientBuf: Similarity.Mat, frameInteg: Int, maxResults: Int = 20,
                                   measure: Similarity.Mat => Float, rotateBuf: Boolean = false ) : ISortedSet[ Sample ] = {
      val buf        = anaClientBuf
      val numChannels= buf.numChannels
      val frames     = Similarity.Mat( frameInteg, numChannels )
      val numFrames  = buf.numFrames - frameInteg + 1
      var res        = ISortedSet.empty[ Sample ]( sampleOrd )
      var resCnt     = 0
      val frameIntegM= frameInteg - 1

      def karlheinz( idx: Int ) {
         val m = measure( frames )
         if( resCnt < maxResults ) {
            res += Sample( idx, m )
            resCnt += 1
         } else if( res.last.measure > m ) {
            res = res.dropRight( 1 ) + Sample( idx, m )
         }
      }

      if( numFrames > 0 ) {
         var x = 0; while( x < frameInteg ) {
            buf.getFrame( 0, frames.arr( x ))
         x += 1 }
         karlheinz( 0 )
      }
      var off = 1; while( off < numFrames ) {
//            val fm = frameMeasure( buf.getFrame( off, chanBuf ))
         if( rotateBuf ) {
            var y = 0; while( y < numChannels ) {
               var prev = frames.arr( 0 )( y )
               var x = frameIntegM; while( x >= 0 ) {   // ouch....
                  val tmp = frames.arr( x )( y )
                  frames.arr( x )( y ) = prev
                  prev = tmp
               x -= 1 }
            y += 1 }
            buf.getFrame( off, frames.arr( frameIntegM ))
         } else {
            buf.getFrame( off, frames.arr( (off - 1) % frameInteg ))
         }
         karlheinz( off )
      off += 1 }

      res
   }
}