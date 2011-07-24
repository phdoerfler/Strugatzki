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

//   private case class Sample( idx: Int, measure: Float ) extends Ordered[ Sample ] {
//       def compare( that: Sample ) : Int = idx.compare( that.idx )
//   }
//   private val sampleOrd = Ordering.ordered[ Sample ]

   private final case class FeatureMatrix( mat: Array[ Array[ Float ]], numFrames: Int, mean: Double, stdDev: Double ) {
      def numChannels = mat.length
      def matSize = numFrames * numChannels
   }
   private final case class InputMatrix( temporal: FeatureMatrix, spectral: FeatureMatrix ) {
      require( temporal.numFrames == spectral.numFrames )

      def numFrames : Int = temporal.numFrames
   }
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

      def readInBuffer( punch: Punch ) : InputMatrix = {
         val start      = fullToFeat( punch.span.start )
         val stop       = fullToFeat( punch.span.stop )
         val frameNum   = stop - start
         val b          = afIn.frameBuffer( frameNum )
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

         def feat( mat: Array[ Array[ Float ]]) = {
            val (mean, stdDev) = stat( mat, 0, frameNum, 0, mat.length )
            FeatureMatrix( mat, frameNum, mean, stdDev )
         }

         InputMatrix( feat( b.take( 1 )), feat( b.drop( 1 )))
      }

      // Outline of Algorithm:
      // - read input feature in-span and out-span
      // - optionally normalize
      val matrixIn         = readInBuffer( settings.punchIn )
      val matrixOutO       = settings.punchOut.map( readInBuffer( _ ))
      afIn.close

      val punchInLen       = matrixIn.numFrames
      val inTempWeight     = settings.punchIn.temporalWeight

      var minSim           = Float.NegativeInfinity
      var bestMeta : ExtrSettings = null
      var bestPunchIn      = 0
      var bestPunchOut     = 0

      def createTempFile( id: String ) : RandomAccessFile = {
         val file = File.createTempFile( "corr_" + id, ".bin" )
         val res  = new RandomAccessFile( file, "rw" )
         file.deleteOnExit()
         res
      }

      val minPunch = fullToFeat( settings.minPunch )
      val maxPunch = fullToFeat( settings.maxPunch )

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
         matrixOutO.foreach { mo => left -= minPunch + mo.numFrames }
         var readSz     = punchInLen   // read full buffer in first round
         var readOff    = 0
         var logicalOff = 0
         // - go through in-span file and calculate correlations
         while( left > 0 ) {
            val chunkLen   = math.min( left, readSz ).toInt
            afExtr.readFrames( b, readOff, chunkLen )
            val temporal = if( inTempWeight > 0f ) {
               correlate( matrixIn.temporal, b, logicalOff % punchInLen, 0 )
            } else 0f
            val spectral = if( inTempWeight < 1f ) {
               correlate( matrixIn.spectral, b, logicalOff % punchInLen, 1 )
            } else 0f
            val sim = temporal * inTempWeight + spectral * (1f - inTempWeight)
            if( matrixOutO.isDefined ) {
               val tf = tmpFile.getOrElse {
                  val res = createTempFile( "in" )
                  tmpFile = Some( res )
                  res
               }
               tf.writeInt( logicalOff )
               tf.writeFloat( sim )
            } else {
               if( sim > minSim ) {
                  minSim      = sim
                  bestMeta    = extrDB
                  bestPunchIn = logicalOff
               }
            }

            left   -= chunkLen
            readOff = (readOff + chunkLen) % punchInLen
            logicalOff += 1
            readSz  = 1 // read single frames in successive round (and rotate buffer)
         }

         // - if there is no punch-out, or if no minimally good correlations have been found,
         //   we're done, otherwise, calculate punch-out correlations
         (matrixOutO, settings.punchOut, tmpFile) match {
            case (Some( matrixOut ), Some( punchOut ), Some( tIn )) =>
               tIn.seek( 0L )
               val piOff0  = tIn.readInt()
               val poOff0  = piOff0 + minPunch   // this is the minimum offset where we begin correlation for punch-out
               val tOut    = createTempFile( "out" )

               left        = afExtr.numFrames - poOff0
               if( left > 0 ) {
                  val outTempWeight = punchOut.temporalWeight
                  afExtr.seekFrame( poOff0 )
                  val punchOutLen   = matrixOut.numFrames
                  readSz            = punchOutLen   // read full buffer in first round
                  readOff           = 0
                  logicalOff        = 0
                  // - go through out-span file and calculate correlations
                  while( left > 0 ) {
                     val chunkLen   = math.min( left, readSz ).toInt
                     afExtr.readFrames( b, readOff, chunkLen )
                     val temporal = if( outTempWeight > 0f ) {
                        correlate( matrixOut.temporal, b, logicalOff % punchOutLen, 0 )
                     } else 0f
                     val spectral = if( outTempWeight < 1f ) {
                        correlate( matrixOut.spectral, b, logicalOff % punchOutLen, 1 )
                     } else 0f
                     val sim = temporal * outTempWeight + spectral * (1f - outTempWeight)
                     tOut.writeFloat( sim )
                     left   -= chunkLen
                     readOff = (readOff + chunkLen) % punchOutLen
                     logicalOff += 1
                     readSz  = 1 // read single frames in successive round (and rotate buffer)
                  }

                  // - finally find the best match
                  tIn.seek( 0L )
                  left = tIn.length / 8
                  while( left > 0 ) {
                     val piOff   = tIn.readInt()
                     val inSim   = tIn.readFloat()
                     // minSim -- the best match so far, is now
                     // defined as min( inSim, outSim )
                     if( inSim > minSim ) {  // ... so this is a necessary condition to consider this offset
                        var poOff   = piOff + minPunch
                        tOut.seek( poOff0 + (piOff - piOff0) )
                        var left2   = math.max( (tOut.length - tOut.getFilePointer) / 4, maxPunch - minPunch + 1 )
                        while( left2 > 0 ) {
                           val outSim  = tOut.readFloat()
                           val sim     = math.min( inSim, outSim )
                           if( sim > minSim ) {
                              minSim         = sim
                              bestMeta       = extrDB
                              bestPunchIn    = piOff
                              bestPunchOut   = poOff

                              // shortcut (with the definition of minSim):
                              // if outSim >= inSim, the search is over for this round
                              // (because minSim is bound by inSim)
                              if( outSim >= inSim ) left2 = 0
                           }
                           left2 -= 1
                           poOff += 1
                        }
                     }
                     left -= 1
                  }
               }

            case _ =>
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

   /*
    * Perform cross correlation between two matrices a and b. A is supposed to be static,
    * thus we expect to have its mean and standard deviation passed in. Both a and b
    * can be larger than the actual matrix, by giving a frame offset and the number of frames,
    * as well as a channel offset and number of channels to process.
    *
    * For efficiency reasons, b may be updated in a rotational manner, thus bFrame + frameLen
    * may exceed the number of frames in b. The algorithm automatically takes the modulus
    * `bFrame + frameLen % b.numFrames` as offset when doing the calculations.
    */
   private def correlate( a: FeatureMatrix, b: Array[ Array[ Float ]], bFrameOff: Int, bChanOff: Int ) : Float = {
      val (bMean, bStdDev) = stat( b, bFrameOff, a.numFrames, bChanOff, a.numChannels )
      val aAdd = -a.mean
      val aMul = 1.0 / a.stdDev
      val bAdd = -bMean
      val bMul = 1.0 / bStdDev

      val numChannels   = a.numChannels
      val numFrames     = a.numFrames
      var sum           = 0.0
      var ch = 0; while( ch < numChannels ) {
         val ca = a.mat( ch )
         val cb = b( ch + bChanOff )
         var i = 0; while( i < numFrames ) {
            sum += ((ca( i ) + aAdd) * aMul)  * ((cb( (i + bFrameOff) % cb.length ) + bAdd) * bMul)
         i += 1 }
      ch += 1 }
      (sum / (a.matSize - 1)).toFloat
   }
}