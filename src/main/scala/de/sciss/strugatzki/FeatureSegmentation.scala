/*
*  FeatureSegmentation.scala
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

import collection.breakOut
import java.io.{FilenameFilter, File}
import actors.Actor
import collection.immutable.{SortedSet => ISortedSet}
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec, AudioFile}
import xml.{NodeSeq, XML}

/**
* A processor which performs segmentation on a given file.
* Returns a given number of best matches (maximum change in
* the feature vector).
*/
object FeatureSegmentation extends aux.ProcessorCompanion {
   /**
    * The result is a sequence of matches, sorted
    * by descending dissimilarity
    */
   type PayLoad = IndexedSeq[ Break ]

   object Break {
      def fromXML( xml: NodeSeq ) : Break = {
         val sim     = (xml \ "sim").text.toFloat
         val pos     = (xml \ "pos").text.toLong
         Break( sim, pos )
      }
   }
   final case class Break( sim: Float, pos: Long ) {
      def toXML =
<match>
   <sim>{sim}</sim>
   <pos>{pos}</pos>
</match>
   }

   // reverse ordering. since sortedset orders ascending according to the ordering,
   // this means we get a sortedset with high similarities at the head and low
   // similarities at the tail, like a priority queue
   private object MatchMinOrd extends Ordering[ Break ] {
      def compare( a: Break, b: Break ) = b.sim compare a.sim
   }

   def apply( settings: Settings )( observer: Observer ) : FeatureSegmentation = {
      new FeatureSegmentation( settings, observer )
   }

   /**
    * All durations, spans and spacings are given in sample frames
    * with respect to the sample rate of the audio input file.
    */
   sealed trait SettingsLike {
      /**
       * The database folder is merely used to retrieve the normalization file,
       * given that `normalize` is `true`.
       */
      def databaseFolder : File
      def metaInput: File
      def span: Option[ Span ]
      def punch: Long
      def temporalWeight: Float
      /** Whether to apply normalization to the features (recommended) */
      def normalize : Boolean
      /** Maximum number of breaks to report */
      def numMatches : Int
      /** Minimum spacing between breaks */
      def minSpacing : Long
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
      var databaseFolder      = new File( "database" )
      var metaInput           = new File( "input_feat.xml" )
      var span                = Option.empty[ Span ]
      var punch               = 22050L
      var temporalWeight      = 0.5f
      var normalize           = true
      var numMatches          = 1
      var minSpacing          = 22050L

      def build = Settings( databaseFolder, metaInput, span, punch, temporalWeight, normalize, numMatches, minSpacing )

      def read( settings: Settings ) {
         databaseFolder = settings.databaseFolder
         metaInput      = settings.metaInput
         span           = settings.span
         punch          = settings.punch
         temporalWeight = settings.temporalWeight
         normalize      = settings.normalize
         numMatches     = settings.numMatches
         minSpacing     = settings.minSpacing
      }
   }

   object Settings {
      implicit def fromBuilder( sb: SettingsBuilder ) : Settings = sb.build

      private def spanFromXML( xml: NodeSeq ) : Span = {
         val start   = (xml \ "start").text.toLong
         val stop    = (xml \ "stop").text.toLong
         Span( start, stop )
      }

      def fromXMLFile( file: File ) : Settings = fromXML( XML.loadFile( file ))
      def fromXML( xml: NodeSeq ) : Settings = {
         val sb = new SettingsBuilder
         sb.databaseFolder = new File( (xml \ "database").text )
         sb.metaInput      = new File( (xml \ "input").text )
         sb.span           = {
            val e = xml \ "span"
            if( e.isEmpty ) None else Some( spanFromXML( e ))
         }
         sb.punch          = (xml \ "punch").text.toLong
         sb.temporalWeight = (xml \ "weight").text.toFloat
         sb.normalize      = (xml \ "normalize").text.toBoolean
         sb.numMatches     = (xml \ "numMatches").text.toInt
         sb.minSpacing     = (xml \ "minSpacing").text.toLong
         sb.build
      }
   }
   final case class Settings( databaseFolder: File, metaInput: File, span: Option[ Span ], punch: Long,
                              temporalWeight: Float, normalize: Boolean, numMatches: Int, minSpacing: Long )
   extends SettingsLike {
      private def spanToXML( span: Span ) =
<span>
   <start>{span.start}</start>
   <stop>{span.stop}</stop>
</span>

      def toXML =
<segmentation>
   <database>{databaseFolder.getPath}</database>
   <input>{metaInput.getPath}</input>
   {span match { case Some( s ) => <span>{spanToXML( s ).child}</span>; case _ => Nil }}
   <punch>{punch}</punch>
   <weight>{temporalWeight}</weight>
   <normalize>{normalize}</normalize>
   <numMatches>{numMatches}</numMatches>
   <minSpacing>{minSpacing}</minSpacing>
</segmentation>
   }

//   private final case class FeatureMatrix( mat: Array[ Array[ Float ]], numFrames: Int, mean: Double, stdDev: Double ) {
//      def numChannels = mat.length
//      def matSize = numFrames * numChannels
//   }
//   private final case class InputMatrix( temporal: FeatureMatrix, spectral: FeatureMatrix, lnAvgLoudness: Double ) {
//      require( temporal.numFrames == spectral.numFrames )
//
//      def numFrames : Int = temporal.numFrames
//   }
}
final class FeatureSegmentation private ( settings: FeatureSegmentation.Settings,
                                         protected val observer: FeatureSegmentation.Observer ) extends aux.Processor {
   import FeatureSegmentation._

   protected val companion = FeatureSegmentation

   protected def body() : Result = {
      import FeatureExtraction.{ Settings => ExtrSettings }

      val extr       = ExtrSettings.fromXML( XML.loadFile( settings.metaInput ))
      val stepSize   = extr.fftSize / extr.fftOverlap

      def fullToFeat( n: Long ) = ((n + (stepSize >> 1)) / stepSize).toInt
      def featToFull( i: Int )  = i.toLong * stepSize

      val normBuf = if( settings.normalize ) {
         val afNorm = AudioFile.openRead( new File( settings.databaseFolder, Strugatzki.NORMALIZE_NAME ))
         require( (afNorm.numChannels == extr.numCoeffs + 1) && afNorm.numFrames == 2L )
         val b = afNorm.buffer( 2 )
         afNorm.read( b )
         b
      } else null // None

      def normalize( /* n: Array[ Array[ Float ]], */ b: Array[ Array[ Float ]], bOff: Int, bLen: Int ) {
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

      val halfWinLen: Int = sys.error( "TODO" )
      val tempWeight     = settings.temporalWeight

      var prio     = ISortedSet.empty[ Break ]( MatchMinOrd )
      var lastBreak : Break = null

      def entryHasSpace = {
         prio.size < settings.numMatches
      }

      def worstSim = {
         if( prio.nonEmpty ) prio.last.sim
         else 0f // Float.NegativeInfinity
      }

      // adds a match to the entry's priority queue. if the queue grows beyond numPerFile,
      // truncates the queue. if the match collides with a previous match that is closer
      // than minSpacing, it is either dropped (if the similarity is equal or smaller) or replaces
      // the previous match (if the similarity is greater).
      def addBreak( b: Break ) {
         if( (lastBreak != null) && ((b.pos - lastBreak.pos) < settings.minSpacing) ) {
            // gotta collapse them
            if( lastBreak.sim < b.sim ) {  // ok, replace previous match
               prio     -= lastBreak
               prio     += b
               lastBreak = b
            } // otherwise ignore the new match
         } else {
            prio     += b
            if( prio.size > settings.numMatches ) {
               prio -= prio.last   // faster than dropRight( 1 ) ?
            }
            lastBreak = b
         }
      }

      val eInBuf  = Array.ofDim[ Float ]( extr.numCoeffs + 1, halfWinLen )
      val winLen = halfWinLen * 2

      val afExtr = AudioFile.openRead( extr.featureOutput )
      try {
         var left       = afExtr.numFrames
         var readSz     = winLen   // read full buffer in first round
         var readOff    = 0
         var logicalOff = 0
         // - go through in-span file and calculate correlations
         while( left > 0 ) {

            if( checkAborted ) return Aborted

            val chunkLen   = math.min( left, readSz ).toInt
            afExtr.read( eInBuf, readOff, chunkLen )
            val eInBufOff = logicalOff % winLen
            normalize( eInBuf, readOff, chunkLen )
            val temporal = if( tempWeight > 0f ) {
               correlate( eInBuf, eInBufOff, 0 )
            } else 0f
            val spectral = if( tempWeight < 1f ) {
               correlate( eInBuf, eInBufOff, 1 )
            } else 0f
            val sim = temporal * tempWeight + spectral * (1f - tempWeight)
            if( entryHasSpace || sim > worstSim ) {
               val pos     = featToFull( logicalOff + halfWinLen )
               val b       = Break( sim, pos )
               addBreak( b )
            }
            left   -= chunkLen
            readOff = (readOff + chunkLen) % winLen
            logicalOff += 1
            readSz  = 1 // read single frames in successive round (and rotate buffer)
         }

      } finally {
         sys.error( "TODO" )
      }

//      // - for each span:
//      extrDBs.zipWithIndex foreach { case (extrDB, extrIdx) =>
//
//         if( checkAborted ) return Aborted
//
//         if( entryPrio.nonEmpty ) entryPrio = entryPrio.empty
//         lastEntryMatch = null
//
//         val afExtr = AudioFile.openRead( extrDB.featureOutput )
//         try {
//            //   - create a temp file
//            //   - write the sliding xcorr to that file
//            // A simple optimization could be to not begin writing the
//            // temp file unless a punch-in correlation is found which is better
//            // than the previous best match. This could also trigger
//            // the punch-out measurement which could thus offset at
//            // first_punch_in + min_punch_len
//            var tInOpen    = false
//            var tInOff     = 0
//            var tInBufOff  = 0
//            var left       = afExtr.numFrames
//            matrixOutO.foreach { mo => left -= minPunch /* + mo.numFrames */}
//            var readSz     = punchInLen   // read full buffer in first round
//            var readOff    = 0
//            var logicalOff = 0
//            // - go through in-span file and calculate correlations
//            while( left > 0 ) {
//
//               if( checkAborted ) return Aborted
//
//               val chunkLen   = math.min( left, readSz ).toInt
//               afExtr.read( eInBuf, readOff, chunkLen )
//               val eInBufOff = logicalOff % punchInLen
//               normalize( eInBuf, readOff, chunkLen )
//               val boost = calcBoost( matrixIn, eInBuf( 0 ))
//               val sim = if( boost <= settings.maxBoost ) {
//                  val temporal = if( inTempWeight > 0f ) {
//                     correlate( matrixIn.temporal, eInBuf, eInBufOff, 0 )
//                  } else 0f
//                  val spectral = if( inTempWeight < 1f ) {
//                     correlate( matrixIn.spectral, eInBuf, eInBufOff, 1 )
//                  } else 0f
//                  temporal * inTempWeight + spectral * (1f - inTempWeight)
//               } else {
//                  0f // Float.NegativeInfinity
//               }
//
//               if( matrixOutO.isDefined ) {
//                  if( tInOpen || entryHasSpace || sim > worstSim ) {
//                     if( !tInOpen ) {
//                        if( tIn == null ) {
//                           tIn = createTempAudioFile( "in", 2 )
//                        } else {
//                           tIn.seek( 0L )
//                        }
//                        tInOff = logicalOff
//                        tInOpen= true
//                     }
//                     tInBuf( 0 )( tInBufOff ) = sim
//                     tInBuf( 1 )( tInBufOff ) = boost
//                     tInBufOff += 1
//                     // flush
//                     if( tInBufOff == 1024 ) {
//                        tIn.write( tInBuf, 0, tInBufOff )
//                        tInBufOff = 0
//                     }
//                  }
//               } else {
//                  if( entryHasSpace || sim > worstSim ) {
//                     val start   = featToFull( logicalOff )
//                     val stop    = featToFull( logicalOff + punchInLen )
//                     val b       = Break( sim, extrDB.audioInput, Span( start, stop ), boost, 1f )
//                     addMatch( b )
//                  }
//               }
//
//               left   -= chunkLen
//               readOff = (readOff + chunkLen) % punchInLen
//               logicalOff += 1
//               readSz  = 1 // read single frames in successive round (and rotate buffer)
//            }
//
//            // - if there is no punch-out, or if no minimally good correlations have been found,
//            //   we're done, otherwise, calculate punch-out correlations
//            (matrixOutO, settings.punchOut, tInOpen) match {
//               case (Some( matrixOut ), Some( punchOut ), true) =>
//                  // flush
//                  if( tInBufOff > 0 ) {
//                     tIn.write( tInBuf, 0, tInBufOff )
//                     tInBufOff = 0
//                  }
//
//                  tIn.seek( 0L )
//
//                  val poOff0  = tInOff + minPunch
//
//                  left        = afExtr.numFrames - (poOff0 /*+ matrixOut.numFrames */)
//                  if( left >= matrixOut.numFrames ) {  // means we actually do at least one full correlation
//                     if( tOut == null ) {
//                        tOut = createTempAudioFile( "out", 2 )
//                     } else {
//                        tOut.seek( 0L )
//                     }
//
//                     val outTempWeight = punchOut.temporalWeight
//                     afExtr.seek( poOff0 )
//                     readSz            = punchOutLen   // read full buffer in first round
//                     readOff           = 0
//                     logicalOff        = 0
//                     // - go through out-span file and calculate correlations
//
//                     var tOutBufOff    = 0
//                     val tOutSize      = left
//                     while( left > 0 ) {
//
//                        if( checkAborted ) return Aborted
//
//                        val chunkLen   = math.min( left, readSz ).toInt
//                        afExtr.read( eOutBuf, readOff, chunkLen )
//                        normalize( eOutBuf, readOff, chunkLen )
//                        val extraBufOff = logicalOff % punchOutLen
//                        val boost = calcBoost( matrixOut, eOutBuf( 0 ))
//                        val sim = if( boost <= settings.maxBoost ) {
//                           val temporal = if( outTempWeight > 0f ) {
//                              correlate( matrixOut.temporal, eOutBuf, extraBufOff, 0 )
//                           } else 0f
//                           val spectral = if( outTempWeight < 1f ) {
//                              correlate( matrixOut.spectral, eOutBuf, extraBufOff, 1 )
//                           } else 0f
//                           temporal * outTempWeight + spectral * (1f - outTempWeight)
//                        } else {
//                           0f // Float.NegativeInfinity
//                        }
//
//                        tOutBuf( 0 )( tOutBufOff ) = sim
//                        tOutBuf( 1 )( tOutBufOff ) = boost
//                        tOutBufOff += 1
//                        if( tOutBufOff == 1024 ) { // flush
//                           tOut.write( tOutBuf, 0, tOutBufOff )
//                           tOutBufOff = 0
//                        }
//
//                        left   -= chunkLen
//                        readOff = (readOff + chunkLen) % punchOutLen
//                        logicalOff += 1
//                        readSz  = 1 // read single frames in successive round (and rotate buffer)
//                     }
//                     // flush
//                     if( tOutBufOff > 0 ) {
//                        tOut.write( tOutBuf, 0, tOutBufOff )
//                        tOutBufOff = 0
//                     }
//
//                     // - finally find the best match
//                     left = afExtr.numFrames - poOff0
//                     tInBufOff   = 1024
//                     var piOff   = tInOff
//                     while( left > 0 ) {
//
//                        if( checkAborted ) return Aborted
//
//                        if( tInBufOff == 1024 ) {
//                           tIn.read( tInBuf, 0, math.min( 1024, left ).toInt )
//                           tInBufOff = 0
//                        }
//
//                        val inSim   = tInBuf( 0 )( tInBufOff )
//                        val boostIn = tInBuf( 1 )( tInBufOff )
//
//                        // worstSim is now
//                        // defined as min( inSim, outSim )
//                        var ws = worstSim       // cache it here
//                        var hs = entryHasSpace  // cahce it here
//                        if( inSim > (ws * ws) ) { // sqrt( inSim * 1 ) > ws
//                           var poOff   = piOff + minPunch
//                           // note: there is room for further optimization:
//                           // we might track in this iteration the best sim
//                           // in tOut, and in the next iteration, if this
//                           // best sim is too bad -- we can just skip over
//                           // the whole previous search span!
//                           val tOutSeek = piOff - tInOff // = numRead from tIn !
//                           tOut.seek( tOutSeek )
//
//                           var left2   = math.min( tOutSize - tOutSeek, maxPunch - minPunch + 1 )
//                           while( left2 > 0 ) {
//
//                              if( checkAborted ) return Aborted
//
//                              val chunkLen = math.min( left2, 1024 ).toInt
//                              tOut.read( tOutBuf, 0, chunkLen )
//
//                              var chunkOff = 0; while( chunkOff < chunkLen ) {
//                                 val outSim  = tOutBuf( 0 )( chunkOff )
//                                 val boostOut= tOutBuf( 1 )( chunkOff )
//
//                                 // ok, let's try geometric mean, meaning that
//                                 // in the case of inSim < outSim, the result
//                                 // could still be differentiated among several
//                                 // outSim! (which would be lost in the case of min( inSim, outSim )
//                                 val sim = math.sqrt( inSim * outSim ).toFloat
//                                 if( hs || sim > ws ) {
//                                    val b = Break( sim, extrDB.audioInput,
//                                       Span( featToFull( piOff ), featToFull( poOff )), boostIn, boostOut )
//                                    addMatch( b )
//                                    // clear cache
//                                    ws = worstSim
//                                    hs = entryHasSpace
//                                 }
//                                 chunkOff += 1
//                                 poOff += 1
//                              }
//                              left2 -= chunkLen // 1
//                           }
//                        }
//                        left -= 1
//                        tInBufOff += 1
//                        piOff += 1
//                     }
//                  }
//
//               case _ =>
//            }
//         } finally {
//            afExtr.close()
//         }
//
//         // - add iter-prio to total-prio, and truncate after num-matches elements
//         allPrio ++= entryPrio
//         if( allPrio.size > settings.numMatches ) allPrio = allPrio.take( settings.numMatches )
//
//         progress( (extrIdx + 1).toFloat / extrDBs.size )
//      }

      val pay = prio.toIndexedSeq
      Success( pay )
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
   private def correlate( a: Array[ Array[ Float ]], bFrameOff: Int, bChanOff: Int ) : Float = {
      sys.error( "TODO" )
//      val numChannels   = a.numChannels
//      val numFrames     = a.numFrames
//      // note: stat does not wrap frame offset around b.numFrames.
//      // we thus assume that b really has data from 0 to a.numFrames!
//      val (bMean, bStdDev) = stat( b, 0 /* FrameOff */, numFrames, bChanOff, numChannels )
//      val aAdd = -a.mean
//      val bAdd = -bMean
//
//      var sum           = 0.0
//      var ch = 0; while( ch < numChannels ) {
//         val ca = a.mat( ch )
//         val cb = b( ch + bChanOff )
//         var i = 0; while( i < numFrames ) {
//            sum += (ca( i ) + aAdd) * (cb( (i + bFrameOff) % cb.length ) + bAdd)
//         i += 1 }
//      ch += 1 }
//      (sum / (a.stdDev * bStdDev * a.matSize)).toFloat  // ensures correlate( a, a ) == 1.0
   }
}