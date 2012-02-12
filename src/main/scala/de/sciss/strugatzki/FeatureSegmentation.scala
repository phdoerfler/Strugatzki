/*
 *  FeatureSegmentation.scala
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

import java.io.File
import collection.immutable.{SortedSet => ISortedSet}
import de.sciss.synth.io.AudioFile
import xml.{NodeSeq, XML}
import actors.Actor

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
<break>
   <sim>{sim}</sim>
   <pos>{pos}</pos>
</break>

      def pretty : String = "Break( sim = " + sim + ", pos = " + pos + ")"
   }

   // sortedset orders ascending according to the ordering, and with this
   // ordering we will have low similarities (high dissimilarities)
   // at the head and high similarities at the tail
   private object BreakMaxOrd extends Ordering[ Break ] {
      def compare( a: Break, b: Break ) = a.sim compare b.sim
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

      /**
       * The XML file holding the extractor parameters corresponding to the
       * audio input file. The audio input file's feature vector output file
       * is determined from this meta file.
       */
      def metaInput: File

      /**
       * An option which restricts segmentation to a given span within the
       * input file. That is, only breaking points within this span are
       * reported. If `None`, the whole file is considered.
       */
      def span: Option[ Span ]

      /**
       * The size of the sliding window over which the features are correlated.
       * That is, for a length of 1.0 second (given in sample frames, hence
       * 44100 for a sample rate of 44100 Hz), at any given point in time,
       * 0.5 seconds left of that point are correlated with 0.5 seconds right
       * of that point. Breaking points are those where correlation is minimised.
       */
      def corrLen: Long

      /**
       * The balance between the feature of loudness curve and spectral composition (MFCC).
       * A value of 0.0 means the segmentation is only performed by considering the
       * spectral features, and a value of 1.0 means the segmentation is taking only
       * the loudness into consideration. Values in between give a measure that takes
       * both features into account with the given priorities.
       */
      def temporalWeight: Float
      /** Whether to apply normalization to the features (recommended) */
      def normalize : Boolean
      /** Maximum number of breaks to report */
      def numBreaks : Int
      /** Minimum spacing between breaks */
      def minSpacing : Long

      final def pretty: String = {
         "Settings(\n   databaseFolder = " + databaseFolder +
                  "\n   metaInput      = " + metaInput +
                  "\n   span           = " + span +
                  "\n   corrLen        = " + corrLen +
                  "\n   temporalWeight = " + temporalWeight +
                  "\n   normalize      = " + normalize +
                  "\n   numBreaks      = " + numBreaks +
                  "\n   minSpacing     = " + minSpacing + "\n)"
      }
   }

   object SettingsBuilder {
      def apply() : SettingsBuilder = new SettingsBuilder()
      def apply( settings: Settings ) : SettingsBuilder = {
         val sb = SettingsBuilder()
         sb.read( settings )
         sb
      }
   }
   final class SettingsBuilder private () extends SettingsLike {
      /**
       * The database folder defaults to `database` (relative path).
       */
      var databaseFolder      = new File( "database" )
      /**
       * The input file's extractor meta data file defaults to
       * `input_feat.xml` (relative path).
       */
      var metaInput           = new File( "input_feat.xml" )
      /**
       * The optional span restriction defaults to `None`.
       */
      var span                = Option.empty[ Span ]
      /**
       * The correlation length defaults to 22050 sample frames
       * (or 0.5 seconds at 44.1 kHz sample rate).
       */
      var corrLen             = 22050L
      /**
       * The temporal weight defaults to 0.5.
       */
      var temporalWeight      = 0.5f
      /**
       * The feature vector normalization flag defaults to `true`.
       */
      var normalize           = true
      /**
       * The number of breaking points reported defaults to 1.
       */
      var numBreaks           = 1
      /**
       * The minimum spacing between breaking points defaults to 22050 sample frames
       * (or 0.5 seconds at 44.1 kHz sample rate).
       */
      var minSpacing          = 22050L

      def build = Settings( databaseFolder, metaInput, span, corrLen, temporalWeight, normalize, numBreaks, minSpacing )

      def read( settings: Settings ) {
         databaseFolder = settings.databaseFolder
         metaInput      = settings.metaInput
         span           = settings.span
         corrLen        = settings.corrLen
         temporalWeight = settings.temporalWeight
         normalize      = settings.normalize
         numBreaks      = settings.numBreaks
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
         val sb = SettingsBuilder()
         sb.databaseFolder = new File( (xml \ "database").text )
         sb.metaInput      = new File( (xml \ "input").text )
         sb.span           = {
            val e = xml \ "span"
            if( e.isEmpty ) None else Some( spanFromXML( e ))
         }
         sb.corrLen        = (xml \ "corr").text.toLong
         sb.temporalWeight = (xml \ "weight").text.toFloat
         sb.normalize      = (xml \ "normalize").text.toBoolean
         sb.numBreaks      = (xml \ "numBreaks").text.toInt
         sb.minSpacing     = (xml \ "minSpacing").text.toLong
         sb.build
      }
   }
   final case class Settings( databaseFolder: File, metaInput: File, span: Option[ Span ], corrLen: Long,
                              temporalWeight: Float, normalize: Boolean, numBreaks: Int, minSpacing: Long )
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
   <corr>{corrLen}</corr>
   <weight>{temporalWeight}</weight>
   <normalize>{normalize}</normalize>
   <numBreaks>{numBreaks}</numBreaks>
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
         try {
            require( (afNorm.numChannels == extr.numCoeffs + 1) && afNorm.numFrames == 2L )
            val b = afNorm.buffer( 2 )
            afNorm.read( b )
            b
         } finally {
            afNorm.cleanUp()
         }
      } else null // None

      val halfWinLen = fullToFeat( settings.corrLen )
      val tempWeight = settings.temporalWeight

      var prio     = ISortedSet.empty[ Break ]( BreakMaxOrd )
      var lastBreak : Break = null

      def entryHasSpace = {
         prio.size < settings.numBreaks
      }

      def highestSim = {
         if( prio.nonEmpty ) prio.last.sim
         else 0f // Float.NegativeInfinity
      }

      // adds a break to the entry's priority queue. if the queue grows beyond numBreaks,
      // truncates the queue. if the match collides with a previous match that is closer
      // than minSpacing, it is either dropped (if the similarity is equal or smaller) or replaces
      // the previous match (if the similarity is greater).
      def addBreak( b: Break ) {
         if( (lastBreak != null) && ((b.pos - lastBreak.pos) < settings.minSpacing) ) {
            // gotta collapse them
            if( lastBreak.sim > b.sim ) {  // ok, replace previous match
               prio     -= lastBreak
               prio     += b
               lastBreak = b
            } // otherwise ignore the new match
         } else {
            prio     += b
            if( prio.size > settings.numBreaks ) {
               prio -= prio.last   // faster than dropRight( 1 ) ?
            }
            lastBreak = b
         }
      }

      val winLen  = halfWinLen * 2
      val eInBuf  = Array.ofDim[ Float ]( extr.numCoeffs + 1, winLen )

      val afExtr = AudioFile.openRead( extr.featureOutput )
      try {
         val (afStart, afStop) = settings.span match {
            case Some( span ) =>
               (math.max( 0, fullToFeat( span.start )), math.min( afExtr.numFrames.toInt, fullToFeat( span.stop )))
            case None =>
               (0, afExtr.numFrames.toInt)
         }
         val afLen = afStop - afStart

         if( afStart > 0 ) afExtr.seek( afStart )
         var left       = afLen // afExtr.numFrames
         var readSz     = winLen   // read full buffer in first round
         var readOff    = 0
         var logicalOff = 0
         var progBlock  = 0

         while( left > 0 ) {
            if( checkAborted ) return Aborted

            val chunkLen   = math.min( left, readSz ).toInt
            afExtr.read( eInBuf, readOff, chunkLen )
            val eInBufOff = logicalOff % winLen
            aux.Math.normalize( normBuf, eInBuf, readOff, chunkLen )
            val temporal = if( tempWeight > 0f ) {
               aux.Math.correlateHalf( 1, halfWinLen, eInBuf, eInBufOff, 0 )
            } else 0f
            val spectral = if( tempWeight < 1f ) {
               aux.Math.correlateHalf( extr.numCoeffs, halfWinLen, eInBuf, eInBufOff, 1 )
            } else 0f
            val sim = temporal * tempWeight + spectral * (1f - tempWeight)
            if( entryHasSpace || sim < highestSim ) {
               val pos     = featToFull( afStart + logicalOff + halfWinLen )
               val b       = Break( sim, pos )
               addBreak( b )
            }
            left   -= chunkLen
            readOff = (readOff + chunkLen) % winLen
            logicalOff += 1
            readSz  = 1 // read single frames in successive round (and rotate buffer)

            progBlock = (progBlock + 1) % 128
            if( progBlock == 0 ) progress( left.toFloat / afLen )
         }
         progress( 1f )

      } finally {
         afExtr.cleanUp()
      }

      val pay = prio.toIndexedSeq
      Success( pay )
   }

//   /*
//    * Perform cross correlation between two matrices a and b. A is supposed to be static,
//    * thus we expect to have its mean and standard deviation passed in. Both a and b
//    * can be larger than the actual matrix, by giving a frame offset and the number of frames,
//    * as well as a channel offset and number of channels to process.
//    *
//    * For efficiency reasons, b may be updated in a rotational manner, thus bFrame + frameLen
//    * may exceed the number of frames in b. The algorithm automatically takes the modulus
//    * `bFrame + frameLen % b.numFrames` as offset when doing the calculations.
//    */
//   private def correlate( numChannels: Int, halfWinSize: Int, a: Array[ Array[ Float ]], frameOff: Int, chanOff: Int ) : Float = {
//      val numFrames        = halfWinSize << 1
//      val (mean, stdDev)   = stat( a, 0, numFrames, chanOff, numChannels )
//      val add              = -mean
//      val matSize          = numChannels * halfWinSize
//
//      var sum = 0.0
//      var ch = 0; while( ch < numChannels ) {
//         val ca = a( ch + chanOff )
//         var i = frameOff; val j = frameOff + halfWinSize
//         while( i < j ) {
//            sum += (ca( i % numFrames ) + add) * (ca( (i + halfWinSize) % numFrames ) + add)
//         i += 1 }
//      ch += 1 }
//      (sum / (stdDev * stdDev * matSize)).toFloat  // ensures correlate( a, a ) == 1.0
//   }

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