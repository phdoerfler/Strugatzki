/*
 *  SelfSimilarity.scala
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
import aux.{ProcessorCompanion, Processor}
import actors.Actor
import de.sciss.synth.io.AudioFile
import xml.{XML, NodeSeq}
import java.awt.image.{DataBufferInt, BufferedImage}
import javax.imageio.ImageIO

object SelfSimilarity extends ProcessorCompanion {
   type PayLoad = Unit

   object ColorScheme {
      def apply( logicalName: String ) : ColorScheme = logicalName match {
         case GrayScale.logicalName       => GrayScale
         case PsychoOptical.logicalName   => PsychoOptical
      }

      def all : Seq[ ColorScheme ] = Seq( GrayScale, PsychoOptical )
      def names : Seq[ String ] = all.map( _.logicalName )
   }
   sealed trait ColorScheme {
      def logicalName: String
   }

   /**
    * Plain gray scale.
    */
   case object GrayScale extends ColorScheme {
      val logicalName = "gray"
   }
   /**
    * Psycho-optically optimized scheme originally taken from
    * Niklas Werner's Sonasound program. Uses a combination of
    * hues and brightnesses that produces as perceptually very
    * regular gradient. It goes from black over violet towards
    * yellow and finally white.
    */
   case object PsychoOptical extends ColorScheme {
      val logicalName = "psycho"
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
       * The file to which the self similarity matrix image is written.
       */
      def imageOutput: File

      /**
       * An option which restricts the calculation to a given span within the
       * input file. If `None`, the whole file is considered.
       */
      def span: Option[ Span ]

      /**
       * The size of the sliding window over which the features are correlated.
       * That is, for a length of 1.0 second (given in sample frames, hence
       * 44100 for a sample rate of 44100 Hz), at any given point in time,
       * 0.5 seconds left of that point are correlated with 0.5 seconds right
       * of that point.
       */
      def corrLen: Long

      /**
       * A decimation factor to produce smaller image size. A factor of 1 means
       * each frame step is performed, a factor of 2 means every second frame
       * is skipped, a factor of 3 means only one in three frames is considered,
       * and so forth.
       */
      def decimation: Int

      /**
       * The balance between the feature of loudness curve and spectral composition (MFCC).
       * A value of 0.0 means the segmentation is only performed by considering the
       * spectral features, and a value of 1.0 means the segmentation is taking only
       * the loudness into consideration. Values in between give a measure that takes
       * both features into account with the given priorities.
       */
      def temporalWeight: Float

      /**
       * The color scheme to use for the image. Either of `GrayScale` and `PsychoOptical`
       */
      def colors: ColorScheme

      /**
       * A warp factor (exponent) applied to the cross correlations before conversion to
       * a color value. Somewhat like a gamma correction. Values smaller than 1 produce
       * brighter images, values greater than 1 produce darker images.
       */
      def colorWarp: Float

      /**
       * The ceiling cross correlation value corresponding to the maximally bright color.
       * Should be less than or equal to 1, and greater than zero. The smaller the value,
       * the earlier clipping occurs, and the more the colors are 'dragged' towards
       * brighter values.
       */
      def colorCeil: Float

      /**
       * Whether to apply normalization to the features (recommended)
       * */
      def normalize : Boolean

      final def pretty: String = {
         "Settings(\n   databaseFolder = " + databaseFolder +
                  "\n   metaInput      = " + metaInput +
                  "\n   imageOutput    = " + imageOutput +
                  "\n   span           = " + span +
                  "\n   corrLen        = " + corrLen +
                  "\n   decimation     = " + decimation +
                  "\n   temporalWeight = " + temporalWeight +
                  "\n   colors         = " + colors +
                  "\n   colorWarp      = " + colorWarp +
                  "\n   colorCeil      = " + colorCeil +
                  "\n   normalize      = " + normalize + "\n)"
      }
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
       * The database folder defaults to `database` (relative path)
       */
      var databaseFolder      = new File( "database" ) // Strugatzki.defaultDir
      /**
       * The correlation input file's extractor meta data file defaults
       * to `input_feat.xml` (relative path)
       */
      var metaInput           = new File( "input_feat.xml" )
      /**
       * The image output file defaults to `output_selfsim.png` (relative path)
       */
      var imageOutput         = new File( "output_selfsim.png" )
      /**
       * The optional span restriction defaults to `None`.
       */
      var span                = Option.empty[ Span ]
      /**
       * The correlation length defaults to 44100 sample frames
       * (or 1.0 seconds at 44.1 kHz sample rate).
       */
      var corrLen             = 44100L
      /**
       * The default decimation factor is `1` (no decimation).
       */
      var decimation          = 1
      /**
       * The temporal weight defaults to 0.5.
       */
      var temporalWeight      = 0.5f
      /**
       * The default color scheme is psycho-optical.
       */
      var colors              = PsychoOptical: ColorScheme
      /**
       * The default color warp is `1.0`.
       */
      var colorWarp           = 1.0f
      /**
       * The default color ceiling is `1.0`.
       */
      var colorCeil           = 1.0f
      /**
       * The feature vector normalization flag defaults to `true`.
       */
      var normalize           = true

      def build = Settings( databaseFolder, metaInput, imageOutput, span, corrLen, decimation, temporalWeight,
                            colors, colorWarp, colorCeil, normalize )

      def read( settings: Settings ) {
         databaseFolder = settings.databaseFolder
         metaInput      = settings.metaInput
         imageOutput    = settings.imageOutput
         span           = settings.span
         corrLen        = settings.corrLen
         decimation     = settings.decimation
         temporalWeight = settings.temporalWeight
         colors         = settings.colors
         colorWarp      = settings.colorWarp
         colorCeil      = settings.colorCeil
         normalize      = settings.normalize
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
         sb.imageOutput    = new File( (xml \ "output").text )
         sb.span           = {
            val e = xml \ "span"
            if( e.isEmpty ) None else Some( spanFromXML( e ))
         }
         sb.corrLen        = (xml \ "corr").text.toLong
         sb.decimation     = (xml \ "decimation").text.toInt
         sb.temporalWeight = (xml \ "weight").text.toFloat
         sb.colors         = ColorScheme( (xml \ "colors").text )
         sb.colorWarp      = (xml \ "colorWarp").text.toFloat
         sb.colorCeil      = (xml \ "colorCeil").text.toFloat
         sb.normalize      = (xml \ "normalize").text.toBoolean
         sb.build
      }
   }
   final case class Settings( databaseFolder: File, metaInput: File, imageOutput: File, span: Option[ Span ],
                              corrLen: Long, decimation: Int, temporalWeight: Float,
                              colors: ColorScheme, colorWarp: Float, colorCeil: Float, normalize: Boolean )
   extends SettingsLike {
      private def spanToXML( span: Span ) =
<span>
   <start>{span.start}</start>
   <stop>{span.stop}</stop>
</span>

      def toXML =
<selfsimilarity>
   <database>{databaseFolder.getPath}</database>
   <input>{metaInput.getPath}</input>
   <output>{imageOutput.getPath}</output>
   {span match { case Some( s ) => <span>{spanToXML( s ).child}</span>; case _ => Nil }}
   <corr>{corrLen}</corr>
   <decimation>{decimation}</decimation>
   <weight>{temporalWeight}</weight>
   <colors>{colors.logicalName}</colors>
   <colorWarp>{colorWarp}</colorWarp>
   <colorCeil>{colorCeil}</colorCeil>
   <normalize>{normalize}</normalize>
</selfsimilarity>
   }

   /**
    * Create a new process to calculate the self similarity matrix of a sound file and save it as an image file.
    *
    * @param settings         the settings for the process (e.g. input and output files)
    * @param observer         function observing the progress of the running process
    * @return                 the self similarity matrix image generating process (to be started)
    */
   def apply( settings: Settings )( observer: Observer ) =
      new SelfSimilarity( observer, settings )
}
class SelfSimilarity( protected val observer: SelfSimilarity.Observer, settings: SelfSimilarity.Settings )
extends Processor {
   import SelfSimilarity._

   protected val companion = SelfSimilarity

   protected def body() : Result = {
      val extr          = FeatureExtraction.Settings.fromXMLFile( settings.metaInput )
      val stepSize      = extr.fftSize / extr.fftOverlap

      def fullToFeat( n: Long ) = ((n + (stepSize >> 1)) / stepSize).toInt

      val halfWinLen    = fullToFeat( settings.corrLen )
      val winLen        = halfWinLen * 2

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

      val tempWeight    = settings.temporalWeight
      val eInBuf        = Array.ofDim[ Float ]( extr.numCoeffs + 1, winLen )

      val afExtr = AudioFile.openRead( extr.featureOutput )
      try {
         val (afStart, afStop) = settings.span match {
            case Some( span ) =>
               (math.max( 0, fullToFeat( span.start )), math.min( afExtr.numFrames.toInt, fullToFeat( span.stop )))
            case None =>
               (0, afExtr.numFrames.toInt)
         }

         val afLen = afStop - afStart

         val numCorrs      = {
            val n = math.max( 0L, afLen - winLen + 1 )
//            require( n <= 0xB504, "32-bit overflow" )
            require( n <= 0x7FFFFFFF, "32-bit overflow" )
            n.toInt
         }
         val (decim, imgExt)  = {
            val d = settings.decimation
            require( d >= 1, "Illegal decimation setting of " + d )
            val i = numCorrs / d
            if( i <= 0xB504 ) (d, i) else {
               val d1 = (numCorrs + 0xB503) / 0xB504
               println( "Warning: Decimation is too small to produce a reasonable image size. Automatically adjusting to " + d1 )
               (d1, numCorrs / d1)
            }
         }
//         require( imgExt < 0xB504, "Image size too large. Try a decimation of " + ((numCorrs + 0xB503) / 0xB504) )
         val numPix     = imgExt.toLong * imgExt.toLong
         val imgExtM1   = imgExt - 1

         if( verbose ) println( "Image extent is " + imgExt + " (yielding a matrix of " + numPix + " pixels)" )

         val colorFun: Float => Int = settings.colors match {
            case GrayScale => (sim: Float) => {
               val i = math.max( 0, math.min( 255, (sim * 255 + 0.5).toInt ))
               (i << 16) | (i << 8) | i
            }

            case PsychoOptical => aux.IntensityColorScheme.apply _
         }
         require( settings.colorWarp > 0, "Illegal color warp setting. Must be > 0, but is " + settings.colorWarp )
         require( settings.colorCeil > 0, "Illegal color ceil setting. Must be > 0, but is " + settings.colorCeil )
         val colorWarp  = settings.colorWarp
         val colorScale = 1.0f / settings.colorCeil

         val img     = new BufferedImage( imgExt, imgExt, BufferedImage.TYPE_INT_RGB )
         val imgData = img.getRaster.getDataBuffer.asInstanceOf[ DataBufferInt ].getData()
         val g       = img.createGraphics()
         try {
            
            if( afStart > 0 ) afExtr.seek( afStart )
            var leftOff    = 0
            var progBlock  = 0
            val stop       = numCorrs / decim * decim
   
            while( leftOff < stop ) {
               if( checkAborted ) return Aborted

               // XXX inefficient -- should just read the extra frame in successive iterations
               afExtr.seek( leftOff + afStart )
               afExtr.read( eInBuf, 0, halfWinLen )
               aux.Math.normalize( normBuf, eInBuf, 0, halfWinLen )

               var rightOff   = leftOff
               while( rightOff < stop ) {

                  // XXX inefficient -- should just read the extra frame in successive iterations
                  afExtr.seek( rightOff + afStart )
                  afExtr.read( eInBuf, halfWinLen, halfWinLen )
                  aux.Math.normalize( normBuf, eInBuf, halfWinLen, halfWinLen )

                  val temporal = if( tempWeight > 0f ) {
                     aux.Math.correlateHalf( 1, halfWinLen, eInBuf, 0, 0 )
                  } else 0f
                  val spectral = if( tempWeight < 1f ) {
                     aux.Math.correlateHalf( extr.numCoeffs, halfWinLen, eInBuf, 0, 1 )
                  } else 0f
                  val sim  = temporal * tempWeight + spectral * (1f - tempWeight)
                  val colr = colorFun( math.pow( math.max( 0f, sim ), colorWarp ).toFloat * colorScale )

                  val off1 = (imgExtM1 - rightOff/decim) * imgExt + (leftOff/decim)
                  val off2 = (imgExtM1 - leftOff/decim)  * imgExt + (rightOff/decim)
                  imgData( off1 ) = colr
                  imgData( off2 ) = colr

                  progBlock = (progBlock + 1) % 128
                  if( progBlock == 0 ) {
                     val off3 = (leftOff/decim) * imgExt + (rightOff/decim)
                     progress( off3.toFloat / numPix )
                  }
                  rightOff += decim
               }
               leftOff += decim
            }

            ImageIO.write( img, "png", settings.imageOutput )
            progress( 1f )
            
         } finally {
            g.dispose()
            img.flush()
         }
      } finally {
         afExtr.cleanUp()
      }

      Success( () )
   }

   protected val Act = new Actor {
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
