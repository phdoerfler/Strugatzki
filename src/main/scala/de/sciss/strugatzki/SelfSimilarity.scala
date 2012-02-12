package de.sciss.strugatzki

import java.io.File
import aux.{ProcessorCompanion, Processor}
import actors.Actor
import de.sciss.synth.io.AudioFile
import java.awt.image.BufferedImage
import xml.{XML, NodeSeq}

object SelfSimilarity extends ProcessorCompanion {
   type PayLoad = Unit

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
       * The balance between the feature of loudness curve and spectral composition (MFCC).
       * A value of 0.0 means the segmentation is only performed by considering the
       * spectral features, and a value of 1.0 means the segmentation is taking only
       * the loudness into consideration. Values in between give a measure that takes
       * both features into account with the given priorities.
       */
      def temporalWeight: Float
      /** Whether to apply normalization to the features (recommended) */
      def normalize : Boolean

      final def pretty: String = {
         "Settings(\n   databaseFolder = " + databaseFolder +
                  "\n   metaInput      = " + metaInput +
                  "\n   imageOutput    = " + imageOutput +
                  "\n   span           = " + span +
                  "\n   corrLen        = " + corrLen +
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
       * The temporal weight defaults to 0.5.
       */
      var temporalWeight      = 0.5f
      /**
       * The feature vector normalization flag defaults to `true`.
       */
      var normalize           = true

      def build = Settings( databaseFolder, metaInput, imageOutput, span, corrLen, temporalWeight, normalize )

      def read( settings: Settings ) {
         databaseFolder = settings.databaseFolder
         metaInput      = settings.metaInput
         imageOutput    = settings.imageOutput
         span           = settings.span
         corrLen        = settings.corrLen
         temporalWeight = settings.temporalWeight
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
         sb.temporalWeight = (xml \ "weight").text.toFloat
         sb.normalize      = (xml \ "normalize").text.toBoolean
         sb.build
      }
   }
   final case class Settings( databaseFolder: File, metaInput: File, imageOutput: File, span: Option[ Span ],
                              corrLen: Long, temporalWeight: Float, normalize: Boolean )
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
   <weight>{temporalWeight}</weight>
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
      val feat          = AudioFile.openRead( extr.featureOutput )
      val stepSize      = extr.fftSize / extr.fftOverlap

      def fullToFeat( n: Long ) = ((n + (stepSize >> 1)) / stepSize).toInt

      val featFrames    = feat.numFrames
      val halfWinLen    = fullToFeat( settings.corrLen )
      val winLen        = halfWinLen * 2
      val numCorrs      = {
         val n = featFrames - winLen + 1
         require( n <= 0x7FFFFFFF, "32-bit overflow" )
         n.toInt
      }

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

      val img           = new BufferedImage( numCorrs, numCorrs, BufferedImage.TYPE_INT_ARGB )

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
