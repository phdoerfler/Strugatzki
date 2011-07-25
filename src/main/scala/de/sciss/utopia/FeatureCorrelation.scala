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
import java.io.{RandomAccessFile, FilenameFilter, File}
import actors.Actor
import collection.immutable.{SortedSet => ISortedSet}

/**
 * A processor which searches through the database and matches
 * entries against a given audio file input. Returns a given
 * number of best matches.
 */
object FeatureCorrelation /* extends ProcessorCompanion */ {
   var verbose = false

   protected lazy val tmpDir = new File( sys.props.getOrElse( "java.io.tmpdir", "/tmp" ))

   type Observer = PartialFunction[ ProgressOrResult, Unit ]
//   type PayLoad

   sealed trait ProgressOrResult
   final case class Progress( percent: Int ) extends ProgressOrResult
   sealed trait Result extends ProgressOrResult
   case class Success( result: PayLoad ) extends Result
   final case class Failure( t: Throwable ) extends Result
   case object Aborted extends Result

   /**
    * The result is a sequence of matches, sorted
    * by descending similarity
    */
   type PayLoad = IndexedSeq[ Match ]

//   final case class Match( sim: Float, file: File, punchIn: Long, punchOut: Long, boostIn: Float, boostOut: Float )
   final case class Match( sim: Float, file: File, punch: Span, boostIn: Float, boostOut: Float )

   // reverse ordering. since sortedset orders ascending according to the ordering,
   // this means we get a sortedset with high similarities at the head and low
   // similarities at the tail, like a priority queue
   private object MatchMinOrd extends Ordering[ Match ] {
      def compare( a: Match, b: Match ) = b.sim compare a.sim
   }

   def apply( settings: Settings )( observer: Observer ) : FeatureCorrelation = {
      new FeatureCorrelation( settings, observer )
   }

   /** where temporal weight is between 0 (just spectral corr) and 1 (just temporal corr) */
   final case class Punch( span: Span, temporalWeight: Float = 0.5f )

   /**
    * All durations, spans and spacings are given in sample frames
    * with respect to the sample rate of the audio input file.
    */
   sealed trait SettingsLike {
      def databaseFolder : File
      def metaInput: File
      /** The span in the audio input serving for correlation to find the punch in material */
      def punchIn: Punch
      /** The span in the audio input serving for correlation to find the punch out material */
      def punchOut : Option[ Punch ]
      /** Minimum length of the material to punch in */
      def minPunch: Long
      /** Maximum length of the material to punch in */
      def maxPunch: Long
      /** Whether to apply normalization to the features (recommended) */
      def normalize : Boolean
      /**
       * Maximum energy boost (as an amplitude factor) allowed for a match to be considered.
       * The estimation of the boost factor for two matched signals
       * is `exp ((ln( loud_in ) - ln( loud_db )) / 0.6 )`
       */
      def maxBoost : Float
      /** Maximum number of matches to report */
      def numMatches : Int
      /** Maximum number of matches to report of a single database entry */
      def numPerFile : Int
      /** Minimum spacing between matches within a single database entry */
      def minSpacing : Long
   }

   final class SettingsBuilder extends SettingsLike {
      var databaseFolder      = new File( Utopia.defaultDir )
      var metaInput           = new File( "input_feat.xml" )
      var punchIn             = Punch( Span( 0L, 44100L ), 0.5f )
      var punchOut            = Option.empty[ Punch ]
      var minPunch            = 22050L
      var maxPunch            = 88200L
      var normalize           = true
      var maxBoost            = 8f
      var numMatches          = 1
      var numPerFile          = 1
      var minSpacing          = 0L // 22050L

      def build = Settings( databaseFolder, metaInput, punchIn, punchOut, minPunch, maxPunch, normalize,
         maxBoost, numMatches, numPerFile, minSpacing )
   }

   object Settings {
      implicit def fromBuilder( sb: SettingsBuilder ) : Settings = sb.build
   }
   final case class Settings( databaseFolder: File, metaInput: File, punchIn: Punch, punchOut: Option[ Punch ],
                              minPunch: Long, maxPunch: Long, normalize: Boolean, maxBoost: Float,
                              numMatches: Int, numPerFile: Int, minSpacing: Long )
   extends SettingsLike

   private final case class FeatureMatrix( mat: Array[ Array[ Float ]], numFrames: Int, mean: Double, stdDev: Double ) {
      def numChannels = mat.length
      def matSize = numFrames * numChannels
   }
   private final case class InputMatrix( temporal: FeatureMatrix, spectral: FeatureMatrix, lnAvgLoudness: Double ) {
      require( temporal.numFrames == spectral.numFrames )

      def numFrames : Int = temporal.numFrames
   }
}
final class FeatureCorrelation private ( settings: FeatureCorrelation.Settings,
                                         protected val observer: FeatureCorrelation.Observer ) /* extends Processor */ {
//   protected val companion = FeatureCorrelation
//   import companion._
   import FeatureCorrelation._

   start()

   protected def body() : Result = {
      import FeatureExtraction.{ Settings => ExtrSettings }

      val extrIn     = ExtrSettings.fromXML( XML.loadFile( settings.metaInput ))
      val stepSize   = extrIn.fftSize / extrIn.fftOverlap

      def fullToFeat( n: Long ) = ((n + (stepSize >> 1)) / stepSize).toInt
      def featToFull( i: Int )  = i.toLong * stepSize

      // collect all valid database files from the folder
      val punchMetas = settings.databaseFolder.listFiles( new FilenameFilter {
         def accept( dir: File, name: String ) = name.endsWith( "_feat.xml" )
      }).toSet - settings.metaInput

      // collect all database entries which match the input resolution
      // (for simplicity, we ignore the fact that the sample rates could differ)
      val extrDBs: IndexedSeq[ ExtrSettings ] = punchMetas.map( file => {
         val e = ExtrSettings.fromXMLFile( file )
         if( (e.numCoeffs == extrIn.numCoeffs) && (e.fftSize / e.fftOverlap == stepSize) ) Some( e ) else None
      })( breakOut ).collect { case Some( e ) => e }

      val normBuf = if( settings.normalize ) {
         val afNorm = AudioFile.openRead( new File( settings.databaseFolder, Utopia.NORMALIZE_NAME ))
         require( (afNorm.numChannels == extrIn.numCoeffs + 1) && afNorm.numFrames == 2L )
         val b = afNorm.frameBuffer( 2 )
         afNorm.readFrames( b )
//         Some( b )
         b
      } else null // None

      def avg( b: Array[ Float ], off: Int, len: Int ) = {
         var sum = 0.0
         var i = off; val stop = off + len; while( i < stop ) {
            sum += b( i )
         i += 1 }
         (sum / len).toFloat
      }

      def calcLnAvgLoud( b: Array[ Float ], bOff: Int, bLen: Int ) = math.log( avg( b, bOff, bLen ))

      def calcBoost( in: InputMatrix, b: Array[ Float ]) : Float = {
         val lnAvgB = calcLnAvgLoud( b, 0, in.numFrames )
         math.exp( (in.lnAvgLoudness - lnAvgB) / 0.6 ).toFloat
      }

      def normalize( /* n: Array[ Array[ Float ]], */ b: Array[ Array[ Float ]], bOff: Int, bLen: Int ) {
         if( normBuf == null ) return
         var ch = 0; var numCh = b.length; while( ch < numCh ) {
            val cb   = b( ch )
//            val cn   = n( ch )
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

      val (matrixIn, matrixOutO) = {
         val afIn = AudioFile.openRead( extrIn.featureOutput )
         try {
            def readInBuffer( punch: Punch ) : InputMatrix = {
               val start      = fullToFeat( punch.span.start )
               val stop       = fullToFeat( punch.span.stop )
               val frameNum   = stop - start
               val b          = afIn.frameBuffer( frameNum )
               afIn.seekFrame( start )
               afIn.readFrames( b )
               normalize( b, 0, frameNum )

               def feat( mat: Array[ Array[ Float ]]) = {
                  val (mean, stdDev) = stat( mat, 0, frameNum, 0, mat.length )
                  FeatureMatrix( mat, frameNum, mean, stdDev )
               }

               InputMatrix( feat( b.take( 1 )), feat( b.drop( 1 )), calcLnAvgLoud( b( 0 ), 0, frameNum ))
            }

            // Outline of Algorithm:
            // - read input feature in-span and out-span
            // - optionally normalize
            (readInBuffer( settings.punchIn ), settings.punchOut.map( readInBuffer( _ )))
         } finally {
            afIn.close
         }
      }

      def createTempFile( id: String ) : RandomAccessFile = {
         val file = File.createTempFile( "corr_" + id, ".bin" )
         file.deleteOnExit()
         val res  = new RandomAccessFile( file, "rw" )
         res
      }

      val punchInLen       = matrixIn.numFrames
      val inTempWeight     = settings.punchIn.temporalWeight

      var allPrio       = ISortedSet.empty[ Match ]( MatchMinOrd )
      var entryPrio     = ISortedSet.empty[ Match ]( MatchMinOrd )
      var lastEntryMatch : Match = null

      val minPunch   = fullToFeat( settings.minPunch )
      val maxPunch   = fullToFeat( settings.maxPunch )
//      val minSpacing = fullToFeat( settings.minSpacing )
//println( "minSpacing = " + settings.minSpacing + " (" + minSpacing + " frames)" )

      def entryHasSpace = {
         val maxEntrySz = math.min( settings.numMatches - allPrio.size, settings.numPerFile )
         entryPrio.size < maxEntrySz
      }
      def worstSim   = entryPrio.lastOption.getOrElse( allPrio.last ).sim

      // adds a match to the entry's priority queue. if the queue grows beyong numPerFile,
      // truncates the queue. if the match collides with a previous match that is closer
      // than minSpacing, it is either dropped (if the similarity is equal or smaller) or replaces
      // the previous match (if the similarity is greater).
      def addMatch( m: Match ) {
         if( (lastEntryMatch != null) && (m.punch.spacing( lastEntryMatch.punch ) < settings.minSpacing) ) {
            // gotta collapse them
            if( lastEntryMatch.sim < m.sim ) {  // ok, replace previous match
               entryPrio     -= lastEntryMatch
               entryPrio     += m
               lastEntryMatch = m
            } // otherwise ignore the new match
         } else {
//if( lastEntryMatch != null ) {
//   println( "Spacing ok for " + lastEntryMatch.file.getName + " @ " + lastEntryMatch.punch + " <--> " + m.punch )
//}

            entryPrio     += m
            if( entryPrio.size > settings.numPerFile ) {
               entryPrio -= entryPrio.last   // faster than dropRight( 1 ) ?
            }
            lastEntryMatch = m
         }
      }

      // - for each span:
      extrDBs.zipWithIndex foreach { case (extrDB, extrIdx) =>

         if( checkAborted ) return Aborted

         if( entryPrio.nonEmpty ) entryPrio = entryPrio.empty
         lastEntryMatch = null

         val afExtr = AudioFile.openRead( extrDB.featureOutput )
         try {
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

               if( checkAborted ) return Aborted

               val chunkLen   = math.min( left, readSz ).toInt
               afExtr.readFrames( b, readOff, chunkLen )
               val bOff = logicalOff % punchInLen
               normalize( b, readOff, chunkLen )
               val boost = calcBoost( matrixIn, b( 0 ))
               val sim = if( boost <= settings.maxBoost ) {
                  val temporal = if( inTempWeight > 0f ) {
                     correlate( matrixIn.temporal, b, bOff, 0 )
                  } else 0f
                  val spectral = if( inTempWeight < 1f ) {
                     correlate( matrixIn.spectral, b, bOff, 1 )
                  } else 0f
                  temporal * inTempWeight + spectral * (1f - inTempWeight)
               } else {
                  Float.NegativeInfinity
               }
               if( matrixOutO.isDefined ) {
                  val tf = tmpFile.getOrElse {
                     val res = createTempFile( "in" )
                     tmpFile = Some( res )
                     res
                  }
                  tf.writeInt( logicalOff )
                  tf.writeFloat( sim )
                  tf.writeFloat( boost )
               } else {
                  if( entryHasSpace || sim > worstSim ) {
                     val start = featToFull( logicalOff )
                     val m = Match( sim, extrDB.audioInput, Span( start, start ), boost, 1f )
                     addMatch( m )
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
//                  /* val boostIn = */ tIn.readFloat() // skip
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

                        if( checkAborted ) return Aborted

                        val chunkLen   = math.min( left, readSz ).toInt
                        afExtr.readFrames( b, readOff, chunkLen )
                        normalize( b, readOff, chunkLen )
                        val bOff = logicalOff % punchOutLen
                        val boost = calcBoost( matrixOut, b( 0 ))
                        val sim = if( boost <= settings.maxBoost ) {
                           val temporal = if( outTempWeight > 0f ) {
                              correlate( matrixOut.temporal, b, bOff, 0 )
                           } else 0f
                           val spectral = if( outTempWeight < 1f ) {
                              correlate( matrixOut.spectral, b, bOff, 1 )
                           } else 0f
                           temporal * outTempWeight + spectral * (1f - outTempWeight)
                        } else {
                           Float.NegativeInfinity
                        }
                        tOut.writeFloat( sim )
                        tOut.writeFloat( boost )
                        left   -= chunkLen
                        readOff = (readOff + chunkLen) % punchOutLen
                        logicalOff += 1
                        readSz  = 1 // read single frames in successive round (and rotate buffer)
                     }

                     // - finally find the best match
                     tIn.seek( 0L )
                     left = tIn.length / 12 // int <off>, float <sim>, float <boost>
                     while( left > 0 ) {

                        if( checkAborted ) return Aborted

                        val piOff   = tIn.readInt()
                        val inSim   = tIn.readFloat()
                        val boostIn = tIn.readFloat()
                        // worstSim is now
                        // defined as min( inSim, outSim )
                        if( entryHasSpace || inSim > worstSim ) { // ... so this is a necessary condition to consider this offset
                           var poOff   = piOff + minPunch
                           tOut.seek( poOff0 + (piOff - piOff0) )
                           var left2   = math.max( (tOut.length - tOut.getFilePointer) / 8, maxPunch - minPunch + 1 ) // float <sim>, float <boost>
                           while( left2 > 0 ) {

                              if( checkAborted ) return Aborted

                              val outSim  = tOut.readFloat()
                              val boostOut= tOut.readFloat()
                              val sim     = math.min( inSim, outSim )
                              if( entryHasSpace || sim > worstSim ) {
                                 val m = Match( sim, extrDB.audioInput,
                                    Span( featToFull( piOff ), featToFull( poOff )), boostIn, boostOut )
                                 addMatch( m )

                                 // shortcut (with the definition of minSim):
                                 // if outSim >= inSim, the search is over for this round
                                 // (because worstSim is bound by inSim)
                                 if( outSim >= inSim && !entryHasSpace ) left2 = 0
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
         } finally {
            afExtr.close
         }

         // - add iter-prio to total-prio, and truncate after num-matches elements
         allPrio ++= entryPrio
         if( allPrio.size > settings.numMatches ) allPrio = allPrio.take( settings.numMatches )

         progress( (extrIdx + 1).toFloat / extrDBs.size )
      }

      val pay = allPrio.toIndexedSeq
      Success( pay )
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
      // note: stat does not wrap frame offset around b.numFrames.
      // we thus assume that b really has data from 0 to a.numFrames!
      val (bMean, bStdDev) = stat( b, 0 /* FrameOff */, a.numFrames, bChanOff, a.numChannels )
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
//      (sum / (a.matSize - 1)).toFloat
      (sum / a.matSize).toFloat  // ensures correlate( a, a ) == 1.0
   }

   // CRAPPY SCALAC CHOKES ON MIXING IN PROCESSOR. FUCKING SHIT. COPYING WHOLE BODY HERE

   def abort() { Act ! Abort }
   protected def start() { Act.start() }

   private object Abort

   private object Act extends Actor {
      def act() {
         ProcT.start()
         var result : /* companion. */ Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.aborted = true
               case res: /* companion. */ Progress =>
                  observer( res )
               case res @ /* companion. */ Aborted =>
                  result = res
               case res: /* companion. */ Failure =>
                  result = res
               case res: /* companion. */ Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }

//   protected def body() : companion.Result

   private object ProcT extends Thread {
      var aborted: Boolean = false
      private var lastProg = -1
      override def run() {
         Act ! (try {
            body()
         } catch {
            case e => /* companion. */ Failure( e )
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