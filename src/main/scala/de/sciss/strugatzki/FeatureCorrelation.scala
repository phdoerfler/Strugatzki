/*
 *  FeatureCorrelation.scala
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
import java.io.{RandomAccessFile, FilenameFilter, File}
import actors.Actor
import collection.immutable.{SortedSet => ISortedSet}
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec, AudioFile}
import xml.{NodeSeq, Node, Elem, XML}

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

   object Match {
      def fromXML( xml: NodeSeq ) : Match = {
         val sim     = (xml \ "sim").text.toFloat
         val file    = new File( (xml \ "file").text )
         val start   = (xml \ "start").text.toLong
         val stop    = (xml \ "stop").text.toLong
         val boostIn = (xml \ "boostIn").text.toFloat
         val boostOut= (xml \ "boostOut").text.toFloat
         Match( sim, file, Span( start, stop ), boostIn, boostOut )
      }
   }
   final case class Match( sim: Float, file: File, punch: Span, boostIn: Float, boostOut: Float ) {
      def toXML =
<match>
   <sim>{sim}</sim>
   <file>{file.getPath}</file>
   <start>{punch.start}</start>
   <stop>{punch.stop}</stop>
   <boostIn>{boostIn}</boostIn>
   <boostOut>{boostOut}</boostOut>
</match>
   }

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
   object Punch {
      def fromXML( xml: NodeSeq ) : Punch = {
         val start   = (xml \ "start").text.toLong
         val stop    = (xml \ "stop").text.toLong
         val weight  = (xml \ "weight").text.toFloat
         Punch( Span( start, stop ), weight )
      }
   }
   final case class Punch( span: Span, temporalWeight: Float = 0.5f ) {
      def toXML =
<punch>
   <start>{span.start}</start>
   <stop>{span.stop}</stop>
   <weight>{temporalWeight}</weight>
</punch>
   }

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

   object SettingsBuilder {
      def apply() : SettingsBuilder = new SettingsBuilder
      def apply( settings: Settings ) : SettingsBuilder = {
         val sb = new SettingsBuilder
         sb.read( settings )
         sb
      }
   }
   final class SettingsBuilder extends SettingsLike {
      var databaseFolder      = new File( "database" ) // Strugatzki.defaultDir
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

      def read( settings: Settings ) {
         databaseFolder = settings.databaseFolder
         metaInput      = settings.metaInput
         punchIn        = settings.punchIn
         punchOut       = settings.punchOut
         minPunch       = settings.minPunch
         maxPunch       = settings.maxPunch
         normalize      = settings.normalize
         maxBoost       = settings.maxBoost
         numMatches     = settings.numMatches
         numPerFile     = settings.numPerFile
         minSpacing     = settings.minSpacing
      }
   }

   object Settings {
      implicit def fromBuilder( sb: SettingsBuilder ) : Settings = sb.build
      def fromXMLFile( file: File ) : Settings = fromXML( XML.loadFile( file ))
      def fromXML( xml: NodeSeq ) : Settings = {
         val sb = new SettingsBuilder
         sb.databaseFolder = new File( (xml \ "database").text )
         sb.metaInput      = new File( (xml \ "input").text )
         sb.punchIn        = Punch.fromXML( xml \ "punchIn" )
         sb.punchOut       = {
            val e = xml \ "punchOut"
            if( e.isEmpty ) None else Some( Punch.fromXML( e ))
         }
         sb.minPunch       = (xml \ "minPunch").text.toLong
         sb.maxPunch       = (xml \ "maxPunch").text.toLong
         sb.normalize      = (xml \ "normalize").text.toBoolean
         sb.maxBoost       = (xml \ "maxBoost").text.toFloat
         sb.numMatches     = (xml \ "numMatches").text.toInt
         sb.numPerFile     = (xml \ "numPerFile").text.toInt
         sb.minSpacing     = (xml \ "minSpacing").text.toLong
         sb.build
      }
   }
   final case class Settings( databaseFolder: File, metaInput: File, punchIn: Punch, punchOut: Option[ Punch ],
                              minPunch: Long, maxPunch: Long, normalize: Boolean, maxBoost: Float,
                              numMatches: Int, numPerFile: Int, minSpacing: Long )
   extends SettingsLike {
      def toXML =
<correlate>
   <database>{databaseFolder.getPath}</database>
   <input>{metaInput.getPath}</input>
   <punchIn>{punchIn.toXML.child}</punchIn>
   {punchOut match { case Some( p ) => <punchOut>{p.toXML.child}</punchOut>; case _ => Nil }}
   <minPunch>{minPunch}</minPunch>
   <maxPunch>{maxPunch}</maxPunch>
   <normalize>{normalize}</normalize>
   <maxBoost>{maxBoost}</maxBoost>
   <numMatches>{numMatches}</numMatches>
   <numPerFile>{numPerFile}</numPerFile>
   <minSpacing>{minSpacing}</minSpacing>
</correlate>
   }

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

//   start()

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
         val afNorm = AudioFile.openRead( new File( settings.databaseFolder, Strugatzki.NORMALIZE_NAME ))
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
         var ch = 0; val numCh = b.length; while( ch < numCh ) {
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

//      def createTempFile( id: String ) : RandomAccessFile = {
//         val file = File.createTempFile( "corr_" + id, ".bin" )
//         file.deleteOnExit()
//         new RandomAccessFile( file, "rw" )
//      }

      def createTempAudioFile( id: String, numChannels: Int ) : AudioFile = {
         val file = File.createTempFile( "corr_" + id, ".aif" )
         file.deleteOnExit()
         AudioFile.openWrite( file, AudioFileSpec( AudioFileType.IRCAM, SampleFormat.Float, numChannels, 44100 ))
      }

      val punchInLen       = matrixIn.numFrames
      val punchOutLen      = matrixOutO.map( _.numFrames ).getOrElse( 0 )
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
//      def worstSim = entryPrio.lastOption.getOrElse( allPrio.last ).sim
      def worstSim = {
         if( entryPrio.nonEmpty ) entryPrio.last.sim
         else if( allPrio.nonEmpty ) allPrio.last.sim
         else 0f // Float.NegativeInfinity
      }

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

      val tInBuf  = Array.ofDim[ Float ]( 2, 1024 )
      val tOutBuf = Array.ofDim[ Float ]( 2, 1024 ) // tOut.frameBuffer( 1024 )
      val eInBuf  = Array.ofDim[ Float ]( extrIn.numCoeffs + 1, punchInLen )
      val eOutBuf = Array.ofDim[ Float ]( extrIn.numCoeffs + 1, punchOutLen )
      var tIn : AudioFile = null
      var tOut: AudioFile = null

//println( matrixIn.temporal.mat(0).toIndexedSeq )
//println( settings )
//println( "punch in " + punchInLen )

      // - for each span:
      extrDBs.zipWithIndex foreach { case (extrDB, extrIdx) =>

         if( checkAborted ) return Aborted

         if( entryPrio.nonEmpty ) entryPrio = entryPrio.empty
         lastEntryMatch = null

//var xSUM = 0.0
//var xCNT = 0

         val afExtr = AudioFile.openRead( extrDB.featureOutput )
         try {
            //   - create a temp file
            //   - write the sliding xcorr to that file
            // A simple optimization could be to not begin writing the
            // temp file unless a punch-in correlation is found which is better
            // than the previous best match. This could also trigger
            // the punch-out measurement which could thus offset at
            // first_punch_in + min_punch_len
            var tInOpen    = false
            var tInOff     = 0
            var tInBufOff  = 0
//            val b          = afExtr.frameBuffer( math.max( punchInLen, punchOutLen ))
            var left       = afExtr.numFrames
            matrixOutO.foreach { mo => left -= minPunch /* + mo.numFrames */}
            var readSz     = punchInLen   // read full buffer in first round
            var readOff    = 0
            var logicalOff = 0
            // - go through in-span file and calculate correlations
            while( left > 0 ) {

               if( checkAborted ) return Aborted

               val chunkLen   = math.min( left, readSz ).toInt
               afExtr.readFrames( eInBuf, readOff, chunkLen )
               val eInBufOff = logicalOff % punchInLen
               normalize( eInBuf, readOff, chunkLen )
               val boost = calcBoost( matrixIn, eInBuf( 0 ))
               val sim = if( boost <= settings.maxBoost ) {
                  val temporal = if( inTempWeight > 0f ) {
                     correlate( matrixIn.temporal, eInBuf, eInBufOff, 0 )
//if( res > 1 ) {
//   println( "temp : " + res + " " + logicalOff )
//   val res1 = correlate( matrixIn.temporal, eInBuf, bOff, 0 )
//}
                  } else 0f
                  val spectral = if( inTempWeight < 1f ) {
                     correlate( matrixIn.spectral, eInBuf, eInBufOff, 1 )
//if( res > 1 ) println( "spec : " + res + " " + logicalOff )
                  } else 0f
                  temporal * inTempWeight + spectral * (1f - inTempWeight)
               } else {
                  0f // Float.NegativeInfinity
               }

//if( sim > 0f ) {
//if( xCNT < 30 ) println( "#" + xCNT + "  " + sim )
//   xSUM += sim * sim
//   xCNT += 1
//}

               if( matrixOutO.isDefined ) {
                  if( tInOpen || entryHasSpace || sim > worstSim ) {
                     if( !tInOpen ) {
                        if( tIn == null ) {
                           tIn = createTempAudioFile( "in", 2 )
                        } else {
                           tIn.seekFrame( 0L )
                        }
                        tInOff = logicalOff
                        tInOpen= true
                     }
                     tInBuf( 0 )( tInBufOff ) = sim
                     tInBuf( 1 )( tInBufOff ) = boost
                     tInBufOff += 1
                     // flush
                     if( tInBufOff == 1024 ) {
                        tIn.writeFrames( tInBuf, 0, tInBufOff )
                        tInBufOff = 0
                     }
                  }
               } else {
                  if( entryHasSpace || sim > worstSim ) {
                     val start   = featToFull( logicalOff )
                     val stop    = featToFull( logicalOff + punchInLen )
                     val m       = Match( sim, extrDB.audioInput, Span( start, stop ), boost, 1f )
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
            (matrixOutO, settings.punchOut, tInOpen) match {
               case (Some( matrixOut ), Some( punchOut ), true) =>
                  // flush
                  if( tInBufOff > 0 ) {
                     tIn.writeFrames( tInBuf, 0, tInBufOff )
                     tInBufOff = 0
                  }

//var ySUM = 0.0
//var yCNT = 0

                  tIn.seekFrame( 0L )

//                  val piOff0  = tIn.readInt()
//                  val poOff0  = piOff0 + minPunch   // this is the minimum offset where we begin correlation for punch-out
                  val poOff0  = tInOff + minPunch

                  left        = afExtr.numFrames - (poOff0 /*+ matrixOut.numFrames */)
                  if( left >= matrixOut.numFrames ) {  // means we actually do at least one full correlation
                     if( tOut == null ) {
                        tOut = createTempAudioFile( "out", 2 )
                     } else {
                        tOut.seekFrame( 0L )
                     }

                     val outTempWeight = punchOut.temporalWeight
                     afExtr.seekFrame( poOff0 )
                     readSz            = punchOutLen   // read full buffer in first round
                     readOff           = 0
                     logicalOff        = 0
                     // - go through out-span file and calculate correlations

                     var tOutBufOff    = 0
                     val tOutSize      = left
                     while( left > 0 ) {

                        if( checkAborted ) return Aborted

                        val chunkLen   = math.min( left, readSz ).toInt
                        afExtr.readFrames( eOutBuf, readOff, chunkLen )
                        normalize( eOutBuf, readOff, chunkLen )
                        val extraBufOff = logicalOff % punchOutLen
                        val boost = calcBoost( matrixOut, eOutBuf( 0 ))
                        val sim = if( boost <= settings.maxBoost ) {
                           val temporal = if( outTempWeight > 0f ) {
                              correlate( matrixOut.temporal, eOutBuf, extraBufOff, 0 )
//if( res > 1 ) println( "temp : " + res + " " + tOutBufOff )
                           } else 0f
                           val spectral = if( outTempWeight < 1f ) {
                              correlate( matrixOut.spectral, eOutBuf, extraBufOff, 1 )
//if( res > 1 ) println( "spec : " + res + " " + tOutBufOff )
                           } else 0f
                           temporal * outTempWeight + spectral * (1f - outTempWeight)
                        } else {
                           0f // Float.NegativeInfinity
                        }

//if( sim > Float.NegativeInfinity ) {
//   ySUM += sim * sim
//   yCNT += 1
//}

                        tOutBuf( 0 )( tOutBufOff ) = sim
                        tOutBuf( 1 )( tOutBufOff ) = boost
                        tOutBufOff += 1
                        if( tOutBufOff == 1024 ) { // flush
                           tOut.writeFrames( tOutBuf, 0, tOutBufOff )
                           tOutBufOff = 0
                        }

//                        tOut.writeFloat( sim )
//                        tOut.writeFloat( boost )
                        left   -= chunkLen
                        readOff = (readOff + chunkLen) % punchOutLen
                        logicalOff += 1
                        readSz  = 1 // read single frames in successive round (and rotate buffer)
                     }
                     // flush
                     if( tOutBufOff > 0 ) {
                        tOut.writeFrames( tOutBuf, 0, tOutBufOff )
                        tOutBufOff = 0
                     }

                     // - finally find the best match
//                     tIn.seek( 0L )
//                     left = tIn.length / 12 // int <off>, float <sim>, float <boost>
//println( "---1 (" + left + ")" )
                     left = afExtr.numFrames - poOff0
                     tInBufOff   = 1024
                     var piOff   = tInOff
                     while( left > 0 ) {

                        if( checkAborted ) return Aborted

                        if( tInBufOff == 1024 ) {
                           tIn.readFrames( tInBuf, 0, math.min( 1024, left ).toInt )
                           tInBufOff = 0
                        }

                        val inSim   = tInBuf( 0 )( tInBufOff )
                        val boostIn = tInBuf( 1 )( tInBufOff )

//                        val piOff   = tIn.readInt()
//                        val inSim   = tIn.readFloat()
//                        val boostIn = tIn.readFloat()
                        // worstSim is now
                        // defined as min( inSim, outSim )
                        var ws = worstSim       // cache it here
                        var hs = entryHasSpace  // cahce it here
//                        if( hs || inSim > ws ) // for sim = min( inSim, outSim )
                        if( inSim > (ws * ws) ) { // sqrt( inSim * 1 ) > ws
                           var poOff   = piOff + minPunch
                           // note: there is room for further optimization:
                           // we might track in this iteration the best sim
                           // in tOut, and in the next iteration, if this
                           // best sim is too bad -- we can just skip over
                           // the whole previous search span!
                           val tOutSeek = piOff - tInOff // = numRead from tIn !
                           tOut.seekFrame( tOutSeek )

//                           tOut.seek( (poOff0 + (piOff - piOff0)) * 8 )
//                           var left2   = math.min( (tOut.length - tOut.getFilePointer) / 8, maxPunch - minPunch + 1 ) // float <sim>, float <boost>
                           var left2   = math.min( tOutSize - tOutSeek, maxPunch - minPunch + 1 )
//println( "---2 (" + left2 + ") " + hs + " | " + ws )
                           while( left2 > 0 ) {

                              if( checkAborted ) return Aborted

                              val chunkLen = math.min( left2, 1024 ).toInt
                              tOut.readFrames( tOutBuf, 0, chunkLen )

                              var chunkOff = 0; while( chunkOff < chunkLen ) {
   //                              val outSim  = tOut.readFloat()
   //                              val boostOut= tOut.readFloat()
                                 val outSim  = tOutBuf( 0 )( chunkOff )
                                 val boostOut= tOutBuf( 1 )( chunkOff )
//                                 val sim     = math.min( inSim, outSim )

                                 // ok, let's try geometric mean, meaning that
                                 // in the case of inSim < outSim, the result
                                 // could still be differentiated among several
                                 // outSim! (which would be lost in the case of min( inSim, outSim )
                                 val sim = math.sqrt( inSim * outSim ).toFloat
//if( sim > 1 ) println( "inSim = " + inSim + " outSim = " + outSim + " sim = " + sim )
                                 if( hs || sim > ws ) {
                                    val m = Match( sim, extrDB.audioInput,
                                       Span( featToFull( piOff ), featToFull( poOff )), boostIn, boostOut )
                                    addMatch( m )
                                    // clear cache
                                    ws = worstSim
                                    hs = entryHasSpace

//                                    // shortcut (with the definition of minSim):
//                                    // if sim == inSim, the search is over for this round
//                                    // (because no further sim can be better than inSim)
//                                    if( outSim >= inSim && !hs ) {
//                                       left2    = 0
//                                       chunkOff = chunkLen
//                                    }
                                 }
                                 chunkOff += 1
                                 poOff += 1
                              }
                              left2 -= chunkLen // 1
//                              poOff += chunkLen // 1
                           }
//println( "---3" )
                        }
                        left -= 1
                        tInBufOff += 1
                        piOff += 1
                     }
//println( "---4" )
//                     tOut.close
                  }
//println( "rms punch in = " + (math.sqrt( xSUM ) / xCNT) ) // + " ; punch out = " + (math.sqrt( ySUM ) / yCNT)

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

      if( tIn != null ) tIn.close
      if( tOut != null ) tOut.close

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
      val numChannels   = a.numChannels
      val numFrames     = a.numFrames
      // note: stat does not wrap frame offset around b.numFrames.
      // we thus assume that b really has data from 0 to a.numFrames!
      val (bMean, bStdDev) = stat( b, 0 /* FrameOff */, numFrames, bChanOff, numChannels )
      val aAdd = -a.mean
//      val aMul = 1.0 / a.stdDev
      val bAdd = -bMean
//      val bMul = 1.0 / bStdDev

      var sum           = 0.0
      var ch = 0; while( ch < numChannels ) {
         val ca = a.mat( ch )
         val cb = b( ch + bChanOff )
         var i = 0; while( i < numFrames ) {
//            sum += ((ca( i ) + aAdd) * aMul)  * ((cb( (i + bFrameOff) % cb.length ) + bAdd) * bMul)
            sum += (ca( i ) + aAdd) * (cb( (i + bFrameOff) % cb.length ) + bAdd)
         i += 1 }
      ch += 1 }
//      (sum / (a.matSize - 1)).toFloat
//      (sum / a.matSize).toFloat  // ensures correlate( a, a ) == 1.0
      (sum / (a.stdDev * bStdDev * a.matSize)).toFloat  // ensures correlate( a, a ) == 1.0
   }

   // CRAPPY SCALAC CHOKES ON MIXING IN PROCESSOR. FUCKING SHIT. COPYING WHOLE BODY HERE

   def abort() { Act ! Abort }
   def start() { Act.start() }

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