/*
 *  Utopia.scala
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

import scopt.OptionParser
import collection.breakOut
import java.io.{FilenameFilter, FileFilter, File}
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec, AudioFile}
import java.util.Locale
import java.text.{DecimalFormat, NumberFormat}

object Utopia {
   val defaultDir       = "/Users/hhrutz/Desktop/new_projects/Utopia/feature"
   val name             = "Utopia"
   val NORMALIZE_NAME   = "feat_norms.aif"

   private lazy val decibelFormat = {
      val res = NumberFormat.getInstance( Locale.US )
      res match {
         case d: DecimalFormat => {
            d.setGroupingUsed( false )
            d.setMinimumFractionDigits( 1 )
            d.setMaximumFractionDigits( 1 )
            d.setNegativeSuffix( " dB" )
            d.setPositiveSuffix( " dB" )
         }
         case _ =>
      }
      res
   }

   private lazy val percentFormat = {
      val res = NumberFormat.getPercentInstance( Locale.US )
      res match {
         case d: DecimalFormat => {
            d.setGroupingUsed( false )
            d.setMinimumFractionDigits( 1 )
            d.setMaximumFractionDigits( 1 )
         }
         case _ =>
      }
      res
   }

   def main( args: Array[ String ]) {
      var which   = ""

      val parser  = new OptionParser( name ) {
         opt( "prepare", "Preparatory stuff (ProcSehen)", which = "sehen" )
         opt( "f", "feature", "Feature extraction", which = "feat" )
         opt( "c", "correlate", "Find best correlation with database", which = "corr" )
         opt( "stats", "Statistics from feature database", which = "stats" )
      }
      if( parser.parse( args.take( 1 ))) {
         val argsRem = args.drop( 1 )
         which match {
            case "sehen"   => ProcSehen.perform()
            case "feat"    => featurePre( argsRem )
            case "stats"   => featureStats( argsRem )
            case "corr"    => featureCorr( argsRem )
            case _         => parser.showUsage
         }
      } else parser.showUsage
   }

   def featureCorr( args: Array[ String ]) {
      var dirOption     = Option.empty[ String ]
      var verbose       = false
      var punchInStart  = Option.empty[ Double ]
      var punchInStop   = Option.empty[ Double ]
      var tempIn        = 0.5
      var punchOutStart = Option.empty[ Double ]
      var punchOutStop  = Option.empty[ Double ]
      var tempOut       = 0.5
      var minPunch      = Option.empty[ Double ]
      var maxPunch      = Option.empty[ Double ]
      var input         = Option.empty[ String ]
      var maxBoost      = 8.0
      var numMatches    = 1
      var numPerFile    = 1
      var minSpacing    = 0.0 // 0.5
      var normalize     = true

      val parser  = new OptionParser( name + " -c" ) {
         opt( "v", "verbose", "Verbose output", verbose = true )
         opt( "d", "dir", "<directory>", "Database directory", (s: String) => dirOption    = Some( s ))
         doubleOpt( "in-start", "Punch in begin (secs)", (d: Double) => punchInStart  = Some( d ))
         doubleOpt( "in-stop", "Punch in end (secs)", (d: Double) => punchInStop   = Some( d ))
         doubleOpt( "in-temp", "Temporal weight for punch in (0 to 1, default 0.5)", tempIn = _ )
         doubleOpt( "out-start", "Punch out begin (secs)", (d: Double) => punchOutStart = Some( d ))
         doubleOpt( "out-stop", "Punch out end (secs)", (d: Double) => punchOutStop  = Some( d ))
         doubleOpt( "out-temp", "Temporal weight for punch out (0 to 1, default 0.5)", tempOut = _ )
         doubleOpt( "dur-min", "Minimum fill duration (secs)", (d: Double) => minPunch = Some( d ))
         doubleOpt( "dur-max", "Maximum fill duration (secs)", (d: Double) => maxPunch = Some( d ))
         doubleOpt( "boost-max", "Maximum loudness boost factor (default 8)", maxBoost = _ )
         intOpt( "m", "num-matches", "Maximum number of matches (default 1)", numMatches = _ )
         intOpt( "num-per-file", "Maximum matches per single file (default 1)", numPerFile = _ )
         doubleOpt( "spacing", "Minimum spacing between matches within one file (default 0.5)", minSpacing = _ )
         arg( "input", "Meta file of input to process", (i: String) => input = Some( i ))
         opt( "no-norm", "Do not apply feature normalization", normalize = false )
      }

      if( parser.parse( args )) {
         (input, punchInStart, punchInStop, minPunch, maxPunch) match {
            case (Some( in ), Some( piStart ), Some( piStop ), Some( pMin ), Some( pMax )) =>
               val inFile  = new File( in )
               val metaIn  = FeatureExtraction.Settings.fromXMLFile( inFile )
               val inSpec  = AudioFile.readSpec( metaIn.audioInput )

               def secsToFrames( s: Double ) = (s * inSpec.sampleRate + 0.5).toLong

               val (ok, punchOutO) = (punchOutStart, punchOutStop) match {
                  case (Some( poStart ), Some( poStop )) =>
                     val outSpan = Span( secsToFrames( poStart ), secsToFrames( poStop ))
                     require( outSpan.length > 0, "Punch out span is empty" )
                     true -> Some( FeatureCorrelation.Punch( outSpan, tempOut.toFloat ))

                  case (None, None) => true -> None
                  case _ => false -> None
               }
               if( ok ) {
                  val inSpan  = Span( secsToFrames( piStart ), secsToFrames( piStop ))
                  require( inSpan.length > 0, "Punch in span is empty" )
                  val punchIn = FeatureCorrelation.Punch( inSpan, tempIn.toFloat )
                  val minFrames  = secsToFrames( pMin )
                  require( minFrames > 0, "Minimum duration is zero" )
                  val maxFrames  = secsToFrames( pMax )
                  require( maxFrames >= minFrames, "Maximum duration is smaller than minimum duration" )

                  FeatureCorrelation.verbose = verbose
                  val set              = new FeatureCorrelation.SettingsBuilder
                  set.databaseFolder   = new File( dirOption.getOrElse( defaultDir ))
                  set.punchIn          = punchIn
                  set.punchOut         = punchOutO
                  set.metaInput        = inFile
                  set.minPunch         = minFrames
                  set.maxPunch         = maxFrames
                  set.normalize        = normalize
                  set.maxBoost         = maxBoost.toFloat
                  set.numMatches       = numMatches
                  set.numPerFile       = numPerFile
                  set.minSpacing       = secsToFrames( minSpacing )

                  def ampToDB( amp: Double ) = 20 * math.log10( amp )
                  def toPercentStr( d: Double ) = percentFormat.format( d )
                  def toDBStr( amp: Double ) = decibelFormat.format( ampToDB( amp ))

                  import FeatureCorrelation._
                  var lastProg = 0
                  FeatureCorrelation( set ) {
                     case Success( res ) if( res.nonEmpty ) =>
                        println( "  Success." )

                        res.foreach { m =>
                           println(  "\nFile      : " + m.file.getAbsolutePath +
                                     "\nSimilarity: " + toPercentStr( m.sim ) +
                                     "\nSpan start: " + m.punch.start +
                                     "\nBoost in  : " + toDBStr( m.boostIn ))
                           if( punchOutO.isDefined ) {
                              println( "Span stop : " + m.punch.stop +
                                     "\nBoost out : " + toDBStr( m.boostOut ))
                           }
                        }
                        println()

                     case Success( _ ) =>
                        println( "  No matches found." )
                     case Failure( e ) =>
                        println( "  Failed: " )
                        e.printStackTrace()
                     case Aborted =>
                        println( "  Aborted" )
                     case Progress( perc ) =>
                        val i = perc >> 2
                        while( lastProg < i ) {
                           print( "#" )
                        lastProg += 1 }
                  }

               } else parser.showUsage

            case _ => parser.showUsage
         }
      } else parser.showUsage
   }

   def featureStats( args: Array[ String ]) {
      var dirOption = Option.empty[ String ]
      var verbose = false

      val parser  = new OptionParser( name + " -f" ) {
         opt( "v", "verbose", "Verbose output", verbose = true )
         opt( "d", "dir", "<directory>", "Database directory", (s: String) => dirOption = Some( s ))
      }
      if( parser.parse( args )) {
         val dir = new File( dirOption.getOrElse( defaultDir ))
         println( "Starting stats... " )
         val paths: IndexedSeq[ File ] = dir.listFiles( new FilenameFilter {
            def accept( d: File, f: String ) = f.endsWith( "_feat.aif" )
         })
         import FeatureStats._
         var lastProg = 0
         FeatureStats( paths ) {
            case Success( spans ) =>
               println( "  Success." )
               val afNorm = AudioFile.openWrite( new File( dir, NORMALIZE_NAME ),
                  AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, spans.size, 44100 ))
               val b = afNorm.frameBuffer( 2 )
               spans.zipWithIndex.foreach { case ((min, max), i) =>
                  b( i )( 0 ) = min.toFloat
                  b( i )( 1 ) = max.toFloat
               }
               afNorm.writeFrames( b )
               afNorm.close
               println( "Done." )
            case Failure( e ) =>
               println( "  Failed: " )
               e.printStackTrace()
            case Aborted =>
               println( "  Aborted" )
            case Progress( perc ) =>
               val i = perc >> 2
               while( lastProg < i ) {
                  print( "#" )
               lastProg += 1 }
         }
       } else parser.showUsage
   }

   def featurePre( args: Array[ String ]) {
      var inputs  = IndexedSeq.empty[ String ]
      var target  = Option.empty[ String ]
      var verbose = false

      val parser  = new OptionParser( name + " -f" ) {
         opt( "v", "verbose", "Verbose output", verbose = true )
         arglistOpt( "inputs...", "List of input files or directories", inputs +:= _ )
         opt( "d", "dir", "<directory>", "Target directory", (s: String) => target = Some( s ))
      }
      if( parser.parse( args )) {
         FeatureExtraction.verbose = verbose
         if( inputs.isEmpty ) {
            parser.showUsage
         }
         val inFiles: List[ File ] = inputs.flatMap( p => {
            val f = new File( p )
            if( f.isFile ) List( f ) else f.listFiles( new FileFilter {
               def accept( f: File ) = try {
                  AudioFile.identify( f ).isDefined
               } catch { case _ => false }
            }).toList
         })( breakOut )
         val targetDir = new File( target.getOrElse( defaultDir ))
         def iter( list: List[ File ]) {
            list match {
               case head :: tail => feature( head, targetDir )( if( _ ) iter( tail ))
               case _ =>
            }
         }
         iter( inFiles )
      } else {
         parser.showUsage
      }
   }

   def feature( inputFile: File, outDir: File )( whenDone: Boolean => Unit ) {
      import FeatureExtraction._

//      val inDir   = "/Users/hhrutz/Desktop/new_projects/Utopia/audio_work"
//      val outDir  = "/Users/hhrutz/Desktop/new_projects/Utopia/feature" // audio_work"
//      val name    = "Raspad_30'58"
//      val name    = "Klangbeispiel7"
//      val name    = "NuclearBoy"
      val set     = new SettingsBuilder
      set.audioInput = inputFile
//      set.audioInput    = n match {
//         case Some( path ) => new File( path )
//         case None         => new File( inDir, name + ".aif" )
//      }
      val name1         = {
         val n = set.audioInput.getName
         val i = n.lastIndexOf( '.' )
         if( i >= 0 ) n.substring( 0, i ) else n
      }
      set.featureOutput = new File( outDir, name1 + "_feat.aif" )
      set.metaOutput    = Some( new File( outDir, name1 + "_feat.xml" ))

      println( "Starting extraction... " + set.audioInput.getName )
      var lastProg = 0
      FeatureExtraction( set ) {
         case Success =>
            println( "  Success." )
            whenDone( true )
         case Failure( e ) =>
            println( "  Failed: " )
            e.printStackTrace()
            whenDone( false )
         case Aborted =>
            println( "  Aborted" )
            whenDone( false )
         case Progress( perc ) =>
//            println( (f * 100).toInt )
            val i = perc >> 2
            while( lastProg < i ) {
               print( "#" )
            lastProg += 1 }
      }
   }
}