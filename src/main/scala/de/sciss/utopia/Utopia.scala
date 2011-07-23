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

object Utopia {
   val defaultDir = "/Users/hhrutz/Desktop/new_projects/Utopia/feature"
   val name       = "Utopia"

   def main( args: Array[ String ]) {
      var which   = ""

      val parser  = new OptionParser( name ) {
         opt( "prepare", "Preparatory stuff (ProcSehen)", which = "sehen" )
         opt( "f", "feature", "Feature extraction", which = "feat" )
         opt( "stats", "Statistics from feature database", which = "stats" )
      }
      if( parser.parse( args.take( 1 ))) {
         which match {
            case "sehen"   => ProcSehen.perform()
            case "feat"    => featurePre( args.drop( 1 ))
            case "stats"   => featureStats( args.drop( 1 ))
            case _         => parser.showUsage
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
         var lastProg = 0
         val paths: IndexedSeq[ File ] = dir.listFiles( new FilenameFilter {
            def accept( d: File, f: String ) = f.endsWith( "_feat.aif" )
         })
         import FeatureStats._
         FeatureStats( paths ) {
            case Success( spans ) =>
               println( "  Success." )
               val afNorm = AudioFile.openWrite( new File( dir, "feat_norms.aif" ),
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