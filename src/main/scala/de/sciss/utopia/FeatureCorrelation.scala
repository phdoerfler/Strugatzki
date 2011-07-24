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

import java.io.{FilenameFilter, File}
import xml.XML
import de.sciss.synth.io.AudioFile
import collection.breakOut
import collection.immutable.{ SortedSet => ISortedSet }

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
   }

   final class SettingsBuilder extends SettingsLike {
      var databaseFolder   = new File( Utopia.defaultDir )
      var metaInput        = new File( "input_feat.xml" )
      var punchIn          = Punch( Span( 0L, 44100L ), 0.5f )
      var punchOut         = Option.empty[ Punch ]
      var minPunch         = 22050L
      var maxPunch         = 88200L

      def build = Settings( databaseFolder, metaInput, punchIn, punchOut, minPunch, maxPunch )
   }

   object Settings {
      implicit def fromBuilder( sb: SettingsBuilder ) : Settings = sb.build
   }
   final case class Settings( databaseFolder: File, metaInput: File, punchIn: Punch, punchOut: Option[ Punch ],
                              minPunch: Long, maxPunch: Long )
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

      val afIn       = AudioFile.openRead( extrIn.featureOutput )

      val bestMatch = extrDBs map { extrDB =>
         val afExtr  = AudioFile.openRead( extrDB.featureOutput )

      }

      Aborted // XXX TODO
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