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
}
final class FeatureCorrelation private ( settings: FeatureCorrelation.Settings,
                                         protected val observer: FeatureCorrelation.Observer ) extends Processor {
   protected val companion = FeatureCorrelation
   import companion._

   protected def body() : Result = {
      val punchMetas = settings.databaseFolder.listFiles( new FilenameFilter {
         def accept( dir: File, name: String ) = name.endsWith( "_feat.xml" )
      }).toSet - settings.metaInput



      Aborted // XXX TODO
   }
}