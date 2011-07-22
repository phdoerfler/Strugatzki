package de.sciss.utopia

import java.io.File

object Utopia {
   def main( args: Array[ String ]) {
      import FeatureExtraction._

      val inDir   = "/Users/hhrutz/Desktop/new_projects/Utopia/audio_work"
      val name    = "Raspad_30'58"
      val set     = new SettingsBuilder
      set.audioInput    = new File( inDir, name + ".aif" )
      set.featureOutput = new File( inDir, name + "_feat.aif" )
      set.metaOutput    = Some( new File( inDir, name + "_feat.xml" ))

      println( "Starting extraction..." )
      FeatureExtraction( set ) {
         case Success =>
            println( "Success." )
         case Failure( e ) =>
            println( "Failed: " )
            e.printStackTrace()
         case Progress( f ) =>
            println( (f * 100).toInt )
      }
   }
}