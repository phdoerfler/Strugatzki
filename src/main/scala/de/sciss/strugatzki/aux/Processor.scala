package de.sciss.strugatzki
package aux

import java.io.File
import de.sciss.synth.io.{AudioFileSpec, SampleFormat, AudioFileType, AudioFile}

trait Processor {
   protected final def createTempAudioFile( id: String, numChannels: Int ) : AudioFile = {
      val file = File.createTempFile( "corr_" + id, ".aif" )
      file.deleteOnExit()
      AudioFile.openWrite( file, AudioFileSpec( AudioFileType.IRCAM, SampleFormat.Float, numChannels, 44100 ))
   }

   protected final def avg( b: Array[ Float ], off: Int, len: Int ) = {
      var sum = 0.0
      var i = off; val stop = off + len; while( i < stop ) {
         sum += b( i )
      i += 1 }
      (sum / len).toFloat
   }

   protected final def stat( mat: Array[ Array[ Float ]], frameOff: Int, frameLen: Int,
                             chanOff: Int, chanLen: Int ) : (Double, Double) = {
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
}