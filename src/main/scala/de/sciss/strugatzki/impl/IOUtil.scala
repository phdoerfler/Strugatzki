/*
 *  IOUtil.scala
 *  (Strugatzki)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.strugatzki
package impl

import java.io.File
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec, AudioFile}

object IOUtil {
  def createTempFile(prefix: String, suffix: String): File = {
    val f = File.createTempFile(prefix, suffix, Strugatzki.tmpDir)
    f.deleteOnExit()
    f
  }

  def createTempAudioFile(id: String, numChannels: Int, sampleRate: Double = 44100): AudioFile = {
    val file  = createTempFile("struga_" + id, ".irc")
    val spec  = AudioFileSpec(fileType = AudioFileType.IRCAM, sampleFormat = SampleFormat.Float,
      numChannels = numChannels, sampleRate = sampleRate)
    AudioFile.openWrite(file, spec)
  }
}