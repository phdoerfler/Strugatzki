/*
 *  FeatureExtraction.scala
 *  (Strugatzki)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
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
 */

package de.sciss.strugatzki
package impl

import sys.process.{ProcessLogger, Process}
import de.sciss.synth
import synth.GE
import synth.io.{AudioFileSpec, SampleFormat, AudioFileType, AudioFile}
import de.sciss.osc.{PacketCodec, Bundle}
import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import xml.XML
import concurrent.{ExecutionContext, Promise}

private[strugatzki] final class FeatureExtraction(val config: FeatureExtraction.Config,
                                                  protected val observer: FeatureExtraction.Observer,
                                                  protected val promise: Promise[Unit])
                                                 (implicit protected val executionContext: ExecutionContext)
  extends ProcessorImpl[Unit, FeatureExtraction.Config] {

  import FeatureExtraction._

  protected val companion = FeatureExtraction

  private var scsynth: Process = null

  protected def body() {
    import NonRealtimeProcessor.{BufferSpec, RenderConfig, render}

    val inSpec       = AudioFile.readSpec(config.audioInput)
    val inChannels  = inSpec.numChannels
    val stepSize    = config.fftSize / config.fftOverlap

//    val coeffRate = spec.sampleRate / stepSize
//
//    val so                = Server.Config()
//    val oscF              = IOUtil.createTempFile("tmp", ".osc")
//    val dummyOutput       = IOUtil.createTempFile("tmp", ".aif")
//    so.inputBusChannels   = spec.numChannels
//    so.sampleRate         = spec.sampleRate.toInt // coeffRate.toInt
//    so.outputBusChannels  = 1
//    so.nrtCommandPath     = oscF.getAbsolutePath
//    so.nrtInputPath       = Some(config.audioInput.getAbsolutePath)
//    so.nrtOutputPath      = dummyOutput.getAbsolutePath
//    so.nrtHeaderFormat    = AudioFileType.AIFF
//    so.nrtSampleFormat    = SampleFormat.Int16
//
//    val s                 = Server.dummy("nrt", so.build)
//    val coeffBufSize      = 1024 // 888
    val numFeatures       = config.numCoeffs + 1
//    val fftBufID          = 0
//    val coeffBufID        = 1
    val fftWinType        = 1 // -1 rect, 0 sine, 1 hann

    def extract(in0: GE): GE = {
      import synth._
      import ugen._

      val in      = config.channelsBehavior match {
        case ChannelsBehavior.Mix   => Mix(in0)
        case ChannelsBehavior.First => in0 \ 0
        case ChannelsBehavior.Last  => in0 \ (inChannels - 1)
      }
      val chain   = FFT("fft".kr, in, 1.0 / config.fftOverlap, fftWinType)
      val coeffs  = MFCC.kr(chain, config.numCoeffs)
      val loud    = Loudness.kr(chain) / 32
      val sig     = Flatten(Seq(loud, coeffs))
      sig
    }

//    val syn       = Synth(s)
//    val fftBuf    = new Buffer(s, fftBufID)
//    val coeffBuf  = new Buffer(s, coeffBufID)
//    val numFFTs   = ((spec.numFrames + stepSize - 1) / stepSize).toInt // + 1
//    val numWrites = (numFFTs + coeffBufSize - 1) / coeffBufSize

    val fftBuf = BufferSpec("fft", numFrames = config.fftSize)

    val rCfg = RenderConfig(
      inFile = config.audioInput, inSpec = inSpec,
      numFeatures = numFeatures, stepSize = stepSize,
      buffers = fftBuf :: Nil,
      outFile = Some(config.featureOutput),
      progress = progress(_), checkAborted = () => checkAborted()
    )

    render(rCfg)(extract)

    config.metaOutput.foreach { metaFile =>
      val xml = config.toXML
      XML.save(metaFile.getAbsolutePath, xml, "UTF-8", xmlDecl = true, doctype = null)
    }
  }

  override protected def cleanUp() {
    if (scsynth != null) {
      scsynth.destroy()
      scsynth = null
    }
  }
}