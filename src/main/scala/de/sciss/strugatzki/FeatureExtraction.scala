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

import java.io.{IOException, File}
import xml.{NodeSeq, XML}
import language.implicitConversions
import annotation.switch
import scala.util.control.NonFatal
import concurrent.{ExecutionContext, Promise}

object FeatureExtraction extends ProcessorCompanion {
  protected def defaultConfig = Config().build

  protected def create(settings: Config, observer: Observer, promise: Promise[Unit])
                      (implicit exec: ExecutionContext): Processor[Unit, FeatureExtraction.Config] =
    new impl.FeatureExtraction(settings, observer, promise)

  object ChannelsBehavior {
    def apply(id: Int): ChannelsBehavior = (id: @switch) match {
      case Mix.id   => Mix
      case First.id => First
      case Last.id  => Last
      case _        => throw new IllegalArgumentException(id.toString)
    }

    /** Input signal's channels are mixed together before taking the analysis */
    case object Mix extends ChannelsBehavior { final val id = 0 }

    /** Just the first channel is used in the analysis (i.e. the left channel if the audio input is stereo */
    case object First extends ChannelsBehavior { final val id = 1 }

    /** Just the last channel is used in the analysis (i.e. the right channel if the audio input is stereo */
    case object Last extends ChannelsBehavior { final val id = 2 }
  }

  /** Defines how analysis data is taken from multi channel files */
  sealed trait ChannelsBehavior {
    def id: Int
  }

  sealed trait ConfigLike {
    /**
     * The input audio file to extract the features from.
     */
    def audioInput: File

    /**
     * The output "audio" file to write the feature vectors to
     * (this will be in AIFF format).
     */
    def featureOutput: File

    /**
     * An optional file to which the extraction settings are
     * saved (in XML format).
     */
    def metaOutput: Option[File]

    /**
     * The number of MFCC used for the spectral feature.
     */
    def numCoeffs: Int

    /**
     * The FFT size used to calculate the feature vectors.
     */
    def fftSize: Int

    /**
     * The FFT overlap factor used to step from vector to vector.
     * This equals fftSize / stepSize, so a value of 2 means
     * the window step is half of the fft size (windows are 50% overlapping).
     */
    def fftOverlap: Int

    /**
     * The channel behaviour determines how to handle multichannel files.
     * Currently the feature vectors are calculated on a mono signal only.
     * This setting determines whether multiple channels in the input audio
     * file are mixed together, or if just the first or just the last
     * channel is used in the extraction process.
     */
    def channelsBehavior: ChannelsBehavior

    final def pretty: String = {
      "Config(\n   audioInput       = " + audioInput +
               "\n   featureOutput    = " + featureOutput +
               "\n   metaOutput       = " + metaOutput +
               "\n   numCoeffs        = " + numCoeffs +
               "\n   fftSize          = " + fftSize +
               "\n   fftOverlap       = " + fftOverlap +
               "\n   channelsBehavior = " + channelsBehavior + "\n)"
    }
  }

  object ConfigBuilder {
    def apply(settings: Config): ConfigBuilder = {
      val sb = Config()
      sb.read(settings)
      sb
    }
  }

  final class ConfigBuilder private[FeatureExtraction]() extends ConfigLike {
    /**
     * The audio input defaults to `input.aif` (relative path)
     */
    var audioInput: File = new File("input.aif")
    /**
     * The feature vector output file defaults to a temporary file
     * beginning with `features` and having suffix `.aif`.
     *
     * @see  Strugatzki#tmpDir
     */
    var featureOutput: File = File.createTempFile("features", ".aif", Strugatzki.tmpDir)
    /**
     * The extraction meta data file option defaults to `None`
     */
    var metaOutput = Option.empty[File]
    /**
     * The number of MFCC defaults to 13.
     */
    var numCoeffs: Int = 13
    /**
     * The FFT size defaults to 1024
     */
    var fftSize: Int = 1024
    /**
     * The FFT overlap defaults to 2
     */
    var fftOverlap: Int = 2
    /**
     * The multichannel behaviour defaults to `Mix`.
     */
    var channelsBehavior: ChannelsBehavior = ChannelsBehavior.Mix

    def build: Config = Impl(audioInput, featureOutput, metaOutput, numCoeffs, fftSize, fftOverlap, channelsBehavior)

    def read(settings: Config) {
      audioInput        = settings.audioInput
      featureOutput     = settings.featureOutput
      metaOutput        = settings.metaOutput
      numCoeffs         = settings.numCoeffs
      fftSize           = settings.fftSize
      fftOverlap        = settings.fftOverlap
      channelsBehavior  = settings.channelsBehavior
    }

    private final case class Impl(audioInput: File, featureOutput: File, metaOutput: Option[File],
                                  numCoeffs: Int, fftSize: Int, fftOverlap: Int, channelsBehavior: ChannelsBehavior)
      extends Config {

      override def productPrefix = "Config"

      def toXML =
<feature>
  <input>{audioInput.getPath}</input>
  <output>{featureOutput.getPath}</output>
  <meta>{metaOutput.map( _.getPath ).getOrElse( "" )}</meta>
  <numCoeffs>{numCoeffs}</numCoeffs>
  <fftSize>{fftSize}</fftSize>
  <fftOverlap>{fftOverlap}</fftOverlap>
  <channels>{channelsBehavior.id}</channels>
</feature>
    }
  }

  object Config {
    def apply() = new ConfigBuilder

    implicit def build(sb: ConfigBuilder): Config = sb.build

    def fromXMLFile(file: File): Config = {
      val xml = try {
        XML.loadFile(file)
      } catch {
        case NonFatal(e) => throw new IOException("In file: " + file.getPath, e)
      }
      fromXML(xml)
    }

    def fromXML(xml: NodeSeq): Config = {
      val sb = Config()
      sb.audioInput     = new File((xml \ "input").text)
      sb.featureOutput  = new File((xml \ "output").text)
      sb.metaOutput     = {
        val e = (xml \ "meta").text
        if (e.isEmpty) None else Some(new File(e))
      }
      sb.numCoeffs        = (xml \ "numCoeffs").text.toInt
      sb.fftSize          = (xml \ "fftSize").text.toInt
      sb.fftOverlap       = (xml \ "fftOverlap").text.toInt
      sb.channelsBehavior = {
        val e = (xml \ "channels").text
        if (e.isEmpty) ChannelsBehavior.Mix else ChannelsBehavior(e.toInt)
      }
      sb.build
    }
  }
  sealed trait Config extends ConfigLike {
    def toXML: xml.Node
  }

  type PayLoad = Unit
}
