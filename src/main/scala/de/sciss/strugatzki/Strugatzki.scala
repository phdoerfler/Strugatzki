/*
 *  Strugatzki.scala
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

import scopt.OptionParser
import collection.breakOut
import java.io.IOException
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFileSpec, AudioFile}
import java.util.Locale
import java.text.{DecimalFormat, NumberFormat}
import FeatureExtraction.{Config => ExtrConfig}
import scala.util.{Failure, Success}
import de.sciss.processor.{ProcessorFactory, Processor}
import concurrent.{Future, Await, ExecutionContext}
import de.sciss.span.Span
import concurrent.duration.Duration
import de.sciss.file._

object Strugatzki {
  import ExecutionContext.Implicits.global

  final val NormalizeName   = "feat_norms.aif"
  var tmpDir                = file(sys.props.getOrElse("java.io.tmpdir", "/tmp"))
  final val name            = "Strugatzki"

  private lazy val decibelFormat = {
    val res = NumberFormat.getInstance(Locale.US)
    res match {
      case d: DecimalFormat => {
        d.setGroupingUsed(false)
        d.setMinimumFractionDigits(1)
        d.setMaximumFractionDigits(1)
        d.setNegativeSuffix(" dB")
        d.setPositiveSuffix(" dB")
      }
      case _ =>
    }
    res
  }

  private lazy val percentFormat = {
    val res = NumberFormat.getPercentInstance(Locale.US)
    res match {
      case d: DecimalFormat => {
        d.setGroupingUsed(false)
        d.setMinimumFractionDigits(1)
        d.setMaximumFractionDigits(1)
      }
      case _ =>
    }
    res
  }

  def main(args: Array[String]): Unit = {
    var which = ""

    val parser = new OptionParser(name) {
      opt("f", "feature", "Feature extraction",                                   action = { which = "feat"  })
      opt("c", "correlate", "Find best correlation with database",                action = { which = "corr"  })
      opt("s", "segmentation", "Find segmentation breaks with a file",            action = { which = "segm"  })
      opt("x", "selfsimilarity", "Create an image of the self similarity matrix", action = { which = "self"  })
      opt("y", "crosssimilarity", "Create a cross-similarity vector file",        action = { which = "cross" })
      opt("stats", "Statistics from feature database",                            action = { which = "stats" })
    }
    if( parser.parse(args.take(1))) {
      val argsRem = args.drop(1)
      which match {
        case "feat"   => featurePre  (argsRem)
        case "stats"  => featureStats(argsRem)
        case "corr"   => featureCorr (argsRem)
        case "segm"   => featureSegm (argsRem)
        case "self"   => featureSelf (argsRem)
        case "cross"  => featureCross(argsRem)
        case _        =>
          parser.showUsage
          sys.exit(1)
      }
    } else sys.exit(1) // parser.showUsage
  }

  private def go(factory: ProcessorFactory { type Repr <: Future[_]})(config: factory.Config)
                (observer: factory.Observer): Unit = {
    val proc = factory.run(config)(observer)
    Await.ready(proc, Duration.Inf)
  }

  def featureCorr(args: Array[String]): Unit = {
    var dirOption     = Option.empty[String]
    var verbose       = false
    var punchInStart  = Option.empty[Double]
    var punchInStop   = Option.empty[Double]
    var tempIn        = 0.5
    var punchOutStart = Option.empty[Double]
    var punchOutStop  = Option.empty[Double]
    var tempOut       = 0.5
    var minPunch      = Option.empty[Double]
    var maxPunch      = Option.empty[Double]
    var input         = Option.empty[String]
    var maxBoost      = 8.0
    var numMatches    = 1
    var numPerFile    = 1
    var minSpacing    = 0.0 // 0.5
    var normalize     = true

    implicit val parser  = new OptionParser(name + " -c") {
      opt("v", "verbose", "Verbose output", action = { verbose = true })
      opt("d", "dir", "<directory>", "Database directory", (s: String) => dirOption    = Some(s))
      doubleOpt("in-start", "Punch in begin (secs)"      , (d: Double) => punchInStart = Some(d))
      doubleOpt("in-stop", "Punch in end (secs)"         , (d: Double) => punchInStop  = Some(d))
      doubleOpt("in-temp", "Temporal weight for punch in (0 to 1, default 0.5)", tempIn = _ )
      doubleOpt("out-start", "Punch out begin (secs)", (d: Double) => punchOutStart = Some(d))
      doubleOpt("out-stop", "Punch out end (secs)", (d: Double) => punchOutStop  = Some( d ))
      doubleOpt("out-temp", "Temporal weight for punch out (0 to 1, default 0.5)", tempOut = _)
      doubleOpt("dur-min", "Minimum fill duration (secs)", (d: Double) => minPunch = Some(d))
      doubleOpt("dur-max", "Maximum fill duration (secs)", (d: Double) => maxPunch = Some(d))
      doubleOpt("boost-max", "Maximum loudness boost factor (default 8)", maxBoost = _)
      intOpt("m", "num-matches", "Maximum number of matches (default 1)", numMatches = _)
      intOpt("num-per-file", "Maximum matches per single file (default 1)", numPerFile = _)
      doubleOpt("spacing", "Minimum spacing between matches within one file (default 0.0)", minSpacing = _)
      arg("input", "Meta file of input to process", (i: String) => input = Some(i))
      opt("no-norm", "Do not apply feature normalization", action = { normalize = false })
    }

    if (!parser.parse(args)) sys.exit(1)

    (input, punchInStart, punchInStop, minPunch, maxPunch, dirOption) match {
      case (Some(in), Some(piStart), Some(piStop), Some(pMin), Some(pMax), Some(dir)) =>
        val inFile = file(in)
        val metaIn = FeatureExtraction.Config.fromXMLFile(inFile)
        val inSpec = AudioFile.readSpec(metaIn.audioInput)

        def secsToFrames(s: Double) = (s * inSpec.sampleRate + 0.5).toLong

        val (ok, punchOutO) = (punchOutStart, punchOutStop) match {
          case (Some(poStart), Some(poStop)) =>
            val outSpan = Span(secsToFrames(poStart), secsToFrames(poStop))
            require(outSpan.length > 0, "Punch out span is empty")
            true -> Some(FeatureCorrelation.Punch(outSpan, tempOut.toFloat))

          case (None, None) => true -> None
          case _            => false -> None
        }
        if (ok) {
          val inSpan    = Span(secsToFrames(piStart), secsToFrames(piStop))
          require(inSpan.length > 0, "Punch in span is empty")
          val punchIn   = FeatureCorrelation.Punch(inSpan, tempIn.toFloat)
          val minFrames = secsToFrames(pMin)
          require(minFrames > 0, "Minimum duration is zero")
          val maxFrames = secsToFrames(pMax)
          require(maxFrames >= minFrames, "Maximum duration is smaller than minimum duration")

          FeatureCorrelation.verbose = verbose
          val con             = FeatureCorrelation.Config()
          con.databaseFolder  = file(dir)
          con.punchIn         = punchIn
          con.punchOut        = punchOutO
          con.metaInput       = inFile
          con.minPunch        = minFrames
          con.maxPunch        = maxFrames
          con.normalize       = normalize
          con.maxBoost        = maxBoost.toFloat
          con.numMatches      = numMatches
          con.numPerFile      = numPerFile
          con.minSpacing      = secsToFrames(minSpacing)

          import Processor._
          var lastProg = 0
          go(FeatureCorrelation)(con) {
            case Result(_, Success(res)) if res.nonEmpty =>
              println("  Success.")

              res.foreach { m =>
                println("\nFile      : " + m.file.getAbsolutePath +
                        "\nSimilarity: " + toPercentStr(m.sim) +
                        "\nSpan start: " + m.punch.start +
                        "\nBoost in  : " + toDBStr(m.boostIn))
                if (punchOutO.isDefined) {
                  println("Span stop : " + m.punch.stop +
                        "\nBoost out : " + toDBStr(m.boostOut))
                }
              }
              println()

            case Result(_, Success(_)) =>
              println("  No matches found.")
            case Result(_, Failure(Aborted())) =>
              println("  Aborted")
            case Result(_, Failure(e)) =>
              println("  Failed: ")
              e.printStackTrace()
            case Progress(_, perc) =>
              val i = (perc * 25).toInt
              while (lastProg < i) {
                print("#")
                lastProg += 1
              }
          }

        } else exit1()

      case _ => exit1()
    }
  }

  private def ampToDB     (amp: Double) = 20 * math.log10(amp)
  private def toPercentStr(d: Double)   = percentFormat.format(d)
  private def toDBStr     (amp: Double) = decibelFormat.format(ampToDB(amp))

  def featureSegm(args: Array[String]): Unit = {
    var dirOption     = Option.empty[String]
    var verbose       = false
    var corrLen       = 0.5
    var temp          = 0.5
    var spanStart     = Option.empty[Double]
    var spanStop      = Option.empty[Double]
    var numBreaks     = 1
    var minSpacing    = 0.2
    var input         = Option.empty[String]
    var normalize     = true

    implicit val parser = new OptionParser(name + " -s") {
      opt("v", "verbose", "Verbose output", action = { verbose = true })
      opt("d", "dir", "<directory>", "Database directory (required for normalization file)", (s: String) => dirOption = Some(s))
      doubleOpt("length", "Correlation length in secs (default: 0.5)", corrLen = _)
      doubleOpt("temp", "Temporal weight (0 to 1, default 0.5)", temp = _)
      doubleOpt("span-start", "Search begin in file (secs)", (d: Double) => spanStart = Some(d))
      doubleOpt("span-stop", "Search end in file (secs)", (d: Double) => spanStop = Some(d))
      intOpt("m", "num-breaks", "Maximum number of breaks (default 1)", numBreaks = _)
      doubleOpt("spacing", "Minimum spacing between matches within one file (default 0.2)", minSpacing = _)
      arg("input", "Meta file of input to process", (i: String) => input = Some(i))
      opt("no-norm", "Do not apply feature normalization", action = { normalize = false })
    }

    if (!parser.parse(args)) sys.exit(1)

    input match {
      case Some(in) =>
        val inFile = file(in)
        val metaIn = FeatureExtraction.Config.fromXMLFile(inFile)
        val inSpec = AudioFile.readSpec(metaIn.audioInput)

        def secsToFrames(s: Double) = (s * inSpec.sampleRate + 0.5).toLong

        val span = (spanStart, spanStop) match {
          case (Some(start) , Some(stop)) => Span     (secsToFrames(start), secsToFrames(stop))
          case (Some(start) , None      ) => Span.from(secsToFrames(start))
          case (None        , Some(stop)) => Span.until                    (secsToFrames(stop))
          case (None        , None      ) => Span.All
        }
        require(span.nonEmpty, "Span is empty")
        val corrFrames = secsToFrames(corrLen)
        require(corrFrames > 0, "Correlation duration is zero")

        FeatureSegmentation.verbose = verbose
        val con             = FeatureSegmentation.Config()
        con.metaInput       = inFile
        con.span            = span
        con.corrLen         = corrFrames
        con.temporalWeight  = temp.toFloat
        con.normalize       = normalize
        con.numBreaks       = numBreaks
        con.minSpacing      = secsToFrames(minSpacing)

        if (normalize) dirOption match {
          case Some(dir) =>
            con.databaseFolder = new File(dir)
          case _ => exit1()
        }

        import Processor._
        var lastProg = 0
        go(FeatureSegmentation)(con) {
          case Result(_, Success(res)) if res.nonEmpty =>
            println("  Success.")

            res.foreach { b =>
              println("\nSimilarity: " + toPercentStr(b.sim) +
                      "\nPosition:   " + b.pos)
            }
            println()

          case Result(_, Success(_)) =>
            println("  No breaks found.")
          case Result(_, Failure(Aborted())) =>
            println("  Aborted")
          case Result(_, Failure(e)) =>
            println("  Failed: ")
            e.printStackTrace()
          case Progress(_, perc) =>
            val i = (perc * 25).toInt
            while (lastProg < i) {
              print("#")
              lastProg += 1
            }
        }

      case _ => exit1()
    }
  }

  def featureSelf(args: Array[String]): Unit = {
    import SelfSimilarity.{ColorScheme, PsychoOptical, Config}
    var dirOption     = Option.empty[String]
    var verbose       = false
    var corrLen       = 1.0
    var decim         = 1
    var temp          = 0.5
    var spanStart     = Option.empty[Double]
    var spanStop      = Option.empty[Double]
    var input         = Option.empty[String]
    var output        = Option.empty[String]
    var colorWarp     = 1.0
    var colorCeil     = 1.0
    var colors        = PsychoOptical: ColorScheme
    var colorInv      = false
    var normalize     = true

    implicit val parser = new OptionParser(name + " -x") {
      opt("v", "verbose", "Verbose output", action = { verbose = true })
      opt("d", "dir", "<directory>", "Database directory (required for normalization file)", (s: String) => dirOption = Some(s))
      doubleOpt("length", "Correlation length in secs (default: 1.0)", corrLen = _)
      doubleOpt("temp", "Temporal weight (0 to 1, default 0.5)", temp = _)
      doubleOpt("span-start", "Correlation begin in file (secs)", (d: Double) => spanStart = Some(d))
      doubleOpt("span-stop", "Correlation end in file (secs)", (d: Double) => spanStop = Some(d))
      opt("c", "colors", "(gray|psycho)", "Color scale (defaults to 'psycho')", (s: String) => colors = ColorScheme(s))
      doubleOpt("color-warp", "Color scale warping factor (default: 1.0)", (d: Double) => colorWarp = d)
      doubleOpt("color-ceil", "Color scale input ceiling (default: 1.0)", (d: Double) => colorCeil = d)
      opt("i", "color-inv", "Inverted color scale", action = { colorInv = true })
      intOpt("m", "decim", "Pixel decimation factor (default: 1)", (i: Int) => decim = i)
      arg("input", "Meta file of input to process", (i: String) => input = Some(i))
      arg("output", "Image output file", (i: String) => output = Some(i))
      opt("no-norm", "Do not apply feature normalization", action = { normalize = false })
    }

    if (!parser.parse(args)) sys.exit(1)

    (input, output) match {
      case (Some(in), Some(out)) =>
        val inFile  = file(in)
        val outFile = file(out)
        val metaIn  = FeatureExtraction.Config.fromXMLFile(inFile)
        val inSpec  = AudioFile.readSpec(metaIn.audioInput)

        def secsToFrames(s: Double) = (s * inSpec.sampleRate + 0.5).toLong

        val span = (spanStart, spanStop) match {
          case (Some(start) , Some(stop)) => Span(secsToFrames(start), secsToFrames(stop))
          case (Some(start) , None      ) => Span.from(secsToFrames(start))
          case (None        , Some(stop)) => Span.until(secsToFrames(stop))
          case (None        , None      ) => Span.all
        }
        require(span.nonEmpty, "Span is empty")
        val corrFrames = secsToFrames(corrLen)
        require(corrFrames > 0, "Correlation duration is zero")

        SelfSimilarity.verbose = verbose
        val con             = Config()
        con.metaInput       = inFile
        con.imageOutput     = outFile
        con.span            = span
        con.corrLen         = corrFrames
        con.decimation      = decim
        con.temporalWeight  = temp.toFloat
        con.colors          = colors
        con.colorWarp       = colorWarp.toFloat
        con.colorCeil       = colorCeil.toFloat
        con.colorInv        = colorInv
        con.normalize       = normalize

        if (normalize) dirOption match {
          case Some(dir) =>
            con.databaseFolder = new File(dir)
          case _ => exit1()
        }

        import Processor._
        var lastProg = 0
        go(SelfSimilarity)(con) {
          case Result(_, Success(_)) =>
            println("  Done.")
            println()
          case Result(_, Failure(Aborted())) =>
            println("  Aborted")
          case Result(_, Failure(e)) =>
            println("  Failed: ")
            e.printStackTrace()
          case Progress(_, perc) =>
            val i = (perc * 25).toInt
            while (lastProg < i) {
              print("#")
              lastProg += 1
            }
        }

      case _ => exit1()
    }
  }

  def featureStats(args: Array[String]): Unit = {
    var dirOption = Option.empty[String]
    var verbose   = false

    implicit val parser = new OptionParser(name + " --stats") {
      opt("v", "verbose", "Verbose output", action = { verbose = true })
      opt("d", "dir", "<directory>", "Database directory", (s: String) => dirOption = Some(s))
    }
    if (!parser.parse(args)) sys.exit(1)

    val dir = dirOption match {
      case Some(d) => d
      case None => exit1()
    }

    println( "Starting stats... " )
    val paths = file(dir).children(_.name.endsWith("_feat.aif"))
    import Processor._
    var lastProg = 0
    go(FeatureStats)(paths) {
      case Result(_, Success(spans)) =>
        println("  Success.")
        val afNorm = AudioFile.openWrite(new File(dir, NormalizeName),
          AudioFileSpec(AudioFileType.AIFF, SampleFormat.Float, spans.size, 44100))
        try {
          val b = afNorm.buffer(2)
          spans.zipWithIndex.foreach {
            case ((min, max), i) =>
              b(i)(0) = min.toFloat
              b(i)(1) = max.toFloat
          }
          afNorm.write(b)
        } finally {
          afNorm.close()
        }
        println("Done.")
      case Result(_, Failure(Aborted())) =>
        println("  Aborted")
      case Result(_, Failure((e))) =>
        println("  Failed: ")
        e.printStackTrace()
      case Progress(_, perc) =>
        val i = (perc * 25).toInt
        while (lastProg < i) {
          print("#")
          lastProg += 1
        }
    }
  }

  private def exit1()(implicit p: OptionParser): Nothing = {
    p.showUsage
    sys.exit(1)
  }

  def featurePre(args: Array[String]): Unit = {
    var inputs      = IndexedSeq.empty[String]
    var dirOption   = Option.empty[String]
    var verbose     = false
    var chanString  = "mix"

    implicit val parser = new OptionParser(name + " -f") {
      opt("v", "verbose", "Verbose output", action = { verbose = true })
      arglistOpt("inputs...", "List of input files or directories", inputs +:= _)
      opt("d", "dir", "<directory>", "Target directory", (s: String) => dirOption = Some(s))
      opt("c", "channels", "(mix|first|last)", "Channel mode (defaults to 'mix')", (s: String) => chanString = s)
    }
    if (!parser.parse(args)) sys.exit(1)
    val dir = dirOption match {
      case Some(d) => d
      case None => exit1()
    }
    if (inputs.isEmpty) exit1()

    import FeatureExtraction.ChannelsBehavior
    val chanMode: ChannelsBehavior = chanString.toLowerCase match {
      case "mix"    => ChannelsBehavior.Mix
      case "first"  => ChannelsBehavior.First
      case "last"   => ChannelsBehavior.Last
      case _        => exit1()
    }

    FeatureExtraction.verbose = verbose
    val inFiles: List[File] = inputs.flatMap(p => {
      val f = new File(p)
      if (f.isFile) {
        f :: Nil
      } else if (f.isDirectory) {
        f.children(f => try {
          AudioFile.identify(f).isDefined
        } catch {
          case _: IOException => false
        })
      } else {
        sys.error(s"Not a valid input: $p")
      }
    })(breakOut)

    val targetDir         = file(dir)
    val con               = ExtrConfig()
    con.channelsBehavior  = chanMode

    def iter(list: List[File]) {
      list match {
        case head :: tail =>
          val name1 = {
            val n = head.getName
            val i = n.lastIndexOf('.')
            if (i >= 0) n.substring(0, i) else n
          }
          con.audioInput    = head
          con.featureOutput = new File(targetDir, name1 + "_feat.aif")
          con.metaOutput    = Some(new File(targetDir, name1 + "_feat.xml"))
          feature(con)(if (_) iter(tail))
        case _ =>
      }
    }
    iter(inFiles)
  }

  def featureCross(args: Array[String]): Unit = {
    var dirOption     = Option.empty[String]
    var verbose       = false
    var temp          = 0.5
    var input1        = "unspecified"
    var input2        = "unspecified"
    var spanStart1    = Option.empty[Double]
    var spanStop1     = Option.empty[Double]
    var spanStart2    = Option.empty[Double]
    var spanStop2     = Option.empty[Double]
    var output        = "unspecified"
    var normalize     = true
    var maxBoost      = 8.0

    implicit val parser = new OptionParser(name + " -y") {
      opt("v", "verbose", "Verbose output", action = { verbose = true })
      opt("d", "dir", "<directory>", "Database directory (required for normalization file)", (s: String) => dirOption = Some(s))
      doubleOpt("temp", "Temporal weight (0 to 1, default 0.5)", temp = _)
      doubleOpt("span1-start", "Correlation begin in first file (secs)", (d: Double) => spanStart1 = Some(d))
      doubleOpt("span1-stop" , "Correlation end in first file (secs)"  , (d: Double) => spanStop1  = Some(d))
      doubleOpt("span2-start", "Correlation begin in second file (secs)", (d: Double) => spanStart2 = Some(d))
      doubleOpt("span2-stop" , "Correlation end in second file (secs)"  , (d: Double) => spanStop2  = Some(d))
      arg("input1", "Meta file of first input to process" , (i: String) => input1 = i)
      arg("input2", "Meta file of second input to process", (i: String) => input2 = i)
      arg("output", "Audio output file", (i: String) => output = i)
      doubleOpt( "boost-max", "Maximum loudness boost factor (default 8)", maxBoost = _ )
      opt("no-norm", "Do not apply feature normalization", action = {
        normalize = false
      })
    }

    if (!parser.parse(args)) sys.exit(1)

    if (normalize && dirOption.isEmpty) {
      Console.err.println(s"Either choose --no-norm or specify a database --dir.")
      sys.exit(1)
    }

    val inFile1 = file(input1)
    val metaIn1 = FeatureExtraction.Config.fromXMLFile(inFile1)
    val inSpec1 = AudioFile.readSpec(metaIn1.audioInput)
    val inFile2 = file(input2)
    val metaIn2 = FeatureExtraction.Config.fromXMLFile(inFile2)
    val inSpec2 = AudioFile.readSpec(metaIn2.audioInput)

    def secsToFrames(s: Double)(implicit spec: AudioFileSpec) = (s * spec.sampleRate + 0.5).toLong

    def mkSpan(start: Option[Double], stop: Option[Double])
              (implicit spec: AudioFileSpec): Span.NonVoid = (start, stop) match {
      case (Some(startS), Some(stopS) ) => Span(secsToFrames(startS), secsToFrames(stopS))
      case (Some(startS), None        ) => Span.from (secsToFrames(startS))
      case (None        , Some(stopS) ) => Span.until(secsToFrames(stopS ))
      case (None        , None        ) => Span.all
    }

    val span1 = mkSpan(spanStart1, spanStop1)(inSpec1)
    val span2 = mkSpan(spanStart2, spanStop2)(inSpec2)

    CrossSimilarity.verbose = verbose
    val con             = CrossSimilarity.Config()
    dirOption.foreach(d => con.databaseFolder = file(d))
    con.metaInput1      = inFile1
    con.metaInput2      = inFile2
    con.normalize       = normalize
    con.maxBoost        = maxBoost.toFloat
    con.audioOutput     = file(output)
    // con.audioOutputType = automatically
    con.span1           = span1
    con.span2           = span2
    con.temporalWeight  = temp.toFloat

    import Processor._
    var lastProg = 0
    go(CrossSimilarity)(con) {
      case Result(_, Success(_)) =>
        println("  Success.")
      case Result(_, Failure(Aborted())) =>
        println("  Aborted")
      case Result(_, Failure(e)) =>
        println("  Failed: ")
        e.printStackTrace()
      case Progress(_, perc) =>
        val i = (perc * 25).toInt
        while (lastProg < i) {
          print("#")
          lastProg += 1
        }
    }
  }

  def feature(con: ExtrConfig)(whenDone: Boolean => Unit): Unit = {
    import Processor._
    println("Starting extraction... " + con.audioInput.getName)
    var lastProg = 0
    go(FeatureExtraction)(con) {
      case Result(_, Success(_)) =>
        println("  Success.")
        whenDone(true)
      case Result(_, Failure(Aborted())) =>
        println("  Aborted")
        whenDone(false)
      case Result(_, Failure(e)) =>
        println("  Failed: ")
        e.printStackTrace()
        whenDone(false)
      case Progress(_, perc) =>
        val i = (perc * 25).toInt
        while (lastProg < i) {
          print("#")
          lastProg += 1
        }
    }
  }
}