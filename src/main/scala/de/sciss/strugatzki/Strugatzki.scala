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
import java.io.{IOException, File}
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFileSpec, AudioFile}
import java.util.Locale
import java.text.{DecimalFormat, NumberFormat}
import FeatureExtraction.{Config => ExtrConfig}
import scala.util.{Failure, Success}
import de.sciss.strugatzki.Processor.Aborted
import concurrent.ExecutionContext

object Strugatzki {
  import ExecutionContext.Implicits.global

  final val NORMALIZE_NAME  = "feat_norms.aif"
  var tmpDir                = new File(sys.props.getOrElse("java.io.tmpdir", "/tmp"))
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

  def main(args: Array[String]) {
    var which = ""

    val parser = new OptionParser(name) {
      opt( "f", "feature", "Feature extraction",                                   action = { which = "feat"  })
      opt( "c", "correlate", "Find best correlation with database",                action = { which = "corr"  })
      opt( "s", "segmentation", "Find segmentation breaks with a file",            action = { which = "segm"  })
      opt( "x", "selfsimilarity", "Create an image of the self similarity matrix", action = { which = "self"  })
      opt( "stats", "Statistics from feature database",                            action = { which = "stats" })
    }
    if( parser.parse(args.take(1))) {
      val argsRem = args.drop(1)
      which match {
        case "feat"   => featurePre  (argsRem)
        case "stats"  => featureStats(argsRem)
        case "corr"   => featureCorr (argsRem)
        case "segm"   => featureSegm (argsRem)
        case "self"   => featureSelf (argsRem)
        case _        =>
          parser.showUsage
          sys.exit(1)
      }
    } else sys.exit(1) // parser.showUsage
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

      implicit val parser  = new OptionParser( name + " -c" ) {
         opt( "v", "verbose", "Verbose output", action = { verbose = true })
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
         doubleOpt( "spacing", "Minimum spacing between matches within one file (default 0.0)", minSpacing = _ )
         arg( "input", "Meta file of input to process", (i: String) => input = Some( i ))
         opt( "no-norm", "Do not apply feature normalization", action = { normalize = false })
      }

    if (!parser.parse(args)) sys.exit(1)

    (input, punchInStart, punchInStop, minPunch, maxPunch, dirOption) match {
         case (Some( in ), Some( piStart ), Some( piStop ), Some( pMin ), Some( pMax ), Some( dir )) =>
            val inFile  = new File( in )
            val metaIn  = FeatureExtraction.Config.fromXMLFile( inFile )
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
               val set              = FeatureCorrelation.Config()
               set.databaseFolder   = new File( dir )
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

               import FeatureCorrelation._
               var lastProg = 0
               FeatureCorrelation(set) {
                  case Result(Success(res)) if (res.nonEmpty) =>
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

                  case Result(Success(_)) =>
                     println( "  No matches found." )
                  case Result(Failure(Aborted())) =>
                     println( "  Aborted" )
                  case Result(Failure(e)) =>
                     println( "  Failed: " )
                     e.printStackTrace()
                  case Progress(perc) =>
                     val i = perc >> 2
                     while( lastProg < i ) {
                        print( "#" )
                     lastProg += 1 }
               }

            } else exit1()

         case _ => exit1()
      }
   }

  private def ampToDB     (amp: Double) = 20 * math.log10(amp)
  private def toPercentStr(d: Double)   = percentFormat.format(d)
  private def toDBStr     (amp: Double) = decibelFormat.format(ampToDB(amp))

  def featureSegm( args: Array[ String ]) {
//      var dirOption     = Option.empty[ String ]
//      var verbose       = false
//      var corrLen       = 0.5
//      var temp          = 0.5
//      var spanStart     = Option.empty[ Double ]
//      var spanStop      = Option.empty[ Double ]
//      var numBreaks     = 1
//      var minSpacing    = 0.2
//      var input         = Option.empty[ String ]
//      var normalize     = true
//
//      implicit val parser  = new OptionParser( name + " -s" ) {
//         opt( "v", "verbose", "Verbose output", action = { verbose = true })
//         opt( "d", "dir", "<directory>", "Database directory (required for normalization file)", (s: String) => dirOption = Some( s ))
//         doubleOpt( "length", "Correlation length in secs (default: 0.5)", corrLen = _ )
//         doubleOpt( "temp", "Temporal weight (0 to 1, default 0.5)", temp = _ )
//         doubleOpt( "span-start", "Search begin in file (secs)", (d: Double) => spanStart = Some( d ))
//         doubleOpt( "span-stop", "Search end in file (secs)", (d: Double) => spanStop  = Some( d ))
//         intOpt( "m", "num-breaks", "Maximum number of breaks (default 1)", numBreaks = _ )
//         doubleOpt( "spacing", "Minimum spacing between matches within one file (default 0.2)", minSpacing = _ )
//         arg( "input", "Meta file of input to process", (i: String) => input = Some( i ))
//         opt( "no-norm", "Do not apply feature normalization", action = { normalize = false })
//      }
//
//      if( !parser.parse( args )) sys.exit( 1 )
//
//      input match {
//         case Some( in ) =>
//            val inFile  = new File( in )
//            val metaIn  = FeatureExtraction.Settings.fromXMLFile( inFile )
//            val inSpec  = AudioFile.readSpec( metaIn.audioInput )
//
//            def secsToFrames( s: Double ) = (s * inSpec.sampleRate + 0.5).toLong
//
//            val span    = (spanStart, spanStop) match {
//               case (Some( start ), Some( stop )) => Some( Span( secsToFrames( start ), secsToFrames( stop )))
//               case (Some( start ), None)         => Some( Span( secsToFrames( start ), inSpec.numFrames ))
//               case (None,          Some( stop )) => Some( Span( 0L, secsToFrames( stop )))
//               case (None,          None)         => None
//            }
//            span.foreach( s => require( s.length > 0, "Span is empty" ))
//            val corrFrames = secsToFrames( corrLen )
//            require( corrFrames > 0, "Correlation duration is zero" )
//
//            FeatureSegmentation.verbose = verbose
//            val set              = FeatureSegmentation.SettingsBuilder()
//            set.metaInput        = inFile
//            set.span             = span
//            set.corrLen          = corrFrames
//            set.temporalWeight   = temp.toFloat
//            set.normalize        = normalize
//            set.numBreaks        = numBreaks
//            set.minSpacing       = secsToFrames( minSpacing )
//
//            if( normalize ) dirOption match {
//               case Some( dir )  =>
//                  set.databaseFolder   = new File( dir )
//               case _ => exit1()
//            }
//
//            import FeatureSegmentation._
//            var lastProg = 0
//            val fs = FeatureSegmentation( set ) {
//               case Success( res ) if( res.nonEmpty ) =>
//                  println( "  Success." )
//
//                  res.foreach { b =>
//                     println(  "\nSimilarity: " + toPercentStr( b.sim ) +
//                               "\nPosition:   " + b.pos )
//                  }
//                  println()
//
//               case Success( _ ) =>
//                  println( "  No breaks found." )
//               case Failure( e ) =>
//                  println( "  Failed: " )
//                  e.printStackTrace()
//               case Aborted =>
//                  println( "  Aborted" )
//               case Progress( perc ) =>
//                  val i = perc >> 2
//                  while( lastProg < i ) {
//                     print( "#" )
//                  lastProg += 1 }
//            }
//            fs.start()
//
//         case _ => exit1()
//      }
   }

   def featureSelf( args: Array[ String ]) {
      import SelfSimilarity.{ColorScheme, PsychoOptical, Config}
      var dirOption     = Option.empty[ String ]
      var verbose       = false
      var corrLen       = 1.0
      var decim         = 1
      var temp          = 0.5
      var spanStart     = Option.empty[ Double ]
      var spanStop      = Option.empty[ Double ]
      var input         = Option.empty[ String ]
      var output        = Option.empty[ String ]
      var colorWarp     = 1.0
      var colorCeil     = 1.0
      var colors        = PsychoOptical: ColorScheme
      var colorInv      = false
      var normalize     = true

      implicit val parser  = new OptionParser( name + " -x" ) {
         opt( "v", "verbose", "Verbose output", action = { verbose = true })
         opt( "d", "dir", "<directory>", "Database directory (required for normalization file)", (s: String) => dirOption = Some( s ))
         doubleOpt( "length", "Correlation length in secs (default: 1.0)", corrLen = _ )
         doubleOpt( "temp", "Temporal weight (0 to 1, default 0.5)", temp = _ )
         doubleOpt( "span-start", "Correlation begin in file (secs)", (d: Double) => spanStart = Some( d ))
         doubleOpt( "span-stop", "Correlation end in file (secs)", (d: Double) => spanStop  = Some( d ))
         opt( "c", "colors", "(gray|psycho)", "Color scale (defaults to 'psycho')", (s: String) => colors = ColorScheme( s ))
         doubleOpt( "color-warp", "Color scale warping factor (default: 1.0)", (d: Double) => colorWarp = d )
         doubleOpt( "color-ceil", "Color scale input ceiling (default: 1.0)", (d: Double) => colorCeil = d )
         opt( "i", "color-inv", "Inverted color scale", action = { colorInv = true })
         intOpt( "m", "decim", "Pixel decimation factor (default: 1)", (i: Int) => decim = i )
         arg( "input", "Meta file of input to process", (i: String) => input = Some( i ))
         arg( "output", "Image output file", (i: String) => output = Some( i ))
         opt( "no-norm", "Do not apply feature normalization", action = { normalize = false })
      }

      if( !parser.parse( args )) sys.exit( 1 )

      (input, output) match {
         case (Some( in ), Some( out )) =>
            val inFile  = new File( in )
            val outFile = new File( out )
            val metaIn  = FeatureExtraction.Config.fromXMLFile( inFile )
            val inSpec  = AudioFile.readSpec( metaIn.audioInput )

            def secsToFrames( s: Double ) = (s * inSpec.sampleRate + 0.5).toLong

            val span    = (spanStart, spanStop) match {
               case (Some( start ), Some( stop )) => Some( Span( secsToFrames( start ), secsToFrames( stop )))
               case (Some( start ), None)         => Some( Span( secsToFrames( start ), inSpec.numFrames ))
               case (None,          Some( stop )) => Some( Span( 0L, secsToFrames( stop )))
               case (None,          None)         => None
            }
            span.foreach( s => require( s.length > 0, "Span is empty" ))
            val corrFrames = secsToFrames( corrLen )
            require( corrFrames > 0, "Correlation duration is zero" )

            SelfSimilarity.verbose = verbose
            val set              = Config()
            set.metaInput        = inFile
            set.imageOutput      = outFile
            set.span             = span
            set.corrLen          = corrFrames
            set.decimation       = decim
            set.temporalWeight   = temp.toFloat
            set.colors           = colors
            set.colorWarp        = colorWarp.toFloat
            set.colorCeil        = colorCeil.toFloat
            set.colorInv         = colorInv
            set.normalize        = normalize

            if( normalize ) dirOption match {
               case Some( dir )  =>
                  set.databaseFolder   = new File( dir )
               case _ => exit1()
            }

            import SelfSimilarity._
            var lastProg = 0
            SelfSimilarity(set) {
               case Result(Success(_)) =>
                  println( "  Done." )
                  println()
               case Result(Failure(Aborted())) =>
                  println( "  Aborted" )
               case Result(Failure(e)) =>
                  println( "  Failed: " )
                  e.printStackTrace()
               case Progress( perc ) =>
                  val i = perc >> 2
                  while( lastProg < i ) {
                     print( "#" )
                  lastProg += 1 }
            }

         case _ => exit1()
      }
   }

   def featureStats( args: Array[ String ]) {
      var dirOption = Option.empty[ String ]
      var verbose = false

      implicit val parser  = new OptionParser( name + " --stats" ) {
         opt( "v", "verbose", "Verbose output", action = { verbose = true })
         opt( "d", "dir", "<directory>", "Database directory", (s: String) => dirOption = Some( s ))
      }
      if( !parser.parse( args )) sys.exit( 1 )

      val dir = dirOption match {
         case Some( d ) => d
         case None => exit1()
      }

      println( "Starting stats... " )
      val paths = file(dir).children(_.name.endsWith("_feat.aif"))
      import FeatureStats._
      var lastProg = 0
      FeatureStats( paths ) {
         case Result(Success(spans)) =>
            println( "  Success." )
            val afNorm = AudioFile.openWrite( new File( dir, NORMALIZE_NAME ),
               AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, spans.size, 44100 ))
            try {
               val b = afNorm.buffer( 2 )
               spans.zipWithIndex.foreach { case ((min, max), i) =>
                  b( i )( 0 ) = min.toFloat
                  b( i )( 1 ) = max.toFloat
               }
               afNorm.write( b )
            } finally {
               afNorm.close()
            }
            println( "Done." )
         case Result(Failure(Aborted())) =>
            println( "  Aborted" )
         case Result(Failure((e))) =>
            println( "  Failed: " )
            e.printStackTrace()
         case Progress(perc) =>
            val i = perc >> 2
            while( lastProg < i ) {
               print( "#" )
            lastProg += 1 }
      }
   }

   private def exit1()( implicit p: OptionParser ) : Nothing = {
      p.showUsage
      sys.exit( 1 )
   }

   def featurePre( args: Array[ String ]) {
      var inputs     = IndexedSeq.empty[ String ]
      var dirOption  = Option.empty[ String ]
      var verbose    = false
      var chanString = "mix"

      implicit val parser  = new OptionParser( name + " -f" ) {
         opt( "v", "verbose", "Verbose output", action = { verbose = true })
         arglistOpt( "inputs...", "List of input files or directories", inputs +:= _ )
         opt( "d", "dir", "<directory>", "Target directory", (s: String) => dirOption = Some( s ))
         opt( "c", "channels", "(mix|first|last)", "Channel mode (defaults to 'mix')", (s: String) => chanString = s )
      }
      if( !parser.parse( args )) sys.exit( 1 )
      val dir = dirOption match {
         case Some( d ) => d
         case None => exit1()
      }
      if( inputs.isEmpty ) exit1()

      import FeatureExtraction.ChannelsBehavior
      val chanMode: ChannelsBehavior = chanString.toLowerCase match {
         case "mix"     => ChannelsBehavior.Mix
         case "first"   => ChannelsBehavior.First
         case "last"    => ChannelsBehavior.Last
         case _         => exit1()
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

     val targetDir = new File(dir)
     val sb = ExtrConfig()
     sb.channelsBehavior = chanMode

     def iter(list: List[File]) {
       list match {
         case head :: tail =>
           val name1 = {
             val n = head.getName
             val i = n.lastIndexOf('.')
             if (i >= 0) n.substring(0, i) else n
           }
           sb.audioInput    = head
           sb.featureOutput = new File(targetDir, name1 + "_feat.aif")
           sb.metaOutput    = Some(new File(targetDir, name1 + "_feat.xml"))
           feature(sb)(if (_) iter(tail))
         case _ =>
       }
     }
     iter(inFiles)
   }

  def feature(set: ExtrConfig)(whenDone: Boolean => Unit) {
    import FeatureExtraction._
    println("Starting extraction... " + set.audioInput.getName)
    var lastProg = 0
    FeatureExtraction(set) {
      case Result(Success(_)) =>
        println("  Success.")
        whenDone(true)
      case Result(Failure(Aborted())) =>
        println("  Aborted")
        whenDone(false)
      case Result(Failure(e)) =>
        println("  Failed: ")
        e.printStackTrace()
        whenDone(false)
      case Progress(perc) =>
        val i = perc >> 2
        while (lastProg < i) {
          print("#")
          lastProg += 1
        }
    }
  }
}